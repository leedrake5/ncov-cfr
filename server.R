library(shiny)
options(shiny.maxRequestSize=30*1024^40)

shinyServer(function(input, output, session) {
    
    output$test <- renderDataTable({
        test.files
    })
    
    observeEvent(input$processdata, {
        
    
    processedData <- reactive({
        nll <- function(cfr, death_shape, death_rate, n_days) {
            cfr <- plogis(cfr)
            expected <- numeric(n_days)
            for(i in days) {
                for(j in 1:n_cases) {
                    d <- i - onset[j]
                    if(d >= 0) {
                        expected[i] <- expected[i] + cfr*diff(pgamma(c(d - 0.5, d + 0.5), shape = death_shape, rate = death_rate))
                    }
                }
            }
            ll <- sum(dpois(deaths, expected, log = TRUE))
            return(-ll)
        }
        
        estimates <- as.data.frame(matrix(NA, nrow = length(file.list), ncol = 4))
        names(estimates) <- c("date", "mle", "lower", "upper")
           withProgress(message = 'Now Running:', value = 0, {
               n <- length(file.list)
           for(i in 1:length(file.list)) {
               # Prepare data
               file_date <- ymd(substr(file.path.list[i], 12, 19))
               exports <- file.list[[i]]
               begin <- ymd(exports$date[1])
               cases <- exports$cases
               deaths <- exports$deaths
               n_cases <- sum(cases)
               n_deaths <- sum(deaths)
               n_days <- length(cases)
               days <- 1:n_days
               interval <- seq(1, n_days + 7, 7)
               onset <- rep(days, cases)
               
               # Fit the model
               free <- c(cfr = 0)
               fixed <- c(death_shape = exp(coef(fit_linton)[[1]]), death_rate = exp(coef(fit_linton)[[2]]), n_days=n_days)
               fit <- mle2(nll, start = as.list(free), fixed = as.list(fixed), method = "Brent", lower = -100, upper = 100)
               
               # Write estimates
               estimates[i, 1] <- as_date(file_date)
               estimates[i, 2] <- plogis(coef(fit)[1])
               estimates[i, 3:4] <- plogis(confint(fit))
               incProgress(1/n, detail = paste("Day ", i))
           }
           
           
           })
           # Save estimates
           estimates$date <- as_date(estimates$date)
           #saveRDS(estimates, "out/cfr.rds")
           list(Estimates=estimates, Cases=cases, Deaths=deaths, Fit=fit, Begin=begin, Interval=interval)
    })
    
    mainPlot <- reactive({
        result.list <- processedData()
        estimates <- result.list$Estimates
        cases <- result.list$Cases
        begin <- result.list$Begin
        interval <- result.list$Interval
        deaths <- result.list$Deaths
        plot_grid(estimates=estimates, begin=begin, interval=interval, deaths=deaths, cases=cases)
    })
    
    output$quadplot <- renderPlot({
        mainPlot()
    })
    
    output$quatplotdownlaod <- downloadHandler(
    filename = function() { paste0("COVID-19", '.jpg', sep='') },
    content = function(file) {
        ggsave(file,mainPlot(), device="jpg")
    }
    )
    
    estimatesTable <- reactive({
        result.list <- processedData()
        estimates <- result.list$Estimates
        #estimates$date <- paste0(day(estimates$date), "_", month(estimates$date))
        #estimates$date <- as.Date(estimates$date, '%d%m%Y')
        estimates
    })
    
    output$estimates <- renderTable({
        estimatesTable()
    })
    
    output$estimatesdownload <- downloadHandler(
    filename <- function(){
        paste("COVID-19", "_estimates.csv")
    },
    
    content = function(file) {
        write.csv(x=estimatesTable(),file=file)
        
    }
    )
    

    
    
    
        })
    
})
