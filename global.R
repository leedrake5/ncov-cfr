# Estimating case fatality ratio (CFR) of COVID-19
# Christian L. Althaus, 15 February 2020

# Making this a shiny App
# Lee Drake, 16 February 2020

###Check for existing versions of packages, and install if necessary
#list.of.packages <- c("lubridate", "bbmle", "plotrix", "grid", "shinythemes")
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) lapply(new.packages, function(x) install.packages(x, repos="http://cran.rstudio.com/", dep = TRUE))

layOut = function(...) {
    
    require(grid)
    
    x <- list(...)
    n <- max(sapply(x, function(x) max(x[[2]])))
    p <- max(sapply(x, function(x) max(x[[3]])))
    pushViewport(viewport(layout = grid.layout(n, p)))
    
    for (i in seq_len(length(x))) {
        print(x[[i]][[1]], vp = viewport(layout.pos.row = x[[i]][[2]],
        layout.pos.col = x[[i]][[3]]))
    }
}

# Load libraries
library(lubridate)
library(bbmle)
library(plotrix)


# Estimating gamma distribution of onset of death from Linton et al. (http://dx.doi.org/10.1101/2020.01.26.20018754)
linton <- function(shape, rate) {
    ssr <- sum((qgamma(c(0.05, 0.5, 0.95), shape = exp(shape), rate = exp(rate)) - c(6.1, 14.3, 28.0))^2)
    return(ssr)
}
free_linton <- c(shape = log(4), rate = log(4/14.3))
fit_linton <- mle2(linton, start = as.list(free_linton), method = "Nelder-Mead")

ncov_dist <- function(shape=log(4), rate=log(4/14.3)){
    free_linton <- c(shape = shape, rate = rate)
    fit_linton <- mle2(linton, start = as.list(free_linton), method = "Nelder-Mead")
    curve(dgamma(x, exp(coef(fit_linton)[[1]]), exp(coef(fit_linton)[[2]])), 0, 40, col = "steelblue", xlab = "Time from onset to death (days)", ylab = "Probability density", frame = FALSE)
}

# Likelihood and expected mortality function
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

# Analyze all data sets of observed COVID-19 cases outside China
# Source: WHO, ECDC and international media
file.path.list <- list.files("data", pattern = ".csv")
file.list <- lapply(file.path.list, function(x) read.csv(paste0("data/", x)))
test.files <- do.call(rbind, file.list)



# Plot the most recent data set
first_bar_plot <- function(cases=cases, begin, interval){
    barplot(cases,
            col = "steelblue", xlab = "Data: WHO Situation Reports", ylab = "Cases",
            main = "Symptom onset in cases outside China", axes = FALSE, frame = FALSE) + axis(1, interval, begin + interval - 1) + axis(2)
}

second_bar_plot <- function(cases, deaths, begin, interval){
    barplot(deaths,
            ylim = c(0, max(cases)),
            col = "tomato", xlab = "Data: WHO, ECDC, Media", ylab = "Deaths",
            main = "Deaths among cases outside China", axes = FALSE, frame = FALSE) + axis(1, interval, begin + interval - 1) + axis(2)
}

# Plot the estimates
estimates_plot <- function(estimates=estimates){
    plotCI(estimates$date, estimates$mle,
           ui = estimates$upper, li = estimates$lower,
           ylim = c(0, 0.2), pch = 16, col = "steelblue",
           xlab = NA, ylab = "Case fatality ratio", axes = FALSE, frame = FALSE)
    axis(1, estimates$date, paste0(day(estimates$date), "/", month(estimates$date)))
    axis(2)
}

plot_grid <- function(estimates, begin, interval, deaths, cases, shape=log(4), rate=log(4/14.3)){
    par(mfrow = c(2, 2))
    first_bar_plot(cases=cases, begin=begin, interval=interval)
    second_bar_plot(cases=cases, deaths=deaths, begin=begin, interval=interval)
    ncov_dist(shape=shape, rate=rate)
    estimates_plot(estimates=estimates)
}

