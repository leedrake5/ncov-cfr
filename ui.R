library(shinythemes)
library(shiny)

ui=list(
tagList(
header=tags$head(tags$style(".table .alignRight {color: black; text-align:right;}"))),


shinyUI(navbarPage("COVID-19 Estimates", id="nav", theme = shinytheme("yeti"),

tabPanel("Outside China",
div(class="outer",
headerPanel("Estimating case fatality ratio of COVID-19 from observed cases outside China"),
fluidRow(
sidebarLayout(
sidebarPanel(width=3,
actionButton('processdata', "Process")
),
mainPanel(
tabsetPanel(
tabPanel("Plots",
plotOutput('quadplot', height="800px"))
#tabPanel("Estimates",
#tableOutput('estimates'),
#tags$hr(),
#downloadButton('estimatesdownload', "Estimates"))


)
)
)
)
)
)
)
)
)
