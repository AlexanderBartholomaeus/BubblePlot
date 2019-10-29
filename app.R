

library(shiny)
library(shinythemes)
library(ggplot2)
library(svglite)
library(DT)
source('bubblePlot.R')

options(shiny.maxRequestSize=15*1024^2)

# Define UI for application that draws a histogram
ui <- function(request) {
  source('uiFile.R', local = T)$value
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  source('serverFile.R', local = T)$value
  
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = 'server')

