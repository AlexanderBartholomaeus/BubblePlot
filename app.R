
library(shiny)
library(shinythemes)
library(ggplot2)
library(reshape2)
library(plyr)
library(svglite)
library(DT)
library(readxl)
source('bubblePlot.R')
source('functions/modals.R')

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

