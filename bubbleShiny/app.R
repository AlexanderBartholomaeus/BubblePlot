

library(shiny)
library(ggplot2)
source('bubblePlot.R')

#options(shiny.maxRequestSize=15*1024^2)

# Define UI for application that draws a histogram
ui <- function(request) {
  fluidPage(
     
     # Application title
     titlePanel("Draw BubblePlot"),
     
     # Sidebar with a slider input for number of bins 
     sidebarLayout(
        sidebarPanel(
          #bookmarkButton(),
          tags$hr(),
          checkboxInput('top100','Top 100 only',value = TRUE),
          # asv file
          fileInput('fileAsv','Select ASV file'),
          textInput('sepAsv','Seperator',value = ';'),
          actionButton('importAsv', 'Load data'),
          tags$hr(),
          # taxa file
          fileInput('fileTaxa','Select taxa file'),
          textInput('sepTaxa','Seperator',value = ';'),
          actionButton('importTaxa', 'Load data'),
          tags$hr(),
          # bubbleplot settings
          sliderInput('bubbleSize','Bubble size',value = 4, step = 1, min = 1, max = 20),
          sliderInput('xlabelsize','X-axis label size',value = 5, step = 1, min = 1, max = 20),
          sliderInput('ylabelsize','Y-axis label size',value = 6, step = 1, min = 1, max = 20)
        ),
        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel(
              title = 'Data loading',
              dataTableOutput('tabAsv'),
              tags$hr(),
              dataTableOutput('tabTaxa')
            ),
            tabPanel(
              title = 'Plot',
              actionButton('colSelectRmAll','Remove all'),
              actionButton('colSelectAddAll','Add all'),
              selectizeInput('colSelect','Select columns',
                             choices = NULL,
                             multiple = TRUE,
                             options = list(plugins = list('remove_button', 'drag_drop'))),
              tags$hr(),
              actionButton('rowSelectRmAll','Remove all'),
              actionButton('rowSelectAddAll','Add all'),
              selectizeInput('rowSelect','Select rows',
                             choices = NULL,
                             multiple = TRUE,
                             options = list(plugins = list('remove_button', 'drag_drop'))),
              checkboxInput('rowSortChoice','Sort by taxonomic rank', value = F),
              selectInput('rowSort','Sort by', choice = NULL),
              tags$hr(),
              selectInput('colorSelect','Select bubble color',choices = NULL),
              tags$hr(),
              actionButton('bubblePlotGo','Create bubble plot'),
              plotOutput('bubblePlot',width=800,height=800)
            )
          )
        )
     )
  )
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  appData <- reactiveValues(
    raw = NULL,
    taxa = NULL
  )
  
  # asv file
  observeEvent(input$importAsv,{
    inFile <- input$fileAsv
    #if(!is.null(inFile)) {
      #dat <- read.table(inFile$datapath, header=T, sep = input$sepAsv, stringsAsFactors = F)
      dat <- read.table('Z:/Alexander Bartholomäus/Atacama_BeateSchneider/2016/pooled/table_ASVs.csv', header=T, sep = input$sepAsv, stringsAsFactors = F)
      if(input$top100){
        appData$raw <- dat[1:100,]
      }
      # update col and row select
      updateSelectizeInput(session,'colSelect',choices = colnames(appData$raw), selected = colnames(appData$raw))
      updateSelectizeInput(session,'rowSelect',choices = rownames(appData$raw), selected = rownames(appData$raw))
    #}
  },ignoreNULL = F)
  output$tabAsv <- renderDataTable(
    head(appData$raw)
  ) 
  # taxa file
  observeEvent(input$importTaxa,{
    inFile <- input$fileTaxa
    #if(!is.null(inFile)) {
      #dat <- read.table(inFile$datapath, header=T, sep = input$sepTaxa, stringsAsFactors = F)
      dat <- read.table('Z:/Alexander Bartholomäus/Atacama_BeateSchneider/2016/pooled/table_ASVs.csv', header=T, sep = input$sepAsv, stringsAsFactors = F)
      if(input$top100){
        appData$taxa <- dat[1:100,]
      }
      # update color select
      #updateSelectInput(session,'colorSelect',choices = colnames(appData$taxa))
      updateSelectInput(session,'colorSelect',choices = colnames(appData$taxa))
      updateSelectInput(session,'rowSort',choices = colnames(appData$taxa))
    #}
  },ignoreNULL = F)
  output$tabTaxa <- renderDataTable(
    head(appData$taxa)
  ) 

  # listen to selection buttons
  observeEvent(input$colSelectRmAll,{
    updateSelectizeInput(session,'colSelect',selected = '')
  })
  observeEvent(input$rowSelectRmAll,{
    updateSelectizeInput(session,'rowSelect',selected = '')
  })
  observeEvent(input$colSelectAddAll,{
    updateSelectizeInput(session,'colSelect',selected = colnames(appData$raw))
  })
  observeEvent(input$rowSelectAddAll,{
    updateSelectizeInput(session,'rowSelect',selected = rownames(appData$raw))
  })
  
  # bubble plot
  bubble <- eventReactive(input$bubblePlotGo,{
    
    dat <- appData$raw[input$rowSelect,input$colSelect]
    tax <- appData$taxa[input$rowSelect,input$colorSelect]
    rowOrd <- NULL
    rowNam <- NULL
    if(input$rowSortChoice){
      rowNam <- paste0(appData$taxa[,input$rowSort],'_',rownames(appData$raw))
      rowOrd <- sort(rowNam)
    } else {
      rowOrd <- input$rowSelect
    }
    #browser()
    g <- bubblePlot(
      dat,
      bubbleColor = tax,
      colOrder = input$colSelect,
      rowOrder = rowOrd,
      rowNames = rowNam,
      bubbleSize = input$bubbleSize,
      xlabelSize = input$xlabelsize,
      ylabelSize = input$ylabelsize)
  })
  output$bubblePlot <- renderPlot({
    plot(bubble())
  })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = 'server')

