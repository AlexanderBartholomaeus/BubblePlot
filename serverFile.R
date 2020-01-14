# define vars
appData <- reactiveValues(
  raw = NULL,
  taxa = NULL,
  warnCols = 1
)

legendData <- reactiveValues(
  bubbleSize = NULL,
  bubbleColorCols = NULL,
  bubbleColorSize = NULL
)

# load data files
source(file.path('server','load_tables.R'), local = T)$value

# tables
output$tabAsv <- renderDataTable(
  head(appData$raw)
) 
output$tabTaxa <- renderDataTable(
  head(appData$taxa)
) 

# add copy as ASV as taxa table
observeEvent(input$useAsv,{
  if(input$useAsv && !is.null(appData$raw)){
    appData$taxa <- appData$raw
    updateSelectInput(
      session,
      'colorSelect',
      choices = colnames(appData$taxa),
      selected = '')
    updateSelectInput(
      session,
      'rowReorder',
      choices = colnames(appData$taxa),
      selected = ''
    )
    updateSelectInput(
      session,
      'sumSelect',
      choices = colnames(appData$taxa),
      selected = ''
    )
  }
})

### listen to selection buttons
# column selection
observeEvent(input$colSelectRmAll,{
  updateSelectizeInput(session,'colSelect',selected = '')
})
observeEvent(input$colSelectAddAll,{
  updateSelectizeInput(session,'colSelect',selected = colnames(appData$raw))
})
# check if columns are numeric and remove
observeEvent(input$colSelect,{
  # if warning is active
  if(appData$warnCols==1){
    colClasses <- sapply(1:length(input$colSelect),function(i){class(appData$raw[,input$colSelect[i]])})
    colClass <- is.element(colClasses,c('numeric','integer'))
    if(sum(colClass) != length(input$colSelect)){
      showModal(
        modalDialog(
          title = 'Non-numeric column selected',
          tags$p('It seems that you selected a non-numeric column that might cause problems during plotting. Please check you selection.'),
          actionButton('colSelectRmNonNumeric','Unselect non-numeric columns'),
          actionButton('colSelectDontAsk','Do not warn me again'),
          footer = modalButton('OK')
        )
      )
    }
  }
})
observeEvent(input$colSelectRmNonNumeric,{
  colClasses <- sapply(1:ncol(appData$raw),function(i){class(appData$raw[,i])})
  colClass <- is.element(colClasses,c('numeric','integer'))
  updateSelectizeInput(session,'colSelect',selected = input$colSelect[colClass])
})
observeEvent(input$colSelectDontAsk,{
  appData$warnCols = 0
})

# row selection
observeEvent(input$rowSelectRmAll,{
  updateSelectizeInput(session,'rowSelect',selected = '')
})
observeEvent(input$rowSelectAddAll,{
  if(length(rownames(appData$raw)) > 200){
    showModal(
      modalDialog(
        title = 'Too many rows',
        tags$p('Too many rows available. It is not recommended to add more than 200 rows. Please select term --ALL-- to keep all rows e.g. for the summarization by taxa.'),
        actionButton('tooManyAddAll','Add all'),
        actionButton('tooManyAdd200','Add first 200'),
        actionButton('tooManyAdd500','Add first 500, just try'),
        footer = modalButton('OK')
      )
    )
  } else {
    updateSelectizeInput(session,'rowSelect',selected = rownames(appData$raw))
  }
})
observeEvent(input$rowSelectAdd20,{
  updateSelectizeInput(session,'rowSelect',selected = rownames(appData$raw)[1:20])
})
observeEvent(input$rowSelectAdd50,{
  updateSelectizeInput(session,'rowSelect',selected = rownames(appData$raw)[1:50])
})
observeEvent(input$rowSelectAdd100,{
  updateSelectizeInput(session,'rowSelect',selected = rownames(appData$raw)[1:100])
})
observeEvent(input$tooManyAddAll,{
  updateSelectizeInput(session,'rowSelect',selected = '--ALL--')
})
observeEvent(input$tooManyAdd200,{
  updateSelectizeInput(session,'rowSelect',selected = rownames(appData$raw)[1:200])
})
observeEvent(input$tooManyAdd500,{
  updateSelectizeInput(session,'rowSelect',selected = rownames(appData$raw)[1:500])
})

# apply reorder on rows / ASV's
observeEvent(input$rowReorderGo,{
  # check if selected
  if(
    !is.null(input$rowReorder) && 
    input$rowReorder != '' && 
    !is.null(input$rowSelect) && 
    length(input$rowSelect) > 0
  ){
    # order 
    rowN <- rownames(appData$raw)[is.element(rownames(appData$raw),input$rowSelect)] # restore original order
    tax <- appData$taxa[,input$rowReorder]
    rowN <- rowN[order(tax)]
    # update
    updateSelectizeInput(session,'rowSelect',selected = rowN)
  } else {
    showModal(
      modalDialog(
        title = 'Select ASV\'s and reorder property',
        tags$p('Please ASV\'s and taxonomic rank to reorder the ASV\'s and try to reorder again.'),
        footer = modalButton('OK')
      )
    )
  }
})

# summarize by
observeEvent(input$sumSelect,{
  if(!is.null(input$sumSelect) && input$sumSelect != ''){
    updateSelectizeInput(session,'sumSelector', choices = appData$taxa[,input$sumSelect], selected = appData$taxa[,input$sumSelect])
  } else {
    updateSelectizeInput(session,'sumSelector',selected = '', choices = '')
  }
})
observeEvent(input$sumSelectorAddAll,{
  if(!is.null(input$sumSelect)){
    updateSelectizeInput(session,'sumSelector',selected = appData$taxa[,input$sumSelect])
  }
})
observeEvent(input$sumSelectorRmAll,{
  updateSelectizeInput(session,'sumSelector',selected = '')
})

# generate bubble plot
#bubble <- eventReactive(input$bubblePlotGo,{
bubble <- reactive({
  if(!is.null(input$rowSelect) && !is.null(input$colSelect)){
  
    #browser()
    # get selected data
    inSel <- input$rowSelect
    if(sum(is.element(input$rowSelect,'--ALL--'))==1){
      dat <- appData$raw[,input$colSelect]
      inSel <- rownames(appData$raw)
    } else {
      dat <- appData$raw[input$rowSelect,input$colSelect]
    }
    
    # summarize
    if(input$sumByTaxa){
      # create
      sumBy <- appData$taxa[inSel,input$sumSelect]
      sumBy[is.na(sumBy)] <- 'NA'
      #dat <- aggregate(dat,by=list(appData$taxa[input$rowSelect,input$sumSelect]),sum)
      dat <- aggregate(dat,by=list(sumBy),sum,na.rm=T)
      rownames(dat) <- dat[,1]
      dat <- dat[,2:ncol(dat)]
      # filter selected
      dat <- dat[input$sumSelector[is.element(input$sumSelector,rownames(dat))],]
    }
    
    # set legend bubble size
    if(!is.null(legendData$bubbleSize) && legendData$bubbleSize!=''){
      legBubbleSize <- legendData$bubbleSize
    }else {
      legBubbleSize <- NULL
    }
    # set legend color columns 
    if(!is.null(legendData$bubbleColorCols) && legendData$bubbleColorCols!=''){
      legColorCols <- legendData$bubbleColorCols
    }else {
      legColorCols <- NULL
    }
    if(!is.null(legendData$bubbleColorSize) && legendData$bubbleColorSize!=''){
      legColorSize <- legendData$bubbleColorSize
    }else {
      legColorSize <- 5
    }
      
    # define color (ignore if summarized)
    if(!is.null(appData$taxa) && nrow(appData$taxa)>0 && !input$sumByTaxa){
      tax <- appData$taxa[inSel,input$colorSelect]
      taxName <- input$colorSelect
    } else if(input$sumByTaxa){
      tax <- rownames(dat)
      taxName <- input$sumSelect
    } else {
      tax <- NULL
      taxName <- NULL
    }
      
    g <- bubblePlot(
      dat,
      bubbleColor = tax,
      bubbleColorName = taxName,
      #colOrder = input$colSelect,
      rowOrder = if(!input$sumByTaxa){input$rowSelect}else{input$sumSelector[is.element(input$sumSelector,rownames(dat))]},
      #rowNames = rowNam,
      bubbleSize = input$bubbleSize,
      stroke = input$bubbleStroke,
      xlabelSize = input$xlabelsize,
      ylabelSize = input$ylabelsize,
      baseTextSize = input$baseSize,
      flipAxis = input$flipAxis,
      legendBubbleSize = legBubbleSize,
      legendColorCols = legColorCols,
      legendColorSize = legColorSize)
  } else {
    showModal(
      modalDialog(
        title = 'Select rows and columns',
        tags$p('Please select rows and columns to plot! Use the tab \'Rows and Columns\' for selection!'),
        footer = modalButton('OK')
      )
    )
  }
})
# make reative to plot size
# output$bubblePlot <- renderUI({
#   tagList(
#     renderPlot(
#       plot(bubble()),
#       width=input$plotWidth,
#       height=input$plotHeight
#     )
#   )
# })
# make png bubble plot directly
output$bubblePlot2 <- renderImage({
  
  ggsave(
    filename = 'bubbleP.png',
    plot = bubble(),
    width = round(input$plotWidth/2,digits = 0),
    height = round(input$plotHeight/2,digits = 0),
    units = 'cm'
  )
  
  # Return a list containing information about the image
  list(src = 'bubbleP.png',
       contentType = "image/png",
       #width = paste0((input$zoom),'%'),
       #height = paste0((input$zoom),'%'),
       alt = "Bubble plot")
})
# reactive ui width
# output$bubblePlotUi <- renderUI({
#   imageOutput('bubblePlot2',width=paste0(input$zoom,'px'),height=paste0(input$plotHeight,'px'))
# })

# modify legend
source(file.path('server','legend_modify.R'), local = T)$value

# save and export
source(file.path('server','save_export.R'), local = T)$value