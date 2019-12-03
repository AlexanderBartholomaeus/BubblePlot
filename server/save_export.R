# download PNG
output$savePng <- downloadHandler(
  filename <- function() {
    paste("bubblePlot", "png", sep=".")
  },
  content = function(file){
    ggsave(
      filename = file,
      plot = bubble(),
      width = round(input$plotWidth/2,digits = 0),
      height = round(input$plotHeight/2,digits = 0),
      units = 'cm'
    )
  }
)

# download PDF
output$savePdf <- downloadHandler(
  filename = 'bubblePlot.pdf',
  content = function(file){
    ggsave(
      file,
      bubble(),
      width = round(input$plotWidth/2,digits = 0),
      height = round(input$plotHeight/2,digits = 0),
      units = 'cm'
    )
  }
)

# download SVG
output$saveSvg <- downloadHandler(
  filename = 'bubblePlot.svg',
  content = function(file){
    ggsave(
      file,
      bubble(),
      width = round(input$plotWidth/2,digits = 0),
      height = round(input$plotHeight/2,digits = 0),
      units = 'cm'
    )
  }
)

# download RData object
output$saveRdata <- downloadHandler(
  filename = 'bubblePlot.RData',
  content = function(file){
    # create dat
    dat <- appData$raw[input$rowSelect,input$colSelect]
    if(!is.null(appData$taxa) && nrow(appData$taxa)>0){
      tax <- appData$taxa[input$rowSelect,input$colorSelect]
    } else {
      tax <- NULL
    }
    bubbleData <- list(
      asvData = appData$raw, 
      taxaData = appData$taxa,
      bubblePlot = bubble(),
      bubblePlotPara = list(
        dat,
        bubbleColor = tax,
        colOrder = input$colSelect,
        rowOrder = input$rowSelect,
        bubbleSize = input$bubbleSize,
        stroke = input$bubbleStroke,
        xlabelSize = input$xlabelsize,
        ylabelSize = input$ylabelsize,
        baseTextSize = input$baseSize
      )
    )
    # save
    save(
      bubbleData,
      file = file
    )
  }
)