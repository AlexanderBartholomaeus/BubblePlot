# open modal to modify
observeEvent(input$legendModify,{
  modal_legend_modify(
    bubbles = legendData$bubbleSize
  )
})
# try to modify
observeEvent(input$legendModifyBubbleGo,{
  bubSize <- as.numeric(gsub("\\s","",unlist(strsplit(input$legendModifyBubble,";"))))
  #browser()
  if(!is.null(bubSize) && bubSize != '' && !is.na(bubSize) && length(bubSize) > 0 && is.numeric(bubSize)){
    legendData$bubbleSize <- bubSize[!is.na(bubSize)]
  } else {
    legendData$bubbleSize <- NULL
    # build error message
    modal_legend_modify(
      bubbles = legendData$bubbleSize,
      errorMessage = 'Some thing went wrong when parsing your input. Please 
      check that you used ; to seperate you numbers!'
    )
  }
})
# apply default legend bubble size
observeEvent(input$legendModifyBubbleDefault,{
  legendData$bubbleSize <- NULL
})
# change the number of columns for legend
observeEvent(input$legendModifyColorNcol,{
  legendData$bubbleColorCols <- input$legendModifyColorNcol
})