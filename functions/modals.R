
### modal to modify legend
modal_legend_modify = function(bubbles = NULL, errorMessage = NULL, bubbleColorSize = 1, bubbleColorCols = 1){
  # check if size already set
  sizePre <- NULL
  if(!is.null(bubbles)){
    sizePre <- paste(bubbles,collapse = ';')
  }
  showModal(
    modalDialog(
      title = 'Modify legend',
      tags$h4('Abundance legend'),
      tags$p(
        'Set the legend BubbleSize. Enter numbers separated by ; to get custom 
        sizes or empty field to get default values. Note: the numbers must be in 
        the range of possible values, otherwise they are not shown.'),
      tags$p(
        style='color:red;',
        HTML(errorMessage)
      ),
      textInput('legendModifyBubble', label = 'Enter bubble sizes', value = sizePre),
      actionButton('legendModifyBubbleGo', 'Set legend bubble sizes'),
      actionButton('legendModifyBubbleDefault', 'Set default size'),
      tags$hr(),
      tags$h4('Color legend'),
      sliderInput(
        inputId = 'legendModifyColorSize',
        label = 'Size of bubbles for color legend', 
        min = 0.1, max = 5, 
        value = bubbleColorSize,
        step = 0.1
      ),
      sliderInput(
        inputId = 'legendModifyColorNcol',
        label = 'Number of columns for color legend', 
        min = 1, max = 5, value = bubbleColorCols, step = 1
      ),
      footer = modalButton('OK')
      
    )
  )
}