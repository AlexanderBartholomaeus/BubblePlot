
### modal to modify legend
modal_legend_modify = function(bubbles = NULL, errorMessage = NULL){
  # check if size already set
  sizePre <- NULL
  if(!is.null(bubbles)){
    sizePre <- paste(bubbles,collapse = ';')
  }
  showModal(
    modalDialog(
      title = 'Modify legend',
      tags$h4('Bubble size'),
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
      tags$h4('Bubble color'),
      sliderInput('legendModifyColorNcol','Number of columns for color legend', min = 1, max = 5, value = 1, step = 1),
      footer = modalButton('OK')
      
    )
  )
}