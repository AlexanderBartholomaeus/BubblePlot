library(reshape2)
library(ggplot2)

#' Bubble plot
#' 
#' @param df Data frame or data matrix with OTU/ASV's in rows and sample in columns 
#' @param colNames Set names of the columns same order as in df
#' @param colOrder Reorder column names by name or index (index starting with 1)
#' @param rowNames Set names of the rows same order as in df 
#' @param rowOrder Reorder row names by name or index (index starting with 1)
#' @param bubbleColor Color of bubbles
#' @param bubbleSize Scale the bubbles
#' @param stroke Set bubbles stroke strength
#' @param flipAxis Flip axis (TRUE/FALSE)
#' @export 
bubblePlot <- function(
  df,
  colNames = NULL,
  colOrder = NULL,
  rowNames = NULL,
  rowOrder = NULL,
  bubbleColor = NULL,
  bubbleSize = 4,
  stroke = 1,
  flipAxis = FALSE,
  xlabelSize = 5,
  ylabelSize = 6,
  baseTextSize = 5){
  
  #browser()
  
  # make copy of df
  df_new <- df
  
  # rename data
  if(!is.null(colNames)){
    # check dimension
    if(length(unique(colNames)) != ncol(df)){
      stop('colNames length not identical with column number of df! Note: colNames must be unique!')
    }
    # rename
    colnames(df_new) <- colNames 
  }
  if(!is.null(rowNames)){
    # check dimension
    if(length(unique(rowNames)) != nrow(df)){
      stop('rowNames length not identical with row number of df! Note: rowNames must be unique!')
    }
    # rename
    rownames(df_new) <- rowNames 
  }
  
  # store rownames for mapping of colors
  rowNames2 <- rownames(df_new)
  
  # melt
  df_new <- melt(as.matrix(df_new))
  
  # add color
  # ToDo: find better solution of coloring
  if(!is.null(bubbleColor)){
    # check dimension
    if(length(bubbleColor) != nrow(df)){
      stop('bubbleColor not with correct dimension')
    }
    # add color
    df_new <- merge(df_new,cbind(rowNames2,bubbleColor),by.x=1,by.y=1)
    colnames(df_new)[4] <- 'color'
  } 
  
  # reorder
  if(!is.null(colOrder)){
    # check dimension
    # ToDo: build a check, which checks with column names and works also if column names are not changed
    # rename
    df_new$Var2 <- factor(df_new$Var2, levels = rev(colOrder)) 
  }
  if(!is.null(rowOrder)){
    # check dimension
    # ToDo: build a check
    # rename
    df_new$Var1 <- factor(df_new$Var1, levels = rowOrder) 
  }
  
  # set 0 values to NA to not appear in the plot
  df_new$value[df_new$value == 0] <- NA
  
  # plot
  g <- ggplot(
    df_new, 
    aes(
      y = Var2, 
      x = Var1
    )
  )
  # color set
  if(!is.null(bubbleColor)){
    g <- g + geom_point(
      aes(
        size = value, 
        fill = color
      ),
      colour = 'black',
      stroke = stroke,
      shape = 21
    ) 
  } else {
    g <- g + geom_point(
      aes(
        size = value
      ),
      colour = 'black',
      fill='grey',
      stroke = stroke,
      shape = 21
    ) 
  }
  g <- g + 
    scale_size(range = c(0, bubbleSize)) +
    scale_colour_discrete() +  
    theme_light(base_size = baseTextSize) +
    #theme_minimal(base_size = 5) +
    theme(
      axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        vjust = 0
      ),
      #legend.spacing.x = unit(round(baseTextSize/10,digits=0),units = 'mm'),
      #legend.spacing.y = unit(round(baseTextSize/10,digits=0),units = 'mm'),
      legend.key.size = unit(round(baseTextSize/5,digits=0),units = 'mm')
    ) +
    theme(axis.text.y = element_text(size = ylabelSize)) +
    theme(axis.text.x = element_text(size = xlabelSize)) +
    #guides(colour = FALSE) +
    #guides(fill = FALSE)
    # scale_y_discrete(
    #   labels = sampleOrder
    # )
    # remove axis headings
    xlab('') +
    ylab('')
  
  # flip axis
  if(flipAxis){
    g <- g +
      coord_flip() +
      scale_x_discrete(limits = rev(levels(df_new$Var1))) +
      scale_y_discrete(limits = rev(levels(df_new$Var2)))
  }
  
  # return
  g
}