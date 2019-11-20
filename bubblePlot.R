library(ggplot2)
library(plyr)

#' Bubble plot
#' 
#' @param df Data frame or data matrix with OTU/ASV's in rows and sample in columns 
#' @param colNames Set names of the columns same order as in df
# @param colOrder Reorder column names by name or index (index starting with 1) ! sort via input data frame
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
  #colOrder = NULL,
  rowNames = NULL,
  rowOrder = NULL,
  bubbleColor = NULL,
  bubbleColorSet = NULL,
  bubbleColorName = NULL,
  bubbleSize = 4,
  stroke = 1,
  flipAxis = FALSE,
  xlabelSize = 5,
  ylabelSize = 6,
  baseTextSize = 5,
  legendBubbleSize = NULL,
  legendColorCols = NULL,
  legendColorSize = 5,
  backgroundHrect = NULL,
  backgroundVline = NULL,
  backgroundVlineWidth = 1,
  backgroundHline = NULL,
  backgroundHlineWidth = 1){
  
  #browser()
  
  # make copy of df
  df_new <- df
  
  # store rownames for mapping of colors
  rowNames2 <- rownames(df_new)
  
  # melt
  rownames(df_new)[rownames(df_new)=='NA'] <- 'N###A' # rename NA to not lose them
  df_new <- reshape2::melt(as.matrix(df_new))
  if(sum(df_new$Var1=='N###A')>0){
    df_new$Var1 <- plyr::mapvalues(df_new$Var1,'N###A','NA') # rename back
  }
  
  # add color
  # ToDo: find better solution of coloring
  if(!is.null(bubbleColor)){
    # check dimension
    if(length(bubbleColor) != nrow(df)){
      stop('bubbleColor not with correct dimension')
    }
    # add color
    df_new <- merge(df_new,cbind(rowNames2,bubbleColor),by.x=1,by.y=1)
    # add color name for legend
    colnames(df_new)[4] <- 'color'
    # if bubbleColorSet give (to prevent color over plots)
    # if(!is.null(bubbleColorSet)){
    #   df_new$color <- factor(df_new$color,levels=c(sort(unique(levels(df_new$color),bubbleColorSet)),'ABC'))
    # }
  } 
  
  # reorder
  # !!! not necessary sort via input data frame
  # if(!is.null(colOrder)){
  #   # check dimension
  #   # ToDo: build a check, which checks with column names and works also if column names are not changed
  #   # rename
  #   df_new$Var2 <- factor(df_new$Var2, levels = rev(colOrder)) 
  # }
  # if(!is.null(rowOrder)){
  #   # check dimension
  #   # ToDo: build a check
  #   # rename
  #   df_new$Var1 <- factor(df_new$Var1, levels = rowOrder) 
  # }
  
  # rename data
  if(!is.null(colNames)){
    # check dimension
    if(length(unique(colNames)) != ncol(df)){
      stop('colNames length not identical with column number of df! Note: colNames must be unique!')
    }
    # rename
    df_new$Var2 <- plyr::mapvalues(df_new$Var2,names(colNames),colNames)
  }
  if(!is.null(rowNames)){
    # check dimension
    if(length(unique(rowNames)) != nrow(df)){
      stop('rowNames length not identical with row number of df! Note: rowNames must be unique!')
    }
    # rename
    df_new$Var1 <- plyr::mapvalues(df_new$Var1,names(rowNames),rowNames)
  }
  
  # set 0 values to NA to not appear in the plot
  df_new$value[df_new$value == 0] <- NA
  
  # plot
  g <- ggplot2::ggplot(
    df_new, 
    aes(
      y = Var2, 
      x = Var1
    )
  )
  
  # flip and reorder axis and make discrete
  if(flipAxis){
    g <- g +
      ggplot2::coord_flip() +
      ggplot2::scale_x_discrete(limits = rev(levels(df_new$Var1))) +
      ggplot2::scale_y_discrete(limits = levels(df_new$Var2))
  } else {
    g <- g +
      ggplot2::scale_x_discrete(limits = levels(df_new$Var1)) +
      ggplot2::scale_y_discrete(limits = rev(levels(df_new$Var2)))
  }
  
  # add background rectangles
  if(!is.null(backgroundHrect)){
    for(i in 1:nrow(backgroundHrect)){
      # color grey
      g <- g + ggplot2::geom_rect(
        xmin=backgroundHrect$x1[i],
        xmax=backgroundHrect$x2[i],
        ymin=backgroundHrect$y1[i],
        ymax=backgroundHrect$y2[i],                  
        color=NA,
        fill='grey90',
        alpha=0.05
      )
      # make rest white
      if(i == 1){
        # for first round color till the beginning
        g <- g + ggplot2::geom_rect(
          xmin=backgroundHrect$x1[i],
          xmax=backgroundHrect$x2[i],
          ymin=-Inf,
          ymax=backgroundHrect$y1[i],                  
          color=NA,
          fill='white',
          alpha=0.05
        )
      }
      # for other round color after the segemt
      if(i == nrow(backgroundHrect)){
        yMin = ymin=backgroundHrect$y2[i]
        yMax = Inf
      } else {
        yMin=backgroundHrect$y2[i]
        yMax=backgroundHrect$y1[i+1]
      }
      g <- g + ggplot2::geom_rect(
        xmin=backgroundHrect$x1[i],
        xmax=backgroundHrect$x2[i],
        ymin=yMin,
        ymax=yMax,                  
        color=NA,
        fill='white',
        alpha=0.05
      )
    }
  }
  # add background lines
  if(!is.null(backgroundHline)){
    g <- g + ggplot2::geom_hline(
      yintercept = backgroundHline + 0.5, 
      linetype='dashed',
      color='grey50',
      size = backgroundHlineWidth
    )
  }
  if(!is.null(backgroundVline)){
    g <- g + ggplot2::geom_vline(
      xintercept = backgroundxline + 0.5, 
      linetype='dashed',
      color='grey50',
      size = backgroundVlineWidth
    )
  }
  
  # color set
  if(!is.null(bubbleColor)){
    g <- g + ggplot2::geom_point(
      aes(
        size = value, 
        fill = color
      ),
      colour = 'black',
      stroke = stroke,
      shape = 21
    ) 
  } else {
    g <- g + ggplot2::geom_point(
      aes(
        size = value
      ),
      colour = 'black',
      fill='grey',
      stroke = stroke,
      shape = 21
    ) 
  }
  # add custome coloring
  if(!is.null(bubbleColorSet)){
    g <- g + ggplot2::scale_fill_manual(
      values = bubbleColorSet
    )
  }
  
  # theme and axis
  g <- g + 
    ggplot2::scale_colour_discrete() +  
    ggplot2::theme_light(base_size = baseTextSize) +
    #theme_minimal(base_size = 5) +
    ggplot2::theme(
      axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        vjust = 0,
        size = xlabelSize 
      ),
      axis.text.y = element_text(size = ylabelSize), 
      legend.key.size = unit(round(legendColorSize/3,digits=1),units = 'mm') # color legend size
    ) +
    #guides(colour = FALSE) +
    #guides(fill = FALSE)
    # scale_y_discrete(
    #   labels = sampleOrder
    # )
    # remove axis headings
    xlab('') +
    ylab('')
  
  # set legend color name
  if(!is.null(bubbleColorName)){
    g <- g + ggplot2::guides(
      fill = guide_legend(
        title=bubbleColorName
        )
      )
  }
  # set legend color columns
  if(!is.null(legendColorCols)){
    g <- g + ggplot2::guides(
      fill = guide_legend(
        ncol = legendColorCols
      )
    )
  }
  
  # set legend bubble size
  if(!is.null(legendBubbleSize)){
    g <- g + ggplot2::scale_size(
      name = 'Abundance',
      range = c(0, bubbleSize),
      breaks = legendBubbleSize,
      labels = gsub("0+$","",as.character(legendBubbleSize))
    )
  } else {
    g <- g + ggplot2::scale_size(
      name = 'Abundance',
      range = c(0, bubbleSize)
    )
  }
  
  # return
  g
}