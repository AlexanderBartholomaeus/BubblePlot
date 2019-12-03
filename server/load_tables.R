# asv file
observeEvent(input$importAsv,{
  if(!is.null(input$fileAsv)) {
    # reset warning
    appData$warnCols = 1
    # try to load
    result = tryCatch({
      # infile
      inFile <- input$fileAsv
      # check file type
      if(grep(".xls[x]{0,1}",inFile$datapath)){
        dat <- data.frame(
          read_excel(
            inFile$datapath
          ),
          stringsAsFactors = F
        )
      } else {
        # read table
        dat <- read.table(
          inFile$datapath,
          header=input$headerAsv,
          sep = input$sepAsv,
          stringsAsFactors = F)
      }
      if(input$firstColRownamesAsv && ncol(dat)>1) {
        appData$raw <- dat[,2:ncol(dat)]
        rownames(appData$raw) <- dat[,1]
      } else {
        appData$raw <- dat
      }
      if(ncol(dat) > 1){
        # update col and row select
        updateSelectizeInput(
          session,
          'colSelect',
          choices = colnames(appData$raw)#,
          #selected = colnames(appData$raw)
        )
        updateSelectizeInput(
          session,
          'rowSelect',
          choices = c(rownames(appData$raw),'--ALL--')#,
          #selected = rownames(appData$raw)
        )
        # show message
        showModal(
          modalDialog(
            title = 'File loaded',
            tags$p('File successfully loaded! Please check the table browser before for correct import.'),
            footer = modalButton('OK')
          )
        )
      } else {
        # show message
        showModal(
          modalDialog(
            title = 'File loading error?',
            tags$p('It seems that the file loading is not correct, only 1 column was detected. 
                   Please check the loaded data below and try to reload with different
                   settings.'),
            footer = modalButton('OK')
            )
          )
      }
    }, warning = function(w) {
      # do nothing
    }, error = function(e) {
      showModal(
        modalDialog(
          title = 'Error',
          tags$p(e),
          footer = modalButton('OK')
        )
      )
    }, finally = {
      # do nothing
    })
    # no file selected
    } else {
      showModal(
        modalDialog(
          title = 'No file selected',
          tags$p('Please select a file!'),
          footer = modalButton('OK')
        )
      )
    }
  },ignoreNULL = T)

# taxa file
observeEvent(input$importTaxa,{
  # check ASV data is loaded
  if(is.null(appData$raw)){
    showModal(
      modalDialog(
        title = 'Load ASV table first',
        tags$p('Please load ASV data first to ensure correct processing of the taxa data.'),
        footer = modalButton('OK')
      )
    )
  } else if(!is.null(input$fileTaxa)) {
    result = tryCatch({
      # infile
      inFile <- input$fileTaxa
      # read table
      dat <- read.table(
        inFile$datapath,
        header=input$headerTaxa,
        sep = input$sepTaxa,
        stringsAsFactors = F)
      # check for same dimension of taxa and asv data
      if(nrow(dat) != nrow(appData$raw)){
        showModal(
          modalDialog(
            title = 'Taxa table with wrong row number',
            tags$p('Taxa table has different number of rows than ASV table.'),
            footer = modalButton('OK')
          )
        )
      } else {
        # use first column
        if(input$firstColRownamesTaxa) {
          appData$taxa <- dat[,2:ncol(dat)]
          rownames(appData$taxa) <- dat[,1]
        } else {
          appData$taxa <- dat
        }
        # reordering
        if(input$reorderTaxa){
          matchRows <- match(rownames(appData$taxa),rownames(appData$raw))
          if(length(matchRows) == ncol(appData$raw)){
            appData$taxa <- appData$taxa[match(rownames(appData$taxa),rownames(appData$raw))]
          } else {
            showModal(
              modalDialog(
                title = 'Reordering not successful',
                tags$p('Reordering not successful was not successful due to incomplete matching of rownames with ASV table.'),
                footer = modalButton('OK')
              )
            )
          }
        }
        # update col select and row reordering selection
        updateSelectInput(
          session,
          'colorSelect',
          choices = colnames(appData$taxa),
          selected = ''
        )
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
        # show message
        showModal(
          modalDialog(
            title = 'File loaded',
            tags$p('File successfully loaded! Please check the table browser before for correct import.'),
            footer = modalButton('OK')
          )
        )
      }
    }, warning = function(w) {
      # do nothing
    }, error = function(e) {
      showModal(
        modalDialog(
          title = 'Error',
          tags$p(e),
          footer = modalButton('OK')
        )
      )
    }, finally = {
      # do nothing
    })
    # no file selected 
  } else {
    showModal(
      modalDialog(
        title = 'No file selected',
        tags$p('Please select a file!'),
        footer = modalButton('OK')
      )
    )
  }
},ignoreNULL = T)