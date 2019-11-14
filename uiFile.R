fluidPage(theme = shinytheme('spacelab'),
  
  # Application title
  titlePanel("Draw BubblePlot"),
  
  tabsetPanel(
    tabPanel(
      title = 'Data loading',
      style = 'margin-left:25px;',
      # asv file
      tags$h4('Select ASV/OTU file'),
      fileInput('fileAsv','Select ASV file'),
      selectInput('sepAsv','Separator', choices = c(';','tab'='\t',','), selected = ';'),
      checkboxInput('headerAsv','Header',value = TRUE),
      checkboxInput('firstColRownamesAsv','Take first columns as rownames',value = FALSE),
      actionButton('importAsv', 'Load data'),
      tags$hr(),
      # taxa file
      tags$h4('Select taxa file'),
      #checkboxInput('useAsv','Taxa in ASV file',value = FALSE),
      actionButton('useAsv','Taxa are in the ASV file'),
      tags$h4(' ... or selecte a separate taxonomy file:'),
      tags$p('The taxonomy file must have the same ordering of rows as the ASV file. 
             Alternatively you can reorder by rownames by selection the checkbox'),
      fileInput('fileTaxa','Select taxa file'),
      selectInput('sepTaxa','Separator', choices = c(';','tab'='\t',','), selected = ';'),
      checkboxInput('headerTaxa','Header',value = TRUE),
      checkboxInput('firstColRownamesTaxa','Take first columns as rownames',value = FALSE),
      checkboxInput('reorderTaxa','Reorder taxa table by rownames',value = FALSE),
      actionButton('importTaxa', 'Load data'),
      tags$hr(),
      # head data tables
      tags$h4('ASV/OTU table'),
      dataTableOutput('tabAsv'),
      tags$hr(),
      tags$h4('Taxonomy table'),
      dataTableOutput('tabTaxa')
    ),
    tabPanel(
      title = 'Rows and Columns',
      style = 'margin-left:25px;',
      tags$h4('Select and order columns'),
      actionButton('colSelectRmAll','Remove all'),
      actionButton('colSelectAddAll','Add all'),
      selectizeInput('colSelect','Select columns',
                     choices = NULL,
                     multiple = TRUE,
                     options = list(plugins = list('remove_button', 'drag_drop')),
                     width = 700),
      tags$hr(),
      tags$h4('Select and order rows'),
      actionButton('rowSelectRmAll','Remove all'),
      actionButton('rowSelectAddAll','Add all'),
      actionButton('rowSelectAdd20','Add top 20'),
      actionButton('rowSelectAdd50','Add top 50'),
      actionButton('rowSelectAdd100','Add top 100'),
      selectizeInput('rowSelect','Select rows',
                     choices = NULL,
                     multiple = TRUE,
                     options = list(plugins = list('remove_button', 'drag_drop')),
                     width = 700),
      selectInput('rowReorder','Reorder ASV\'s by taxonomy', choices = NULL),
      actionButton('rowReorderGo', 'Apply reordering'),
      tags$hr(),
      tags$h4('Select bubble coloring'),
      selectInput('colorSelect','Color based on',choices = NULL),
      tags$hr(),
      tags$h4('Summarize by taxonomy'),
      tags$p('Summarizing by taxonomy sums up abundances by on the selected taxonimc rank.'),
      checkboxInput('sumByTaxa', 'Summuarize by taxonomy',value = FALSE),
      selectInput('sumSelect','Sum by',choices = NULL),
      actionButton('sumSelectorAddAll','Add all'),
      actionButton('sumSelectorRmAll','Remove all'),
      selectizeInput('sumSelector','Select taxa',
                     choices = NULL,
                     multiple = TRUE,
                     options = list(plugins = list('remove_button', 'drag_drop')),
                     width = 700),
      tags$span(height=150)
    ),
    
    tabPanel(
      title = 'Bubble Plot',
      
      tags$br(),
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
        sidebarPanel(
          width = 3,
          #actionButton('bubblePlotGo','Create/update bubble plot'),
          #tags$hr(),
          # bubbleplot settings
          sliderInput('bubbleSize','Bubble size',value = 4, step = 1, min = 1, max = 50),
          sliderInput('bubbleSizeMin','Minimal bubble size',value = 0, step = 1, min = 0, max = 10),
          sliderInput('bubbleStroke','Bubble stroke(border)',value = 0.3, step = 0.1, min = 0, max = 2),
          sliderInput('xlabelsize','X-axis label size',value = 3, step = 1, min = 1, max = 20),
          sliderInput('ylabelsize','Y-axis label size',value = 3, step = 1, min = 1, max = 20),
          checkboxInput('flipAxis','Flip axis', value = F),
          sliderInput('baseSize','Legend text size',value = 3, step = 1, min = 1, max = 20),
          sliderInput('plotWidth','Plot width',value = 10, step = 1, min = 2, max = 30),
          sliderInput('plotHeight','Plot height',value = 10, step = 1, min = 2, max = 30),
          actionButton('legendModify','Modify legend')
        ),
        # Show a plot of the generated distribution
        mainPanel(
          #uiOutput('bubblePlot'),
          #plotOutput('bubblePlot')
          #tags$hr(),
          imageOutput('bubblePlot2',width='100%',height='100%')
        )
      )
    ),
    tabPanel(
      title = 'Save plot',
      style = 'margin-left:25px;',
      tags$br(),
      downloadButton('savePng','Download PNG'),
      tags$hr(),
      downloadButton('savePdf','Download PDF'),
      tags$hr(),
      downloadButton('saveSvg','Download SVG'),
      tags$hr(),
      downloadButton('saveRdata','Download RData including all parameters')
    )
  )
  
  
  
)