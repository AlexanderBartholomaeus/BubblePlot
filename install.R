# This is a script to install all packages needed for the bubbleplot
# to execute source the file, type: source('install.R')
# depending on your actual path, you may need to give the path: source(file.path('path_to_script','install.R'))


# list packages to install
myPacks <- c(
  'shiny',
  'shinythemes',
  'ggplot2',
  'svglite',
  'DT',
  'reshape2'
)
# install each
install.packages(myPacks)
