# Shiny Global File

# Packages
library(BioMonTools)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyWidgets)
# masks shinydashboardPlus::progressBar
# masks shinyjs::alert

# File Size
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 200 MB.
mb_limit <- 200
options(shiny.maxRequestSize = mb_limit * 1024^2)

# Folders
path_data <- file.path("data")
path_results <- file.path("results")
