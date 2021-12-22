# Shiny Global File

# Packages----
library(BioMonTools)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyWidgets)
library(shinyBS)
library(DT)
# masks shinydashboardPlus::progressBar
# masks shinyjs::alert

# Source ----
sb_main            <- source("external/sb_main.R", local = TRUE)$value
db_main            <- source("external/db_main.R", local = TRUE)$value
tab_code_about     <- source("external/tab_about.R", local = TRUE)$value
tab_code_import    <- source("external/tab_import.R", local = TRUE)$value
tab_code_subsample <- source("external/tab_subsample.R", local = TRUE)$value
tab_code_markexcl  <- source("external/tab_markexcl.R", local = TRUE)$value
tab_code_taxamaps  <- source("external/tab_taxamaps.R", local = TRUE)$value
tab_code_calcmet   <- source("external/tab_calcmet.R", local = TRUE)$value

# File Size ----
# By default, the file size limit is 5MB.
mb_limit <- 200
options(shiny.maxRequestSize = mb_limit * 1024^2)

# Folders----
path_data <- file.path("data")
path_results <- file.path("results")

# create results subfolders
dir_results_sub <- c("subsample", "markexcl", "taxamaps", "calcmet")
for (i in dir_results_sub){
  dir_new <- file.path("results", i)
  if(dir.exists(dir_new) == FALSE) {
    dir.create(dir_new)
  } else {
    message(paste0("Directory already exists; ", i))
  }## IF ~ dir.exists
}## FOR ~ i

# calcmet----
sel_community <- c("bugs", "fish", "algae")

# taxamaps ----
maps_database <- c("world", "usa", "state", "county")

