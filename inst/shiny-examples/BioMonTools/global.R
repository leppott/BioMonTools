# Shiny Global File

# Version ----
pkg_version <- "1.0.2.9036"

# Packages----
library(BioMonTools)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyWidgets)
library(shinyBS)
library(DT)
library(writexl)
# masks shinydashboardPlus::progressBar
# masks shinyjs::alert

# Source ----
main_db_sb         <- source("external/main_db_sb.R", local = TRUE)$value
main_db_body       <- source("external/main_db_body.R", local = TRUE)$value
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
for (i in dir_results_sub) {
  dir_new <- file.path("results", i)
  if (dir.exists(dir_new) == FALSE) {
    dir.create(dir_new)
  } else {
    message(paste0("Directory already exists; ", i))
  }## IF ~ dir.exists
}## FOR ~ i

# calcmet----
sel_community <- c("bugs", "fish", "algae")

# taxamaps ----
maps_database <- c("world", "usa", "state", "county")

# Copy MetricNames
# fn_metrics <- "MetricNames.xlsx"
# path_metrics <- paste0("https://github.com/leppott/BioMonTools/raw/main/inst/extdata/"
#                        , fn_metrics)
# file.copy(path_metrics, file.path("www", fn_metrics), overwrite = TRUE)

# Copy Metrics File to Shiny www for download
# fn_metrics <- "MetricNames.xlsx"
# pn_metrics <- file.path(system.file(package="BioMonTools")
#                         , "extdata"
#                         , fn_metrics)
#
# dn_shiny <- file.path("www")
# pn_shiny <- file.path(dn_shiny, fn_metrics)
#
# file.copy(pn_metrics, pn_shiny, overwrite = TRUE)





