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
#source("external/sb_main.R")
sb_main <- source("external/sb_main.R", local = TRUE)$value
# tab_Help    <- source("external/tab_Help.R", local = TRUE)$value
# tab_Combine <- source("external/tab_Combine.R", local = TRUE)$value
# tab_Calc    <- source("external/tab_Calc.R", local = TRUE)$value
# tab_Plot    <- source("external/tab_Plot.R", local = TRUE)$value
#tab_mainbody  <- source("external/tab_mainbody.R", local = TRUE)$value
db_main       <- source("external/db_main.R", local = TRUE)$value
tab_code_about     <- source("external/tab_about.R", local = TRUE)$value
tab_code_import  <- source("external/tab_import.R", local = TRUE)$value
tab_code_subsample <- source("external/tab_subsample.R", local = TRUE)$value
tab_code_markexcl  <- source("external/tab_markexcl.R", local = TRUE)$value
tab_code_taxamaps  <- source("external/tab_taxamaps.R", local = TRUE)$value
tab_code_calcmet   <- source("external/tab_calcmet.R", local = TRUE)$value

# File Size ----
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 200 MB.
mb_limit <- 200
options(shiny.maxRequestSize = mb_limit * 1024^2)

# Folders----
path_data <- file.path("data")
path_results <- file.path("results")

# calcmet----
sel_community <- c("bugs", "fish", "algae")
