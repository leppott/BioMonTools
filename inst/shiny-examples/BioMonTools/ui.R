#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Sources Pages ####
tab_Help    <- source("external/tab_Help.R", local = TRUE)$value
# tab_Combine <- source("external/tab_Combine.R", local = TRUE)$value
 tab_Import  <- source("external/tab_Import.R", local = TRUE)$value
# tab_Calc    <- source("external/tab_Calc.R", local = TRUE)$value
# tab_Plot    <- source("external/tab_Plot.R", local = TRUE)$value

# Define UI
# shinyUI(
#   navbarPage("BioMonTools, v0.5.0.9062"
#              , tab_Help()
#              #, tab_Combine()
#              , tab_Import()
#              #, tab_Calc()
#              #, tab_Plot()
#   )##navbarPage ~ END
# )##shinyUI~END

dashboardPage(
  dashboardHeader(title = "BioMonTools")
  , dashboardSidebar()
  , dashboardBody()
) ## dashboardPage ~ END

# https://rstudio.github.io/shinydashboard/get_started.html

