#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define UI
# shinyUI(
#   navbarPage("BioMonTools, v1.0.0.9001"
#              , tab_Help()
#              #, tab_Combine()
#              , tab_Import()
#              #, tab_Calc()
#              #, tab_Plot()
#   )##navbarPage ~ END
# )##shinyUI~END

dashboardPage(
  header = dashboardHeader(title = "BioMonTools")
  , sidebar = dashboardSidebar(main_db_sb("leftsidebarmenu"))
  , body = dashboardBody(main_db_body("dbBody"))
) ## dashboardPage ~ END

# https://rstudio.github.io/shinydashboard/get_started.html

