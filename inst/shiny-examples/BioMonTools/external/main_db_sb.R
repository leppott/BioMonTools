#Sidebar----
#sb_main <- function(id) {
function(id) {
  dashboardSidebar(
    width = 275
    , HTML("&nbsp;&nbsp;<font size=5><b>Steps</b></font>")
    #Steps, do *not* need to be done sequentially----
    , sidebarMenu(id = id
      , menuItem(text = "About"
               , tabName = "tab_about"
               , icon = icon("home")
               )## menuItem ~ About ~ END
      , menuItem(text = "Load Data"
                 , tabName = "tab_import"
                 , icon = icon("file-upload")
                 , startExpanded = TRUE)
      , menuItem(text = "Data Preparation"
                 , icon = icon("toolbox") # sliders-h, toolbox, screwdriver-wrench
                 # , menuSubItem("Taxa List Check"
                 #               , tabName = "tab_taxalistcheck"
                 #               , icon = icon("check-square"))
                 , menuSubItem("Translate Taxa and Assign Attributes"
                               , tabName = "tab_filebuilder_taxatrans"
                               , icon = icon("language"))
                 , menuSubItem("Subsample"
                               , tabName = "tab_subsample"
                               , icon = icon("microscope"))
                 , menuSubItem("Mark Redundant Taxa"
                               , tabName = "tab_markexcl"
                               , icon = icon("clone"))
                 )## menuItem ~ Data Preparation ~ END
      , menuItem(text = "Analysis"
                 , icon = icon("cogs") # laptop
                 , menuSubItem("Taxa Maps", tabName = "tab_taxamaps"
                               , icon = icon("map"))
                 , menuSubItem("Calculate Metrics", tabName = "tab_calcmet"
                               , icon = icon("calculator"))
                 # , menuSubItem("Data Summary"
                 #                 , tabName = "tab_summary"
                 #               , icon = icon("chart-bar"))
                 #, menuSubItem("Export"
                 #               , tabName = "tab_export")
                 )## menuItem ~ Analysis ~ END
      # , menuItem(text = "Summary"
      #            , icon = icon("flag-checkered")
      #            , menuSubItem("X", tabName = "tab_X")
      #            , menuSubItem("Y", tabName = "tab_Y")
      #            , menuSubItem("Z", tabName = "tab_Z")
      #            )## menuItem ~ Summary ~ END
    )## sidebarMenu ~ END
  )## dashboardSidebar ~ END
}## FUNCTION ~ END
