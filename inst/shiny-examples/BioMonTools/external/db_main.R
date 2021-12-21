# Main

function(id) {
  dashboardBody(
    tabItems(
      tabItem(tabName = "tab_about", tab_code_about())
      , tabItem(tabName = "tab_import", tab_code_import())
      , tabItem(tabName = "tab_subsample", tab_code_subsample())
      , tabItem(tabName = "tab_markexcl", tab_code_markexcl())
      , tabItem(tabName = "tab_taxamaps", tab_code_taxamaps())
      , tabItem(tabName = "tab_calcmet", tab_code_calcmet())
    )## tabItems
  )## dashboardBody ~ END
}## FUNCTION ~ END


# body <- dashboardBody(
#   tabItems(
#     tabItem(tabName = "tab_about", h2("About"))
#     , tabItem(tabName = "tab_import", h2("Import"))
#   )## tabItems
# )## dashboardBody ~ END
