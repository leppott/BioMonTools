# Main

function(id) {
    tabItems(
      tabItem(tabName = "tab_about", tab_code_about())
      , tabItem(tabName = "tab_import", tab_code_import())
      , tabItem(tabName = "tab_filebuilder_taxatrans",
                tab_code_filebuilder_taxatrans())
      , tabItem(tabName = "tab_subsample", tab_code_subsample())
      , tabItem(tabName = "tab_markexcl", tab_code_markexcl())
      , tabItem(tabName = "tab_taxamaps", tab_code_taxamaps())
      , tabItem(tabName = "tab_calcmet", tab_code_calcmet())
      # , tabItem(tabName = "tab_rep_single"
      #           , tab_code_rep_single())
      # , tabItem(tabName = "tab_rep_multi"
      #           , tab_code_rep_multi())
    )## tabItems
}## FUNCTION ~ END


# body <- dashboardBody(
#   tabItems(
#     tabItem(tabName = "tab_about", h2("About"))
#     , tabItem(tabName = "tab_import", h2("Import"))
#   )## tabItems
# )## dashboardBody ~ END
