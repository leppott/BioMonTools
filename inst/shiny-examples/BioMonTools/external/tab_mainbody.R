# Main Body Tabs
#~~~~~~~~~~~~~~~
# Activated from main sidebar
#~~~~~~~~~~~~~~~

tabItems(
    tabItem(tabName = "tab_about"
            , h2("ABOUT content")
            )## tabItem ~ about ~ END
  , tabItem(tabName = "tab_subsample"
            , h2("Subsample content")
            )## tabItem ~ subsample ~ END
  , tabItem(tabName = "tab_markexcl"
            , h2("Mark Excluded Taxa content")
            )## excl taxa ~ about ~ END
  , tabItem(tabName = "tab_taxamaps"
            , h2("Taxa Maps content")
            )## tabItem ~ taxa maps ~ END
  , tabItem(tabName = "tab_calcmet"
            , h2("Calculate Metrics content")
            )## tabItem ~ calc metrics ~ END
)## tabItems ~ END
