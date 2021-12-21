# TaxaMaps Panel

function() {
  tabPanel("tabpan_taxamaps"
           , fluidPage(h2("Taxa Distribution Maps"
                          #, style  = "text-align:center"
                          )
                       , p("This function allows the user to create maps of taxa
                           distributions.")

                       , br()
                       , p("1. Upload a file.")
                     #  , bsButton(but_subsamp_import, label = "Import - subsample")
                       , p("2. Define columns.")

                     , uiOutput("UI_col_taxamaps_SampID")
                     , uiOutput("UI_col_taxamaps_TaxaID")
                     , uiOutput("UI_col_taxamaps_Count")
                     , uiOutput("UI_col_taxamaps_Lat")
                     , uiOutput("UI_col_taxamaps_Long")
                     , uiOutput("UI_col_taxamaps_Group")
                       , p("3. Define Map Extent")
                       , p("Defined by data")
                       , p("4. Run")
                      # , bsButton(but_taxamap_run, label = "Create Taxa Map")
                       , "download"
                        )## fluidPage ~ END
           # , includeHTML(file.path("external", "Help.html"))
           )##tabPanel ~ END
}##FUNCTION ~ END
