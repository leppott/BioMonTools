# Calculate Metrics Panel

function() {
  tabPanel("tabpan_calcmet"
           , fluidPage(h2("Calculate Metrics"
                          #, style  = "text-align:center"
                          )
                       , p("This function allows the user to calculate metrics for
                           benthic macroinvertebrates, fish, or periphyton")
                       , p("Column names are predefined.")
                       , p("All metrics will be calculated.")

                       , br()
                       , p("1. Define Community")
                       , selectInput("calcmet_community"
                                     , label = "Community"
                                     , choices = sel_community
                                     , selected = "bugs")
                       , p("2. Columns.")
                        , p("inform user of what expected.")

                       , p("4. Run")
                     #  , bsButton(but_calcmet_run, label = "Calculate Metrics")
                       , "download"
                        )## fluidPage ~ END
           # , includeHTML(file.path("external", "Help.html"))
           )##tabPanel ~ END
}##FUNCTION ~ END
