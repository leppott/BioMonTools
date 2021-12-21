# Mark Excluded Taxa Panel

function() {
  tabPanel("tabpan_markexcl"
           , fluidPage(h2("Mark Redundant Taxa"
                          #, style  = "text-align:center"
                          )
                       , p("This function allows the user to mark redundant taxa")

                       , br()
                       , p("1. Upload a file.")

                       , p("2. Define columns (Sample ID, Taxa ID, Count).")
                       , uiOutput("UI_col_markexcl_SampID")
                       , uiOutput("UI_col_markexcl_TaxaID")
                       , uiOutput("UI_col_markexcl_Count")
                       , p("Check boxes for each phylo level.")
                       , p("default names to all upper case")

                       , p("3. Define Map Extent")
                       , p("Defined by data")
                       , p("4. Run")
                     #  , bsButton(but_markexcl_run, label = "Mark Redundant Taxa")
                       , "download"
                        )## fluidPage ~ END
           # , includeHTML(file.path("external", "Help.html"))
           )##tabPanel ~ END
}##FUNCTION ~ END
