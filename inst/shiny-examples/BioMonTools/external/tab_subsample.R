# Subsample Panel

function() {
  tabPanel("tabpan_subsample"
           , fluidPage(h2("Subsample"
                          #, style  = "text-align:center"
                          )
                       , p("This function allows the user to subsample (rarify)
                           down to a fixed count.")
                       , p("If a seed is given the results are reproducible
                           (with the same unmodified file).")
                       , br()
                       , p("1. Upload a file.")
                       , p("Use 'Load Data' option in left sidebar.")
                       , p("2. Define columns (Sample ID, Taxa ID, Count).")
                    , uiOutput("UI_col_subsamp_SampID")
                    , uiOutput("UI_col_subsamp_TaxaID")
                    , uiOutput("UI_col_subsamp_Count")
                       , p("3. Targets")
                       , numericInput("subsamp_target", label = "Target Size", value = 300)
                       , numericInput("subsamp_seed", label = "Numeric Seed", value = NA)
                       , p("4. Run Function")
                       , bsButton("b_subsample_run", label = "Run Function")
                       , p("5. Download Results")
                       , useShinyjs()
                       , shinyjs::disabled(downloadButton("b_subsample_download"
                                                       , "Download Results"))
                    # , bsPopover(id = "subsamp_col_SampID"
                    #             , title = "Column Name"
                    #             , content = "Enter relevant column name.")
                    # , bsPopover(id = "subsamp_col_TaxaID"
                    #             , title = "Column Name"
                    #             , content = "Enter relevant column name.")
                    # , bsPopover(id = "subsamp_col_Count"
                    #             , title = "Column Name"
                    #             , content = "Enter relevant column name.")
                    # , bsPopover(id = "subsamp_seed"
                    #             , title = "Seed"
                    #             , content = "Number at least 6 digits to reproduce results. If left blank a random number is used.")
                        )## fluidPage ~ END
           # , includeHTML(file.path("external", "Help.html"))
           )##tabPanel ~ END
}##FUNCTION ~ END
