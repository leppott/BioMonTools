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
                       , p("This is done from 'Load Data' on the left sidebar.")
                    #   , bsButton(but_subsamp_import, label = "Import - subsample")
                       , p("2. Define columns (Sample ID, Taxa ID, Count).")
                       #, textInput("subsamp_col_SampID", label = "SampID", value = "SampID")
                    , uiOutput("UI_col_subsamp_SampID")
                    , uiOutput("UI_col_subsamp_TaxaID")
                    , uiOutput("UI_col_subsamp_Count")
                       , p("3. Seed")
                       , numericInput("subsamp_seed", label = "Numeric Seed", value = NA)
                       , p("4. Run data.")
                     #  , bsButton(but_subsamp_run, label = "Subsample")
                       , p("download")
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
