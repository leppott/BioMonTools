# Subsample Panel

function() {
  sidebarLayout(
    sidebarPanel(

        h2("Subsample")
         , p("1. Upload a file.")
         , p("Use 'Load Data' option in left sidebar.")
         , p("2. Define columns (Sample ID, Taxa ID, Count).")
         , uiOutput("UI_col_subsamp_SampID")
         , uiOutput("UI_col_subsamp_TaxaID")
         , uiOutput("UI_col_subsamp_Count")
         , p("3. Targets")
         , numericInput("subsamp_target", label = "Target Number of Organisms", value = 300)
         , numericInput("subsamp_seed", label = "(OPTIONAL) Numeric Seed", value = NA)
        , p("See text to right for more explanation.")
         #, p("4. Run Function")
        , hr()
         , bsButton("b_subsample_run", label = "Run Function")
         #, p("5. Download Results")
         , useShinyjs()
         , shinyjs::disabled(downloadButton("b_subsample_download"
                                         , "Download Results"))

        #                                                      , "Download Results"))
        #                   # , bsPopover(id = "subsamp_col_SampID"
        #                   #             , title = "Column Name"
        #                   #             , content = "Enter relevant column name.")
        #                   # , bsPopover(id = "subsamp_col_TaxaID"
        #                   #             , title = "Column Name"
        #                   #             , content = "Enter relevant column name.")
        #                   # , bsPopover(id = "subsamp_col_Count"
        #                   #             , title = "Column Name"
        #                   #             , content = "Enter relevant column name.")
        #                   # , bsPopover(id = "subsamp_seed"
        #                   #             , title = "Seed"
        #                   #             , content = "Number at least 6 digits to reproduce results. If left blank a random number is used.")


    )## sidebarPanel ~ END
    , mainPanel(
      includeHTML(file.path("www", "ShinyHTML_Subsample.html"))

    )## mainPanel ~ END




  )## sidebarLayout ~ END




}##FUNCTION ~ END
