# Calculate Metrics Panel

function() {
  sidebarLayout(
    sidebarPanel(
       h2("Calculate Metrics")
       # , p("This function allows the user to calculate metrics for
       #     benthic macroinvertebrates, fish, or periphyton")
       # , p("Column names are predefined.")
       # , p("All metrics will be calculated.")
       # , br()
       , p("1. Upload a file.")
       , p("Use 'Load Data' option in left sidebar.")
       , p("2. Define Community")
       , selectInput("calcmet_community"
                     , label = "Community"
                     , choices = sel_community
                     , selected = "bugs")
       #, p("3. Columns.")
       # , p("Required Columns:")
       # , p("SAMPLEID, TAXAID, N_TAXA, EXCLUDE, INDEX_NAME, INDEX_CLASS
       #     , NONTARGET, SAMP_BIOMASS, PHYLUM, SUBPHYLUM, CLASS
       #     , SUBCLASS, INFRAORDER, ORDER, FAMILY, SUBFAMILY, TRIBE
       #     , GENUS")
       # , p("Optional Columns (if missing metrics for these columns will calculate as 0):")
       # , p("FFG, HABIT, LIFE_CYCLE, TOLVAL, BCG_ATTR
       #     , THERMAL_INDICATOR, FFG2, TOLVAL2, LONGLIVED
       #     , NOTEWORTHY, HABITAT, UFC, ELEVATION_ATTR
       #     , GRADIENT_ATTR, WSAREA_ATTR, REPRODUCTION
       #     , CONNECTIVITY, SCC")
       , p("3. Extra columns to keep in output")
       , p("Required columns will not be available for selection.")
       , uiOutput("UI_col_calcmet_Cols2Keep")
       #, p("4. Run Function")
       , hr()
       , bsButton("b_calcmet_run", label = "Run Function")
       #, p("5. Download Results")
       , useShinyjs()
       , shinyjs::disabled(downloadButton("b_calcmet_download"
                                          , "Download Results"))
        )## sidebarPanel ~ END
    , mainPanel(
        tabsetPanel(
          tabPanel("bugs"
                   , includeHTML(file.path("www", "ShinyHTML_CalcMetrics_bugs.html"))

                   , tableOutput("tbl_calcmet_bugs")

                   , em("Last updated: 2022-02-02")


                   )
          , tabPanel("fish"
                     , includeHTML(file.path("www", "ShinyHTML_CalcMetrics_fish.html"))
                     )
          , tabPanel("algae"
                     , includeHTML(file.path("www", "ShinyHTML_CalcMetrics_algae.html"))
                     )
        )## tabsetPanel
    )## mainPanel ~ END
  )##sidebarLayout ~ END
}##FUNCTION ~ END
