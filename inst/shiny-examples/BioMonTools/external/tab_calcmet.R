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
                       , p("1. Upload a file.")
                       , p("Use 'Load Data' option in left sidebar.")
                       , p("1. Define Community")
                       , selectInput("calcmet_community"
                                     , label = "Community"
                                     , choices = sel_community
                                     , selected = "bugs")
                       , p("2. Columns.")
                        , p("Required Columns:")
                       , p("SAMPLEID, TAXAID, N_TAXA, EXCLUDE, INDEX_NAME, INDEX_REGION
                           , NONTARGET, SAMP_BIOMASS, PHYLUM, SUBPHYLUM, CLASS
                           , SUBCLASS, INFRAORDER, ORDER, FAMILY, SUBFAMILY, TRIBE
                           , GENUS")
                       , p("Optional Columns (if missing metrics for these columns will calculate as 0):")
                       , p("FFG, HABIT, LIFE_CYCLE, TOLVAL, BCG_ATTR
                           , THERMAL_INDICATOR, FFG2, TOLVAL2, LONGLIVED
                           , NOTEWORTHY, HABITAT, UFC, ELEVATION_ATTR
                           , GRADIENT_ATTR, WSAREA_ATTR, REPRODUCTION
                           , CONNECTIVITY, SCC")
                       , p("3. Extra columns to keep in output")
                       , p("Required columns will not be available for selection.")
                       , uiOutput("UI_col_calcmet_Cols2Keep")
                       , p("4. Run Function")
                       , bsButton("b_calcmet_run", label = "Run Function")
                       , p("5. Download Results")
                       , useShinyjs()
                       , shinyjs::disabled(downloadButton("b_calcmet_download"
                                                          , "Download Results"))
                        )## fluidPage ~ END
           # , includeHTML(file.path("external", "Help.html"))
           )##tabPanel ~ END
}##FUNCTION ~ END
