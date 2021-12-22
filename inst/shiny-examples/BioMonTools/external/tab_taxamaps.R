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
                       , p("Use 'Load Data' option in left sidebar.")
                       , p("2. Define columns.")
                       , uiOutput("UI_col_taxamaps_SampID")
                       , uiOutput("UI_col_taxamaps_TaxaID")
                       , uiOutput("UI_col_taxamaps_Count")
                       , uiOutput("UI_col_taxamaps_Lat")
                       , uiOutput("UI_col_taxamaps_Long")
                       , uiOutput("UI_col_taxamaps_Group")
                       , p("3. Define Map")
                       , selectInput("taxamaps_database"
                                     , label = "Map Database"
                                     , choices = maps_database
                                     , selected = "state")
                       , p("Character vector that names the polygons to draw.")
                       , p("e.g., new york,new jersey,pennsylvania (only comma between entries) OR michigan::north")
                       , textInput("taxamaps_regions"
                                    , label = "Map Region"
                                    , value = "")
                       # map("usa", names = TRUE, plot = FALSE)
                       # , selectInput("taxamaps_leg_loc"
                       #               , label = "Legend Location"
                       #               , choices = c("left", "right", "bottom"
                       #                             , "bottomleft", "bottomright")
                       #               , selected = "bottomleft")
                       , p("4. Run Function")
                       , bsButton("b_taxamaps_run", label = "Run Function")
                       , p("5. Download Results")
                       , useShinyjs()
                     , shinyjs::disabled(downloadButton("b_taxamaps_download"
                                                        , "Download Results"))
                     ,
                        )## fluidPage ~ END
           # , includeHTML(file.path("external", "Help.html"))
           )##tabPanel ~ END
}##FUNCTION ~ END
