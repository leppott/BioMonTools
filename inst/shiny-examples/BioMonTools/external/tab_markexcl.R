# Mark Excluded Taxa Panel

function() {
  tabPanel("tabpan_markexcl"
           , fluidPage(h2("Mark Redundant Taxa"
                          #, style  = "text-align:center"
                          )
                       , p("This function allows the user to mark redundant taxa")

                       , br()
                       , p("1. Upload a file.")
                       , p("Use 'Load Data' option in left sidebar.")
                       , p("2. Define columns (Sample ID, Taxa ID, Count).")
                       , uiOutput("UI_col_markexcl_SampID")
                       , uiOutput("UI_col_markexcl_TaxaID")
                       , uiOutput("UI_col_markexcl_Count")
                       , textInput("markexcl_Exclude"
                                   , label = "Exclude Column"
                                   , value = "Exclude")
                       , p("3. TaxaLevels")
                       , p("TaxaLevels are used in the order below.")
                       , p("Kingdom, Phylum, SubPhylum, Class, SubClass
                           , Order, SubOrder, InfraOrder, SuperFamily, Family
                           , SubFamily, Tribe, Genus, SubGenus, Species, Variety")
                       , uiOutput("UI_col_markexcl_Phylo")
                       #, p("Current version uses all phylogenetic names present in file.")
                       , p("4. Run Function")
                       , bsButton("b_markexcl_run", label = "Run Function")
                       , p("5. Download Results")
                       , useShinyjs()
                       , shinyjs::disabled(downloadButton("b_markexcl_download"
                                                          , "Download Results"))
                        )## fluidPage ~ END
           # , includeHTML(file.path("external", "Help.html"))
           )##tabPanel ~ END
}##FUNCTION ~ END
