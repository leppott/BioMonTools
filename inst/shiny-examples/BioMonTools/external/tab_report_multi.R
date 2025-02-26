# Report, Multiple Sites/Sample

function() {
  sidebarLayout(
    sidebarPanel(
      h2("Report, Multiple Sites/Samples")
      , p("This function will render all steps and make available files for download.")
      , br()

      , h4("A. Upload a zip file.")
      , p("Template file in root folder of zip file contains the necessary files and locations.")
      , fileInput("fn_input_rep_multi"
                  , label = "Import Zip File for Report, Multi"
                  , multiple = FALSE
                  , accept = ".zip"
      )

      , h4("D. Run Operation")
      , p("This button will generate the report")
      , shinyBS::bsButton("b_calc_rep_multi"
                                            , label = "Run Operation")

      , h4("E. Download Output")
      , p("All input and output files will be available in a single zip file.")
      , shinyjs::disabled(downloadButton("b_download_rep_multi"
                                         , "Download Results"))

    )## sidebarPanel
    , mainPanel(
        tabsetPanel(type = "tabs"
                  , tabPanel(title = "Calc_Report_Multi_About"
                             # ,includeHTML(file.path("www"
                             #                        , "rmd_html"
                             #                        , "ShinyHTML_Report_Multi_1About.html"))
                  )## tabPanel
        )## tabsetPanel
    )## mainPanel ~ END
  )##sidebarLayout ~ END
}##FUNCTION ~ END
