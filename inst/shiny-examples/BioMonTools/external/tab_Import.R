# Import Page

function(){
  tabPanel("1. Import Data"
    # SideBar ####
    , sidebarLayout(
        sidebarPanel(
          # 0. Progress
          #, tags$hr()
          h4("1.A. Load File, Measured Values")
          , h5("Select file parameters")
          #, checkboxInput('header', 'Header', TRUE)
          , radioButtons('sep', 'Separator',
                         c(Comma=',',
                          # Semicolon=';',
                           Tab='\t'),
                         ',')
          , fileInput('fn_input', 'Choose file to upload',
                      accept = c(
                        'text/csv',
                        'text/comma-separated-values',
                        'text/tab-separated-values',
                        'text/plain',
                        '.csv',
                        '.tsv',
                        '.txt'
                      )
          )##fileInput~END
          #, tags$hr()
          , h4("1.B. Load File, Lake Areas")
          , radioButtons('sep2', 'Separator',
                         c(Comma=',',
                         #  Semicolon=';',
                           Tab='\t'),
                         ',')
          , fileInput('fn_input2', 'Choose file to upload',
                      accept = c(
                        'text/csv',
                        'text/comma-separated-values',
                        'text/tab-separated-values',
                        'text/plain',
                        '.csv',
                        '.tsv',
                        '.txt'
                      )
          )##fileInput~END

          , h4("1.C. Load File, Measure, No Depth")
          , radioButtons('sep3', 'Separator',
                         c(Comma=',',
                         #  Semicolon=';',
                           Tab='\t'),
                         ',')
          , fileInput('fn_input3', 'Choose file to upload',
                      accept = c(
                        'text/csv',
                        'text/comma-separated-values',
                        'text/tab-separated-values',
                        'text/plain',
                        '.csv',
                        '.tsv',
                        '.txt'
                      )
          )##fileInput~END

        )##sidebarPanel~END
      # Main Panel ####
      , mainPanel(
          p("The 'separator' allows the user to upload different file formats
            (csv, tsv, or txt).")
          , p("A measured values file is required.")
          , p("If an area file is not uploaded then only stratification will be
              calculated.")
          , p("Imported data files are displayed in step 2.")
          , p(paste0("File uploads (separately) are limited to a maximum of "
                     ,mb_limit
                     , " MB in size."))
      )##mainPanel~END

    )##sidebarLayout~END
  )##tabPanel ~ END
}## FUNCTION ~ END
