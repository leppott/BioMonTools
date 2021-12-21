#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

shinyServer(function(input, output) {

  # IMPORT ----

  ## Import, DF ----
  file_watch <- reactive({
    # trigger for df_import()
    input$fn_input
    #paste(input$fn_input, input$but_radio_load)
  })## file_watch

  df_import <- eventReactive(file_watch(), {
    # use a multi-item reactive so keep on a single line

    inFile <- input$fn_input

    #
    if(is.null(inFile)) {
      return()
    }

    # Define file
    fn_inFile <- inFile$datapath

    # Read user imported file
    df_input <- read.table(fn_inFile
                         , header = TRUE
                         , sep = input$sep
                         , quote = "\""
                         , stringsAsFactors = FALSE)


    # # Write to "Results" folder - Import as TSV
    # fn_input <- file.path(path_results, "data_input", "data_import_measure.tsv")
    # write.table(df_input, fn_input, row.names=FALSE, col.names=TRUE, sep="\t")
    #
    # # Copy to "Results" folder - Import "as is"
    # file.copy(input$fn_input$datapath, file.path(path_results, "data_input"
    #                                              , input$fn_input$name))
    #
    return(df_input)
    #
  })##df_import~END


  ## Import, Table ----
  output$df_data_DT <- DT::renderDT({

    df_data <- df_import()

  }##expression~END
  , filter="top"
  , caption = "Table. Uploaded data."
  , options=list(scrollX=TRUE
                 , lengthMenu = c(5, 10, 25, 50, 100, 1000)
                 , autoWidth = TRUE
                 )
  )##df_data_DT~END

  col_input <- eventReactive(file_watch(), {
    # temp df
    df_temp <- df_import()
    # Column Names
    input_colnames <- names(df_temp)
    #
    return(input_colnames)
  })## col_input



  # MARKEXCL ----

  ## MarkExcl, Colnames ----
  output$UI_col_markexcl_SampID <- renderUI({
    str_col <- "SampID"
    selectInput("markexcl_col_SampID"
                , label = str_col
                , choices = names(df_import())
                , multiple = FALSE)
  })## UI_colnames

  output$UI_col_markexcl_TaxaID <- renderUI({
    str_col <- "TaxaID"
    selectInput("markexcl_col_TaxaID"
                , label = str_col
                , choices = names(df_import())
                , multiple = FALSE)
  })## UI_colnames

  output$UI_col_markexcl_Count <- renderUI({
    str_col <- "Count"
    selectInput("markexcl_col_Count"
                , label = str_col
                , choices = names(df_import())
                , multiple = FALSE)
  })## UI_colnames

  output$UI_col_markexcl_Phylo <- renderUI({
    str_col <- ""
    selectInput("markexcl_col_Count"
                , label = str_col
                , choices = names(df_import())
                , multiple = FALSE)
  })## UI_colnames


  # # validate required columns
  # col_req <- c("station"
  #              , "layer"
  #              , "latitude"
  #              , "longitude"
  #              , "cbSeg92"
  #              , "state"
  #              , "stationGrpName"
  #              , "parmName"
  #              , "gamName"
  #              , "periodName"
  #              , "seasonName"
  #              , "gamDiff.bl.mn.obs"
  #              , "gamDiff.cr.mn.obs"
  #              , "gamDiff.abs.chg.obs"
  #              , "gamDiff.pct.chg"
  #              , "gamDiff.chg.pval")
  # # Check
  # col_req_match <- col_req %in% colnames(df_input)
  # col_missing <- col_req[!col_req_match]
  # #
  # validate(need(sum(col_req_match) == length(col_req)
  #               , paste0("ERROR\nRequired columns missing from the data:\n"
  #                        , paste("* ", col_missing, collapse = "\n"))))



  # SUBSAMPLE ----

  ## Subsample, Colnames ----
  output$UI_col_subsamp_SampID <- renderUI({
    str_col <- "SampID"
    selectInput("subsamp_col_SampID"
                , label = str_col
                , choices = names(df_import())
                , multiple = FALSE)
  })## UI_colnames

  output$UI_col_subsamp_TaxaID <- renderUI({
    str_col <- "TaxaID"
    selectInput("subsamp_col_TaxaID"
                , label = str_col
                , choices = names(df_import())
                , multiple = FALSE)
  })## UI_colnames

  output$UI_col_subsamp_Count <- renderUI({
    str_col <- "Count"
    selectInput("subsamp_col_Count"
                , label = str_col
                , choices = names(df_import())
                , multiple = FALSE)
  })## UI_colnames

  # TAXAMAPS ----

  ## TaxaMaps, Colnames ----
  output$UI_col_taxamaps_SampID <- renderUI({
    str_col <- "SampID"
    selectInput("taxamaps_col_SampID"
                , label = str_col
                , choices = names(df_import())
                , multiple = FALSE)
  })## UI_colnames

  output$UI_col_taxamaps_TaxaID <- renderUI({
    str_col <- "TaxaID"
    selectInput("taxamaps_col_TaxaID"
                , label = str_col
                , choices = names(df_import())
                , multiple = FALSE)
  })## UI_colnames

  output$UI_col_taxamaps_Count <- renderUI({
    str_col <- "Count"
    selectInput("taxamaps_col_Count"
                , label = str_col
                , choices = names(df_import())
                , multiple = FALSE)
  })## UI_colnames

  output$UI_col_taxamaps_Lat <- renderUI({
    str_col <- "Lat"
    selectInput("taxamaps_col_Lat"
                , label = str_col
                , choices = names(df_import())
                , multiple = FALSE)
  })## UI_colnames

  output$UI_col_taxamaps_Long <- renderUI({
    str_col <- "Longitude"
    selectInput("taxamaps_col_Long"
                , label = str_col
                , choices = names(df_import())
                , multiple = FALSE)
  })## UI_colnames

  output$UI_col_taxamaps_Group <- renderUI({
    str_col <- "Group"
    selectInput("taxamaps_col_Group"
                , label = str_col
                , choices = names(df_import())
                , multiple = FALSE)
  })## UI_colnames


  # METCALC ----

})## shinyServer ~ END
