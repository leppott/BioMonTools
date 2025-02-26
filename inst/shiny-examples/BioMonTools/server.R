#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

shinyServer(function(input, output) {

  # TABLES ----
  ## Table, About ----
  output$tbl_about <- renderTable({
    # Table Contents
    t_names <- c("Group", "Function", "Description")
    t_col1 <- c("Data Preparation", "Data Preparation", "Analysis", "Analysis")
    t_col2 <- c("Subsample"              #1
                , "Mark Redundant Taxa"  #2
                , "Taxa Maps"            #3
                , "Calculate Metrics"    #4
    )
    t_col3 <- c("Subsample your abundance data to a fixed count per sample. For example, if 300-organisms is your target, and any of your samples have more than 300 organisms, you can use this function to randomly subsample your data so that the total number of individuals is 300. This is done to make richness metrics comparable (since higher numbers of taxa generally occur in samples in which more individuals are counted). The function code is from USEPA Corvallis John Van Sickle’s R code for RIVPACS (v1.0, 2005-06-10)."
                # 2
                , "Avoid double-counting taxa that may be redundant (or non-distinct, depending on your preferred terminology). For example, if organisms identified as Dytiscidae (family-level) and Oreodytes (genus-level) are both present in a sample, organisms identified as Dytiscidae could be the same taxon as Oreodytes. The R tool identifies and marks redundant taxa on a sample-by-sample basis so that they can be excluded from richness metric calculations."
                # 3
                , "Examine the spatial distribution of each taxon with presence/absence maps. These maps can be useful for reconciling differences in taxonomy and tracking shifts in spatial distributions of indicator taxa at the regional scale. Users can color-code data points by a grouping variable (e.g., data source)."
                # 4
                , "Calculate metrics based on the traits/attributes in the input file. Column headings and attribute entries need to follow the specific naming convention described here. For RMN analyses, attribute assignments should come from the appropriate regional traits table. Currently, the Shiny app is set up to calculate the following groups of benthic macroinvertebrate metrics: standard bioassessment, major taxonomic groups, thermal and hydrologic indicators, functional feeding group (FFG), habit, tolerance, life cycle/voltinism, and Biological Condition Gradient (BCG). More metrics (including some for fish) are available on the BioMonTools R package. You can request the addition of new metrics by emailing Erik (Erik.Leppo@tetratech.com) or posting your request on the BioMonTools GitHub page (https://github.com/leppott/BioMonTools/discussions).")

    t_cap <- "BioMonTools shiny application functions."

    # Table
    myTable <- data.frame(t_col1, t_col2, t_col3)
    names(myTable) <- t_names

    # Display
    ## Knitr
    # knitr::kable(myTable
    #              , caption = t_cap
    #              # , format = "html"
    #             )

    myTable

    }
    , striped = TRUE
    , bordered = TRUE
    , caption = "BioMonTools shiny application functions."
    , caption.placement = "top"
    )## tbl_about

  ## Table, CalcMet_Bugs ----
  output$tbl_calcmet_bugs <- renderTable({
    # Import
    df_tbl <- read.csv(file.path("www", "linked_files", "CalcMetrics"
                        , "BioMonTools_BugMetricInputFileReqs_v2_20220124.csv"))

    df_tbl
  }
  , striped = TRUE
  , bordered = TRUE
  , caption = "Metric Calculation Input Requirements, bugs."
  , caption.placement = "top"
  )## tbl_calcmet_bugs

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
    if (is.null(inFile)) {
      return()
    }

    # Define file
    fn_inFile <- inFile$datapath

    # Read user imported file
    # df_input <- read.table(fn_inFile
    #                      , header = TRUE
    #                      , sep = input$sep
    #                      , quote = "\""
    #                      , stringsAsFactors = FALSE)
    # Add extra colClasses parameter for BCG_Attr
    # the "i" values default to complex numbers
    # Will get a 'warning'
    df_input <- read.table(fn_inFile
                         , header = TRUE
                         , sep = input$sep
                         , quote = "\""
                         , stringsAsFactors = FALSE
                         , colClasses = c("BCG_Attr" = "character"
                                          , "BCG_ATTR" = "character"
                                          , "bcg_attr" = "character"))


    # # Write to "Results" folder - Import as TSV
    # fn_input <- file.path(path_results, "data_input", "data_import_measure.tsv")
    # write.table(df_input, fn_input, row.names=FALSE, col.names=TRUE, sep="\t")
    #
    # # Copy to "Results" folder - Import "as is"
    # file.copy(input$fn_input$datapath, file.path(path_results, "data_input"
    #                                              , input$fn_input$name))
    #

    ## button, enable, calc ----
    shinyjs::enable("b_calc_taxatrans")
    # shinyjs::enable("b_calc_indexclass")
    # shinyjs::enable("b_calc_indexclassparam")
    # shinyjs::enable("b_calc_bcg")
    # shinyjs::enable("b_calc_ibi")
    # shinyjs::enable("b_calc_met_therm")
    # shinyjs::enable("b_calc_modtherm")
    # shinyjs::enable("b_calc_mtti")
    # shinyjs::enable("b_calc_bdi")

    return(df_input)
    #
  })##df_import~END


  ## Import, Table ----
  output$df_data_DT <- DT::renderDT({

    df_data <- df_import()

  }##expression~END
  , filter = "top"
  , caption = "Table. Uploaded data."
  , options = list(scrollX=TRUE
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

  # FB, TAXATRANS ----
  ## TaxaTrans, UI ----

  output$fn_input_display_taxatrans <- renderText({
    inFile <- input$fn_input

    if (is.null(inFile)) {
      return("..No file uploaded yet...")
    }##IF~is.null~END

    return(paste0("'", inFile$name, "'"))

  })## fn_input_display_taxatrans

  output$UI_taxatrans_pick_official <- renderUI({
    str_col <- "Calculation"
    selectInput("taxatrans_pick_official"
                , label = str_col
                , choices = c("", df_pick_taxoff[, "project"])
                , multiple = FALSE)
  })## UI_colnames

  # output$UI_taxatrans_pick_official_project <- renderUI({
  #   str_col <- "Official Taxa Data, Column Taxa_ID"
  #   selectInput("taxatrans_pick_official_project"
  #               , label = str_col
  #               , choices = names(df_pick_taxoff)
  #               , multiple = FALSE)
  # })## UI_colnames

  output$UI_taxatrans_user_col_taxaid <- renderUI({
    str_col <- "Column, TaxaID"
    selectInput("taxatrans_user_col_taxaid"
                , label = str_col
                , choices = c("", names(df_import()))
                , selected = "TaxaID"
                , multiple = FALSE)
  })## UI_colnames

  output$UI_taxatrans_user_col_drop <- renderUI({
    str_col <- "Columns to Drop"
    selectInput("taxatrans_user_col_drop"
                , label = str_col
                , choices = c("", names(df_import()))
                , multiple = TRUE)
  })## UI_colnames

  output$UI_taxatrans_user_col_n_taxa <- renderUI({
    str_col <- "Column, Taxa Count (number of individuals or N_Taxa)"
    selectInput("taxatrans_user_col_n_taxa"
                , label = str_col
                , choices = c("", names(df_import()))
                , selected = "N_Taxa"
                , multiple = FALSE)
  })## UI_colnames

  output$UI_taxatrans_user_col_groupby <- renderUI({
    str_col <- "Columns to Keep in Output"
    selectInput("taxatrans_user_col_groupby"
                , label = str_col
                , choices = c("", names(df_import()))
                , multiple = TRUE)
  })## UI_colnames

  output$UI_taxatrans_user_col_sampid <- renderUI({
    str_col <- "Column, Unique Sample Identifier (e.g., SampleID)"
    selectInput("taxatrans_user_col_sampid"
                , label = str_col
                , choices = c("", names(df_import()))
                , selected = "SampleID"
                , multiple = FALSE)
  })## UI_colnames


  # ## TaxaTrans, combine ----
  # observeEvent(input$cb_TaxaTrans_Summ, {
  #   # turn on/off extra selection boxes based on checkbox
  #   if(input$cb_TaxaTrans_Summ == TRUE) {
  #     shinyjs::enable("UI_taxatrans_user_col_n_taxa")
  #     shinyjs::enable("UI_taxatrans_user_col_groupby")
  #   } else {
  #     shinyjs::disable("UI_taxatrans_user_col_n_taxa")
  #     shinyjs::disable("UI_taxatrans_user_col_groupby")
  #   }## IF ~ checkbox
  #
  # }, ignoreInit = FALSE
  # , ignoreNULL = FALSE)## observerEvent ~ cb_TaxaTrans_Summ
  # #})


  ## b_Calc_TaxaTrans ----
  observeEvent(input$b_calc_taxatrans, {
    shiny::withProgress({

      ### Calc, 00, Initialize ----
      prog_detail <- "Calculation, Taxa Translator..."
      message(paste0("\n", prog_detail))

      # Number of increments
      prog_n <- 7
      prog_sleep <- 0.25

      ## Calc, 01, Import User Data ----
      prog_detail <- "Import Data, User"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Remove existing files in "results"
      clean_results()

      # Copy user files to results sub-folder
      path_results_sub <- "taxatrans"
      copy_import_file(import_file = input$fn_input)

      # result folder and files
      fn_abr <- abr_taxatrans
      fn_abr_save <- paste0("_", fn_abr, "_")
      # path_results_sub <- file.path(path_results
      #                               , paste(abr_results, fn_abr, sep = "_"))
      # # Add "Results" folder if missing
      # boo_Results <- dir.exists(file.path(path_results_sub))
      # if (boo_Results == FALSE) {
      #   dir.create(file.path(path_results_sub))
      # }
      # Add "reference" folder if missing
      path_results_ref <- file.path(path_results, dn_files_ref)
      boo_Results <- dir.exists(file.path(path_results_ref))
      if (boo_Results == FALSE) {
        dir.create(file.path(path_results_ref))
      }
      # Add "Results" folder based on user selection later in this step

      # button, disable, download
      shinyjs::disable("b_download_taxatrans")

      # Import data
      # data
      inFile <- input$fn_input
      fn_input_base <- tools::file_path_sans_ext(inFile$name)
      message(paste0("Import, file name, base: ", fn_input_base))
      df_input <- read.delim(inFile$datapath
                             , header = TRUE
                             , sep = input$sep
                             , stringsAsFactors = FALSE)
      # QC, FAIL if TRUE
      if (is.null(df_input)) {
        return(NULL)
      }

      ## Calc, 02, Gather and Test Inputs  ----
      prog_detail <- "QC Inputs"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Fun Param, Define
      sel_proj <- input$taxatrans_pick_official
      sel_user_taxaid <- input$taxatrans_user_col_taxaid
      #sel_col_drop <- unlist(input$taxatrans_user_col_drop)
      sel_user_ntaxa <- input$taxatrans_user_col_n_taxa
      sel_user_groupby <- unlist(input$taxatrans_user_col_groupby)
      sel_summ <- input$cb_TaxaTrans_Summ

      fn_taxoff <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                  , "filename"]
      fn_taxoff_meta <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                       , "metadata_filename"]
      col_taxaid_official_match <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                                  , "taxaid"]
      col_taxaid_official_project <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                                    , "calc_taxaid"]
      col_drop_project <- unlist(strsplit(df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                                         , "col_drop"], ","))
      fn_taxoff_attr <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                       , "attributes_filename"]
      fn_taxoff_attr_meta <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                            , "attributes_metadata_filename"]
      col_taxaid_attr <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                        , "attributes_taxaid"]
      sel_user_sampid <- input$taxatrans_user_col_sampid

      sel_taxaid_drop <-  df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                         , "taxaid_drop"]
      dir_proj_results <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                         , "dir_results"]


      # include = yes; unique(sel_user_groupby)
      # include sampid, taxaid, and n_taxa so not dropped
      user_col_keep <- names(df_input)[names(df_input) %in% c(sel_user_groupby
                                                              , sel_user_sampid
                                                              , sel_user_taxaid
                                                              , sel_user_ntaxa)]
      # flip to col_drop
      user_col_drop <- names(df_input)[!names(df_input) %in% user_col_keep]

      # Fun Param, Test

      if (sel_proj == "") {
        # end process with pop up
      }## IF ~ sel_proj

      if (is.na(fn_taxoff_meta) | fn_taxoff_meta == "") {
        # set value to NULL
        df_official_metadata <- NULL
      }## IF ~ fn_taxaoff_meta

      if (is.na(sel_user_ntaxa) | sel_user_ntaxa == "") {
        sel_user_ntaxa <- NULL
      }## IF ~ fn_taxaoff_meta

      if (is.null(sel_summ)) {
        sel_summ <- FALSE
      }## IF ~ sel_summ

      if (sel_taxaid_drop == "NULL") {
        sel_taxaid_drop <- NULL
      }## IF ~ sel_taxaid_drop


      message(paste0("User response to summarize duplicate sample taxa = "
                     , sel_summ))

      dn_files <- paste(abr_results, dir_proj_results, sep = "_")

      # Add "Results" folder if missing
      path_results_sub <- file.path(path_results, dn_files)
      boo_Results <- dir.exists(file.path(path_results_sub))
      if (boo_Results == FALSE) {
        dir.create(file.path(path_results_sub))
      }

      if (is.null(sel_user_taxaid)) {
        # end process with pop up
        msg <- "'SampID' column name is required and is missing!"
        shinyalert::shinyalert(title = "Taxa Translate"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## IF ~ sampid

      if (is.null(sel_user_taxaid)) {
        # end process with pop up
        msg <- "'TaxaID' column name is required and is missing!"
        shinyalert::shinyalert(title = "Taxa Translate"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## IF ~ taxaid

      if (is.null(sel_user_ntaxa)) {
        # end process with pop up
        msg <- "'N_Taxa' column name is required and is missing!"
        shinyalert::shinyalert(title = "Taxa Translate"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## IF ~ n_taxa

      ## Calc, 03, Import Official Data (and Metadata)  ----
      prog_detail <- "Import Data, Official and Metadata"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      ## Data,  Official Taxa----
      url_taxoff <- file.path(url_bmt_base
                              , "taxa_official"
                              , "RMN"
                              , fn_taxoff)
      httr::GET(url_taxoff
                , httr::write_disk(temp_taxoff <- tempfile(fileext = ".csv")))

      df_taxoff <- read.csv(temp_taxoff)

      ## Data, Official Taxa, Meta Data----
      if (!is.null(fn_taxoff_meta)) {
        url_taxoff_meta <- file.path(url_bmt_base
                                     , "taxa_official"
                                     , "RMN"
                                     , fn_taxoff_meta)
        httr::GET(url_taxoff_meta
                  , httr::write_disk(temp_taxoff_meta <- tempfile(fileext = ".csv")))

        df_taxoff_meta <- read.csv(temp_taxoff_meta)
      }## IF ~ fn_taxaoff_meta

      ## Data, Official Attributes----
      if (!is.null(fn_taxoff_attr)) {
        url_taxoff_attr <- file.path(url_bmt_base
                                     , "taxa_official"
                                     , "RMN"
                                     , fn_taxoff_attr)
        httr::GET(url_taxoff_attr
                  , httr::write_disk(temp_taxoff_attr <- tempfile(fileext = ".csv")))

        df_taxoff_attr <- read.csv(temp_taxoff_attr)
      }## IF ~ fn_taxoff_attr

      ## Data, Official Attributes, Meta Data----
      if (!is.null(fn_taxoff_meta)) {
        url_taxoff_attr_meta <- file.path(url_bmt_base
                                          , "taxa_official"
                                          , "RMN"
                                          , fn_taxoff_attr_meta)
        httr::GET(url_taxoff_attr_meta
                  , httr::write_disk(temp_taxoff_attr_meta <- tempfile(fileext = ".csv")))

        df_taxoff_attr_meta <- read.csv(temp_taxoff_attr_meta)
      }## IF ~ fn_taxaoff_meta

      ### Check TaxaID for bad characters----
      tnames_user <- sort(unique(df_input[, sel_user_taxaid]))
      tnames_iconv <- iconv(tnames_user)
      tnames_bad <- tnames_user[is.na(tnames_iconv) |
                                  tnames_user != tnames_iconv]
      tnames_recnum <- which(df_input[, sel_user_taxaid] %in% tnames_bad)
      if (length(tnames_bad) != 0) {
        # end process with pop up
        msg <- paste0("Bad (non-ASCII) characters in taxa names!"
                      , "\n\n"
                      , "Imported file record numbers:"
                      , "\n"
                      , "R doesn't count the title row so add one to get the row number in Excel."
                      , "\n\n"
                      , paste(tnames_recnum, collapse = "\n")
        )
        shinyalert::shinyalert(title = "Taxa Translate"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        validate(msg)
      }## IF ~ sel_user_taxaid ~ non-ASCII

      ## Calc, 03, Run Function ----
      prog_detail <- "Calculate, Taxa Trans"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # function parameters
      df_user                 <- df_input
      df_official             <- df_taxoff
      df_official_metadata    <- df_taxoff_meta
      taxaid_user             <- sel_user_taxaid
      taxaid_official_match   <- col_taxaid_official_match
      taxaid_official_project <- col_taxaid_official_project
      taxaid_drop             <- sel_taxaid_drop
      col_drop                <- user_col_drop #NULL #sel_col_drop
      sum_n_taxa_boo          <- TRUE
      sum_n_taxa_col          <- sel_user_ntaxa
      sum_n_taxa_group_by     <- c(sel_user_sampid
                                   , sel_user_taxaid
                                   , sel_user_groupby)

      ## run the function ----
      taxatrans_results <- BioMonTools::taxa_translate(df_user
                                                       , df_official
                                                       , df_official_metadata
                                                       , taxaid_user
                                                       , taxaid_official_match
                                                       , taxaid_official_project
                                                       , taxaid_drop
                                                       , col_drop
                                                       , sum_n_taxa_boo
                                                       , sum_n_taxa_col
                                                       , sum_n_taxa_group_by)

      ## Munge ----

      # Remove non-project taxaID cols
      # Specific to shiny project, not a part of the taxa_translate function
      col_keep <- !names(taxatrans_results$merge) %in% col_drop_project
      taxatrans_results$merge <- taxatrans_results$merge[, col_keep]

      # Attributes if have 2nd file
      if (!is.na(fn_taxoff_attr)) {
        df_ttrm <- taxatrans_results$merge
        # drop translation file columns
        col_keep_ttrm <- names(df_ttrm)[names(df_ttrm) %in% c(sel_user_sampid
                                                              , sel_user_taxaid
                                                              , sel_user_ntaxa
                                                              , "Match_Official"
                                                              , sel_user_groupby)]
        df_ttrm <- df_ttrm[, col_keep_ttrm]

        ### COMBINE same TaxaID and sum N_Taxa----
        # 20240806 from MNcalc
        boo_combine_taxa <- TRUE
        if (boo_combine_taxa) {
          # use 'known' values with tidyverse then change back
          #
          # name field to known value
          col_ttrm_ntaxa <- "ttrm_ntaxa"
          df_ttrm[, col_ttrm_ntaxa] <- df_ttrm[, sel_user_ntaxa]
          col_non_ntaxa <- names(df_ttrm)[!names(df_ttrm) %in% sel_user_ntaxa]
          # drop ntaxa
          df_ttrm <- df_ttrm[, col_non_ntaxa]
          # columns by (for summarize)
          col_by <- col_non_ntaxa[!col_non_ntaxa %in% col_ttrm_ntaxa]
          # sum
          df_ttrm <- dplyr::summarise(df_ttrm
                                      , .by = dplyr::all_of(col_by)
                                      , sum_ntaxa = sum(ttrm_ntaxa, na.rm = TRUE)
          )
          # rename 'known' ntaxa back to 'user' value
          names(df_ttrm)[names(df_ttrm) == "sum_ntaxa"] <- sel_user_ntaxa
        }## boo_combine_taxa

        # merge with attributes
        df_merge_attr <- merge(df_ttrm
                               , df_taxoff_attr
                               , by.x = taxaid_user
                               , by.y = col_taxaid_attr
                               , all.x = TRUE
                               , sort = FALSE
                               , suffixes = c("_xDROP", "_yKEEP"))
        # Drop duplicate names from Trans file (x)
        col_keep <- names(df_merge_attr)[!grepl("_xDROP$"
                                                , names(df_merge_attr))]
        df_merge_attr <- df_merge_attr[, col_keep]
        # KEEP and rename duplicate names from Attribute file (y)
        names(df_merge_attr) <- gsub("_yKEEP$", "", names(df_merge_attr))
        # Save back to results list
        taxatrans_results$merge <- df_merge_attr

        # QC check
        # testthat::expect_equal(nrow(df_merge_attr), nrow(df_ttrm))
        # testthat::expect_equal(sum(df_merge_attr[, sel_user_ntaxa], na.rm = TRUE)
        #                        , sum(df_ttrm[, sel_user_ntaxa], na.rm = TRUE))
      }## IF ~ !is.na(fn_taxoff_attr)

      # Reorder by SampID and TaxaID
      taxatrans_results$merge <- taxatrans_results$merge[
        order(taxatrans_results$merge[, sel_user_sampid]
              , taxatrans_results$merge[, sel_user_taxaid]), ]

      # Add input filenames
      taxatrans_results$merge[, "file_taxatrans"] <- fn_taxoff
      taxatrans_results$merge[, "file_attributes"] <- fn_taxoff_attr


      # Resort columns
      col_start <- c(sel_user_sampid
                     , sel_user_taxaid
                     , sel_user_ntaxa
                     , "file_taxatrans"
                     , "file_attributes")
      col_other <- names(taxatrans_results$merge)[!names(taxatrans_results$merge)
                                                  %in% col_start]
      taxatrans_results$merge <- taxatrans_results$merge[, c(col_start
                                                             , col_other)]

      # Convert required file names to standard
      ## do at end so don't have to modify any other variables
      boo_req_names <- TRUE
      if (boo_req_names == TRUE) {
        names(taxatrans_results$merge)[names(taxatrans_results$merge)
                                       %in% sel_user_sampid] <- "SampleID"
        names(taxatrans_results$merge)[names(taxatrans_results$merge)
                                       %in% sel_user_taxaid] <- "TaxaID"
        names(taxatrans_results$merge)[names(taxatrans_results$merge)
                                       %in% sel_user_ntaxa] <- "N_Taxa"
      }## IF ~ boo_req_names

      # Hack/Fix
      # Noteworthy NA causing issue later in Shiny app
      # 20231201, only if have Noteworthy
      if ("NOTEWORTHY" %in% toupper(taxatrans_results$merge)) {
        taxatrans_results$merge$Noteworthy <- ifelse(is.na(taxatrans_results$merge$Noteworthy)
                                                     , FALSE
                                                     , TRUE)
      }## IF ~ Noteworthy


      ## Calc, 04, Save Results ----
      prog_detail <- "Save Results"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Save files

      ## File version names
      df_save <- data.frame(Calculation = sel_proj
                            , OperationalTaxonomicUnit = col_taxaid_official_project
                            , TranslationTable = fn_taxoff
                            , AttributeTable = fn_taxoff_attr)
      fn_part <- paste0(dir_proj_results, fn_abr_save, "source", ".csv")
      write.csv(df_save
                , file.path(path_results_sub, fn_part)
                , row.names = FALSE)
      rm(df_save, fn_part)

      ## Taxa User
      # saved when imported

      # 2023-11-03, save original filenames
      # add taxatrans metadata

      ## Taxa Official
      # df_save <- df_official
      # fn_part <- paste0(fn_abr_save, "1official", ".csv")
      # write.csv(df_save
      #           , file.path(path_results_ref, paste0(fn_input_base, fn_part))
      #           , row.names = FALSE)
      # rm(df_save, fn_part)
      file.copy(temp_taxoff
                , file.path(path_results_ref, fn_taxoff))

      ## Taxa Official, meta data
      # df_save <- taxatrans_results$official_metadata # df_taxoff_meta
      # fn_part <- paste0(fn_abr_save, "1metadata", ".csv")
      # write.csv(df_save
      #           , file.path(path_results_ref, paste0(fn_input_base, fn_part))
      #           , row.names = FALSE)
      # rm(df_save, fn_part)
      file.copy(temp_taxoff_meta
                , file.path(path_results_ref, fn_taxoff_meta))

      ## Taxa Official, Attributes
      # df_save <- df_taxoff_attr
      # fn_part <- paste0(path_results_ref, "1attributes", ".csv")
      # write.csv(df_save
      #           , file.path(path_results, paste0(fn_input_base, fn_part))
      #           , row.names = FALSE)
      # rm(df_save, fn_part)
      file.copy(temp_taxoff_attr
                , file.path(path_results_ref, fn_taxoff_attr))

      ## Taxa Official, Attributes, meta data
      # df_save <- taxatrans_results$official_metadata # df_taxoff_meta
      # fn_part <- paste0(fn_abr_save, "1metadata", ".csv")
      # write.csv(df_save
      #           , file.path(path_results_ref, paste0(fn_input_base, fn_part))
      #           , row.names = FALSE)
      # rm(df_save, fn_part)
      file.copy(temp_taxoff_attr_meta
                , file.path(path_results_ref, fn_taxoff_attr_meta))

      ## translate - crosswalk
      df_save <- taxatrans_results$taxatrans_unique # df_taxoff_meta
      fn_part <- paste0(dir_proj_results, fn_abr_save, "modify", ".csv")
      write.csv(df_save
                , file.path(path_results_sub, fn_part)
                , row.names = FALSE)
      rm(df_save, fn_part)

      ## Non Match
      df_save <- data.frame(taxatrans_results$nonmatch)
      fn_part <- paste0(dir_proj_results, fn_abr_save, "nonmatch", ".csv")
      write.csv(df_save
                , file.path(path_results_sub, fn_part)
                , row.names = FALSE)
      rm(df_save, fn_part)

      ## Taxa Trans
      df_save <- taxatrans_results$merge
      fn_part <- paste0(dir_proj_results, fn_abr_save, "TAXAATTR", ".csv")
      write.csv(df_save
                , file.path(path_results_sub, fn_part)
                , row.names = FALSE)
      rm(df_save, fn_part)

      ## Calc, 05, Create Zip ----
      prog_detail <- "Create Zip File For Download"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Create zip file for download
      fn_4zip <- list.files(path = path_results
                            , full.names = TRUE)
      zip::zip(file.path(path_results, "results.zip"), fn_4zip)

      ## Calc, 06, Info Pop Up ----
      prog_detail <- "Calculate, Info"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(2 * prog_sleep)

      # Inform user about number of taxa mismatches
      ## calc number of mismatch
      df_mismatch <- data.frame(taxatrans_results$nonmatch)
      n_taxa_mismatch <- nrow(df_mismatch)
      msg <- paste0("Number of mismatch taxa = ", n_taxa_mismatch, "\n\n"
                    , "Any mismatched taxa in 'mismatch' file in results download.")
      shinyalert::shinyalert(title = "Taxa Translate, Non Matching Taxa"
                             , text = msg
                             , type = "info"
                             , closeOnEsc = TRUE
                             , closeOnClickOutside = TRUE)
      #validate(msg)

      ## Calc, 07, Clean Up ----
      prog_detail <- "Calculate, Clean Up"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      rm(df_mismatch)

      # button, enable, download
      shinyjs::enable("b_download_taxatrans")

    }## expr ~ withProgress ~ END
    , message = "Taxa Translator"
    )## withProgress

  }##expr ~ ObserveEvent

  )##observeEvent ~ b_taxatrans_calc

  ## b_download_TaxaTrans ----
  output$b_download_taxatrans <- downloadHandler(

    filename = function() {
      inFile <- input$fn_input
      fn_input_base <- tools::file_path_sans_ext(inFile$name)
      fn_abr <- abr_taxatrans
      fn_abr_save <- paste0("_", fn_abr, "_")
      paste0(fn_input_base
             , fn_abr_save
             , format(Sys.time(), "%Y%m%d_%H%M%S")
             , ".zip")
    } ,
    content = function(fname) {##content~START

      file.copy(file.path(path_results, "results.zip"), fname)

    }##content~END
    #, contentType = "application/zip"
  )##download ~ TaxaTrans

  # SUBSAMPLE ----

  ## Subsample, ColNames ----
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

  ## Subsample, B Run ----
  observeEvent(input$b_subsample_run, {
    shiny::withProgress({
      #
      # Number of increments
      n_inc <- 5

      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Initialize")
      Sys.sleep(0.25)

      # enable download button
      shinyjs::disable("b_subsample_download")

      # Function, Parameters
      fun_inbug <- df_import()
      fun_sample.ID <- input$subsamp_col_SampID
      fun_abund <- input$subsamp_col_Count
      fun_subsiz <- input$subsamp_target
      fun_mySeed <- input$subsamp_seed
      fun_taxaID <- input$subsamp_col_TaxaID

      # QC

      # Function, Run
      #
      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Run Function")
      Sys.sleep(0.25)
      #
      df_fun <- BioMonTools::rarify(inbug = fun_inbug
                                    , sample.ID = fun_sample.ID
                                    , abund = fun_abund
                                    , subsiz = fun_subsiz
                                    , mySeed = fun_mySeed)
      # Munge
      #
      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Munge Results")
      Sys.sleep(0.25)
      #
      df_fun <- df_fun[, c(fun_sample.ID, fun_taxaID, fun_abund)]
      df_munge <- merge(fun_inbug, df_fun
                        , by = c(fun_sample.ID, fun_taxaID)
                        , suffixes = c("_Orig", paste0("_", fun_subsiz)))

      # QC check
      # compare totals
      df_totals <- aggregate(x = df_munge[, c(paste0(fun_abund, "_Orig")
                                          , paste0(fun_abund, "_", fun_subsiz))]
                            , by = list(df_munge[, fun_sample.ID])
                            , FUN = sum)
      names(df_totals)[1] <- fun_sample.ID
     # df_totals <- df_totals[order(paste0(fun_abund, "_Orig"), fun_sample.ID), ]


      # Function, Save
      #
      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Save Results")
      Sys.sleep(0.25)
      #
      fn_results <- file.path("results", "subsample", "results_subsample.csv")
      write.csv(df_munge, fn_results, row.names = FALSE)
      #
      fn_totals <- file.path("results", "subsample", "results_subsample_totals.csv")
      write.csv(df_totals, fn_totals, row.names = FALSE)

      # Create Zip file
      #
      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Create Zip File")
      Sys.sleep(0.25)
      #
      # Create zip file
      fn_4zip <- list.files(path = file.path("results", "subsample")
                            , full.names = TRUE)
      zip(file.path("results", "subsample.zip"), fn_4zip)

      # enable download button
      shinyjs::enable("b_subsample_download")
      #
    }##expr~withProgress~END
    , message = "SubSample"
    )##withProgress~END
  }##expr~ObserveEvent~END
  )##observeEvent~END

  ## Subsample, B Download ----
  output$b_subsample_download <- downloadHandler(
    # use index and date time as file name
    #myDateTime <- format(Sys.time(), "%Y%m%d_%H%M%S")

    filename = function() {
      paste0("BioMonTools_SubSample_"
             , format(Sys.time(), "%Y%m%d_%H%M%S")
             , ".zip")
    },
    content = function(fname) {##content~START
      # tmpdir <- tempdir()
      #setwd(tempdir())
      # fs <- c("input.csv", "metval.csv", "metsc.csv")
      # file.copy(inFile$datapath, "input.csv")
      # file.copy(inFile$datapath, "metval.tsv")
      # file.copy(inFile$datapath, "metsc.tsv")
      # file.copy(inFile$datapath, "IBI_plot.jpg")
      # write.csv(datasetInput(), file="input.csv", row.names = FALSE)
      # write.csv(datasetInput(), file="metval.csv", row.names = FALSE)
      # write.csv(datasetInput(), file="metsc.csv", row.names = FALSE)
      #
      # Create Zip file
      #zip(zipfile = fname, files=fs)
      #if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip")
      # , fname)}

      file.copy(file.path("results", "subsample.zip"), fname)

      #
    }##content~END
    #, contentType = "application/zip"
  )##downloadData~END


  # MARKEXCL ----

  ## MarkExcl, ColNames ----
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
    str_col <- "Taxa Levels"
    #
    # Get TaxaLevel names present in user file
    names_df <- names(df_import())
    phylo_all <- c("Kingdom"
                   , "Phylum"
                   , "SubPhylum"
                   , "Class"
                   , "SubClass"
                   , "Order"
                   , "SubOrder"
                   , "InfraOrder"
                   , "SuperFamily"
                   , "Family"
                   , "SubFamily"
                   , "Tribe"
                   , "Genus"
                   , "SubGenus"
                   , "Species"
                   , "Variety")
    phylo_user <- names_df[toupper(names_df) %in% toupper(phylo_all)]
    phylo_user_sort <- phylo_user[order(match(toupper(phylo_user), toupper(phylo_all)))]
    #
    selectInput("markexcl_Phylo"
                , label = str_col
                , choices = phylo_user_sort
                , multiple = TRUE
                , selected = phylo_user_sort)
  })## UI_colnames


  ## MarkExcl, B Run -----
  observeEvent(input$b_markexcl_run, {
    shiny::withProgress({
      #
      # Number of increments
      n_inc <- 3

      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Initialize")
      Sys.sleep(0.25)

      # enable download button
      shinyjs::disable("b_markexcl_download")

      # Function, Parameters
      fun_df_samptax <- df_import()
      fun_SampID     <- input$markexcl_col_SampID
      fun_TaxaID     <- input$markexcl_col_TaxaID
      fun_TaxaCount  <- input$markexcl_col_Count
      fun_Exclude    <- input$markexcl_Exclude
      fun_TaxaLevels <- input$markexcl_Phylo

      # ## Get TaxaLevel names present in user file
      # names_df <- names(fun_df_samptax)
      # phylo_all <- c("Kingdom"
      #                 , "Phylum"
      #                 , "SubPhylum"
      #                 , "Class"
      #                 , "SubClass"
      #                 , "Order"
      #                 , "SubOrder"
      #                 , "InfraOrder"
      #                 , "SuperFamily"
      #                 , "Family"
      #                 , "SubFamily"
      #                 , "Tribe"
      #                 , "Genus"
      #                 , "SubGenus"
      #                 , "Species"
      #                 , "Variety")
      # fun_TaxaLevels <- phylo_all[toupper(phylo_all) %in% toupper(names_df)]

      # QC

      # Function, Run
      #
      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Run Function")
      Sys.sleep(0.25)
      #
      df_fun <- BioMonTools::markExcluded(df_samptax = fun_df_samptax
                                          , SampID = fun_SampID
                                          , TaxaID = fun_TaxaID
                                          , TaxaCount = fun_TaxaCount
                                          , Exclude = fun_Exclude
                                          , TaxaLevels = fun_TaxaLevels
                                          , Exceptions = NA)

      # Function, Save
      #
      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Save Results")
      Sys.sleep(0.25)
      #
      fn_results <- file.path("results", "markexcl", "results_markexcl.csv")
      write.csv(df_fun, fn_results, row.names = FALSE)
      #

      # enable download button
      shinyjs::enable("b_markexcl_download")
      #
    }##expr~withProgress~END
    , message = "Calculating IBI"
    )##withProgress~END
  }##expr~ObserveEvent~END
  )##observeEvent~ b_markexcl

  ## MarkExcl, B Download ----
  output$b_markexcl_download <- downloadHandler(
    # use index and date time as file name
    #myDateTime <- format(Sys.time(), "%Y%m%d_%H%M%S")

    filename = function() {
      paste0("BioMonTools_MarkExcl_", "_"
            , format(Sys.time(), "%Y%m%d_%H%M%S")
            , ".csv")
    },
    content = function(fname) {##content~START
      # tmpdir <- tempdir()
      #setwd(tempdir())
      # fs <- c("input.csv", "metval.csv", "metsc.csv")
      # file.copy(inFile$datapath, "input.csv")
      # file.copy(inFile$datapath, "metval.tsv")
      # file.copy(inFile$datapath, "metsc.tsv")
      # file.copy(inFile$datapath, "IBI_plot.jpg")
      # write.csv(datasetInput(), file="input.csv", row.names = FALSE)
      # write.csv(datasetInput(), file="metval.csv", row.names = FALSE)
      # write.csv(datasetInput(), file="metsc.csv", row.names = FALSE)
      #
      # Create Zip file
      #zip(zipfile = fname, files=fs)
      #if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip")
      # , fname)}

      file.copy(file.path("results", "markexcl", "results_markexcl.csv"), fname)

      #
    }##content~END
    #, contentType = "application/zip"
  )##downloadData~END


  # TAXAMAPS ----

  ## TaxaMaps, ColNames ----
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
                , label = "Latitude" #str_col
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
                , label = "(OPTIONAL) Grouping Variable" #str_col
                , choices = names(df_import())
                , multiple = FALSE)
  })## UI_colnames

  ## TaxaMaps, B Run ----
  observeEvent(input$b_taxamaps_run, {
    shiny::withProgress({

            # Number of increments
      n_inc <- 2

      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Initialize")
      Sys.sleep(0.25)

      # enable download button
      shinyjs::disable("b_subsample_download")

      # Function, Parameters
      fun_df_obs <- df_import()
      fun_SampID <- input$taxamaps_col_SampID
      fun_TaxaID <- input$taxamaps_col_TaxaID
      fun_TaxaCount <- input$taxamaps_col_Count
      fun_Lat <- input$taxamaps_col_Lat
      fun_Long <- input$taxamaps_col_Long
      fun_database <- input$taxamaps_database
      fun_regions <- unlist(strsplit(input$taxamaps_regions, ","))
      fun_map_grp <- input$taxamaps_col_Group
      #fun_leg_loc <- input$taxamaps_leg_loc

      # QC

      # Function, Run
      #
      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Run Function")
      Sys.sleep(0.25)
      #
      BioMonTools::MapTaxaObs(df_obs = fun_df_obs
                              , SampID = fun_SampID
                              , TaxaID = fun_TaxaID
                              , TaxaCount = fun_TaxaCount
                              , Lat = fun_Lat
                              , Long = fun_Long
                              , output_dir = file.path("results", "taxamaps")
                              , ouput_type = "pdf"
                              , database = fun_database
                              , regions = fun_regions
                              , map_grp = fun_map_grp
                              #, leg_loc = fun_leg_loc
                              )


      # enable download button
      shinyjs::enable("b_taxamaps_download")
      #
    }##expr~withProgress~END
    , message = "Taxa Maps"
    )##withProgress~END
  }##expr~ObserveEvent~END
  )##observeEvent~b_CalcIBI~END

  ## TaxaMaps, B Download ----
  output$b_taxamaps_download <- downloadHandler(
    # use index and date time as file name
    #myDateTime <- format(Sys.time(), "%Y%m%d_%H%M%S")

    filename = function() {
      paste0("BioMonTools_TaxaMaps_"
             , format(Sys.time(), "%Y%m%d_%H%M%S")
             , ".pdf")
    },
    content = function(fname) {##content~START
      # tmpdir <- tempdir()
      #setwd(tempdir())
      # fs <- c("input.csv", "metval.csv", "metsc.csv")
      # file.copy(inFile$datapath, "input.csv")
      # file.copy(inFile$datapath, "metval.tsv")
      # file.copy(inFile$datapath, "metsc.tsv")
      # file.copy(inFile$datapath, "IBI_plot.jpg")
      # write.csv(datasetInput(), file="input.csv", row.names = FALSE)
      # write.csv(datasetInput(), file="metval.csv", row.names = FALSE)
      # write.csv(datasetInput(), file="metsc.csv", row.names = FALSE)
      #
      # Create Zip file
      #zip(zipfile = fname, files=fs)
      #if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip")
      # , fname)}

      file.copy(file.path("results", "taxamaps", "maps.taxa.pdf"), fname)

      #
    }##content~END
    #, contentType = "application/zip"
  )##downloadData~END

  # CALCMET ----

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

  ## calcmet, ColNames ----
  output$UI_col_calcmet_Cols2Keep <- renderUI({
    str_col <- "Columns to keep"
    col_req <- c("SAMPLEID", "TAXAID", "N_TAXA", "EXCLUDE", "INDEX_NAME"
                 , "INDEX_CLASS", "NONTARGET", "PHYLUM", "SUBPHYLUM", "CLASS"
                 , "SUBCLASS", "INFRAORDER", "ORDER", "FAMILY", "SUBFAMILY"
                 , "TRIBE", "GENUS", "FFG", "HABIT", "LIFE_CYCLE", "TOLVAL"
                 , "BCG_ATTR", "THERMAL_INDICATOR", "LONGLIVED", "NOTEWORTHY"
                 , "FFG2", "TOLVAL2", "HABITAT", "UFC", "ELEVATION_ATTR"
                 , "GRADIENT_ATTR", "WSAREA_ATTR")
    col_all <- names(df_import())
    col_choices <- col_all[!(toupper(col_all) %in% toupper(col_req))]
    selectInput("calcmet_Cols2Keep"
                , label = str_col
                , choices = col_choices
                , multiple = TRUE)
  })## UI_colnames

  ## calcmet, B Run ----
  observeEvent(input$b_calcmet_run, {
    shiny::withProgress({
      #
      # Number of increments
      n_inc <- 6
      sleep_num <- 0.33 #0.33

      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Initialize")
      Sys.sleep(sleep_num)

      # enable download button
      shinyjs::disable("b_calcmet_download")

      # Function, Parameters
      fun_fun.DF <- df_import()
      fun_fun.Community <- input$calcmet_community
      fun_fun.cols2keep <- unlist(input$calcmet_Cols2Keep)

      # QC

      # Function, Run
      #
      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Run Function")
      Sys.sleep(sleep_num)
      #
      df_fun <- BioMonTools::metric.values(fun.DF = fun_fun.DF
                                           , fun.Community = fun_fun.Community
                                           , fun.MetricNames = NULL
                                           , boo.Adjust = FALSE
                                           , fun.cols2keep = fun_fun.cols2keep
                                           , boo.marine = FALSE
                                           , boo.Shiny = TRUE
                                           , verbose = TRUE)

      # Function, Save
      #
      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Save Results, csv")
      Sys.sleep(sleep_num)
      #
      fn_results <- file.path("results", "calcmet", "results_calcmet.csv")
      write.csv(df_fun, fn_results, row.names = FALSE)
      #
      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Save Results, Excel")
      Sys.sleep(sleep_num)
      #
      # Save as Excel
      df_metnames <- readxl::read_excel(system.file("extdata/MetricNames.xlsx"
                                                    , package = "BioMonTools")
                                        , guess_max = 10^6
                                        , sheet = "MetricMetadata"
                                        , skip = 4)
      fn_metvalxl <- file.path("results"
                               , "calcmet"
                               , paste0("results_calcmet.xlsx"))
      BioMonTools::metvalgrpxl(fun.DF.MetVal = df_fun
                               , fun.DF.xlMetNames = df_metnames
                               , fun.Community = fun_fun.Community
                               , fun.MetVal.Col2Keep = c("SAMPLEID"
                                                         , "INDEX_NAME"
                                                         , "INDEX_CLASS")
                               , fun.xlGrpCol = "Sort_Group2"
                               , file.out = fn_metvalxl)

      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Save Results, zip")
      Sys.sleep(sleep_num)

      # Create zip file
      fn_4zip <- list.files(path = file.path("results", "calcmet")
                            , full.names = TRUE)
      zip(file.path("results", "calcmet.zip"), fn_4zip)

      # enable download button
      shinyjs::enable("b_calcmet_download")

      #
    }##expr~withProgress~END
    , message = "Calculate Metrics"
    )##withProgress~END
  }##expr~ObserveEvent~END
  )##observeEvent~b_calcmet~END

  ## calcmet, B Download ----
  output$b_calcmet_download <- downloadHandler(
    # use index and date time as file name
    #myDateTime <- format(Sys.time(), "%Y%m%d_%H%M%S")

    filename = function() {
      paste0("BioMonTools_CalcMet_"
             , format(Sys.time(), "%Y%m%d_%H%M%S")
             , ".zip")
    },
    content = function(fname) {##content~START
      # tmpdir <- tempdir()
      #setwd(tempdir())
      # fs <- c("input.csv", "metval.csv", "metsc.csv")
      # file.copy(inFile$datapath, "input.csv")
      # file.copy(inFile$datapath, "metval.tsv")
      # file.copy(inFile$datapath, "metsc.tsv")
      # file.copy(inFile$datapath, "IBI_plot.jpg")
      # write.csv(datasetInput(), file="input.csv", row.names = FALSE)
      # write.csv(datasetInput(), file="metval.csv", row.names = FALSE)
      # write.csv(datasetInput(), file="metsc.csv", row.names = FALSE)
      #
      # Create Zip file
      #zip(zipfile = fname, files=fs)
      #if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip")
      # , fname)}

      file.copy(file.path("results", "calcmet.zip"), fname)

      #
    }##content~END
    #, contentType = "application/zip"
  )##downloadData~END

})## shinyServer ~ END
