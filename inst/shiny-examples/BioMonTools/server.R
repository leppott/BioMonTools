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
                 , "INDEX_REGION", "NONTARGET", "PHYLUM", "SUBPHYLUM", "CLASS"
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

      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Initialize")
      Sys.sleep(0.25)

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
      Sys.sleep(0.25)
      #
      df_fun <- BioMonTools::metric.values(fun.DF = fun_fun.DF
                                           , fun.Community = fun_fun.Community
                                           , fun.MetricNames = NULL
                                           , boo.Adjust = FALSE
                                           , fun.cols2keep = fun_fun.cols2keep
                                           , boo.marine = FALSE
                                           , boo.Shiny = TRUE)


      # Function, Save
      #
      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Save Results, csv")
      Sys.sleep(0.25)
      #
      fn_results <- file.path("results", "calcmet", "results_calcmet.csv")
      write.csv(df_fun, fn_results, row.names = FALSE)
      #
      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Save Results, Excel")
      Sys.sleep(0.25)
      #
      # Save as Excel
      df_metnames <- readxl::read_excel(system.file("extdata/MetricNames.xlsx"
                                                    , package="BioMonTools")
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
                                                         , "INDEX_REGION")
                               , file.out = fn_metvalxl)

      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Save Results, zip")
      Sys.sleep(0.25)

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
