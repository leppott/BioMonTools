#' @title Calculate metric values
#'
#' @description This function calculates metric values for bugs and fish.
#' Inputs are a data frame with SampleID and taxa with phylogenetic and
#' autecological information (see below for required fields by community).
#' The dplyr package is used to generate the metric values.
#'
#' @details All percent metric results are 0-100.
#'
#' No manipulations of the taxa are performed by this routine.
#' All benthic macroinvertebrate taxa should be identified to the appropriate
#' operational taxonomic unit (OTU).
#'
#' Any non-count taxa should be identified in the "Exclude" field as "TRUE".
#' These taxa will be excluded from taxa richness metrics (but will count for
#' all others).
#'
#' Any non-target taxa should be identified in the "NonTarget"
#' field as "TRUE".  Non-target taxa are those that are not part of your
#' intended #' capture list; e.g., fish,  herps, water column taxa, or water
#' surface taxa in a benthic sample.  The target list will vary by program.  The
#' non-target taxa will be removed prior to any calculations.
#'
#' Excluded taxa are ambiguous taxa (on a sample basis), i.e.,
#' the parent taxa when child taxa are present.  For example, the parent taxa
#' Chironomidae would be excluded when the child taxa Tanytarsini is present.
#' Both would be excluded when Tanytarsus is present.  The markExcluded function
#' can be used to populated this field.
#'
#' There are a number of required fields (see below) for metric to calculation.
#' If any fields are missing the user will be prompted as to which are missing
#' and if the user wants to continue or quit.  If the user continues the missing
#' fields will be added but will be filled with zero or NA (as appropriate).
#' Any metrics based on the missing fields will not be valid.
#'
#' A future update may turn these fields into function parameters.  This would
#' allow the user to tweak the function inputs to match their data rather than
#' having to update their data to match the function.
#'
#' Required fields, all communities:
#'
#' * SAMPLEID (character or number, must be unique)
#'
#' * TAXAID (character or number, must be unique)
#'
#' * N_TAXA
#'
#' * INDEX_NAME
#'
#' * INDEX_CLASS (BCG or MMI site category; e.g., for BCG PacNW valid values
#' are "hi" or "lo")
#'
#' Additional Required fields, bugs:
#'
#' * EXCLUDE (valid values are TRUE and FALSE)
#'
#' * NONTARGET (valid values are TRUE and FALSE)
#'
#' * PHYLUM, SUBPHYLUM, CLASS, SUBCLASS, INFRAORDER, ORDER, FAMILY, SUBFAMILY,
#' TRIBE, GENUS
#'
#' * FFG, HABIT, LIFE_CYCLE, TOLVAL, BCG_ATTR, THERMAL_INDICATOR, FFG2, TOLVAL2,
#' LONGLIVED, NOTEWORTHY, HABITAT, UFC, ELEVATION_ATTR, GRADIENT_ATTR,
#' WSAREA_ATTR, HABSTRUCT
#'
#' Additional Required fields, fish:
#'
#' * N_ANOMALIES
#'
#' * SAMP_BIOMASS (biomass total for sample, funciton uses max in case entered
#' for all taxa in sample)
#'
#' * DA_MI2, SAMP_WIDTH_M, SAMP_LENGTH_M, , TYPE, TOLER, NATIVE, TROPHIC, SILT,
#' FAMILY, GENUS, HYBRID, BCG_ATTR, THERMAL_INDICATOR, ELEVATION_ATTR,
#' GRADIENT_ATTR, WSAREA_ATTR, REPRODUCTION, HABITAT, CONNECTIVITY, SCC
#'
#' Additional Required fields, algae:
#'
#' * EXCLUDE, NONTARGET, PHYLUM, ORDER, FAMILY, GENUS, BC_USGS, TROPHIC_USGS,
#' SAP_USGS, PT_USGS, O_USGS, SALINITY_USGS, BAHLS_USGS, P_USGS, N_USGS,
#' HABITAT_USGS, N_FIXER_USGS, MOTILITY_USGS, SIZE_USGS, HABIT_USGS,
#' MOTILE2_USGS, TOLVAL, DIATOM_ISA, DIAT_CL, POLL_TOL, BEN_SES, DIATAS_TP,
#' DIATAS_TN, DIAT_COND, DIAT_CA, MOTILITY, NF
#'
#' Valid values for fields:
#'
#' * FFG: CG, CF, PR, SC, SH
#'
#' * HABIT: BU, CB, CN, SP, SW
#'
#' * LIFE_CYCLE: UNI, SEMI, MULTI
#'
#' * THERMAL_INDICATOR: STENOC, COLD, COOL, WARM, STENOW, EURYTHERMAL
#' , COWA, NA
#'
#' * LONGLIVED: TRUE, FALSE
#'
#' * NOTEWORTHY: TRUE, FALSE
#'
#' * HABITAT: BRAC, DEPO, GENE, HEAD, RHEO, RIVE, SPEC, UNKN
#'
#' * UFC: integers 1:6 (taxonomic uncertainty frequency class)
#'
#' * ELEVATION_ATTR: LOW, HIGH
#'
#' * GRADIENT_ATTR: LOW, MOD, HIGH
#'
#' * WSAREA_ATTR: SMALL, MEDIUM, LARGE, XLARGE
#'
#' * REPRODUCTION: BROADCASTER, SIMPLE NEST, COMPLEX NEST, BEARER, MIGRATORY
#'
#' * CONNECTIVITY: TRUE, FALSE
#'
#' * SCC (Species of Conservation Concern): TRUE, FALSE
#'
#' 'Columns to keep' are additional fields in the input file that the user wants
#' retained in the output.  Fields need to be those that are unique per sample
#' and not associated with the taxa.  For example, the fields used in
#' qc.check(); Area_mi2, SurfaceArea, Density_m2, and Density_ft2.
#'
#' If fun.MetricNames is provided only those metrics will be returned in the
#' provided order. This variable can be used to sort the metrics per the user's
#' preferences. By default the metric names will be returned in the groupings
#' that were used for calculation.
#'
#' The fields TOLVAL2 and FFG2 are provided to allow the user to calculate
#' metrics based on alternative scenarios.  For example, including both HBI and
#' NCBI where the NCBI uses a different set of tolerance values (TOLVAL2).
#'
#' If TAXAID is 'NONE' and N_TAXA is '0' then metrics **will** be calculated
#' with that record. Other values for TAXAID with N_TAXA = 0 will be removed
#' before calculations.
#'
#' For 'Oligochete' metrics either Class or Subclass is required for
#' calculation.
#'
#' The parameter boo.Shiny can be set to TRUE when accessing this function in
#' Shiny. Normally the QC check for required fields is interactive.  Setting
#' boo.Shiny to TRUE will always continue.  The default is FALSE.
#'
#' The parameter 'taxaid_dni' denotes taxa to be included in Do Not Include
#' (DNI) metrics but dropped from all other metrics.  Only for benthic metrics.
#'
#' Breaking change from 0.5 to 0.6 with change from Index_Name to Index_Class.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @param fun.DF Data frame of taxa (list required fields)
#' @param fun.Community Community name for which to calculate metric values
#' (bugs, fish, or algae)
#' @param fun.MetricNames Optional vector of metric names to be returned.
#' If none are supplied then all will be returned.  Default=NULL
#' @param boo.Adjust Optional boolean value on whether to perform adjustments of
#' values prior to scoring.  Default = FALSE but may be TRUE for certain
#' metrics.
#' @param fun.cols2keep Column names of fun.DF to retain in the output.  Uses
#' column names.
# @param MetricSort How metric names should be sort; NA = as is, AZ =
# alphabetical.  Default = NULL.
#' @param boo.marine Should estuary/marine metrics be included.
#' Ignored if fun.MetricNames is not null. Default = FALSE.
#' @param boo.Shiny Boolean value for if the function is accessed via Shiny.
#' Default = FALSE.
#' @param verbose Include messages to track progress.  Default = FALSE
#' @param metric_subset Subset of metrics to be generated.  Internal function.
#' Default = NULL
#' @param taxaid_dni Taxa names to be included in DNI (Do Not Include) metrics
#' (n = 3) but dropped for all other metrics.  Only for benthic metrics.
#' Default = NULL
#'
#' @return data frame of SampleID and metric values
#'
#' @examples
#' # Example 1, data already in R
#'
#' df_metric_values_bugs <- metric.values(data_benthos_PacNW, "bugs")
#'
#' \dontrun{
#' # View Results
#' View(df_metric_values_bugs)
#' }
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Example 2, data from Excel
#'\dontrun{
#' # Packages
#' library(readxl)
#' library(reshape2)
#'
#' df_samps_bugs <- read_excel(system.file("extdata/Data_Benthos.xlsx"
#'                                        , package = "BioMonTools")
#'                             , guess_max = 10^6)
#'
#' # Columns to keep
#' myCols <- c("Area_mi2", "SurfaceArea", "Density_m2", "Density_ft2")
#'
#' # Run Function
#' df_metric_values_bugs <- metric.values(df_samps_bugs[1:100, ]
#'                                        , "bugs"
#'                                        , fun.cols2keep = myCols)
#'
#' # View Results
#' View(df_metric_values_bugs)
#' }
#'
#' # Get data in long format so can QC results more easily
#' df_long <- melt(df_metric_values_bugs, id.vars = c("SAMPLEID"
#'                                                  , "INDEX_NAME"
#'                                                  , "INDEX_CLASS"
#'                                                  , toupper(myCols))
#'                           , variable.name = "METRIC_NAME"
#'                           , value.name = "METRIC_VALUE")
#'
#'\dontrun{
#' # Save Results
#' write.table(df_long, file.path(tempdir(), "metric.values.tsv")
#'             , col.names = TRUE, row.names = FALSE, sep = "\t")
#'
#' # DataExplorer Report
#' library(DataExplorer)
#' create_report(df_metric_values_bugs
#'               , output_file = file.path(tempdir()
#'                                  , "DataExplorer_Report_MetricValues.html"))
#' create_report(df_samps_bugs
#'               , output_file = file.path(tempdir()
#'                                    , "DataExplorer_Report_BugSamples.html"))
#' }
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Example 3, specific metrics or metrics in a specific order
#' ## reuse df_samps_bugs from above
#'
#' # metric names to keep (in this order)
#' myMetrics <- c("ni_total", "nt_EPT", "nt_Ephem", "pi_tv_intol", "pi_Ephem"
#'                , "nt_ffg_scrap", "pi_habit_climb")
#'
#' # Run Function
#' df_metric_values_bugs_myMetrics <- metric.values(df_samps_bugs, "bugs"
#'                                                , fun.MetricNames = myMetrics)
#'\dontrun{
#' # View Results
#' View(df_metric_values_bugs_myMetrics)
#' }
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Example 4, fish metrics
#'
#' df_metric_values_fish <- metric.values(data_fish_MBSS, "fish")
#'
#'\dontrun{
#' # View Results
#' View(df_metric_values_fish)
#' }
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Example 5, periphyton (algae) metrics
#'
#' # df_metric_values_periphyton <- metric.values(data_diatom_mmi_dev, "algae")
#'
#'\dontrun{
#' # View Results
# # View(df_metric_values_periphyton)
#' }
#'
# #~~~~~~~~~~~~~~~~~~~~~~~
# # INDIANA BCG
#
# library(readxl)
# library(reshape2)
#
# df.samps.bugs <- read_excel(system.file("./extdata/Data_BCG_Indiana.xlsx"
#                            , package="BCGcalc"), sheet="R_Input")
# dim(df.samps.bugs)
# # rename some fields
# names(df.samps.bugs)
# names(df.samps.bugs)[names(df.samps.bugs)=="VisitNum"] <- "SampleID"
# names(df.samps.bugs)[names(df.samps.bugs)=="FinalID"] <- "TaxaID"
# names(df.samps.bugs)[names(df.samps.bugs)=="Count"] <- "N_Taxa"
# # Add field
# df.samps.bugs[, "INDEX_NAME"] <- "BCG.IN"
# #
# # Run Function
# myDF <- df.samps.bugs
# df.metric.values.bugs <- metric.values(myDF, "bugs")
#
# # View Results
# View(df.metric.values.bugs)
#
# # Get data in long format so can QC results more easily
# df.long <- melt(df.metric.values.bugs, id.vars=c("SAMPLEID"
#                                                 , "INDEX_NAME"
#                                                 , "INDEX_CLASS")
#                           , variable.name="METRIC_NAME"
#                           , value.name="METRIC_VALUE")
# # Save Results
# write.table(df.long, "metric.values.tsv", col.names=TRUE, row.names=FALSE
#      , sep="\t")
#
# # DataExplorer Report
# library(DataExplorer)
# create_report(df.metric.values.bugs, "DataExplorer_Report_MetricValues.html")
# create_report(df.samps.bugs, "DataExplorer_Report_BugSamples.html")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# QC 20180319
#
# library(readxl)
# df.samps.bugs <- read_excel(system.file("./extdata/Data_BCG_PacNW.xlsx"
#                                       , package="BCGcalc"))
# fun.DF <- df.samps.bugs
# # PREP
# fun.DF <- as.data.frame(fun.DF)
# names(fun.DF) <- toupper(names(fun.DF))
# fun.DF <- fun.DF[fun.DF[,"N_TAXA"]>0, ]
# fun.DF <- fun.DF[fun.DF[,"NONTARGET"]==FALSE,]
# fun.DF[,"INDEX_CLASS"] <- toupper(fun.DF[,"INDEX_CLASS"])
# #
# myDF <- fun.DF
# #
# # Convert values to upper case (FFG, Habit, Life_Cycle)
# myDF[, "HABIT"] <- toupper(myDF[, "HABIT"])
# myDF[, "FFG"] <- toupper(myDF[, "FFG"])
# myDF[, "LIFE_CYCLE"] <- toupper(myDF[, "LIFE_CYCLE"])
# myDF[, "THERMAL_INDICATOR"] <- toupper(myDF[, "THERMAL_INDICATOR"])
# # Add extra columns for FFG and Habit
# # (need unique values for functions in summarise)
# # each will be TRUE or FALSE
# myDF[, "HABIT_BU"] <- grepl("BU", myDF[, "HABIT"])
# myDF[, "HABIT_CB"] <- grepl("CB", myDF[, "HABIT"])
# myDF[, "HABIT_CN"] <- grepl("CN", myDF[, "HABIT"])
# myDF[, "HABIT_SP"] <- grepl("SP", myDF[, "HABIT"])
# myDF[, "HABIT_SW"] <- grepl("SW", myDF[, "HABIT"])
# myDF[, "FFG_COL"]  <- grepl("CG", myDF[, "FFG"])
# myDF[, "FFG_FIL"]  <- grepl("CF", myDF[, "FFG"])
# myDF[, "FFG_PRE"]  <- grepl("PR", myDF[, "FFG"])
# myDF[, "FFG_SCR"]  <- grepl("SC", myDF[, "FFG"])
# myDF[, "FFG_SHR"]  <- grepl("SH", myDF[, "FFG"])
# myDF[, "LC_MULTI"] <- grepl("MULTI", myDF[, "LIFE_CYCLE"])
# myDF[, "LC_SEMI"]  <- grepl("SEMI", myDF[, "LIFE_CYCLE"])
# myDF[, "LC_UNI"]   <- grepl("UNI", myDF[, "LIFE_CYCLE"])
# myDF[, "TI_COLD"] <- grepl("COLD", myDF[, "THERMAL_INDICATOR"])
# myDF[, "TI_COLDCOOL"]   <- grepl("COLD_COOL", myDF[, "THERMAL_INDICATOR"])
# myDF[, "TI_COOLWARM"]   <- grepl("COOL_WARM", myDF[, "THERMAL_INDICATOR"])
# myDF[, "TI_WARM"]   <- grepl("WARM", myDF[, "THERMAL_INDICATOR"])
# #
# `%>%` <- dplyr::`%>%`
# mySamp <- "06039CSR_Bug_2006-07-13_0"
# x <- dplyr::filter(myDF, SAMPLEID==mySamp)
# # 26 taxa
# x <- dplyr::filter(myDF, SAMPLEID==mySamp, (BCG_ATTR == "4" | BCG_ATTR == "5"
#                                     | BCG_ATTR == "6"))
# # 22 taxa (good)
# x <- dplyr::filter(myDF, SAMPLEID==mySamp
#                    , (is.na(CLASS)==TRUE | (CLASS != "Insecta" & CLASS != "ARACHNIDA"))
#                    , (is.na(ORDER) == TRUE | (ORDER != "DECAPODA" & ORDER!="RISSOOIDEA"))
#                    , (is.na(GENUS) == TRUE | GENUS!="Juga")
#                    , (BCG_ATTR == "4" | BCG_ATTR == "5" | BCG_ATTR == "6")
#                    )
# # filter works here
# # 5 taxa and 202 ind.
#
# met.val <- dplyr::summarise(dplyr::group_by(x, SAMPLEID, INDEX_NAME, INDEX_CLASS)
#                  # individuals #
#                  , ni_total=sum(N_TAXA)
#                  #
#                  , nt_NonInsArachDecaClump_BCG_att456 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
#                                & (is.na(CLASS)==TRUE | (CLASS != "Insecta" & CLASS != "ARACHNIDA"))
#                                & (is.na(ORDER) == TRUE | (ORDER != "DECAPODA" & ORDER!="RISSOOIDEA"))
#                                & (is.na(GENUS) == TRUE | GENUS!="Juga")
#                                & (BCG_ATTR == "4" | BCG_ATTR == "5" | BCG_ATTR == "6")]
#                         , na.rm = TRUE)
#                  #
#                  , pi_NonInsArachDecaClump_BCG_att456 = sum(N_TAXA[
#                               (is.na(CLASS)==TRUE | (CLASS != "Insecta" & CLASS != "ARACHNIDA"))
#                              & (is.na(ORDER) == TRUE | (ORDER != "DECAPODA" & ORDER!="RISSOOIDEA"))
#                              & (is.na(GENUS) == TRUE | GENUS!="Juga")
#                              & (BCG_ATTR == "4" | BCG_ATTR == "5" | BCG_ATTR == "6")]
#                       , na.rm = TRUE)
# )
# View(met.val)
#

#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DomN metric
# library(readxl)
# library(dplyr)
# df.samps.bugs <- read_excel(system.file("./extdata/Data_BCG_PacNW.xlsx"
#                                       , package="BCGcalc"))
# myDF <- as.data.frame(df.samps.bugs)
# names(myDF) <- toupper(names(myDF))
#
#
# # arrang in descending order (SampleID and N_Taxa)
# x <- myDF %>% arrange(SampleID, desc(N_Taxa))
# #y <- x %>% top_n(2, wt=N_Taxa) # only gets 2 rows on entire DF
# y <- x %>% group_by(SampleID) %>% filter(row_number()<=5)
# a <- table(y$SampleID)
#
# X <- myDF %>% arrange(SampleID, desc(N_Taxa)) %>%
#                 group_by(SampleID) %>%
#                   filter(row_number()<=5)
# A <- table(X$SampleID)
# View(A)
#
# # then Sum N_Taxa by SampleID
#
# # too many results (i.e., those with ties)
# z <- myDF %>% group_by(SampleID) %>% top_n(5, N_Taxa)
# View(z)
# table(z$SampID)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OLD, Remove
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Metrics, MBSS Index, Fish
# myIndex <- "MBSS.2005.Fish"
# # Thresholds
# thresh <- metrics_scoring
# # get metric names for myIndex
# (myMetrics.Fish <- as.character(droplevels(unique(
#                           thresh[thresh[,"Index_Name"]==myIndex,"Metric"]))))
# # Taxa Data
# myDF.Fish <- taxa_fish
# myMetric.Values.Fish <- metric.values(myDF.Fish, "fish", myMetrics.Fish)
# View(myMetric.Values.Fish)
#
# # Metrics, Index, Benthic Macroinvertebrates, genus
# # (generate values then scores)
# myIndex <- "MBSS.2005.Bugs"
# # Thresholds
# thresh <- metrics_scoring
# # get metric names for myIndex
# (myMetrics.Bugs.MBSS <- as.character(droplevels(unique(thresh
#                                     [thresh[,"Index_Name"]==myIndex,"Metric"]))))
# # Taxa Data
# myDF.Bugs.MBSS <- taxa_bugs_genus
# myMetric.Values.Bugs.MBSS <- metric.values(myDF.Bugs.MBSS, "bugs", myMetrics.Bugs.MBSS)
# View(myMetric.Values.Bugs.MBSS)
#
# # Metrics, MSW Index, Benthic Macroinvertebrates, family
# myIndex <- "MSW.1999.Bugs"
# # Thresholds
# thresh <- metrics_scoring
# # get metric names for myIndex
# (myMetrics.Bugs.MSW <- as.character(droplevels(unique(thresh
#                                   [thresh[,"Index_Name"]==myIndex,"Metric"]))))
# # Taxa Data
# myDF.Bugs.MSW <- taxa_bugs_family
# myMetric.Values.Bugs.MSW <- metric.values(myDF.Bugs.MSW, "bugs", myMetrics.Bugs.MSW)
# View(myMetric.Values.Bugs.MSW)
#~~~~~~~~~~~~~~~~~~~~~~~~~~
# # QC
# ## Fish
# myIndex <- "MBSS.2005.Fish"
# thresh <- metrics_scoring
# (myMetrics.Fish <- as.character(droplevels(unique(thresh
#                                   [thresh[,"Index_Name"]==myIndex,"Metric"]))))
# myDF <- myDF.Fish
# myMetric.Values.Fish <- metric.values(myDF.Fish, "SampleID", "fish", myMetrics.Fish, TRUE)
# fun.DF <- myDF.Fish
# fun.SampID <- "SampleID"
# fun.Community <- "fish"
# fun.MetricNames <- myMetrics.Fish"
#~~~~~~
# fun.DF <- df_samps_bugs
# fun.Community <- "bugs"
# fun.MetricNames <- c("nt_total", "nt_EPT")
# boo.Adjust <- FALSE
# fun.cols2keep=NULL
# MetricNames <- fun.MetricNames
# cols2keep <- fun.cols2keep
#~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
metric.values <- function(fun.DF
                          , fun.Community
                          , fun.MetricNames = NULL
                          , boo.Adjust = FALSE
                          , fun.cols2keep = NULL
                          , boo.marine = FALSE
                          , boo.Shiny = FALSE
                          , verbose = FALSE
                          , metric_subset = NULL
                          , taxaid_dni = NULL) {
  boo_debug_main <- FALSE
  debug_main_num <- 0
  debug_main_num_total <- 7
  boo_QC <- FALSE

  # QC
  if (boo_QC) {
    fun.DF <- data_benthos_PacNW#[, 1:32] # 598, 37
    #fun.DF <- data_benthos_MBSS # 5066, 37
    fun.Community <- "bugs"
    fun.MetricNames <- NULL
    boo.Adjust <- FALSE
    fun.cols2keep <- NULL
    boo.marine <- FALSE
    boo.Shiny <- FALSE
    verbose <- TRUE
    metric_subset <- NULL
    taxaid_dni <- NULL

    # Create DNI sample
    taxaid_dni <- "DNI"
    fun.DF <- rbind(fun.DF, fun.DF[1, ])
    fun.DF[nrow(fun.DF), "TaxaID"] <- taxaid_dni
    tail(fun.DF)

  }## boo_QC

  # global variable bindings
  N_TAXA <- TAXAID <- NULL

  # define pipe
  `%>%` <- dplyr::`%>%`
  # Munge ####
  if (verbose == TRUE) {
    debug_topic <- "munge"
    debug_main_num <- debug_main_num + 1
    msg <- paste0("debug_metval_main, "
                  , debug_main_num
                  , "/"
                  , debug_main_num_total
                  , ", "
                  , debug_topic)
    message(msg)
  }## IF ~ verbose
  # Data Munging (common to all data types)
  # Convert to data.frame.  Code breaks if fun.DF is a tibble.
  fun.DF <- as.data.frame(fun.DF)
  # convert Field Names to UPPER CASE
  names(fun.DF) <- toupper(names(fun.DF))
  # convert cols2keep to UPPER CASE
  if (!is.null(fun.cols2keep)) {
    #names(fun.cols2keep) <- toupper(fun.cols2keep)
    fun.cols2keep <- toupper(fun.cols2keep)
  }##IF~!is.null(fun.cols2keep)~END
  # subset to upper case
  if (!is.null(metric_subset)) {
    metric_subset <- toupper(metric_subset)
  } else {
    metric_subset <- "ALL"
  }##IF~!is.null(metric_subset)~END


  metric_subset <- ifelse(is.na(metric_subset), NA, toupper(metric_subset))

  # QC, missing cols ----
  # bare minimum, applies to all communities
  if (verbose == TRUE) {
    debug_topic <- "QC missing cols"
    debug_main_num <- debug_main_num + 1
    msg <- paste0("debug_metval_main, "
                  , debug_main_num
                  , "/"
                  , debug_main_num_total
                  , ", "
                  , debug_topic)
    message(msg)
  }## IF ~ boo_debug_main
  #QC, Add required fields for this part of the code
  col.req <- c("SAMPLEID", "TAXAID", "N_TAXA", "INDEX_NAME", "INDEX_CLASS")
  col.req.missing <- col.req[!(col.req %in% toupper(names(fun.DF)))]
  num.col.req.missing <- length(col.req.missing)

  # Trigger prompt if any missing fields (and session is interactive)
  if (num.col.req.missing != 0 & interactive() == TRUE) {
    myPrompt.01 <- paste0("There are ",num.col.req.missing," missing fields in the data:")
    myPrompt.02 <- paste(col.req.missing, collapse = ", ")
    myPrompt.03 <- "If you continue the metrics associated with these fields will be invalid."
    myPrompt.04 <- "For example, if the HABIT field is missing all habit related metrics will not be correct."
    myPrompt.05 <- "Do you wish to continue (YES or NO)?"

    myPrompt <- paste(" "
                      , myPrompt.01
                      , myPrompt.02
                      , " "
                      , myPrompt.03
                      , myPrompt.04
                      , myPrompt.05
                      , sep = "\n")
    #user.input <- readline(prompt=myPrompt)
    user.input <- NA
    # special condition for Shiny
    # Shiny counts as interactive()==TRUE but cannot access this prompt in Shiny.
    if (boo.Shiny == FALSE) {
      user.input <- utils::menu(c("YES", "NO"), title = myPrompt)
    } else {
      message(myPrompt)
      message("boo.Shiny == TRUE so prompt skipped and value set to '1'.")
      user.input <- 1
    }## IF ~ boo.Shiny ~ END

    # any answer other than "YES" will stop the function.
    if (user.input != 1) {##IF.user.input.START
      stop(paste("The user chose *not* to continue due to missing fields: "
                 , paste(paste0("   ",col.req.missing), collapse = "\n")
                 , sep = "\n"))
    }##IF.user.input.END

    # Add missing fields
    if (verbose == TRUE) {
      debug_topic <- "add missing fields"
      debug_main_num <- debug_main_num + 1
      msg <- paste0("debug_metval_main, "
                    , debug_main_num
                    , "/"
                    , debug_main_num_total
                    , ", "
                    , debug_topic)
      message(msg)
    }## IF ~ verbose
    ## Add missing, Index_Name
    req.name <- "INDEX_NAME"
    if (req.name %in% col.req.missing) {
      fun.DF[, req.name] <- "BioMonTools"
    }## IF ~ req.name
    ## Add missing, INDEX_CLASS
    req.name <- "INDEX_CLASS"
    if (req.name %in% col.req.missing) {
      fun.DF[, req.name] <- fun.Community
    }## IF ~ req.name
    ## Add missing, N_Taxa
    req.name <- c("SAMPLEID", "TAXAID", "N_TAXA")
    if (sum(req.name %in% col.req.missing) == length(req.name)) {
      req.name.missing <- req.name[req.name %in% col.req.missing]
      stop(paste("Required columns missing: "
                 , paste(paste0("   ",req.name.missing), collapse = "\n"),sep = "\n"))
    }## IF ~ req.name
    ## old
    #fun.DF[,col.req.missing] <- NA_character_
    warning(paste("Metrics related to the following fields are invalid:"
                  , paste(paste0("   ", col.req.missing), collapse = "\n"), sep = "\n"))
  }##IF.num.col.req.missing.END

  # message col names
  if (verbose == TRUE) {
    debug_topic <- "colnames:"
    debug_main_num <- debug_main_num + 1
    msg <- paste0("debug_metval_main, "
                  , debug_main_num
                  , "/"
                  , debug_main_num_total
                  , ", "
                  , debug_topic)
    msg <- paste(msg
                 , paste("    ", names(fun.DF), collapse = "\n")
                 , sep = "\n")
    message(msg)
  }## IF ~ verbose

  # Remove Count = 0 taxa unless TaxaID = NONE
  if (verbose == TRUE) {
    debug_topic <- "remove count 0"
    debug_main_num <- debug_main_num + 1
    msg <- paste0("debug_metval_main, "
                  , debug_main_num
                  , "/"
                  , debug_main_num_total
                  , ", "
                  , debug_topic)
    message(msg)
  }## IF ~ verbose
  #fun.DF <- fun.DF[fun.DF[,"N_TAXA"]>0, ]
  fun.DF <- fun.DF %>% dplyr::filter(N_TAXA > 0 | TAXAID == "NONE")
  # non-target taxa removed in community function, if appropriate
  #
  # SiteType to upper case
  if (verbose == TRUE) {
    debug_topic <- "sitetype toupper"
    debug_main_num <- debug_main_num + 1
    msg <- paste0("debug_metval_main, "
                  , debug_main_num
                  , "/"
                  , debug_main_num_total
                  , ", "
                  , debug_topic)
    message(msg)
  }## IF ~ verbose
 # fun.DF[,"INDEX_CLASS"] <- toupper(fun.DF[,"INDEX_CLASS"])
  # convert community to upper case
  fun.Community <- toupper(fun.Community)

  # run the proper sub function
  if (verbose == TRUE) {
    debug_topic <- "start subfunctions"
    debug_main_num <- debug_main_num + 1
    msg <- paste0("debug_metval_main, "
                  , debug_main_num
                  , "/"
                  , debug_main_num_total
                  , ", "
                  , debug_topic)
    message(msg)
  }## IF ~ verbose

  # Subfunctions ----
  # Run subfunction based on community
  if (fun.Community == "BUGS") {##IF.START
    metric.values.bugs(myDF = fun.DF
                       , MetricNames = fun.MetricNames
                       , boo.Adjust = boo.Adjust
                       , cols2keep = fun.cols2keep
                       , MetricSort = NA
                       , boo.marine = boo.marine
                       , boo.Shiny = boo.Shiny
                       , verbose = verbose
                       , metric_subset = metric_subset
                       , taxaid_dni = taxaid_dni)
  } else if (fun.Community == "FISH") {
    metric.values.fish(myDF = fun.DF
                       , MetricNames = fun.MetricNames
                       , boo.Adjust = boo.Adjust
                       , cols2keep = fun.cols2keep
                       , boo.Shiny = boo.Shiny
                       , verbose = verbose)
  } else if (fun.Community == "ALGAE") {
    metric.values.algae(myDF = fun.DF
                        , MetricNames = fun.MetricNames
                        , boo.Adjust = boo.Adjust
                        , cols2keep = fun.cols2keep
                        , MetricSort = NA
                        , boo.Shiny = boo.Shiny
                        , verbose = verbose)
  }##IF.END
}##FUNCTION.metric.values.START
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Calculate metric values, Bugs
#'
#' @description Subfunction of metric.values for use with Benthic
#' Macroinvertebrates
#'
#' @details For internal use only.  Called from metric.values().
#'
#' @param myDF Data frame of taxa.
#' @param MetricNames Optional vector of metric names to be returned.
#' @param boo.Adjust Optional boolean value on whether to perform adjustments of
#' values prior to scoring.  Default = FALSE but may be TRUE for certain
#' metrics.
#' @param cols2keep Column names of fun.DF to retain in the output.  Uses
#' column names.
#' @param MetricSort How metric names should be sort; NA = as is
#' , AZ = alphabetical.  Default = NULL.
#' @param boo.marine Should estuary/marine metrics be included.
#' Ignored if fun.MetricNames is not null. Default = FALSE.
#' @param boo.Shiny Boolean value for if the function is accessed via Shiny.
#' Default = FALSE.
#' @param verbose Include messages to track progress.  Default = FALSE
#' @param metric_subset Subset of metrics to be generated.  Internal function.
#' Default = NULL
#' @param taxaid_dni Taxa names to be included in DNI (Do Not Include) metrics
#' (n = 3) but dropped for all other metrics.  Only for benthic metrics.
#' Default = NULL
#'
#' @return Data frame
#'
#' @keywords internal
#'
#' @export
metric.values.bugs <- function(myDF
                               , MetricNames = NULL
                               , boo.Adjust = FALSE
                               , cols2keep = NULL
                               , MetricSort = NA
                               , boo.marine = FALSE
                               , boo.Shiny
                               , verbose
                               , metric_subset
                               , taxaid_dni = NULL) {
  #
  # QC
  boo_QC <- FALSE
  if (boo_QC) {
    myDF <- fun.DF
    boo.Adjust <- boo.Adjust
    cols2keep <- fun.cols2keep
    MetricSort <- NA
    boo.marine <- boo.marine
    boo.Shiny <- boo.Shiny
    verbose <- verbose
    metric_subset <- NULL
    taxaid_dni <- "DNI"  #added last entry in previous function
  }## IF ~ boo_QC

  time_start <- Sys.time()

  # not carrying over from previous?!
  names(myDF) <- toupper(names(myDF))

  debug_sub_community <- "BUGS"
  boo_debug_bugs <- FALSE
  debug_sub_num <- 0
  debug_sub_num_total <- 18

  # global variable bindings ----
  INDEX_NAME <- INDEX_CLASS <- SAMPLEID <- TAXAID <- N_TAXA <- EXCLUDE <-
    BCG_ATTR <- NONTARGET <- LONGLIVED <- NOTEWORTHY <- TOLVAL <- TOLVAL2 <-
    UFC <- ELEVATION_ATTR <- GRADIENT_ATTR <- WSAREA_ATTR <- NULL
  FFG2_PRE <- TI_CORECOLD <- TI_COLD <- TI_COOL <- TI_WARM <- TI_NA <-
    TI_CORECOLD_COLD <- TI_COOL_WARM <- NULL
  PHYLUM <- SUBPHYLUM <- CLASS <- SUBCLASS <- INFRAORDER <- ORDER <-
    FAMILY <- SUBFAMILY <- TRIBE <- GENUS <- NULL
  FFG_COL <- FFG_FIL <- FFG_PRE <- FFG_SCR <- FFG_SHR <- FFG_MAH <- FFG_PIH <-
    FFG_XYL <- FFG_OMN <- FFG_PAR <- HABITAT_SPEC <- HABITAT_UNKN <- NULL
  ni_total <- ni_Americo <- ni_Gnorimo <- ni_EPT <- ni_Trich <- nt_Amph <-
    nt_total <- nt_Bival <- nt_Coleo <- nt_COET <- nt_Deca <- nt_Dipt <-
    nt_Ephem <- nt_EPT <- nt_ET <- nt_Gast <- nt_Insect <- nt_Isop <- nt_Mega <-
    nt_NonIns <- nt_Nudib <- nt_Odon <- nt_OET <- nt_Oligo <- nt_Pleco <-
    nt_POET <- nt_Poly <- nt_PolyNoSpion <- nt_Spion <- nt_Trich <- ni_Chiro <-
    nt_Chiro <- nt_NonInsArachDeca_BCG_att456 <-
    nt_NonInsArachDecaJugaRiss_BCG_att456 <- ni_dom02_NoJugaRiss_BCG_att456 <-
    nt_NonIns_BCG_att456 <- nt_NonInsJugaRiss_BCG_att456 <- nt_BCG_att1m <-
    nt_BCG_att12 <- nt_BCG_att1i2 <- nt_BCG_att123 <- nt_BCG_att1i23 <-
    nt_BCG_att2 <- nt_BCG_att23 <- nt_BCG_att234 <- nt_BCG_att3 <-
    nt_BCG_att4 <- nt_BCG_att45 <- nt_BCG_att5 <- nt_BCG_att56 <- nt_BCG_att6 <-
    nt_BCG_attNA <- nt_EPT_BCG_att123 <- nt_ti_c <- nt_ti_cc <- nt_ti_cw <-
    nt_ti_w <- nt_tv_intol <- nt_tv_intol4 <- nt_tv_toler <- nt_tv_ntol <-
    nt_tv_stol <- nt_ffg_col <- nt_ffg_filt <- nt_ffg_pred <- nt_habitat_brac <-
    nt_habitat_depo <- nt_habitat_gene <- nt_habitat_head <- nt_habitat_rheo <-
    nt_habitat_rive <- nt_habitat_spec <- nt_habitat_unkn <- nt_BCG_att1 <-
    nt_BCG_att1i <- HABITAT_BRAC <- HABITAT_DEPO <- HABITAT_GENE <-
    HABITAT_HEAD <- HABITAT_RHEO <- HABITAT_RIVE <- HABIT_BU <- HABIT_CB <-
    HABIT_CN <- HABIT_SP <- HABIT_SW <- LC_MULTI <- LC_SEMI <- LC_UNI <-
    ni_dom02 <- ni_dom03 <- ni_dom04 <- ni_dom05 <- ni_dom06 <- ni_dom07 <-
    ni_dom08 <- ni_dom09 <- ni_dom10 <- nt_ffg_scrap <- nt_ffg_shred <-
    nt_habit_burrow <- nt_habit_climb <- nt_habit_cling <- nt_habit_sprawl <-
    nt_habit_swim <- nt_volt_multi <- nt_volt_semi <- nt_volt_uni <- x_Shan_e <-
    NULL
  nt_ffg_mah <- nt_ffg_pih <- nt_ffg_xyl <- nt_ffg_omn <- nt_ffg_par <-
    pi_ffg_mah <- pi_ffg_pih <- pi_ffg_xyl <- pi_ffg_omn <- pi_ffg_par <-
    pt_ffg_mah <- pt_ffg_pih <- pt_ffg_xyl <- pt_ffg_omn <- pt_ffg_par <- NULL
  nt_ECT <- pi_ECT <- pt_ECT <- NULL
  WSAREA_S <- WSAREA_M <- WSAREA_L <- WSAREA_XL <- NULL
  ELEVATION_HIGH <- ELEVATION_LOW <- GRADIENT_HIGH <- GRADIENT_LOW <-
    GRADIENT_MOD <- TI_EURY <- nt_BCG_att456 <- nt_EPT_BCG_att1i23 <-
    nt_NonInsTrombJuga_BCG_att456 <- nt_Tromb <- nt_ti_cold <- nt_ti_cool <-
    nt_ti_cool_warm <- nt_ti_corecold <- nt_ti_corecold_cold <- nt_ti_eury <-
    nt_ti_na <- nt_ti_warm <- NULL
  HABSTRUCT <- HABSTRUCT_CS <- HABSTRUCT_NF <- HABSTRUCT_RM <- HABSTRUCT_SG <-
    nt_Hempit <- pi_Hemipt <- pt_Hemipt <- nt_oneind <- NULL
  nt_BCG_att4b <- pi_BCG_att4b <- pt_BCG_att4b <- nt_BCG_att1i234b5 <-
    pi_BCG_att1i234b5 <- pt_BCG_att1i234b5 <- nt_BCG_att1i234w5 <-
    pi_BCG_att1i234w5 <- pt_BCG_att1i234w5 <- NULL
  nt_Hemipt <- nt_dni <- pi_dni <- pt_dni <- NULL
  pi_EphemNoBaeTri <- nt_EphemNoBaeTri <- nt_COETNoBraBaeHydTri <- x_BCICTQa <-
    NULL

  # define pipe
  `%>%` <- dplyr::`%>%`

  # QC----
  ## QC, Missing Cols ----
  if (verbose == TRUE) {
    debug_topic <- "QC, missing cols"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                , debug_sub_num
                , "/"
                , debug_sub_num_total
                , ", "
                , debug_topic)
    message(msg)
  }## IF ~ verbose

  # QC, Required Fields
  col.req_character <- c("SAMPLEID", "TAXAID", "INDEX_NAME", "INDEX_CLASS"
                    , "PHYLUM", "SUBPHYLUM", "CLASS", "SUBCLASS", "INFRAORDER"
                    , "ORDER", "FAMILY", "SUBFAMILY", "TRIBE", "GENUS"
                    , "FFG", "HABIT", "LIFE_CYCLE"
                    , "BCG_ATTR", "THERMAL_INDICATOR"
                    , "FFG2", "HABITAT", "ELEVATION_ATTR"
                    , "GRADIENT_ATTR", "WSAREA_ATTR", "HABSTRUCT"
                    , "BCG_ATTR2")
  col.req_logical <- c("EXCLUDE", "NONTARGET"
                       , "LONGLIVED", "NOTEWORTHY", "AIRBREATHER")
  col.req_numeric <- c("N_TAXA", "TOLVAL", "TOLVAL2", "UFC")
  col.req <- c(col.req_character, col.req_logical, col.req_numeric)
  # col.req <- c("SAMPLEID", "TAXAID", "N_TAXA", "EXCLUDE", "INDEX_NAME"
  #             , "INDEX_CLASS", "NONTARGET", "PHYLUM", "SUBPHYLUM", "CLASS"
  #             , "SUBCLASS", "INFRAORDER", "ORDER", "FAMILY", "SUBFAMILY"
  #             , "TRIBE", "GENUS", "FFG", "HABIT", "LIFE_CYCLE", "TOLVAL"
  #             , "BCG_ATTR", "THERMAL_INDICATOR", "LONGLIVED", "NOTEWORTHY"
  #             , "FFG2", "TOLVAL2", "HABITAT", "UFC", "ELEVATION_ATTR"
  #             , "GRADIENT_ATTR", "WSAREA_ATTR")
  col.req.missing <- col.req[!(col.req %in% toupper(names(myDF)))]
  col.req.missing_char <- col.req_character[!(col.req_character %in% toupper(names(myDF)))]
  col.req.missing_log <- col.req_logical[!(col.req_logical %in% toupper(names(myDF)))]
  col.req.missing_num <- col.req_numeric[!(col.req_numeric %in% toupper(names(myDF)))]

  num.col.req.missing <- length(col.req.missing)
  num.col.req.missing_char <- length(col.req.missing_char)
  num.col.req.missing_log <- length(col.req.missing_log)
  num.col.req.missing_num <- length(col.req.missing_num)

  # Trigger prompt if any missing fields (and session is interactive)
  if (num.col.req.missing != 0) {

    # Create prompt for missing columns
    myPrompt.01 <- paste0("There are ",num.col.req.missing," missing fields in the data:")
    myPrompt.02 <- paste(col.req.missing, collapse = ", ")
    myPrompt.03 <- "If you continue the metrics associated with these fields will be invalid."
    myPrompt.04 <- "For example, if the HABIT field is missing all habit related metrics will not be correct."
    myPrompt.05 <- "Do you wish to continue (YES or NO)?"

    myPrompt <- paste(" "
                      , myPrompt.01
                      , myPrompt.02
                      , " "
                      , myPrompt.03
                      , myPrompt.04
                      , myPrompt.05
                      , sep = "\n")
    user.input <- NA

    if (interactive() == TRUE & boo.Shiny == FALSE) {
      #user.input <- readline(prompt=myPrompt)
      user.input <- utils::menu(c("YES", "NO"), title = myPrompt)
    } else {
      message(myPrompt)
      message("boo.Shiny == TRUE and interactive == FALSE
               so prompt skipped and value set to '1'.")
      user.input <- 1
    }## IF ~ interactive & boo.Shiny

    # # special condition for Shiny
    # # Shiny counts as interactive()==TRUE locally
    #        but cannot access this prompt in Shiny.
    # if (boo.Shiny==FALSE) {
    #   user.input <- utils::menu(c("YES", "NO"), title=myPrompt)
    # } else {
    #   message(myPrompt)
    #   message("boo.Shiny == TRUE so prompt skipped and value set to '1'.")
    #   user.input <- 1
    # }## IF ~ boo.Shiny ~ END

    # any answer other than "YES" will stop the function.
    if (user.input != 1) {
      stop(paste("The user chose *not* to continue due to missing fields: "
                  , paste(paste0("   ",col.req.missing), collapse = "\n"), sep = "\n"))
    }##IF.user.input.END

    # Add missing fields
    #myDF[, col.req.missing] <- NA
    if (num.col.req.missing_char > 0) {
      myDF[, col.req.missing_char] <- NA_character_
    }
    if (num.col.req.missing_log > 0) {
      myDF[, col.req.missing_log] <- NA
    }
    if (num.col.req.missing_num > 0) {
      myDF[, col.req.missing_num] <- NA_real_
    }
    warning(paste("Metrics related to the following fields are invalid:"
                  , paste(paste0("   ", col.req.missing)
                          , collapse = "\n")
                  , sep = "\n"))
  }##IF.num.col.req.missing.END

  # message col names
  if (verbose == TRUE) {
    debug_topic <- "colnames"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    msg <- paste(msg
                 , paste("    ", names(myDF), collapse = "\n")
                 , sep = "\n")
    message(msg)
  }## IF ~ verbose

  ## QC, Cols2Keep ----
  # remove duplicates with required so no errors, e.g., SAMPLEID
  cols2keep <- cols2keep[!cols2keep %in% col.req]

  ## QC, Exclude----
  # ensure TRUE/FALSE
  if (verbose == TRUE) {
    debug_topic <- "QC, cols, values, Exclude"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
    myCol <- "EXCLUDE"
    col_TF <- myCol %in% names(myDF)
    msg <- paste0("Column (", myCol, ") exists; ", col_TF)
    message(msg)
  }## IF ~ verbose
  Exclude.T <- sum(myDF$EXCLUDE == TRUE, na.rm = TRUE)
  if (Exclude.T == 0) {
    warning("EXCLUDE column does not have any TRUE values. \n  Valid values are TRUE or FALSE.  \n  Other values are not recognized.")
  }##IF.Exclude.T.END

  ## QC, NonTarget----
  # ensure as TRUE/FALSE
  if (verbose == TRUE) {
    debug_topic <- "QC, cols, values, NonTarget"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
    myCol <- "NONTARGET"
    col_TF <- myCol %in% names(myDF)
    msg <- paste0("Column (", myCol, ") exists; ", col_TF)
    message(msg)
  }## IF ~ verbose

  NonTarget.F <- sum(myDF$NONTARGET == FALSE, na.rm = TRUE)
  if (NonTarget.F == 0) {
    warning("NONTARGET column does not have any FALSE values. \n  Valid values are TRUE or FALSE.  \n  Other values are not recognized.")
  }##IF.Exclude.T.END

  ## QC, TolVal----
  # need as numeric, if have "NA" as character it fails
  if (verbose == TRUE) {
    debug_topic <- "QC, cols, numeric, TolVal"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
    myCol <- "TOLVAL"
    col_TF <- myCol %in% names(myDF)
    msg <- paste0("Column (", myCol, ") exists; ", col_TF)
    message(msg)
  }## IF ~ verbose
  TolVal_Char_NA <- myDF[, "TOLVAL"] == "NA"
  # Fails with mix of NA and "NA", rework
  TolVal_Char_NA <- TolVal_Char_NA == TRUE & !is.na(TolVal_Char_NA)
  if (sum(TolVal_Char_NA, na.rm = TRUE) > 0) {
    #myDF[TolVal_Char_NA, "TOLVAL"] <- NA
    myDF[, "TOLVAL"] <- as.numeric(myDF[, "TOLVAL"])
    # will give a warning - NAs introduced by coercion
    msg <- "Updated col class; TOLVAL to numeric"
    message(msg)
  }##IF ~ TOLVAL ~ END

  ## QC, TolVal2----
  # need as numeric, if have "NA" as character it fails
  if (verbose == TRUE) {
    debug_topic <- "QC, cols, numeric, TolVal2"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
    myCol <- "TOLVAL2"
    col_TF <- myCol %in% names(myDF)
    msg <- paste0("Column (", myCol, ") exists; ", col_TF)
    message(msg)
  }## IF ~ verbose
  TolVal2_Char_NA <- myDF[, "TOLVAL2"] == "NA"
  if (sum(TolVal2_Char_NA, na.rm = TRUE) > 0) {
    myDF[TolVal2_Char_NA, "TOLVAL2"] <- NA
    myDF[, "TOLVAL2"] <- as.numeric(myDF[, "TOLVAL2"])
    msg <- "Updated col class; TOLVAL2 to numeric"
    message(msg)
  }##IF ~ TOLVAL2 ~ END

  ## QC, UFC----
  # need as numeric, if have "NA" as character it fails
  if (verbose == TRUE) {
    debug_topic <- "QC, cols, numeric, UFC"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
    myCol <- "UFC"
    col_TF <- myCol %in% names(myDF)
    msg <- paste0("Column (", myCol, ") exists; ", col_TF)
    message(msg)
  }## IF ~ verbose
  UFC_Char_NA <- myDF[, "UFC"] == "NA"
  if (sum(UFC_Char_NA, na.rm = TRUE) > 0) {
    myDF[UFC_Char_NA, "UFC"] <- NA
    myDF[, "UFC"] <- as.numeric(myDF[, "UFC"])
    msg <- "Updated col class; UFC to numeric"
    message(msg)
  }##IF ~ UFC ~ END

  ## QC, BCG_Attr ----
  # need as character, if complex all values fail
  if (verbose == TRUE) {
    debug_topic <- "QC, cols, complex, BCG_Attr"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
    myCol <- "BCG_ATTR"
    col_TF <- myCol %in% names(myDF)
    msg <- paste0("Column (", myCol, ") exists; ", col_TF)
    message(msg)
  }## IF ~ verbose

  BCG_Complex <- is.complex(myDF[, "BCG_ATTR"])
  # only tigger if have a complex field
  if (BCG_Complex == TRUE) {
    if (interactive() & boo.Shiny == FALSE) {
      msg <- "**BCG_ATTR is complex!**"
      msg2 <- "BCG metrics will not calculate properly."
      msg3 <- "Reimport data with column class defined."
      msg4 <- "Use either Fix1 or Fix2.  Replace 'foo.csv' with your file."
      msg5 <- ""
      msg6 <- "# Fix 1, base R"
      msg7 <- "df_data <- read.csv('foo.csv', colClass=c('BCG_Attr'='character'))"
      msg8 <- ""
      msg9 <- "# Fix 2, tidyverse"
      msg10 <- "# install package if needed and load it"
      msg11 <- "if(!require(readr)) {install.packages('readr')}"
      msg12 <- "# import file and convert from tibble to data frame"
      msg13 <- "df_data <- as.data.frame(read_csv('foo.csv'))"
      msg14 <- ""
      #
      message(paste(msg, msg2, msg3, msg4, msg5, msg6, msg7, msg8, msg9, msg10
                    , msg11, msg12, msg13, msg14, sep = "\n"))
    }## IF ~ interactive & boo.Shiny == FALSE

    if (interactive() == FALSE | boo.Shiny == TRUE) {
      # > df$BCG_Attr_char <- as.character(df$BCG_Attr)
      # > df$BCG_Attr_char <- sub("^0\\+", "", df$BCG_Attr_char)
      # > df$BCG_Attr_char <- sub("\\+0i$", "", df$BCG_Attr_char)
      # > table(df$BCG_Attr, df$BCG_Attr_char)
      myDF[, "BCG_ATTR"] <- as.character(myDF[, "BCG_ATTR"])
      myDF[, "BCG_ATTR"] <- sub("^0\\+", "", myDF[, "BCG_ATTR"])
      myDF[, "BCG_ATTR"] <- sub("\\+0i$", "", myDF[, "BCG_ATTR"])
    }## IF ~ interactive() == FALSE | boo.Shiny == TRUE

  }##IF ~ BCG_Attr ~ END

  # Data Munging ####
  if (verbose == TRUE) {
    debug_topic <- "Munging"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
  }## IF ~ verbose

  # Remove NonTarget Taxa (added back 20200715, missing since 20200224)
  # Function fails if all NA (e.g., column was missing) (20200724)
  if (verbose == TRUE) {
    debug_topic <- "Munging, NonTarget"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
    myCol <- "NONTARGET"
    col_TF <- myCol %in% names(myDF)
    msg <- paste0("Column (", myCol, ") exists; ", col_TF)
    message(msg)
  }## IF ~ verbose

  myDF <- dplyr::filter(myDF, NONTARGET != TRUE | is.na(NONTARGET))

  # # Convert columns to upper case (Phylo, FFG, Habit, Life_Cycle)
  if (verbose == TRUE) {
    debug_topic <- "Munging, text cols, toupper"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
  }## IF ~ verbose

  # col2upper <- c("TAXAID", "PHYLUM", "SUBPHYLUM", "CLASS", "SUBCLASS"
  #                , "INFRAORDER", "ORDER", "FAMILY", "SUBFAMILY"
  #                , "TRIBE", "GENUS"
  #                , "HABIT", "FFG", "LIFE_CYCLE", "THERMAL_INDICATOR"
  #                , "FFG2", "HABITAT"
  #                , "ELEVATION_ATTR", "GRADIENT_ATTR", "WSAREA_ATTR")
  col2upper <- col.req_character[!(col.req_character %in%
                                  c("SAMPLEID", "INDEX_NAME", "INDEX_CLASS"))]
  # #myDF <- apply(myDF[, col2upper], 2, toupper)

  for (i in col2upper) {
    myDF[, i] <- toupper(myDF[, i])
  }## FOR ~ i ~ END
  # use toupper() earlier, don't need
  # removed as causing issues with shiny.io with some missing fields
  # 2022-02-21, previous no longer present, redo here (all fields now present)

  # Add extra columns for some fields
  # (need unique values for functions in summarise)
  # each will be TRUE or FALSE
  # finds any match so "CN, CB" is both "CN" and "CB"
  if (verbose == TRUE) {
    debug_topic <- "Munging, TF"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
  }## IF ~ verbose

  # match, any
  myDF[, "HABIT_BU"]     <- grepl("BU", myDF[, "HABIT"])
  myDF[, "HABIT_CB"]     <- grepl("CB", myDF[, "HABIT"])
  myDF[, "HABIT_CN"]     <- grepl("CN", myDF[, "HABIT"])
  myDF[, "HABIT_SP"]     <- grepl("SP", myDF[, "HABIT"])
  myDF[, "HABIT_SW"]     <- grepl("SW", myDF[, "HABIT"])
  myDF[, "FFG_COL"]      <- grepl("(CG|GC)", myDF[, "FFG"])
  myDF[, "FFG_FIL"]      <- grepl("(CF|FC)", myDF[, "FFG"])
  myDF[, "FFG_PRE"]      <- grepl("PR", myDF[, "FFG"])
  myDF[, "FFG_SCR"]      <- grepl("SC", myDF[, "FFG"])
  myDF[, "FFG_SHR"]      <- grepl("SH", myDF[, "FFG"])
  myDF[, "FFG_MAH"]      <- grepl("MH", myDF[, "FFG"])
  myDF[, "FFG_OMN"]      <- grepl("OM", myDF[, "FFG"])
  myDF[, "FFG_PAR"]      <- grepl("PA", myDF[, "FFG"])
  myDF[, "FFG_PIH"]      <- grepl("PH", myDF[, "FFG"])
  myDF[, "FFG_XYL"]      <- grepl("XY", myDF[, "FFG"])
  myDF[, "LC_MULTI"]     <- grepl("MULTI", myDF[, "LIFE_CYCLE"])
  myDF[, "LC_SEMI"]      <- grepl("SEMI", myDF[, "LIFE_CYCLE"])
  myDF[, "LC_UNI"]       <- grepl("UNI", myDF[, "LIFE_CYCLE"])
  myDF[, "FFG2_PRE"]     <- grepl("PR", myDF[, "FFG2"])
  myDF[, "TI_STENOCOLD"] <- grepl("STENOC", myDF[, "THERMAL_INDICATOR"])
  myDF[, "TI_COLD"]      <- grepl("COLD", myDF[, "THERMAL_INDICATOR"])
  myDF[, "TI_COOL"]      <- grepl("COOL", myDF[, "THERMAL_INDICATOR"])
  myDF[, "TI_WARM"]      <- grepl("WARM", myDF[, "THERMAL_INDICATOR"])
  myDF[, "TI_STENOWARM"] <- grepl("STENOW", myDF[, "THERMAL_INDICATOR"])
  myDF[, "TI_EURY"]      <- grepl("EURYTHERMAL", myDF[, "THERMAL_INDICATOR"])
  myDF[, "TI_COWA"]      <- grepl("COWA", myDF[,"THERMAL_INDICATOR"])
  myDF[, "HS_CS"]        <- grepl("CS", myDF[, "HABSTRUCT"])
  myDF[, "HS_NF"]        <- grepl("NF", myDF[, "HABSTRUCT"])
  myDF[, "HS_RM"]        <- grepl("RM", myDF[, "HABSTRUCT"])
  myDF[, "HS_SG"]        <- grepl("SG", myDF[, "HABSTRUCT"])
  # match, exact only
  myDF[, "TI_NA"]          <- is.na(myDF[, "THERMAL_INDICATOR"]) |
                                  myDF[, "THERMAL_INDICATOR"] == ""
  myDF[, "HABITAT_BRAC"]   <- "BRAC" == myDF[, "HABITAT"]
  myDF[, "HABITAT_DEPO"]   <- "DEPO" == myDF[, "HABITAT"]
  myDF[, "HABITAT_GENE"]   <- "GENE" == myDF[, "HABITAT"]
  myDF[, "HABITAT_HEAD"]   <- "HEAD" == myDF[, "HABITAT"]
  myDF[, "HABITAT_RHEO"]   <- "RHEO" == myDF[, "HABITAT"]
  myDF[, "HABITAT_RIVE"]   <- "RIVE" == myDF[, "HABITAT"]
  myDF[, "HABITAT_SPEC"]   <- "SPEC" == myDF[, "HABITAT"]
  myDF[, "HABITAT_UNKN"]   <- "UNKN" == myDF[, "HABITAT"]
  myDF[, "ELEVATION_LOW"]  <- "LOW" == myDF[, "ELEVATION_ATTR"]
  myDF[, "ELEVATION_HIGH"] <- "HIGH" == myDF[, "ELEVATION_ATTR"]
  myDF[, "GRADIENT_LOW"]   <- "LOW" == myDF[, "GRADIENT_ATTR"]
  myDF[, "GRADIENT_MOD"]   <- "MOD" == myDF[, "GRADIENT_ATTR"]
  myDF[, "GRADIENT_HIGH"]  <- "HIGH" == myDF[, "GRADIENT_ATTR"]
  myDF[, "WSAREA_S"]       <- "SMALL" == myDF[, "WSAREA_ATTR"]
  myDF[, "WSAREA_M"]       <- "MEDIUM" == myDF[, "WSAREA_ATTR"]
  myDF[, "WSAREA_L"]       <- "LARGE" == myDF[, "WSAREA_ATTR"]
  myDF[, "WSAREA_XL"]      <- "XLARGE" == myDF[, "WSAREA_ATTR"]

  #
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # skip above in testing
  # myDF[, "HABIT_BU"] <- FALSE
  # myDF[, "HABIT_CB"] <- FALSE
  # myDF[, "HABIT_CN"] <- FALSE
  # myDF[, "HABIT_SP"] <- FALSE
  # myDF[, "HABIT_SW"] <- FALSE
  # myDF[, "FFG_COL"]  <- FALSE
  # myDF[, "FFG_FIL"]  <- FALSE
  # myDF[, "FFG_PRE"]  <- FALSE
  # myDF[, "FFG_SCR"]  <- FALSE
  # myDF[, "FFG_SHR"]  <- FALSE
  # myDF[, "LC_MULTI"] <- FALSE
  # myDF[, "LC_SEMI"]  <- FALSE
  # myDF[, "LC_UNI"]   <- FALSE
  # # exact matches only
  # myDF[, "TI_COLD"]     <- FALSE
  # myDF[, "TI_COLDCOOL"] <- FALSE
  # myDF[, "TI_COOLWARM"] <- FALSE
  # myDF[, "TI_WARM"]     <- FALSE
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  # Calculate Metrics (could have used pipe, %>%)
  # met.val <- myDF %>%
  #                 dplyr::group_by(SAMPLEID, INDEX_NAME, INDEX_CLASS) %>%
  #                   dplyr::summarise(ni_total=sum(N_TAXA)
  #                         , nt_total=dplyr::n_distinct(TAXAID[EXCLUDE != TRUE], na.rm = TRUE)
  #                         , ni_max= max(N_TAXA)
  #                         , ni_dom01=dplyr::top_n(n=1, wt=N_TAXA)
  #                   )
  #https://stackoverflow.com/questions/45365484/how-to-find-top-n-descending-values-in-group-in-dplyr
  # may have to create a 2nd output with domX metrics then join together.
  # dom.val <- myDF %>%
  #               group_by(SAMPLEID, INDEX_NAME, INDEX_CLASS) %>%
  #                 summarise(N_TAXA=n()) %>%
  #                   top_n(n=3, wt=N_TAXA) %>%
  #                     arrange()
  # https://groups.google.com/forum/#!topic/manipulatr/ZzohinbNsJc

  # X <- myDF %>% arrange(SampleID, desc(N_Taxa)) %>%
  #   group_by(SampleID) %>%
  #   filter(row_number()<=5)


  # Create Dominant N ####
  # Create df for Top N (without ties)
  if (verbose == TRUE) {
    debug_topic <- "Munging, Dom"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
  }## IF ~ verbose

  # DF for dom so same taxa get combined
  # 2023-10-24, remove taxaid_dni
  myDF_dom <- dplyr::summarise(dplyr::group_by(myDF
                                               , INDEX_NAME
                                               , INDEX_CLASS
                                               , SAMPLEID
                                               , TAXAID
                                               , GENUS
                                               , ORDER
                                               , BCG_ATTR)
                               , N_TAXA = sum(N_TAXA, na.rm = TRUE)
                               , .groups = "drop_last") %>%
    dplyr::filter(!TAXAID %in% taxaid_dni) # doesn't work if do first

  df.dom01 <- dplyr::arrange(myDF_dom, SAMPLEID, dplyr::desc(N_TAXA)) %>%
                            dplyr::group_by(SAMPLEID)  %>%
                                dplyr::filter(dplyr::row_number() <= 1)
  df.dom02 <-  dplyr::arrange(myDF_dom, SAMPLEID, dplyr::desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID)  %>%
    dplyr::filter(dplyr::row_number() <= 2)
  df.dom03 <-  dplyr::arrange(myDF_dom, SAMPLEID, dplyr::desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID) %>%
    dplyr::filter(dplyr::row_number() <= 3)
  df.dom04 <-  dplyr::arrange(myDF_dom, SAMPLEID, dplyr::desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID) %>%
    dplyr::filter(dplyr::row_number() <= 4)
  df.dom05 <-  dplyr::arrange(myDF_dom, SAMPLEID, dplyr::desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID) %>%
    dplyr::filter(dplyr::row_number() <= 5)
  df.dom06 <-  dplyr::arrange(myDF_dom, SAMPLEID, dplyr::desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID) %>%
    dplyr::filter(dplyr::row_number() <= 6)
  df.dom07 <-  dplyr::arrange(myDF_dom, SAMPLEID, dplyr::desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID) %>%
    dplyr::filter(dplyr::row_number() <= 7)
  df.dom08 <-  dplyr::arrange(myDF_dom, SAMPLEID, dplyr::desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID) %>%
    dplyr::filter(dplyr::row_number() <= 8)
  df.dom09 <- dplyr::arrange(myDF_dom, SAMPLEID, dplyr::desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID) %>%
    dplyr::filter(dplyr::row_number() <= 9)
  df.dom10 <-  dplyr::arrange(myDF_dom, SAMPLEID, dplyr::desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID)  %>%
    dplyr::filter(dplyr::row_number() <= 10)
  df.dom02_NoJugaRiss_BCG_att456 <-  dplyr::arrange(myDF_dom, SAMPLEID
                                                    , dplyr::desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID) %>%
    dplyr::filter((is.na(GENUS) == TRUE | GENUS != "JUGA")
                  & (is.na(ORDER) == TRUE | ORDER != "RISSOOIDEA")
                  & (BCG_ATTR == "4" | BCG_ATTR == "5" | BCG_ATTR == "6")) %>%
    dplyr::filter(dplyr::row_number() <= 2)
  df.dom01_BCG_att4 <- dplyr::arrange(myDF_dom, SAMPLEID, dplyr::desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID)  %>%
    dplyr::filter(BCG_ATTR == "4") %>%
    dplyr::filter(dplyr::row_number() <= 1)
  df.dom01_BCG_att5 <- dplyr::arrange(myDF_dom, SAMPLEID, dplyr::desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID)  %>%
    dplyr::filter(BCG_ATTR == "5") %>%
    dplyr::filter(dplyr::row_number() <= 1)

  # Summarise Top N
  df.dom01.sum <- dplyr::summarise(dplyr::group_by(df.dom01
                                                   , SAMPLEID
                                                   , INDEX_NAME
                                                   , INDEX_CLASS)
                            , ni_dom01 = sum(N_TAXA, na.rm = TRUE)
                            , .groups = "drop_last")
  df.dom02.sum <- dplyr::summarise(dplyr::group_by(df.dom02
                                                   , SAMPLEID
                                                   , INDEX_NAME
                                                   , INDEX_CLASS)
                            , ni_dom02 = sum(N_TAXA, na.rm = TRUE)
                            , .groups = "drop_last")
  df.dom03.sum <- dplyr::summarise(dplyr::group_by(df.dom03
                                                   , SAMPLEID
                                                   , INDEX_NAME
                                                   , INDEX_CLASS)
                            , ni_dom03 = sum(N_TAXA, na.rm = TRUE)
                            , .groups = "drop_last")
  df.dom04.sum <- dplyr::summarise(dplyr::group_by(df.dom04
                                                   , SAMPLEID
                                                   , INDEX_NAME
                                                   , INDEX_CLASS)
                            , ni_dom04 = sum(N_TAXA, na.rm = TRUE)
                            , .groups = "drop_last")
  df.dom05.sum <- dplyr::summarise(dplyr::group_by(df.dom05
                                                   , SAMPLEID
                                                   , INDEX_NAME
                                                   , INDEX_CLASS)
                            , ni_dom05 = sum(N_TAXA, na.rm = TRUE)
                            , .groups = "drop_last")
  df.dom06.sum <- dplyr::summarise(dplyr::group_by(df.dom06
                                                   , SAMPLEID
                                                   , INDEX_NAME
                                                   , INDEX_CLASS)
                            , ni_dom06 = sum(N_TAXA, na.rm = TRUE)
                            , .groups = "drop_last")
  df.dom07.sum <- dplyr::summarise(dplyr::group_by(df.dom07
                                                   , SAMPLEID
                                                   , INDEX_NAME
                                                   , INDEX_CLASS)
                            , ni_dom07 = sum(N_TAXA, na.rm = TRUE)
                            , .groups = "drop_last")
  df.dom08.sum <- dplyr::summarise(dplyr::group_by(df.dom08
                                                   , SAMPLEID
                                                   , INDEX_NAME
                                                   , INDEX_CLASS)
                            , ni_dom08 = sum(N_TAXA, na.rm = TRUE)
                            , .groups = "drop_last")
  df.dom09.sum <- dplyr::summarise(dplyr::group_by(df.dom09
                                                   , SAMPLEID
                                                   , INDEX_NAME
                                                   , INDEX_CLASS)
                            , ni_dom09 = sum(N_TAXA, na.rm = TRUE)
                            , .groups = "drop_last")
  df.dom10.sum <- dplyr::summarise(dplyr::group_by(df.dom10
                                                   , SAMPLEID
                                                   , INDEX_NAME
                                                   , INDEX_CLASS)
                            , ni_dom10 = sum(N_TAXA, na.rm = TRUE)
                            , .groups = "drop_last")
  df.dom02_NoJugaRiss_BCG_att456.sum <- dplyr::summarise(dplyr::group_by(df.dom02_NoJugaRiss_BCG_att456
                                                                       , SAMPLEID
                                                                       , INDEX_NAME
                                                                       , INDEX_CLASS)
                                                       , ni_dom02_NoJugaRiss_BCG_att456 = sum(N_TAXA)
                                                       , .groups = "drop_last")
  df.dom01_BCG_att4.sum <- dplyr::summarise(dplyr::group_by(df.dom01_BCG_att4
                                                   , SAMPLEID
                                                   , INDEX_NAME
                                                   , INDEX_CLASS)
                                   , ni_dom01_BCG_att4 = sum(N_TAXA, na.rm = TRUE)
                                   , .groups = "drop_last")
  df.dom01_BCG_att5.sum <- dplyr::summarise(dplyr::group_by(df.dom01_BCG_att5
                                                   , SAMPLEID
                                                   , INDEX_NAME
                                                   , INDEX_CLASS)
                                   , ni_dom01_BCG_att5 = sum(N_TAXA, na.rm = TRUE)
                                   , .groups = "drop_last")


  # Add column of domN to main DF
  myDF <- merge(myDF, df.dom01.sum, all.x = TRUE)
  myDF <- merge(myDF, df.dom02.sum, all.x = TRUE)
  myDF <- merge(myDF, df.dom03.sum, all.x = TRUE)
  myDF <- merge(myDF, df.dom04.sum, all.x = TRUE)
  myDF <- merge(myDF, df.dom05.sum, all.x = TRUE)
  myDF <- merge(myDF, df.dom06.sum, all.x = TRUE)
  myDF <- merge(myDF, df.dom07.sum, all.x = TRUE)
  myDF <- merge(myDF, df.dom08.sum, all.x = TRUE)
  myDF <- merge(myDF, df.dom09.sum, all.x = TRUE)
  myDF <- merge(myDF, df.dom10.sum, all.x = TRUE)
  myDF <- merge(myDF, df.dom02_NoJugaRiss_BCG_att456.sum, all.x = TRUE)
  myDF <- merge(myDF, df.dom01_BCG_att4.sum, all.x = TRUE)
  myDF <- merge(myDF, df.dom01_BCG_att5.sum, all.x = TRUE)

  # Convert NA to 0 (avoid -Inf in later calculations)
  myDF[is.na(myDF[, "ni_dom02_NoJugaRiss_BCG_att456"])
       , "ni_dom02_NoJugaRiss_BCG_att456"] <- 0
  myDF[is.na(myDF[, "ni_dom01_BCG_att4"]), "ni_dom01_BCG_att4"] <- 0
  myDF[is.na(myDF[, "ni_dom01_BCG_att5"]), "ni_dom01_BCG_att5"] <- 0

  # Clean up extra Dom data frames
  rm(myDF_dom)
  rm(df.dom01)
  rm(df.dom02)
  rm(df.dom03)
  rm(df.dom04)
  rm(df.dom05)
  rm(df.dom06)
  rm(df.dom07)
  rm(df.dom08)
  rm(df.dom09)
  rm(df.dom10)
  rm(df.dom02_NoJugaRiss_BCG_att456)
  rm(df.dom01_BCG_att4)
  rm(df.dom01_BCG_att5)
  rm(df.dom01.sum)
  rm(df.dom02.sum)
  rm(df.dom03.sum)
  rm(df.dom04.sum)
  rm(df.dom05.sum)
  rm(df.dom06.sum)
  rm(df.dom07.sum)
  rm(df.dom08.sum)
  rm(df.dom09.sum)
  rm(df.dom10.sum)
  rm(df.dom02_NoJugaRiss_BCG_att456.sum)
  rm(df.dom01_BCG_att4.sum)
  rm(df.dom01_BCG_att5.sum)

  # Metric Calc -----
  if (verbose == TRUE) {
    debug_topic <- "Calc, metrics"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
  }## IF ~ verbose

  time_start2 <- Sys.time()

  # Need for metrics without taxaid_dni
  myDF.dni_F <- dplyr::filter(myDF, !TAXAID %in% taxaid_dni)

  if (metric_subset == "MTTI") {
    ## Metric Calc, MTTI ----

    ### met.val, DNI = FALSE----
    met.val.dni_F <- dplyr::summarise(dplyr::group_by(myDF.dni_F
                                                , SAMPLEID
                                                , INDEX_NAME
                                                , INDEX_CLASS)
                                #
                                # one metric per line
                                #
                                #
                                , ni_total = sum(N_TAXA, na.rm = TRUE)
                                , nt_total = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                      & N_TAXA > 0], na.rm = TRUE)
                                , pi_dom01 = 100 * max(N_TAXA, na.rm = TRUE) / ni_total
                                , pi_dom02 = 100 * max(ni_dom02, na.rm = TRUE) / ni_total
                                # WAopt
                                , x_tv2_min = min(TOLVAL2, na.rm = TRUE)
                                , x_tv2_max = max(TOLVAL2, na.rm = TRUE)

                                , .groups = "drop_last"
    )## met.val.dni_F ~ END

    ### met.val, DNI = TRUE----
    met.val.dni_T <- dplyr::summarise(dplyr::group_by(myDF
                                                      , SAMPLEID
                                                      , INDEX_NAME
                                                      , INDEX_CLASS)
                                      #
                                      # one metric per line
                                      #
                                      #### totals ----
                                      , ni_total = sum(N_TAXA, na.rm = TRUE)
                                      , nt_total = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                             & N_TAXA > 0], na.rm = TRUE)
                                      #### DNI ----
                                      , nt_dni = sum(TAXAID == "DNI", na.rm = TRUE)
                                      , pi_dni = 100 * sum(N_TAXA[TAXAID == "DNI"]
                                                           , na.rm = TRUE) / ni_total
                                      , pt_dni = 100 * nt_dni / nt_total

                                      , .groups = "drop_last"
    )## met.val.dni_F ~ END

    ### met.val, join----
    cols2match <- c("SAMPLEID", "INDEX_NAME", "INDEX_CLASS")
    met_dni <- c("nt_dni", "pi_dni", "pt_dni")
    met.val <- dplyr::left_join(met.val.dni_F
                                , met.val.dni_T[, c(cols2match, met_dni)])

  } else {

    ## Metric Calc, ALL ----
    ### met.val, DNI = FALSE----
    met.val.dni_F <- dplyr::summarise(dplyr::group_by(myDF.dni_F
                                                , SAMPLEID
                                                , INDEX_NAME
                                                , INDEX_CLASS)
                                #
                                # one metric per line
                                #
                                ### Individuals ####
                                , ni_total = sum(N_TAXA, na.rm = TRUE)
                                , ni_totalNoDeca = sum(N_TAXA[is.na(ORDER) == TRUE |
                                                                ORDER != "DECAPODA"], na.rm = TRUE)
                                , li_total = log(ni_total)
                                , ni_Chiro = sum(N_TAXA[FAMILY == "CHIRONOMIDAE"], na.rm = TRUE)
                                , ni_Dipt = sum(N_TAXA[ORDER == "DIPTERA"], na.rm = TRUE)
                                , ni_EPT = sum(N_TAXA[ORDER == "EPHEMEROPTERA" |
                                                        ORDER == "PLECOPTERA" |
                                                        ORDER == "TRICHOPTERA"], na.rm = TRUE)
                                , ni_Trich = sum(N_TAXA[ORDER == "TRICHOPTERA"], na.rm = TRUE)
                                , ni_Americo = sum(N_TAXA[GENUS == "AMERICOROPHIUM"], na.rm = TRUE)
                                , ni_Gnorimo = sum(N_TAXA[GENUS == "GNORIMOSPHAEROMA"], na.rm = TRUE)
                                , ni_brackish = ni_Americo + ni_Gnorimo
                                , ni_Ramello = sum(N_TAXA[GENUS == "RAMELLOGAMMARUS"], na.rm = TRUE)


                                ### Phylo ####
                                #### nt_phylo ----
                                # account for "NONE" in nt_total, should be the only 0 N_TAXA
                                , nt_total = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                      & N_TAXA > 0], na.rm = TRUE)
                                , nt_Amph = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                     & ORDER == "AMPHIPODA"]
                                                              , na.rm = TRUE)
                                , nt_Bival = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                      & CLASS == "BIVALVIA"]
                                                               , na.rm = TRUE)
                                , nt_Capit = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                      & FAMILY == "CAPITELLIDAE"]
                                                               , na.rm = TRUE)
                                , nt_Caridea = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                        & INFRAORDER == "CARIDEA"]
                                                                 , na.rm = TRUE)
                                #, nt_Chiro ## in special Chironomidae section
                                , nt_Coleo = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                      & ORDER == "COLEOPTERA"]
                                                               , na.rm = TRUE)
                                , nt_COET = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                     & (ORDER == "COLEOPTERA"
                                                                        | ORDER == "ODONATA"
                                                                        | ORDER == "EPHEMEROPTERA"
                                                                        | ORDER == "TRICHOPTERA")]
                                                              , na.rm = TRUE)
                                , nt_COETNoBraBaeHydTri = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                     & (ORDER == "COLEOPTERA"
                                                                        | ORDER == "ODONATA"
                                                                        | ORDER == "EPHEMEROPTERA"
                                                                        | ORDER == "TRICHOPTERA")
                                                                     & (is.na(FAMILY) == TRUE
                                                                        | FAMILY != "BAETIDAE")
                                                                     & (is.na(FAMILY) == TRUE
                                                                        | FAMILY != "HYDROPSYCHIDAE")
                                                                     & (is.na(GENUS) == TRUE
                                                                        | GENUS != "BRACHYCENTRUS")
                                                                     & (is.na(GENUS) == TRUE
                                                                        | GENUS != "TRICORYTHODES")]
                                                              , na.rm = TRUE)
                                , nt_CruMol = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                       & PHYLUM == "MOLLUSCA"]
                                                                , na.rm = TRUE) +
                                  dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                           & SUBPHYLUM == "CRUSTACEA"]
                                                    , na.rm = TRUE)
                                , nt_Deca = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                     & ORDER == "DECAPODA"]
                                                              , na.rm = TRUE)
                                , nt_Dipt = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                     & ORDER == "DIPTERA"]
                                                              , na.rm = TRUE)
                                , nt_ECT = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                    & (ORDER == "EPHEMEROPTERA"
                                                                       | ORDER == "COLEOPTERA"
                                                                       | ORDER == "TRICHOPTERA")]
                                                             , na.rm = TRUE)
                                , nt_Ephem = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                      & ORDER == "EPHEMEROPTERA"]
                                                               , na.rm = TRUE)
                                , nt_EphemNoBaeTri = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                      & ORDER == "EPHEMEROPTERA"
                                                                      & (is.na(FAMILY) == TRUE
                                                                         | FAMILY != "BAETIDAE")
                                                                      & (is.na(GENUS) == TRUE
                                                                         | GENUS != "TRICORYTHODES")]
                                                                      , na.rm = TRUE)
                                , nt_Ephemerellid = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                             & FAMILY == "EPHEMERELLIDAE"]
                                                                      , na.rm = TRUE)
                                , nt_EPT = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                    & (ORDER == "EPHEMEROPTERA"
                                                                       | ORDER == "PLECOPTERA"
                                                                       | ORDER == "TRICHOPTERA")]
                                                             , na.rm = TRUE)
                                , nt_ET = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                   & (ORDER == "EPHEMEROPTERA"
                                                                      | ORDER == "TRICHOPTERA")]
                                                            , na.rm = TRUE)
                                , nt_Gast = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                     & CLASS == "GASTROPODA"]
                                                              , na.rm = TRUE)
                                , nt_Hemipt = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                       & ORDER == "HEMIPTERA"]
                                                                , na.rm = TRUE)
                                , nt_Hepta = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                      & FAMILY == "HEPTAGENIIDAE"]
                                                               , na.rm = TRUE)
                                , nt_Insect = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                       & CLASS == "INSECTA"]
                                                                , na.rm = TRUE)
                                , nt_Isop = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                     & ORDER == "ISOPODA"]
                                                              , na.rm = TRUE)
                                , nt_Mega = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                     & ORDER == "MEGALOPTERA"]
                                                              , na.rm = TRUE)
                                , nt_Mol = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                    & PHYLUM == "MOLLUSCA"]
                                                             , na.rm = TRUE)
                                , nt_Nereid = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                       & FAMILY == "NEREIDIDAE"]
                                                                , na.rm = TRUE)
                                , nt_Nemour = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                       & FAMILY == "NEMOURIDAE"]
                                                                , na.rm = TRUE)
                                , nt_NonIns = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                       & (CLASS != "INSECTA"
                                                                          | is.na(CLASS))]
                                                                , na.rm = TRUE)
                                , nt_Nudib = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                      & ORDER == "NUDIBRANCHIA"]
                                                               , na.rm = TRUE)
                                , nt_Odon = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                     & ORDER == "ODONATA"]
                                                              , na.rm = TRUE)
                                , nt_OET = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                    & (ORDER == "EPHEMEROPTERA"
                                                                       | ORDER == "TRICHOPTERA"
                                                                       | ORDER == "ODONATA")]
                                                             , na.rm = TRUE)
                                , nt_Oligo = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                      & (CLASS == "OLIGOCHAETA"
                                                                         | SUBCLASS == "OLIGOCHAETA")]
                                                               , na.rm = TRUE)
                                , nt_Perlid = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                       & FAMILY == "PERLIDAE"]
                                                                , na.rm = TRUE)
                                , nt_Pleco = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                      & ORDER == "PLECOPTERA"]
                                                               , na.rm = TRUE)
                                , nt_POET = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                     & (ORDER == "EPHEMEROPTERA"
                                                                        | ORDER == "PLECOPTERA"
                                                                        | ORDER == "TRICHOPTERA"
                                                                        | ORDER == "ODONATA")]
                                                              , na.rm = TRUE)
                                , nt_Poly = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                     & CLASS == "POLYCHAETA"]
                                                              , na.rm = TRUE)
                                , nt_PolyNoSpion = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                    & CLASS == "POLYCHAETA"
                                                                    & (is.na(FAMILY) == TRUE
                                                                       | FAMILY != "SPIONIDAE")]
                                                             , na.rm = TRUE)
                                , nt_Ptero = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                      & GENUS == "PTERONARCYS"]
                                                               , na.rm = TRUE)
                                , nt_Rhya = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                     & GENUS == "RHYACOPHILA"]
                                                              , na.rm = TRUE)
                                , nt_Spion = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                      & FAMILY == "SPIONIDAE"]
                                                               , na.rm = TRUE)
                                , nt_Tipulid = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                        & FAMILY == "TIPULIDAE"]
                                                                 , na.rm = TRUE)
                                , nt_Trich = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                      & ORDER == "TRICHOPTERA"]
                                                               , na.rm = TRUE)
                                , nt_TrichNoHydro = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                             & ORDER == "TRICHOPTERA"
                                                                             & (is.na(FAMILY) == TRUE
                                                                                | FAMILY != "HYDROPSYCHIDAE")]
                                                                      , na.rm = TRUE)
                                , nt_Tromb = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                      & ORDER == "TROMBIDIFORMES"]
                                                               , na.rm = TRUE)
                                , nt_Tubif = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                      & FAMILY == "TUBIFICIDAE"]
                                                               , na.rm = TRUE)
                                # ,intolMol, ,

                                #### pi_phylo ####
                                , pi_Ampe = NA #pi_Ampeliscidae
                                , pi_AmpeHaust = NA
                                , pi_Amph = 100 * sum(N_TAXA[ORDER == "AMPHIPODA"]
                                                      , na.rm = TRUE) / ni_total
                                , pi_AmphIsop = 100 * sum(N_TAXA[ORDER == "AMPHIPODA"
                                                                 | ORDER == "ISOPODA"]
                                                          , na.rm = TRUE) / ni_total
                                , pi_Baet = 100 * sum(N_TAXA[FAMILY == "BAETIDAE"]
                                                      , na.rm = TRUE) / ni_total
                                #, pi_Baet2Ephem
                                , pi_Bival = 100 * sum(N_TAXA[CLASS == "BIVALVIA"]
                                                       , na.rm = TRUE) / ni_total
                                , pi_Caen = 100 * sum(N_TAXA[FAMILY == "CAENIDAE"]
                                                      , na.rm = TRUE) / ni_total
                                , pi_Capit = 100 * sum(N_TAXA[FAMILY == "CAPITELLIDAE"]
                                                       , na.rm = TRUE) / ni_total
                                , pi_Cheu =  100 * sum(N_TAXA[GENUS == "CHEUMATOPSYCHE"]
                                                       , na.rm = TRUE) / ni_total
                                , pi_Cirra = 100 * sum(N_TAXA[FAMILY == "CIRRATULIDAE"]
                                                       , na.rm = TRUE) / ni_total
                                , pi_Clite = 100 * sum(N_TAXA[CLASS == "CLITELLATA"]
                                                       , na.rm = TRUE) / ni_total
                                , pi_Coleo = 100 * sum(N_TAXA[ORDER == "COLEOPTERA"]
                                                       , na.rm = TRUE) / ni_total
                                , pi_COET = 100 * sum(N_TAXA[ORDER == "COLEOPTERA"
                                                             | ORDER == "ODONATA"
                                                             | ORDER == "EPHEMEROPTERA"
                                                             | ORDER == "TRICHOPTERA"]
                                                      , na.rm = TRUE) / ni_total
                                , pi_Corb = 100 * sum(N_TAXA[GENUS == "CORBICULA"]
                                                      , na.rm = TRUE) / ni_total
                                , pi_CorixPhys = 100 * sum(N_TAXA[FAMILY == "CORIXIDAE"
                                                                  | FAMILY == "PHYSIDAE"]
                                                           , na.rm = TRUE) / ni_total
                                , pi_CraCaeGam = 100 * sum(N_TAXA[GENUS == "CRANGONYX"
                                                                  | GENUS == "CAECIDOTEA"
                                                                  | GENUS == "GAMMARUS"]
                                                           , na.rm = TRUE) / ni_total
                                , pi_Cru = 100 * sum(N_TAXA[SUBPHYLUM == "CRUSTACEA"]
                                                     , na.rm = TRUE) / ni_total
                                , pi_CruMol = 100 * sum(N_TAXA[PHYLUM == "MOLLUSCA"
                                                               | SUBPHYLUM == "CRUSTACEA"]
                                                        , na.rm = TRUE) / ni_total
                                , pi_Deca = 100 * sum(N_TAXA[ORDER == "DECAPODA"]
                                                      , na.rm = TRUE) / ni_total
                                , pi_Dipt = 100 * sum(N_TAXA[ORDER == "DIPTERA"]
                                                      , na.rm = TRUE) / ni_total
                                , pi_DiptNonIns = 100 * sum(N_TAXA[ORDER == "DIPTERA"
                                                                   | CLASS != "INSECTA"
                                                                   | is.na(CLASS)]
                                                            , na.rm = TRUE) / ni_total
                                , pi_ECT = 100 * sum(N_TAXA[ORDER == "EPHEMEROPTERA"
                                                            | ORDER == "COLEOPTERA"
                                                            | ORDER == "TRICHOPTERA"]
                                                     , na.rm = TRUE) / ni_total
                                , pi_Ephem = 100 * sum(N_TAXA[ORDER == "EPHEMEROPTERA"]
                                                       , na.rm = TRUE) / ni_total
                                , pi_EphemNoCae = 100 * sum(N_TAXA[ORDER == "EPHEMEROPTERA"
                                                                   & (is.na(FAMILY) == TRUE
                                                                      | FAMILY != "CAENIDAE")]
                                                            , na.rm = TRUE) / ni_total
                                , pi_EphemNoCaeBae = 100 * sum(N_TAXA[ORDER == "EPHEMEROPTERA"
                                                                      & (is.na(FAMILY) == TRUE
                                                                         | FAMILY != "CAENIDAE")
                                                                      & (is.na(FAMILY) == TRUE
                                                                         | FAMILY != "BAETIDAE")]
                                                               , na.rm = TRUE) / ni_total
                                , pi_EphemNoBaeTri = 100 * sum(N_TAXA[ORDER == "EPHEMEROPTERA"
                                                                      & (is.na(FAMILY) == TRUE
                                                                         | FAMILY != "BAETIDAE")
                                                                      & (is.na(GENUS) == TRUE
                                                                         | GENUS != "TRICORYTHODES")]
                                                               , na.rm = TRUE) / ni_total
                                , pi_EPT = 100 * sum(N_TAXA[ORDER == "EPHEMEROPTERA"
                                                            | ORDER == "TRICHOPTERA"
                                                            | ORDER == "PLECOPTERA"]
                                                     , na.rm = TRUE) / ni_total
                                , pi_EPTNoBaeHydro = 100 * sum(N_TAXA[(ORDER == "EPHEMEROPTERA"
                                                                       & (is.na(FAMILY) == TRUE
                                                                          | FAMILY != "BAETIDAE"))
                                                                      | (ORDER == "TRICHOPTERA"
                                                                         & (is.na(FAMILY) == TRUE
                                                                            | FAMILY != "HYDROPSYCHIDAE"))
                                                                      | ORDER == "PLECOPTERA"]
                                                               , na.rm = TRUE) / ni_total
                                , pi_EPTNoCheu = 100 * sum(N_TAXA[ORDER == "EPHEMEROPTERA"
                                                                  | ORDER == "TRICHOPTERA"
                                                                  | ORDER == "PLECOPTERA"
                                                                  & (is.na(GENUS) == TRUE
                                                                     | GENUS != "CHEUMATOPSYCHE")]
                                                           , na.rm = TRUE) / ni_total
                                , pi_EPTNoHydro = 100 * sum(N_TAXA[(ORDER == "EPHEMEROPTERA")
                                                                   | (ORDER == "TRICHOPTERA"
                                                                      & (is.na(FAMILY) == TRUE
                                                                         | FAMILY != "HYDROPSYCHIDAE"))
                                                                   | ORDER == "PLECOPTERA"]
                                                            , na.rm = TRUE) / ni_total
                                , pi_ET = 100 * sum(N_TAXA[ORDER == "EPHEMEROPTERA"
                                                           | ORDER == "TRICHOPTERA"]
                                                    , na.rm = TRUE) / ni_total
                                , pi_Gast = 100 * sum(N_TAXA[CLASS == "GASTROPODA"]
                                                      , na.rm = TRUE) / ni_total
                                , pi_Haust = 100 * sum(N_TAXA[FAMILY == "HAUSTORIIDAE"]
                                                       , na.rm = TRUE) / ni_total
                                , pi_Hemipt = 100 * sum(N_TAXA[ORDER == "HEMIPTERA"]
                                                        , na.rm = TRUE) / ni_total
                                , pi_Hesion = 100 * sum(N_TAXA[FAMILY == "HESIONIDAE"]
                                                        , na.rm = TRUE) / ni_total
                                , pi_Hydro = 100 * sum(N_TAXA[FAMILY == "HYDROPSYCHIDAE"]
                                                       , na.rm = TRUE) / ni_total
                                , pi_Hydro2EPT = 100 * sum(N_TAXA[FAMILY == "HYDROPSYCHIDAE"]
                                                           , na.rm = TRUE)/ni_EPT
                                , pi_Hydro2Trich = 100 * sum(N_TAXA[FAMILY == "HYDROPSYCHIDAE"]
                                                             , na.rm = TRUE)/ni_Trich
                                , pi_Insect = 100 * sum(N_TAXA[CLASS == "INSECTA"]
                                                        , na.rm = TRUE) / ni_total
                                , pi_Isop = 100 * sum(N_TAXA[ORDER == "ISOPODA"]
                                                      , na.rm = TRUE) / ni_total
                                , pi_IsopGastHiru = 100 * sum(N_TAXA[ORDER == "ISOPODA"
                                                                     | CLASS == "GASTROPODA"
                                                                     | SUBCLASS == "HIRUDINEA"]
                                                              , na.rm = TRUE) / ni_total
                                , pi_Juga = 100 * sum(N_TAXA[GENUS == "JUGA"]
                                                      , na.rm = TRUE) / ni_total
                                , pi_JugaFlumi = 100 * sum(N_TAXA[GENUS == "JUGA"
                                                                  | GENUS == "FLUMINICOLA"]
                                                           , na.rm = TRUE) / ni_total
                                , pi_Lucin = 100 * sum(N_TAXA[FAMILY == "LUCINIDAE"]
                                                       , na.rm = TRUE) / ni_total
                                , pi_LucinTellin = 100 * sum(N_TAXA[FAMILY == "LUCINIDAE"
                                                                    | FAMILY == "TELLINIDAE"]
                                                             , na.rm = TRUE) / ni_total
                                , pi_Mega = 100 * sum(N_TAXA[ORDER == "MEGALOPTERA"]
                                                      , na.rm = TRUE) / ni_total
                                , pi_Mol = 100 * sum(N_TAXA[PHYLUM == "MOLLUSCA"]
                                                     , na.rm = TRUE) / ni_total
                                , pi_Nemata = 100 * sum(N_TAXA[PHYLUM == "NEMATA"]
                                                        , na.rm = TRUE) / ni_total
                                , pi_Nereid = 100 * sum(N_TAXA[FAMILY == "NEREIDIDAE"]
                                                        , na.rm = TRUE) / ni_total
                                , pi_Nudib = 100 * sum(N_TAXA[ORDER == "NUDIBRANCHIA"]
                                                       , na.rm = TRUE) / ni_total
                                , pi_NonIns = 100 * sum(N_TAXA[CLASS != "INSECTA"
                                                               | is.na(CLASS)]
                                                        , na.rm = TRUE) / ni_total
                                , pi_Odon = 100 * sum(N_TAXA[ORDER == "ODONATA"]
                                                      , na.rm = TRUE) / ni_total
                                , pi_OET = 100 * sum(N_TAXA[ORDER == "ODONATA"
                                                            | ORDER == "EPHEMEROPTERA"
                                                            | ORDER == "TRICHOPTERA"]
                                                     , na.rm = TRUE) / ni_total
                                , pi_Oligo = 100 * sum(N_TAXA[CLASS == "OLIGOCHAETA"
                                                              | SUBCLASS == "OLIGOCHAETA"]
                                                       , na.rm = TRUE) / ni_total
                                , pi_Orbin = 100 * sum(N_TAXA[FAMILY == "ORBINIIDAE"]
                                                       , na.rm = TRUE) / ni_total
                                , pi_Pleco = 100 * sum(N_TAXA[ORDER == "PLECOPTERA"]
                                                       , na.rm = TRUE) / ni_total
                                , pi_POET = 100 * sum(N_TAXA[ORDER == "PLECOPTERA"
                                                             | ORDER == "ODONATA"
                                                             | ORDER == "EPHEMEROPTERA"
                                                             | ORDER == "TRICHOPTERA"]
                                                      , na.rm = TRUE) / ni_total
                                , pi_Poly = 100 * sum(N_TAXA[CLASS == "POLYCHAETA"]
                                                      , na.rm = TRUE) / ni_total
                                , pi_Spion = 100 * sum(N_TAXA[FAMILY == "SPIONIDAE"]
                                                       , na.rm = TRUE) / ni_total
                                , pi_Spion2Poly = 100 * sum(N_TAXA[CLASS == "POLYCHAETA" |
                                                                     FAMILY == "SPIONIDAE"]
                                                            , na.rm = TRUE) / ni_total
                                , pi_Sphaer = 100 * sum(N_TAXA[FAMILY == "SPHAERIIDAE"]
                                                        , na.rm = TRUE) / ni_total
                                , pi_SphaerCorb = 100 * sum(N_TAXA[FAMILY == "SPHAERIIDAE" |
                                                                     GENUS == "CORBICULA"]
                                                            , na.rm = TRUE) / ni_total
                                , pi_Tellin = 100 * sum(N_TAXA[FAMILY == "TELLINIDAE"]
                                                        , na.rm = TRUE) / ni_total
                                , pi_Trich = 100 * sum(N_TAXA[ORDER == "TRICHOPTERA"]
                                                       , na.rm = TRUE) / ni_total
                                , pi_TrichNoHydro = 100 * sum(N_TAXA[ORDER == "TRICHOPTERA"
                                                                     & (is.na(FAMILY) == TRUE |
                                                                          FAMILY != "HYDROPSYCHIDAE")]
                                                              , na.rm = TRUE) / ni_total
                                , pi_Tromb = 100 * sum(N_TAXA[ORDER == "TROMBIDIFORMES"]
                                                       , na.rm = TRUE) / ni_total
                                , pi_Tubif = 100 * sum(N_TAXA[FAMILY == "TUBIFICIDAE"]
                                                       , na.rm = TRUE) / ni_total
                                , pi_Xanth = 100 * sum(N_TAXA[FAMILY == "XANTHIDAE"]
                                                       , na.rm = TRUE) / ni_total
                                # Cole2Odon,
                                #EPTsenstive in tolerance group


                                #### pt_phylo ####
                                , pt_Amph = 100 * nt_Amph / nt_total
                                , pt_Bival = 100 * nt_Bival / nt_total
                                , pt_Coleo = 100 * nt_Coleo / nt_total
                                , pt_COET = 100 * nt_COET / nt_total
                                , pt_Deca = 100 * nt_Deca / nt_total
                                , pt_Dipt = 100 * nt_Dipt / nt_total
                                , pt_ECT = 100 * nt_ECT / nt_total
                                , pt_Ephem = 100 * nt_Ephem / nt_total
                                , pt_EPT = 100 * nt_EPT / nt_total
                                , pt_ET = 100 * nt_ET / nt_total
                                , pt_Gast = 100 * nt_Gast / nt_total
                                , pt_Hemipt = 100 * nt_Hemipt / nt_total
                                , pt_Insect = 100 * nt_Insect / nt_total
                                , pt_Isop = 100 * nt_Isop / nt_total
                                , pt_Mega = 100 * nt_Mega / nt_total
                                , pt_NonIns = 100 * nt_NonIns / nt_total
                                , pt_Nudib = 100 * nt_Nudib / nt_total
                                , pt_Odon = 100 * nt_Odon / nt_total
                                , pt_OET = 100 * nt_OET / nt_total
                                , pt_Oligo = 100 * nt_Oligo / nt_total
                                , pt_Pleco = 100 * nt_Pleco / nt_total
                                , pt_POET = 100 * nt_POET / nt_total
                                , pt_Poly = 100 * nt_Poly / nt_total
                                , pt_PolyNoSpion = 100 * nt_PolyNoSpion / nt_total
                                , pt_Spion = 100 * nt_Spion / nt_total
                                , pt_Trich = 100 * nt_Trich / nt_total
                                , pt_TrichNoHydro = 100 * nt_TrichNoHydro / nt_total
                                , pt_Tromb = 100 * nt_Tromb / nt_total

                                #### ratio_phylo ####
                                # nt_X / log(ni_total)
                                # X = specialty group, e.g., Bivalves
                                # Log is natural log
                                #, rt_Amph = nt_Amph
                                #, rt_AmpCar = nt_AmpCar
                                #, rt_Bivalve = nt_Bivalve
                                #, rt_Capit = nt_Capit
                                #, rt_Car = nt_Car
                                #, rt_Coleo = nt_Coleo
                                #, rt_CruMol = nt_CruMol
                                #, rt_Deca = nt_Deca
                                #, rt_Ephem = nt_Ephem
                                #, rt_EPT = nt_EPT
                                #, rt_Gast = nt_Gast
                                #, rt_Isop = nt_Isop
                                #, rt_Nereid = nt_Nereid
                                #, rt_Nudib = nt_Nudib
                                #, rt_Oligo = nt_Oligo
                                #, rt_Pleco = nt_Pleco
                                #, rt_Poly = nt_Poly
                                #, rt_PolyNoSpion = nt_PolyNoSpion
                                #, rt_Ptero = nt_Ptero
                                #, rt_Spion = nt_Spion
                                #, rt_Trich = nt_Trich
                                #, rt_Tubif = nt_Tubif


                                ### Midges ####
                                # Family    = Chironomidae
                                # subfamily = Chironominae
                                # subfamily = Orthocladiinae
                                # subfamily = Tanypodinae
                                # Tribe     = Tanytarsini
                                , nt_Chiro = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                      & FAMILY == "CHIRONOMIDAE"]
                                                               , na.rm = TRUE)
                                , pi_Chiro = 100 * ni_Chiro / ni_total
                                , pt_Chiro = 100 * nt_Chiro / nt_total
                                , pi_Ortho = 100 * sum(N_TAXA[SUBFAMILY == "ORTHOCLADIINAE"]
                                                       , na.rm = TRUE) / ni_total
                                , pi_Tanyt = 100 * sum(N_TAXA[TRIBE == "TANYTARSINI"]
                                                       , na.rm = TRUE) / ni_total
                                , pi_Tanyp = 100 * sum(N_TAXA[SUBFAMILY == "TANYPODINAE"]
                                                       , na.rm = TRUE) / ni_total
                                , pi_Chi2Dipt = 100 * ni_Chiro / ni_Dipt
                                , pi_COC2Chi = 100 * sum(N_TAXA[GENUS == "CHIRONOMUS"
                                                                | GENUS == "CRICOTOPUS"
                                                                | GENUS == "CRICOTOPUS/ORTHOCLADIUS"
                                                                | GENUS == "ORTHOCLADIUS/CRICOTOPUS"
                                                                | GENUS == "ORTHOCLADIUS"]
                                                         , na.rm = TRUE)/ni_Chiro

                                , pi_ChCr2Chi = 100 * sum(N_TAXA[GENUS == "CHIRONOMUS"
                                                                 | GENUS == "CRICOTOPUS"]
                                                          , na.rm = TRUE)/ni_Chiro
                                , pi_Orth2Chi = 100 * sum(N_TAXA[SUBFAMILY == "ORTHOCLADIINAE"]
                                                          , na.rm = TRUE)/ni_Chiro
                                , pi_Tanyp2Chi = 100 * sum(N_TAXA[SUBFAMILY == "TANYPODINAE"]
                                                           , na.rm = TRUE)/ni_Chiro
                                #,nt_Ortho (Marine)
                                #MB_pi_OrthocladiinaeCricotopusChironomus2Chironomidae
                                # rt_Chiro, Ortho, Tanyt
                                , pi_ChiroAnne = 100 * sum(N_TAXA[PHYLUM == "ANNELIDA"
                                                                  | FAMILY == "CHIRONOMIDAE"]
                                                           , na.rm = TRUE) / ni_total

                                ### Other misc ----
                                # dominant
                                , pi_dom02_BCG_att456_NoJugaRiss = 100 * max(ni_dom02_NoJugaRiss_BCG_att456) / ni_total
                                #
                                # 20180608, rework PacNW
                                # NonINSECTA, Attribute 456
                                , nt_NonIns_BCG_att456 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                                  & (is.na(CLASS) == TRUE
                                                                                     | CLASS != "INSECTA")
                                                                                  & (BCG_ATTR == "4"
                                                                                     | BCG_ATTR == "5"
                                                                                     | BCG_ATTR == "6")]
                                                                           , na.rm = TRUE)
                                , pi_NonIns_BCG_att456 = 100 * sum(N_TAXA[
                                  (is.na(CLASS) == TRUE
                                   | CLASS != "INSECTA")
                                  & (BCG_ATTR == "4"
                                     | BCG_ATTR == "5"
                                     | BCG_ATTR == "6")]
                                  , na.rm = TRUE) / ni_total
                                , pt_NonIns_BCG_att456 = 100 * nt_NonIns_BCG_att456 / nt_total
                                # NonInsectaJugaRiss, Attribute 456
                                , nt_NonInsJugaRiss_BCG_att456 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                                          & (is.na(CLASS) == TRUE
                                                                                             | CLASS != "INSECTA")
                                                                                          & (is.na(ORDER) == TRUE
                                                                                             | ORDER != "RISSOOIDEA")
                                                                                          & (is.na(GENUS) == TRUE
                                                                                             | GENUS != "JUGA")
                                                                                          & (BCG_ATTR == "4"
                                                                                             | BCG_ATTR == "5"
                                                                                             | BCG_ATTR == "6")]
                                                                                   , na.rm = TRUE)
                                , pi_NonInsJugaRiss_BCG_att456 = 100 * sum(N_TAXA[
                                  (is.na(CLASS) == TRUE
                                   | CLASS != "INSECTA")
                                  & (is.na(ORDER) == TRUE
                                     | ORDER != "RISSOOIDEA")
                                  & (is.na(GENUS) == TRUE
                                     | GENUS != "JUGA")
                                  & (BCG_ATTR == "4"
                                     | BCG_ATTR == "5"
                                     | BCG_ATTR == "6")]
                                  , na.rm = TRUE) / ni_total
                                , pt_NonInsJugaRiss_BCG_att456 = 100 * nt_NonInsJugaRiss_BCG_att456 / nt_total
                                # 20180815, Percent BAETIS TRICAUDATUS COMPLEX + SIMULIIDAE individual
                                , pi_SimBtri = 100 * (sum(N_TAXA[FAMILY == "SIMULIIDAE"], na.rm = TRUE)
                                                      + sum(N_TAXA[TAXAID == "BAETIS TRICAUDATUS COMPLEX"]
                                                            , na.rm = TRUE)) / ni_total
                                # 20181018, MS, sensitive COLEOPTERA & (Family is Null or not Hydrophyilidae)
                                , pi_Colesens = 100 * sum(N_TAXA[ORDER == "COLEOPTERA"
                                                                 & (FAMILY != "HYDROPHILIDAE"
                                                                    | is.na(FAMILY) == TRUE)]
                                                          , na.rm = TRUE) / ni_total
                                # 20181207, BCG PacNW, Level 1 Signal metrics
                                , nt_longlived =  dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                           & LONGLIVED == TRUE]
                                                                    , na.rm = TRUE)
                                , pt_longlived = 100 * nt_longlived / nt_total
                                , nt_noteworthy = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                           & NOTEWORTHY == TRUE]
                                                                    , na.rm = TRUE)
                                , nt_ffg2_pred =  dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                           & FFG2_PRE == TRUE]
                                                                    , na.rm = TRUE)
                                , ni_Noto = sum(N_TAXA[GENUS == "NOTOMASTUS"], na.rm = TRUE)


                                ### Thermal Indicators ----
                                #### nt_ti----
                                , nt_ti_stenocold =      dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                                  & TI_STENOCOLD == TRUE]
                                                                           , na.rm = TRUE)
                                , nt_ti_cold =           dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                                  & TI_COLD == TRUE]
                                                                           , na.rm = TRUE)
                                , nt_ti_cool =           dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                                  & TI_COOL == TRUE]
                                                                           , na.rm = TRUE)
                                , nt_ti_warm =           dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                                  & TI_WARM == TRUE]
                                                                           , na.rm = TRUE)
                                , nt_ti_stenowarm =      dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                                  & TI_STENOWARM == TRUE]
                                                                           , na.rm = TRUE)
                                , nt_ti_eury =           dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                                  & TI_EURY == TRUE]
                                                                           , na.rm = TRUE)
                                , nt_ti_cowa =   dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                          & TI_COWA == TRUE]
                                                                   , na.rm = TRUE)
                                , nt_ti_na =             dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                                  & TI_NA == TRUE]
                                                                           , na.rm = TRUE)
                                , nt_ti_stenocold_cold = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                                  & (TI_STENOCOLD == TRUE |
                                                                                       TI_COLD == TRUE)]
                                                                           , na.rm = TRUE)
                                , nt_ti_stenocold_cold_cool = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                                       & (TI_STENOCOLD == TRUE |
                                                                                            TI_COLD == TRUE |
                                                                                            TI_COOL == TRUE)]
                                                                                , na.rm = TRUE)
                                , nt_ti_cowa_warm_stenowarm =       dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                                             & (TI_COWA == TRUE |
                                                                                                  TI_WARM == TRUE |
                                                                                                  TI_STENOWARM == TRUE )]
                                                                                      , na.rm = TRUE)
                                , nt_ti_warm_stenowarm = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                                  & (TI_WARM == TRUE |
                                                                                       TI_STENOWARM == TRUE)]
                                                                           , na.rm = TRUE)

                                #### pi_ti----
                                , pi_ti_stenocold =        100 * sum(N_TAXA[TI_STENOCOLD == TRUE]
                                                                     , na.rm = TRUE) / ni_total
                                , pi_ti_cold =             100 * sum(N_TAXA[TI_COLD == TRUE]
                                                                     , na.rm = TRUE) / ni_total
                                , pi_ti_cool =             100 * sum(N_TAXA[TI_COOL == TRUE]
                                                                     , na.rm = TRUE) / ni_total
                                , pi_ti_warm =             100 * sum(N_TAXA[TI_WARM == TRUE]
                                                                     , na.rm = TRUE) / ni_total
                                , pi_ti_stenowarm =        100 * sum(N_TAXA[TI_STENOWARM == TRUE]
                                                                     , na.rm = TRUE) / ni_total
                                , pi_ti_eury =             100 * sum(N_TAXA[TI_EURY == TRUE]
                                                                     , na.rm = TRUE) / ni_total
                                , pi_ti_cowa =             100 * sum(N_TAXA[TI_COWA == TRUE]
                                                                     , na.rm = TRUE) / ni_total
                                , pi_ti_na =               100 * sum(N_TAXA[TI_NA == TRUE]
                                                                     , na.rm = TRUE) / ni_total
                                , pi_ti_stenocold_cold =   100 * sum(N_TAXA[TI_STENOCOLD == TRUE |
                                                                              TI_COLD == TRUE]
                                                                     , na.rm = TRUE) / ni_total
                                , pi_ti_stenocold_cold_cool = 100 * sum(N_TAXA[TI_STENOCOLD == TRUE |
                                                                                 TI_COLD == TRUE |
                                                                                 TI_COOL == TRUE]
                                                                        , na.rm = TRUE) / ni_total
                                , pi_ti_cowa_warm_stenowarm = 100 * sum(N_TAXA[TI_COWA == TRUE |
                                                                                 TI_WARM == TRUE |
                                                                                 TI_STENOWARM == TRUE]
                                                                        , na.rm = TRUE) / ni_total
                                , pi_ti_warm_stenowarm =   100 * sum(N_TAXA[TI_WARM == TRUE |
                                                                              TI_STENOWARM == TRUE]
                                                                     , na.rm = TRUE) / ni_total


                                #### pt_ti ----
                                , pt_ti_stenocold =           100 * nt_ti_stenocold / nt_total
                                , pt_ti_cold =                100 * nt_ti_cold / nt_total
                                , pt_ti_cool =                100 * nt_ti_cool / nt_total
                                , pt_ti_warm =                100 * nt_ti_warm / nt_total
                                , pt_ti_stenowarm =           100 * nt_ti_stenowarm / nt_total
                                , pt_ti_eury =                100 * nt_ti_eury / nt_total
                                , pt_ti_cowa =                100 * nt_ti_cowa / nt_total
                                , pt_ti_na =                  100 * nt_ti_na / nt_total
                                , pt_ti_stenocold_cold =      100 * nt_ti_stenocold_cold / nt_total
                                , pt_ti_stenocold_cold_cool = 100 * nt_ti_stenocold_cold_cool / nt_total
                                , pt_ti_cowa_warm_stenowarm = 100 * nt_ti_cowa_warm_stenowarm / nt_total
                                , pt_ti_warm_stenowarm =      100 * nt_ti_warm_stenowarm / nt_total

                                ### ratio
                                , ri_ti_sccc_wsw = pi_ti_stenocold_cold_cool / pi_ti_warm_stenowarm


                                ### Tolerance ----
                                # 4 and 6 are WV GLIMPSS (no equal)
                                , nt_tv_intol = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                         & TOLVAL >= 0
                                                                         & TOLVAL <= 3]
                                                                  , na.rm = TRUE)
                                , nt_tv_intol2 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                         & TOLVAL >= 0
                                                                         & TOLVAL <= 2]
                                                                  , na.rm = TRUE)
                                , nt_tv_intol4 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                          & TOLVAL >= 0
                                                                          & TOLVAL < 4]
                                                                   , na.rm = TRUE)
                                , nt_tv_toler = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                         & TOLVAL >= 7
                                                                         & TOLVAL <= 10]
                                                                  , na.rm = TRUE)
                                , nt_tv_toler6 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                         & TOLVAL >= 6
                                                                         & TOLVAL <= 10]
                                                                  , na.rm = TRUE)
                                , nt_tv_toler8 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                          & TOLVAL >= 8
                                                                          & TOLVAL <= 10]
                                                                   , na.rm = TRUE)
                                , pi_tv_intol = 100 * sum(N_TAXA[TOLVAL >= 0
                                                               & TOLVAL <= 3], na.rm = TRUE) / ni_total
                                , pi_tv_intol4 = 100 * sum(N_TAXA[TOLVAL >= 0
                                                                & TOLVAL < 4], na.rm = TRUE) / ni_total
                                , pi_tv_toler = 100 * sum(N_TAXA[TOLVAL >= 7
                                                               & TOLVAL <= 10], na.rm = TRUE) / ni_total
                                , pi_tv_toler6 = 100 * sum(N_TAXA[TOLVAL > 6
                                                                & TOLVAL <= 10], na.rm = TRUE) / ni_total
                                , pi_tv_toler8 = 100 * sum(N_TAXA[TOLVAL >= 8
                                                                  & TOLVAL <= 10], na.rm = TRUE) / ni_total
                                , pt_tv_intol = 100 * nt_tv_intol / nt_total
                                , pt_tv_intol4 = 100 * nt_tv_intol4 / nt_total
                                , pt_tv_toler = 100 * nt_tv_toler / nt_total
                                , pt_tv_toler6 = 100 * nt_tv_toler6 / nt_total
                                , pt_tv_toler8 = 100 * nt_tv_toler8 / nt_total


                                #,nt_tvfam_intol = dplyr::n_distinct(TAXAID[EXCLUDE!=TRUE & FAM_TV<=3 & !is.na(FAM_TV)])
                                # pi_Baet2Eph, pi_Hyd2EPT, pi_Hyd2Tri, in Pct Ind group
                                # nt_intMol (for marine)
                                # intol4_EPT is PA not WV so [0-4].
                                , nt_tv_intol4_EPT = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                              & TOLVAL >= 0
                                                                              & TOLVAL <= 4
                                                                              & (ORDER == "EPHEMEROPTERA"
                                                                                 | ORDER == "TRICHOPTERA"
                                                                                 | ORDER == "PLECOPTERA")]
                                                                       , na.rm = TRUE)
                                # USEPA, WSA and NRSA
                                ## ntol is not tolerant
                                ## stol is super tolerant
                                , nt_tv_ntol = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                        & TOLVAL >= 0
                                                                        & TOLVAL < 6]
                                                                 , na.rm = TRUE)
                                , nt_tv_stol = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                        & TOLVAL >= 8
                                                                        & TOLVAL <= 10]
                                                                 , na.rm = TRUE)
                                , pi_tv_ntol = 100 * sum(N_TAXA[TOLVAL >= 0
                                                                & TOLVAL < 6]
                                                         , na.rm = TRUE) / ni_total
                                , pi_tv_stol = 100 * sum(N_TAXA[TOLVAL >= 8
                                                                & TOLVAL <= 10]
                                                         , na.rm = TRUE) / ni_total
                                , pt_tv_ntol = 100 * nt_tv_ntol / nt_total
                                , pt_tv_stol = 100 * nt_tv_stol / nt_total


                                ### Tolerance2 ####
                                ## special condition tolerance values
                                # MBSS
                                , pi_tv2_intol = sum(N_TAXA[TOLVAL2 <= 3
                                                            & !is.na(TOLVAL2)])/sum(N_TAXA[!is.na(TOLVAL2)])
                                #, pi_tv_intolurb=pi_tv2_intol
                                , pi_tv2_toler_ISA_SalHi_xFL = NA
                                , pi_tv2_intol_ISA_SalHi_xFL = NA
                                , pt_tv2_intol_ISA_SalHi_xFL = NA

                                ### FFG #####
                                #### nt_ffg----
                                , nt_ffg_col = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                        & FFG_COL == TRUE]
                                                                 , na.rm = TRUE)
                                , nt_ffg_filt = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                         & FFG_FIL == TRUE]
                                                                  , na.rm = TRUE)
                                , nt_ffg_pred = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                         & FFG_PRE == TRUE]
                                                                  , na.rm = TRUE)
                                , nt_ffg_scrap = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                          & FFG_SCR == TRUE]
                                                                   , na.rm = TRUE)
                                , nt_ffg_shred = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                          & FFG_SHR == TRUE]
                                                                   , na.rm = TRUE)
                                , nt_ffg_mah = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                        & FFG_MAH == TRUE]
                                                                 , na.rm = TRUE)
                                , nt_ffg_omn = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                        & FFG_OMN == TRUE]
                                                                 , na.rm = TRUE)
                                , nt_ffg_par = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                        & FFG_PAR == TRUE]
                                                                 , na.rm = TRUE)
                                , nt_ffg_pih = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                        & FFG_PIH == TRUE]
                                                                 , na.rm = TRUE)
                                , nt_ffg_xyl = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                        & FFG_XYL == TRUE]
                                                                 , na.rm = TRUE)
                                , nt_ffg_pred_scrap_shred = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                                     & (FFG_PRE == TRUE |
                                                                                          FFG_SCR == TRUE |
                                                                                          FFG_SHR == TRUE)]
                                                                              , na.rm = TRUE)
                                , nt_ffg_pred_NoChi = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                         & FFG_PRE == TRUE
                                                                         & (is.na(FAMILY) == TRUE
                                                                            | FAMILY != "CHIRONOMIDAE")]
                                                                  , na.rm = TRUE)

                                #### pi_ffg----
                                , pi_ffg_col = 100 * sum(N_TAXA[FFG_COL == TRUE]
                                                         , na.rm = TRUE) / ni_total
                                , pi_ffg_filt = 100 * sum(N_TAXA[FFG_FIL == TRUE]
                                                          , na.rm = TRUE) / ni_total
                                , pi_ffg_pred = 100 * sum(N_TAXA[FFG_PRE == TRUE]
                                                          , na.rm = TRUE) / ni_total
                                , pi_ffg_scrap = 100 * sum(N_TAXA[FFG_SCR == TRUE]
                                                           , na.rm = TRUE) / ni_total
                                , pi_ffg_shred = 100 * sum(N_TAXA[FFG_SHR == TRUE]
                                                           , na.rm = TRUE) / ni_total
                                , pi_ffg_mah = 100 * sum(N_TAXA[FFG_MAH == TRUE]
                                                         , na.rm = TRUE) / ni_total
                                , pi_ffg_omn = 100 * sum(N_TAXA[FFG_OMN == TRUE]
                                                         , na.rm = TRUE) / ni_total
                                , pi_ffg_par = 100 * sum(N_TAXA[FFG_PAR == TRUE]
                                                         , na.rm = TRUE) / ni_total
                                , pi_ffg_pih = 100 * sum(N_TAXA[FFG_PIH == TRUE]
                                                         , na.rm = TRUE) / ni_total
                                , pi_ffg_xyl = 100 * sum(N_TAXA[FFG_XYL == TRUE]
                                                         , na.rm = TRUE) / ni_total
                                , pi_ffg_col_filt = 100 * sum(N_TAXA[FFG_COL == TRUE |
                                                                       FFG_FIL == TRUE]
                                                              , na.rm = TRUE) / ni_total

                                #### pt_ffg----
                                , pt_ffg_col =   100 * nt_ffg_col / nt_total
                                , pt_ffg_filt =  100 * nt_ffg_filt / nt_total
                                , pt_ffg_pred =  100 * nt_ffg_pred / nt_total
                                , pt_ffg_scrap = 100 * nt_ffg_scrap / nt_total
                                , pt_ffg_shred = 100 * nt_ffg_shred / nt_total
                                , pt_ffg_mah =   100 * nt_ffg_mah / nt_total
                                , pt_ffg_omn =   100 * nt_ffg_omn / nt_total
                                , pt_ffg_par =   100 * nt_ffg_par / nt_total
                                , pt_ffg_pih =   100 * nt_ffg_pih / nt_total
                                , pt_ffg_xyl =   100 * nt_ffg_xyl / nt_total

                                #, pi_ffg_infc
                                #, rt_ffg_infc, converborbelt, scavbrow, subsurf, watercol
                                #, rt_ffg_pred
                                # carnivoreomnivore, deepdeposit, suspension

                                ### FFG2 ####
                                # marine
                                ## nt_ffg2
                                , nt_ffg2_intface = NA
                                , nt_ffg2_subsurf = NA
                                ## pi_ffg2
                                , pi_ffg2_scavburr = NA
                                ## pt_ffg2
                                # = conveyorbelt, interface, scavengerbrowser, subsurface, watercolumn, predator

                                ### Habit ####
                                #(need to be wild card. that is, counts both CN,CB and CB as climber)
                                #### nt_habit----
                                , nt_habit_burrow = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                             & HABIT_BU == TRUE]
                                                                      , na.rm = TRUE)
                                , nt_habit_climb = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                            & HABIT_CB == TRUE]
                                                                     , na.rm = TRUE)
                                , nt_habit_climbcling = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                                 & (HABIT_CB == TRUE
                                                                                    | HABIT_CN == TRUE)])
                                , nt_habit_cling = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                            & HABIT_CN == TRUE]
                                                                     , na.rm = TRUE)
                                , nt_habit_sprawl = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                             & HABIT_SP == TRUE]
                                                                      , na.rm = TRUE)
                                , nt_habit_swim = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                           & HABIT_SW == TRUE]
                                                                    , na.rm = TRUE)

                                #### pi_habit----
                                , pi_habit_burrow = 100 * sum(N_TAXA[HABIT_BU == TRUE]
                                                              , na.rm = TRUE) / ni_total
                                , pi_habit_climb = 100 * sum(N_TAXA[HABIT_CB == TRUE]
                                                             , na.rm = TRUE) / ni_total
                                , pi_habit_climbcling = 100 * sum(N_TAXA[HABIT_CB == TRUE
                                                                         | HABIT_CN == TRUE]
                                                                  , na.rm = TRUE) / ni_total
                                , pi_habit_cling = 100 * sum(N_TAXA[HABIT_CN == TRUE]
                                                             , na.rm = TRUE) / ni_total
                                , pi_habit_sprawl = 100 * sum(N_TAXA[HABIT_SP == TRUE]
                                                              , na.rm = TRUE) / ni_total
                                , pi_habit_swim = 100 * sum(N_TAXA[HABIT_SW == TRUE]
                                                            , na.rm = TRUE) / ni_total
                                #### pt_habit----
                                , pt_habit_burrow =     100 * nt_habit_burrow / nt_total
                                , pt_habit_climb =      100 * nt_habit_climb / nt_total
                                , pt_habit_climbcling = 100 * nt_habit_climbcling / nt_total
                                , pt_habit_cling =      100 * nt_habit_cling / nt_total
                                , pt_habit_sprawl =     100 * nt_habit_sprawl / nt_total
                                , pt_habit_swim =       100 * nt_habit_swim / nt_total
                                ## Oddball
                                # might not need habit != cling for Pleco
                                , pi_habit_cling_PlecoNoCling = 100 * sum(N_TAXA[HABIT_CN == TRUE
                                                                                 | (ORDER == "PLECOPTERA"
                                                                                    & HABIT_CN == FALSE)]
                                                                          , na.rm = TRUE) / ni_total



                                ### Life Cycle ####
                                # pi and nt for mltvol, semvol, univol
                                #### nt_LifeCycle----
                                , nt_volt_multi = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                           & LC_MULTI == TRUE]
                                                                    , na.rm = TRUE)
                                , nt_volt_semi = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                          & LC_SEMI == TRUE]
                                                                   , na.rm = TRUE)
                                , nt_volt_uni = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                         & LC_UNI == TRUE]
                                                                  , na.rm = TRUE)
                                #### pi_LifeCycle----
                                , pi_volt_multi = 100 * sum(N_TAXA[LC_MULTI == TRUE]
                                                            , na.rm = TRUE) / ni_total
                                , pi_volt_semi = 100 * sum(N_TAXA[LC_SEMI == TRUE]
                                                           , na.rm = TRUE) / ni_total
                                , pi_volt_uni = 100 * sum(N_TAXA[LC_UNI == TRUE]
                                                          , na.rm = TRUE) / ni_total
                                #### pt_LifeCycle----
                                , pt_volt_multi = 100 * nt_volt_multi / nt_total
                                , pt_volt_semi = 100 * nt_volt_semi / nt_total
                                , pt_volt_uni = 100 * nt_volt_uni / nt_total


                                ### Dominant N ####
                                ## uses previously defined values added to myDF
                                , pi_dom01 = 100 * max(N_TAXA, na.rm = TRUE) / ni_total
                                , pi_dom02 = 100 * max(ni_dom02, na.rm = TRUE) / ni_total
                                , pi_dom03 = 100 * max(ni_dom03, na.rm = TRUE) / ni_total
                                , pi_dom04 = 100 * max(ni_dom04, na.rm = TRUE) / ni_total
                                , pi_dom05 = 100 * max(ni_dom05, na.rm = TRUE) / ni_total
                                , pi_dom06 = 100 * max(ni_dom06, na.rm = TRUE) / ni_total
                                , pi_dom07 = 100 * max(ni_dom07, na.rm = TRUE) / ni_total
                                , pi_dom08 = 100 * max(ni_dom08, na.rm = TRUE) / ni_total
                                , pi_dom09 = 100 * max(ni_dom09, na.rm = TRUE) / ni_total
                                , pi_dom10 = 100 * max(ni_dom10, na.rm = TRUE) / ni_total

                                # , pi_dom01alt= dplyr::top_n(N_TAXA, n=1) / ni_total
                                #https://stackoverflow.com/questions/27766054/getting-the-top-values-by-group
                                # top_n uses ties so can't use it

                                ### Indices ####
                                #, x_AMBI - may need extra function or like "top" functions do some precalc
                                #,x_Becks.CLASS1=n_distinct(N_TAXA[EXCLUDE!=TRUE & TolVal>=0 & TolVal<=2.5])
                                #,x_Becks.CLASS2=n_distinct(N_TAXA[EXCLUDE!=TRUE & TolVal>=2.5 & TolVal<=4])
                                , x_Becks = (2 * dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                          & TOLVAL >= 0
                                                                          & TOLVAL <= 1.5]
                                                                   , na.rm = TRUE)) +
                                  (1 * dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                & TOLVAL > 1.5
                                                                & TOLVAL <= 4]
                                                         , na.rm = TRUE))
                                , x_Becks3 = (3 * dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                           & TOLVAL >= 0
                                                                           & TOLVAL <= 0.5]
                                                                    , na.rm = TRUE)) +
                                  (2 * dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                & TOLVAL > 0.5
                                                                & TOLVAL <= 1.5]
                                                         , na.rm = TRUE)) +
                                  (1 * dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                & TOLVAL > 1.5
                                                                & TOLVAL <= 2.5]
                                                         , na.rm = TRUE))

                                #,x_HBI_numer=sum(N_TAXA*TOLVAL, na.rm = TRUE)
                                #,x_HBI_denom=sum(N_TAXA[!is.na(TOLVAL) & TOLVAL>=0], na.rm = TRUE)
                                , x_HBI = sum(N_TAXA * TOLVAL, na.rm = TRUE)/sum(N_TAXA[!is.na(TOLVAL)
                                                                                        & TOLVAL >= 0]
                                                                                 , na.rm = TRUE)
                                , x_HBI2 = sum(N_TAXA * TOLVAL2, na.rm = TRUE)/sum(N_TAXA[!is.na(TOLVAL2)
                                                                                          & TOLVAL2 >= 0]
                                                                                   , na.rm = TRUE)
                                , x_NCBI = sum(N_TAXA * TOLVAL2, na.rm = TRUE)/sum(N_TAXA[!is.na(TOLVAL2)
                                                                                          & TOLVAL2 >= 0]
                                                                                   , na.rm = TRUE)
                                , x_BCICTQa = sum(TOLVAL2[EXCLUDE != TRUE], na.rm = TRUE) / nt_total

                                # Shannon-Weiner
                                #, x_Shan_Num= -sum(log(N_TAXA / ni_total)), na.rm = TRUE)
                                #, x_Shan_e=x_Shan_Num/log(exp(1))
                                , x_Shan_e = -sum((N_TAXA / ni_total) * log((N_TAXA / ni_total))
                                                  , na.rm = TRUE)
                                , x_Shan_2 = x_Shan_e/log(2)
                                , x_Shan_10 = x_Shan_e/log(10)
                                #, x_D Simpson
                                , x_D = 1 - sum((N_TAXA / ni_total)^2, na.rm = TRUE)
                                #, X_D_G (Gleason) - [nt_total]/Log([ni_total])
                                , x_D_G = (nt_total) / log(ni_total)
                                #, x_D_Mg Margalef -  ([nt_total]-1)/Log([ni_total])
                                , x_D_Mg = (nt_total - 1) / log(ni_total)
                                #, x_Hbe
                                #, x_H (Shannon)
                                # Evenness, Pielou
                                # H / Hmax  Hmax is log(nt_total)
                                , x_Evenness = x_Shan_e/log(nt_total)
                                # evenness - different from Pielou in MS Coastal Metric Calc 2011 db

                                ### Density ####
                                # Numbers per area sampled

                                ### Estuary-Marine ####
                                # Mixed in with other metrics
                                , x_Becks_tv2 = NA

                                ### Habitat ####
                                # BCG PacNW group 2020
                                # BRAC brackish
                                # DEPO depositional
                                # GENE generalist
                                # HEAD headwater
                                # RHEO rheophily
                                # RIVE riverine
                                # SPEC specialist
                                # UNKN unknown
                                #
                                #### nt_habitat----
                                , nt_habitat_brac = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                             & HABITAT_BRAC == TRUE]
                                                                      , na.rm = TRUE)
                                , nt_habitat_depo = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                             & HABITAT_DEPO == TRUE]
                                                                      , na.rm = TRUE)
                                , nt_habitat_gene = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                             & HABITAT_GENE == TRUE]
                                                                      , na.rm = TRUE)
                                , nt_habitat_head = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                             & HABITAT_HEAD == TRUE]
                                                                      , na.rm = TRUE)
                                , nt_habitat_rheo = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                             & HABITAT_RHEO == TRUE]
                                                                      , na.rm = TRUE)
                                , nt_habitat_rive = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                             & HABITAT_RIVE == TRUE]
                                                                      , na.rm = TRUE)
                                , nt_habitat_spec = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                             & HABITAT_SPEC == TRUE]
                                                                      , na.rm = TRUE)
                                , nt_habitat_unkn = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                             & HABITAT_UNKN == TRUE]
                                                                      , na.rm = TRUE)
                                #### pi_habitat----
                                , pi_habitat_brac = 100 * sum(N_TAXA[HABITAT_BRAC == TRUE]
                                                              , na.rm = TRUE) / ni_total
                                , pi_habitat_depo = 100 * sum(N_TAXA[HABITAT_DEPO == TRUE]
                                                              , na.rm = TRUE) / ni_total
                                , pi_habitat_gene = 100 * sum(N_TAXA[HABITAT_GENE == TRUE]
                                                              , na.rm = TRUE) / ni_total
                                , pi_habitat_head = 100 * sum(N_TAXA[HABITAT_HEAD == TRUE]
                                                              , na.rm = TRUE) / ni_total
                                , pi_habitat_rheo = 100 * sum(N_TAXA[HABITAT_RHEO == TRUE]
                                                              , na.rm = TRUE) / ni_total
                                , pi_habitat_rive = 100 * sum(N_TAXA[HABITAT_RIVE == TRUE]
                                                              , na.rm = TRUE) / ni_total
                                , pi_habitat_spec = 100 * sum(N_TAXA[HABITAT_SPEC == TRUE]

                                                              , na.rm = TRUE) / ni_total
                                , pi_habitat_unkn = 100 * sum(N_TAXA[HABITAT_UNKN == TRUE]
                                                              , na.rm = TRUE) / ni_total
                                #### pt_habitat----
                                , pt_habitat_brac = 100 * nt_habitat_brac / nt_total
                                , pt_habitat_depo = 100 * nt_habitat_depo / nt_total
                                , pt_habitat_gene = 100 * nt_habitat_gene / nt_total
                                , pt_habitat_head = 100 * nt_habitat_head / nt_total
                                , pt_habitat_rheo = 100 * nt_habitat_rheo / nt_total
                                , pt_habitat_rive = 100 * nt_habitat_rive / nt_total
                                , pt_habitat_spec = 100 * nt_habitat_spec / nt_total
                                , pt_habitat_unkn = 100 * nt_habitat_unkn / nt_total

                                ### BCG ####
                                # 1i, 1m, 1t
                                # Xi, Xm, Xt
                                # 5i, 5m, 5t
                                # 6i, 6m, 6t
                                # toupper(), 2022-02-22
                                #### BCG_nt----
                                , nt_BCG_att1 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                         & BCG_ATTR == "1"]
                                                                  , na.rm = TRUE)
                                , nt_BCG_att1i = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                          & BCG_ATTR == "1I"]
                                                                   , na.rm = TRUE)
                                , nt_BCG_att1m = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                          & BCG_ATTR == "1M"]
                                                                   , na.rm = TRUE)
                                , nt_BCG_att12 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                          & (BCG_ATTR == "1"
                                                                             | BCG_ATTR == "2")]
                                                                   , na.rm = TRUE)
                                , nt_BCG_att1234 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                          & (BCG_ATTR == "1"
                                                                             | BCG_ATTR == "2"
                                                                             | BCG_ATTR == "3"
                                                                             | BCG_ATTR == "4")]
                                                                   , na.rm = TRUE)
                                , nt_BCG_att1i2 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                           & (BCG_ATTR == "1I"
                                                                              | BCG_ATTR == "2")]
                                                                    , na.rm = TRUE)
                                , nt_BCG_att123 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                           &
                                                                             (BCG_ATTR == "1"
                                                                              | BCG_ATTR == "2"
                                                                              | BCG_ATTR == "3")]
                                                                    , na.rm = TRUE)
                                , nt_BCG_att1i23 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                            & (BCG_ATTR == "1I"
                                                                               | BCG_ATTR == "2"
                                                                               | BCG_ATTR == "3")]
                                                                     , na.rm = TRUE)
                                , nt_BCG_att1i236i = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                              & (BCG_ATTR == "1I"
                                                                                 | BCG_ATTR == "2"
                                                                                 | BCG_ATTR == "3"
                                                                                 | BCG_ATTR == "6I")]
                                                                       , na.rm = TRUE)
                                , nt_BCG_att2 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                         & (BCG_ATTR == "2")]
                                                                  , na.rm = TRUE)
                                , nt_BCG_att23 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                          & (BCG_ATTR == "2"
                                                                             | BCG_ATTR == "3")]
                                                                   , na.rm = TRUE)
                                , nt_BCG_att234 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                           & (BCG_ATTR == "2"
                                                                              | BCG_ATTR == "3"
                                                                              | BCG_ATTR == "4")]
                                                                    , na.rm = TRUE)
                                , nt_BCG_att3 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                         & (BCG_ATTR == "3")]
                                                                  , na.rm = TRUE)
                                , nt_BCG_att4 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                         & (BCG_ATTR == "4")]
                                                                  , na.rm = TRUE)
                                , nt_BCG_att45 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                          & (BCG_ATTR == "4"
                                                                             | BCG_ATTR == "5")]
                                                                   , na.rm = TRUE)
                                , nt_BCG_att456 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                           & (BCG_ATTR == "4"
                                                                              | BCG_ATTR == "5"
                                                                              | BCG_ATTR == "6")]
                                                                    , na.rm = TRUE)
                                , nt_BCG_att5 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                         & (BCG_ATTR == "5")]
                                                                  , na.rm = TRUE)
                                , nt_BCG_att56 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                          & (BCG_ATTR == "5"
                                                                             | BCG_ATTR == "6")]
                                                                   , na.rm = TRUE)
                                , nt_BCG_att56t = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                           & (BCG_ATTR == "5"
                                                                              | BCG_ATTR == "6T")]
                                                                    , na.rm = TRUE)
                                , nt_BCG_att6 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                         &  (BCG_ATTR == "6")]
                                                                  , na.rm = TRUE)
                                , nt_BCG_att6i = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                          &  (BCG_ATTR == "6I")]
                                                                   , na.rm = TRUE)
                                , nt_BCG_att6m = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                          &  (BCG_ATTR == "6M")]
                                                                   , na.rm = TRUE)
                                , nt_BCG_att6t = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                          &  (BCG_ATTR == "6T")]
                                                                   , na.rm = TRUE)
                                , nt_BCG_attNA = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                          &  is.na(BCG_ATTR)]
                                                                   , na.rm = TRUE)
                                , nt_BCG_att4b = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                          & (BCG_ATTR2 == "4_BETTER")]
                                                                   , na.rm = TRUE)
                                , nt_BCG_att4m = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                          & (BCG_ATTR2 == "4_MIDDLE")]
                                                                   , na.rm = TRUE)
                                , nt_BCG_att4w = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                          & (BCG_ATTR2 == "4_WORSE")]
                                                                   , na.rm = TRUE)
                                , nt_BCG_att1i234b = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                              & BCG_ATTR2 == "4_BETTER"]
                                                                       , na.rm = TRUE) +
                                  dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                           & (BCG_ATTR == "1I"
                                                              | BCG_ATTR == "2"
                                                              | BCG_ATTR == "3")]
                                                    , na.rm = TRUE)

                                , nt_BCG_att4w5 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                           & BCG_ATTR2 == "4_WORSE"]
                                                                    , na.rm = TRUE) +
                                  dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                           & (BCG_ATTR == "5")]
                                                    , na.rm = TRUE)

                                #### BCG_nt_Phylo----
                                , nt_Ephem_BCG_att1i2 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                                 & ORDER == "EPHEMEROPTERA"
                                                                                 & (BCG_ATTR == "1I"
                                                                                    | BCG_ATTR == "2")]
                                                                          , na.rm = TRUE)
                                , nt_EPT_BCG_att123 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                               & (ORDER == "EPHEMEROPTERA"
                                                                                  | ORDER == "TRICHOPTERA"
                                                                                  | ORDER == "PLECOPTERA")
                                                                               & (BCG_ATTR == "1"
                                                                                  | BCG_ATTR == "2"
                                                                                  | BCG_ATTR == "3")]
                                                                        , na.rm = TRUE)
                                , nt_EPT_BCG_att1i23 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                                & (ORDER == "EPHEMEROPTERA"
                                                                                   | ORDER == "TRICHOPTERA"
                                                                                   | ORDER == "PLECOPTERA")
                                                                                & (BCG_ATTR == "1I"
                                                                                   | BCG_ATTR == "2"
                                                                                   | BCG_ATTR == "3")]
                                                                         , na.rm = TRUE)
                                , nt_Pleco_BCG_att1i2 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                                 & ORDER == "PLECOPTERA"
                                                                                 & (BCG_ATTR == "1I"
                                                                                    | BCG_ATTR == "2")]
                                                                          , na.rm = TRUE)
                                , nt_Trich_BCG_att1i2 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                                 & ORDER == "TRICHOPTERA"
                                                                                 & (BCG_ATTR == "1I"
                                                                                    | BCG_ATTR == "2")]
                                                                          , na.rm = TRUE)


                                , nt_Coleo_BCG_att234b4m = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                            & ORDER == "COLEOPTERA"
                                                                            & (BCG_ATTR == "2"
                                                                               | BCG_ATTR == "3"
                                                                               | BCG_ATTR2 == "4_BETTER"
                                                                               | BCG_ATTR2 == "4_MIDDLE")]
                                                                              , na.rm = TRUE)

                                , nt_Ephem_BCG_att234b4m = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                             & ORDER == "EPHEMEROPTERA"
                                                                             & (BCG_ATTR == "2"
                                                                                | BCG_ATTR == "3"
                                                                                | BCG_ATTR2 == "4_BETTER"
                                                                                | BCG_ATTR2 == "4_MIDDLE")]
                                                                              , na.rm = TRUE)
                                , nt_Odon_BCG_att234b4m = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                           & ORDER == "ODONATA"
                                                                           & (BCG_ATTR == "2"
                                                                              | BCG_ATTR == "3"
                                                                              | BCG_ATTR2 == "4_BETTER"
                                                                              | BCG_ATTR2 == "4_MIDDLE")]
                                                                            , na.rm = TRUE)
                                , nt_Trich_BCG_att234b4m = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                             & ORDER == "TRICHOPTERA"
                                                                             & (BCG_ATTR == "2"
                                                                                | BCG_ATTR == "3"
                                                                                | BCG_ATTR2 == "4_BETTER"
                                                                                | BCG_ATTR2 == "4_MIDDLE")]
                                                                            , na.rm = TRUE)

                                #### BCG_pi----
                                , pi_BCG_att1 = 100 * sum(N_TAXA[(BCG_ATTR == "1")]
                                                          , na.rm = TRUE) / ni_total
                                , pi_BCG_att1i = 100 * sum(N_TAXA[(BCG_ATTR == "1I")]
                                                           , na.rm = TRUE) / ni_total
                                , pi_BCG_att1m = 100 * sum(N_TAXA[(BCG_ATTR == "1M")]
                                                           , na.rm = TRUE) / ni_total
                                , pi_BCG_att12 = 100 * sum(N_TAXA[(BCG_ATTR == "1"
                                                                   | BCG_ATTR == "2")]
                                                           , na.rm = TRUE) / ni_total
                                , pi_BCG_att1i2 = 100 * sum(N_TAXA[(BCG_ATTR == "1I"
                                                                    | BCG_ATTR == "2")]
                                                            , na.rm = TRUE) / ni_total
                                , pi_BCG_att123 = 100 * sum(N_TAXA[(BCG_ATTR == "1"
                                                                    | BCG_ATTR == "2"
                                                                    | BCG_ATTR == "3")]
                                                            , na.rm = TRUE) / ni_total
                                , pi_BCG_att1i23 = 100 * sum(N_TAXA[(BCG_ATTR == "1I"
                                                                     | BCG_ATTR == "2"
                                                                     | BCG_ATTR == "3")]
                                                             , na.rm = TRUE) / ni_total
                                , pi_BCG_att1i236i = 100 * sum(N_TAXA[(BCG_ATTR == "1I"
                                                                       | BCG_ATTR == "2"
                                                                       | BCG_ATTR == "3"
                                                                       | BCG_ATTR == "6I")]
                                                               , na.rm = TRUE) / ni_total
                                , pi_BCG_att2 = 100 * sum(N_TAXA[(BCG_ATTR == "2")]
                                                          , na.rm = TRUE) / ni_total
                                , pi_BCG_att23 = 100 * sum(N_TAXA[(BCG_ATTR == "2"
                                                                   | BCG_ATTR == "3")]
                                                           , na.rm = TRUE) / ni_total
                                , pi_BCG_att234 = 100 * sum(N_TAXA[(BCG_ATTR == "2"
                                                                    | BCG_ATTR == "3"
                                                                    | BCG_ATTR == "4")]
                                                            , na.rm = TRUE) / ni_total
                                , pi_BCG_att3 = 100 * sum(N_TAXA[(BCG_ATTR == "3")]
                                                          , na.rm = TRUE) / ni_total
                                , pi_BCG_att4 = 100 * sum(N_TAXA[(BCG_ATTR == "4")]
                                                          , na.rm = TRUE) / ni_total
                                , pi_BCG_att45 = 100 * sum(N_TAXA[(BCG_ATTR == "4"
                                                                   | BCG_ATTR == "5")]
                                                           , na.rm = TRUE) / ni_total
                                , pi_BCG_att456 = 100 * sum(N_TAXA[(BCG_ATTR == "4"
                                                                    | BCG_ATTR == "5"
                                                                    | BCG_ATTR == "6")]
                                                            , na.rm = TRUE) / ni_total
                                , pi_BCG_att5 = 100 * sum(N_TAXA[(BCG_ATTR == "5")]
                                                          , na.rm = TRUE) / ni_total
                                , pi_BCG_att5extra = 100 * sum(N_TAXA[(BCG_ATTR == "5"
                                                                       | BCG_ATTR == "5.5")]
                                                               , na.rm = TRUE) / ni_total
                                , pi_BCG_att56 = 100 * sum(N_TAXA[(BCG_ATTR == "5"
                                                                   | BCG_ATTR == "6")]
                                                           , na.rm = TRUE) / ni_total
                                , pi_BCG_att56t = 100 * sum(N_TAXA[(BCG_ATTR == "5"
                                                                    | BCG_ATTR == "6T")]
                                                            , na.rm = TRUE) / ni_total
                                , pi_BCG_att6 = 100 * sum(N_TAXA[(BCG_ATTR == "6")]
                                                          , na.rm = TRUE) / ni_total
                                , pi_BCG_att6i = 100 * sum(N_TAXA[(BCG_ATTR == "6I")]
                                                           , na.rm = TRUE) / ni_total
                                , pi_BCG_att6m = 100 * sum(N_TAXA[(BCG_ATTR == "6M")]
                                                           , na.rm = TRUE) / ni_total
                                , pi_BCG_att6t = 100 * sum(N_TAXA[(BCG_ATTR == "6T")]
                                                           , na.rm = TRUE) / ni_total
                                , pi_BCG_attNA = 100 * sum(N_TAXA[(is.na(BCG_ATTR) == TRUE)]
                                                           , na.rm = TRUE) / ni_total
                                , pi_BCG_att4b = 100 * sum(N_TAXA[(BCG_ATTR2 == "4_BETTER")]
                                                           , na.rm = TRUE) / ni_total
                                , pi_BCG_att4m = 100 * sum(N_TAXA[(BCG_ATTR2 == "4_MIDDLE")]
                                                           , na.rm = TRUE) / ni_total
                                , pi_BCG_att4w = 100 * sum(N_TAXA[(BCG_ATTR2 == "4_WORSE")]
                                                           , na.rm = TRUE) / ni_total
                                , pi_BCG_att1i234b = 100 * (sum(N_TAXA[BCG_ATTR2 == "4_BETTER"]
                                                                , na.rm = TRUE) +
                                                              sum(N_TAXA[BCG_ATTR == "1I"
                                                                         | BCG_ATTR == "2"
                                                                         | BCG_ATTR == "3"]
                                                                  , na.rm = TRUE)) / ni_total
                                , pi_BCG_att4w5 = 100 * (sum(N_TAXA[BCG_ATTR2 == "4_WORSE"]
                                                             , na.rm = TRUE) +
                                                           sum(N_TAXA[BCG_ATTR == "5"]
                                                               , na.rm = TRUE)) / ni_total

                                #### BCG_pi_Phylo----
                                , pi_EPT_BCG_att123 = 100 * sum(N_TAXA[(ORDER == "EPHEMEROPTERA"
                                                                        | ORDER == "TRICHOPTERA"
                                                                        | ORDER == "PLECOPTERA")
                                                                       & (BCG_ATTR == "1"
                                                                          | BCG_ATTR == "2"
                                                                          | BCG_ATTR == "3")]
                                                                , na.rm = TRUE) / ni_total
                                , pi_EPT_BCG_att1i23 = 100 * sum(N_TAXA[(ORDER == "EPHEMEROPTERA"
                                                                         | ORDER == "TRICHOPTERA"
                                                                         | ORDER == "PLECOPTERA")
                                                                        & (BCG_ATTR == "1I"
                                                                           | BCG_ATTR == "2"
                                                                           | BCG_ATTR == "3")]
                                                                 , na.rm = TRUE) / ni_total

                                #### BCG_pt----
                                , pt_BCG_att1 =    100 * nt_BCG_att1 / nt_total
                                , pt_BCG_att1i =   100 * nt_BCG_att1i / nt_total
                                , pt_BCG_att1m =   100 * nt_BCG_att1m / nt_total
                                , pt_BCG_att12 =   100 * nt_BCG_att12 / nt_total
                                , pt_BCG_att1234 = 100 * nt_BCG_att1234 / nt_total
                                , pt_BCG_att1i2 =  100 * nt_BCG_att1i2 / nt_total
                                , pt_BCG_att123 =  100 * nt_BCG_att123 / nt_total
                                , pt_BCG_att1i23 = 100 * nt_BCG_att1i23 / nt_total
                                , pt_BCG_att1i236i = 100 * nt_BCG_att1i236i / nt_total
                                , pt_BCG_att2 =   100 * nt_BCG_att2 / nt_total
                                , pt_BCG_att23 =  100 * nt_BCG_att23 / nt_total
                                , pt_BCG_att234 = 100 * nt_BCG_att234 / nt_total
                                , pt_BCG_att3 =   100 * nt_BCG_att3 / nt_total
                                , pt_BCG_att4 =   100 * nt_BCG_att4 / nt_total
                                , pt_BCG_att45 =  100 * nt_BCG_att45 / nt_total
                                , pt_BCG_att456 = 100 * nt_BCG_att456 / nt_total
                                , pt_BCG_att5 =   100 * nt_BCG_att5 / nt_total
                                , pt_BCG_att56 =  100 * nt_BCG_att56 / nt_total
                                , pt_BCG_att56t = 100 * nt_BCG_att56t / nt_total
                                , pt_BCG_att6 =   100 * nt_BCG_att6 / nt_total
                                , pt_BCG_att6i =  100 * nt_BCG_att6i / nt_total
                                , pt_BCG_att6m =  100 * nt_BCG_att6m / nt_total
                                , pt_BCG_att6t =  100 * nt_BCG_att6t / nt_total
                                , pt_BCG_attNA =  100 * nt_BCG_attNA / nt_total
                                , pt_BCG_att4b =  100 * nt_BCG_att4b / nt_total
                                , pt_BCG_att4m =  100 * nt_BCG_att4m / nt_total
                                , pt_BCG_att4w =  100 * nt_BCG_att4w / nt_total
                                , pt_BCG_att1i234b = 100 * nt_BCG_att1i234b / nt_total
                                , pt_BCG_att4w5 = 100 * nt_BCG_att4w5 / nt_total

                                #### BCG_special ----
                                # BCG_pt_Phylo
                                , pt_EPT_BCG_att123 =  100 * nt_EPT_BCG_att123 / nt_total
                                , pt_EPT_BCG_att1i23 = 100 * nt_EPT_BCG_att1i23 / nt_total

                                #### BCG_pi_dom ----
                                , pi_dom01_BCG_att4 = 100 * max(ni_dom01_BCG_att4, na.rm = TRUE) / ni_total
                                , pi_dom01_BCG_att5 = 100 * max(ni_dom01_BCG_att5, na.rm = TRUE) / ni_total

                                # domX_BCG
                                # pi_dom01_att 4, 5, 56
                                # pi_dom05_att 123, not 456

                                ### UFC ----
                                #Taxonomic Uncertainty Frequency Class (use HBI calculation)
                                , x_UFC = sum(N_TAXA * UFC, na.rm = TRUE) / sum(N_TAXA[!is.na(UFC)
                                                                                       & UFC >= 1
                                                                                       & UFC <= 6]
                                                                                , na.rm = TRUE)
                                ### Elevation ----
                                , nt_elev_low = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                         & ELEVATION_LOW == TRUE]
                                                                  , na.rm = TRUE)
                                , nt_elev_high = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                          & ELEVATION_HIGH == TRUE]
                                                                   , na.rm = TRUE)

                                ### Gradient ----
                                , nt_grad_low = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                         & GRADIENT_LOW == TRUE]
                                                                  , na.rm = TRUE)
                                , nt_grad_mod = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                         & GRADIENT_MOD == TRUE]
                                                                  , na.rm = TRUE)
                                , nt_grad_high = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                          & GRADIENT_HIGH == TRUE]
                                                                   , na.rm = TRUE)

                                ### WS_Area ----
                                , nt_wsarea_small = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                             & WSAREA_S == TRUE]
                                                                      , na.rm = TRUE)
                                , nt_wsarea_medium = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                              & WSAREA_M == TRUE]
                                                                       , na.rm = TRUE)
                                , nt_wsarea_large = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                             & WSAREA_L == TRUE]
                                                                      , na.rm = TRUE)
                                , nt_wsarea_xlarge = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                              & WSAREA_XL == TRUE]
                                                                       , na.rm = TRUE)

                                ### Habitat Structure ----
                                #### nt_habstruct----
                                , nt_habstruct_coarsesub = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                                    & HS_CS == TRUE]
                                                                             , na.rm = TRUE)
                                , nt_habstruct_noflow = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                                 & HS_NF == TRUE]
                                                                          , na.rm = TRUE)
                                , nt_habstruct_rootmat = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                                  & HS_RM == TRUE]
                                                                           , na.rm = TRUE)
                                , nt_habstruct_snag = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                               & HS_SG == TRUE]
                                                                        , na.rm = TRUE)
                                , nt_habstruct_NA = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                             &  is.na(HABSTRUCT)]
                                                                      , na.rm = TRUE)
                                #### pi_habstruct----
                                , pi_habstruct_coarsesub = 100 * sum(N_TAXA[HS_CS == TRUE]
                                                                     , na.rm = TRUE) / ni_total
                                , pi_habstruct_noflow = 100 * sum(N_TAXA[HS_NF == TRUE]
                                                                  , na.rm = TRUE) / ni_total
                                , pi_habstruct_rootmat = 100 * sum(N_TAXA[HS_RM == TRUE]
                                                                   , na.rm = TRUE) / ni_total
                                , pi_habstruct_snag = 100 * sum(N_TAXA[HS_SG == TRUE]
                                                                , na.rm = TRUE) / ni_total
                                , pi_habstruct_NA = 100 * sum(N_TAXA[(is.na(HABSTRUCT) == TRUE)]
                                                              , na.rm = TRUE) / ni_total
                                #### pt_habstruct----
                                , pt_habstruct_coarsesub = 100 * nt_habstruct_coarsesub / nt_total
                                , pt_habstruct_noflow = 100 * nt_habstruct_noflow / nt_total
                                , pt_habstruct_rootmat = 100 * nt_habstruct_rootmat / nt_total
                                , pt_habstruct_snag = 100 * nt_habstruct_snag / nt_total
                                , pt_habstruct_NA = 100 * nt_habstruct_NA / nt_total
                                ### nval_habstruct
                                , nval_habstruct = sum(HS_CS + HS_NF + HS_RM + HS_SG, na.rm = TRUE)

                                ### Number Group within Group ----
                                #### nord_Order----
                                , nord_COET = dplyr::n_distinct(ORDER[ORDER == "COLEOPTERA"
                                                                      | ORDER == "ODONATA"
                                                                      | ORDER == "EPHEMEROPTERA"
                                                                      | ORDER == "TRICHOPTERA"]
                                                                , na.rm = TRUE)

                                #### nfam_Order----
                                , nfam_Coleo = dplyr::n_distinct(FAMILY[ORDER == "COLEOPTERA"]
                                                                 , na.rm = TRUE)
                                , nfam_Ephem = dplyr::n_distinct(FAMILY[ORDER == "EPHEMEROPTERA"]
                                                                 , na.rm = TRUE)
                                , nfam_Odon  = dplyr::n_distinct(FAMILY[ORDER == "ODONATA"]
                                                                 , na.rm = TRUE)
                                , nfam_Trich = dplyr::n_distinct(FAMILY[ORDER == "TRICHOPTERA"]
                                                                 , na.rm = TRUE)

                                #### ngen_Order----
                                , ngen_Coleo = dplyr::n_distinct(GENUS[ORDER == "COLEOPTERA"]
                                                                 , na.rm = TRUE)
                                , ngen_Ephem = dplyr::n_distinct(GENUS[ORDER == "EPHEMEROPTERA"]
                                                                 , na.rm = TRUE)
                                , ngen_Odon  = dplyr::n_distinct(GENUS[ORDER == "ODONATA"]
                                                                 , na.rm = TRUE)
                                , ngen_Trich = dplyr::n_distinct(GENUS[ORDER == "TRICHOPTERA"]
                                                                 , na.rm = TRUE)
                                #### ngen_Family----
                                , ngen_Elmid = dplyr::n_distinct(GENUS[FAMILY == "ELMIDAE"]
                                                                 , na.rm = TRUE)

                                ### SPECIAL ####
                                # oddball or specialized metrics
                                # , ni_NonIns = sum(N_TAXA[CLASS==NA | CLASS!="INSECTA"], na.rm = TRUE)
                                # , ni_NonArach = sum(N_TAXA[CLASS==NA | CLASS!="ARACHNIDA"], na.rm = TRUE)
                                # , ni_NonDeca = sum(N_TAXA[ORDER==NA | ORDER!="DECAPODA"], na.rm = TRUE)
                                #
                                # , ni_clumpy = sum(N_TAXA[GENUS=="JUGA" & GENUS=="Rissoidea"], na.rm = TRUE)
                                # , ni_Nonclumpy = sum(N_TAXA[GENUS!="JUGA" & GENUS!="Rissoidea"], na.rm = TRUE)
                                #
                                # PacNW, NonIns_select
                                #This metric excludes Class INSECTA, Class ARACHNIDA and Order DECAPODA;
                                # and only includes Attribute IV, V, VI taxa.
                                , nt_NonInsArachDeca_BCG_att456 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                                           & (is.na(CLASS) == TRUE
                                                                                              | (CLASS != "INSECTA"
                                                                                                 & CLASS != "ARACHNIDA"))
                                                                                           & (is.na(ORDER) == TRUE
                                                                                              | ORDER != "DECAPODA")
                                                                                           & (BCG_ATTR == "4"
                                                                                              | BCG_ATTR == "5"
                                                                                              | BCG_ATTR == "6")]
                                                                                    , na.rm = TRUE)
                                , pi_NonInsArachDeca_BCG_att456 = 100 * sum(N_TAXA[
                                  (is.na(CLASS) == TRUE
                                   | (CLASS != "INSECTA"
                                      & CLASS != "ARACHNIDA"))
                                  & (is.na(ORDER) == TRUE
                                     | ORDER != "DECAPODA")
                                  & (BCG_ATTR == "4"
                                     | BCG_ATTR == "5"
                                     | BCG_ATTR == "6")]
                                  , na.rm = TRUE) / ni_total
                                , pt_NonInsArachDeca_BCG_att456 = 100 * nt_NonInsArachDeca_BCG_att456 / nt_total
                                # PacNW, NonIns_select_NonClump
                                # above but also non-clumpy
                                #clumpy' taxa (Juga [genus] and RISSOOIDEA [superfamily as Order] in PacNW);
                                #and it only includes Attribute IV, V, VI taxa.
                                , nt_NonInsArachDecaJugaRiss_BCG_att456 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                                                   & (is.na(CLASS) == TRUE
                                                                                                      | (CLASS != "INSECTA"
                                                                                                         & CLASS != "ARACHNIDA"))
                                                                                                   & (is.na(ORDER) == TRUE
                                                                                                      | (ORDER != "DECAPODA"
                                                                                                         & ORDER != "RISSOOIDEA"))
                                                                                                   & (is.na(GENUS) == TRUE
                                                                                                      | GENUS != "JUGA")
                                                                                                   & (BCG_ATTR == "4"
                                                                                                      | BCG_ATTR == "5"
                                                                                                      | BCG_ATTR == "6")]
                                                                                            , na.rm = TRUE)
                                , pi_NonInsArachDecaJugaRiss_BCG_att456 = 100 * sum(N_TAXA[
                                  (is.na(CLASS) == TRUE
                                   | (CLASS != "INSECTA"
                                      & CLASS != "ARACHNIDA"))
                                  & (is.na(ORDER) == TRUE
                                     | (ORDER != "DECAPODA"
                                        & ORDER != "RISSOOIDEA"))
                                  & (is.na(GENUS) == TRUE
                                     | GENUS != "JUGA")
                                  & (BCG_ATTR == "4"
                                     | BCG_ATTR == "5"
                                     | BCG_ATTR == "6")]
                                  , na.rm = TRUE) / ni_total
                                , pt_NonInsArachDecaJugaRiss_BCG_att456 = 100 * nt_NonInsArachDecaJugaRiss_BCG_att456 / nt_total

                                , nt_NonInsTrombJuga_BCG_att456 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                                           & (is.na(CLASS) == TRUE
                                                                                              | CLASS != "INSECTA")
                                                                                           & (is.na(ORDER) == TRUE
                                                                                              | ORDER != "TROMBIDIFORMES")
                                                                                           & (is.na(GENUS) == TRUE
                                                                                              | GENUS != "JUGA")
                                                                                           & (BCG_ATTR == "4"
                                                                                              | BCG_ATTR == "5"
                                                                                              | BCG_ATTR == "6")]
                                                                                    , na.rm = TRUE)
                                , pi_NonInsTrombJuga_BCG_att456 = 100 * sum(N_TAXA[
                                  (is.na(CLASS) == TRUE
                                   | CLASS != "INSECTA")
                                  & (is.na(ORDER) == TRUE
                                     | ORDER != "TROMBIDIFORMES")
                                  & (is.na(GENUS) == TRUE
                                     | GENUS != "JUGA")
                                  & (BCG_ATTR == "4"
                                     | BCG_ATTR == "5"
                                     | BCG_ATTR == "6")]
                                  , na.rm = TRUE) / ni_total
                                , pt_NonInsTrombJuga_BCG_att456 = 100 * nt_NonInsTrombJuga_BCG_att456 / nt_total

                                , nt_oneind = dplyr::n_distinct(TAXAID[N_TAXA == 1
                                                                       & EXCLUDE != TRUE]
                                                                , na.rm = TRUE)
                                , pt_oneind = 100 * nt_oneind / nt_total

                                , nt_airbreath = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                          & AIRBREATHER == TRUE]
                                                                   , na.rm = TRUE)
                                , pi_airbreath = 100 * sum(N_TAXA[AIRBREATHER == TRUE]
                                                           , na.rm = TRUE) / ni_total
                                , pt_airbreath = 100 * nt_airbreath / nt_total
                                # NM BCG
                                , ni_total_300 = 100 * (ni_total / 300)
                                , nt_Mol_Non_BCG_att6 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                            & PHYLUM == "MOLLUSCA"
                                                                            & (is.na(BCG_ATTR)
                                                                               | BCG_ATTR != "6")]
                                                                     , na.rm = TRUE)

                                #
                                , .groups = "drop_last")## met.val.dni_F


    ##met.val, DNI = TRUE----
    met.val.dni_T <- dplyr::summarise(dplyr::group_by(myDF
                                                      , SAMPLEID
                                                      , INDEX_NAME
                                                      , INDEX_CLASS)
                                      #
                                      # one metric per line
                                      #
                                      ### totals----
                                      , ni_total = sum(N_TAXA, na.rm = TRUE)
                                      , nt_total = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                            & N_TAXA > 0], na.rm = TRUE)

                                      ### DNI----
                                      , nt_dni = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                          & TAXAID == "DNI"]
                                                                   , na.rm = TRUE)
                                      , pi_dni = 100 * sum(N_TAXA[TAXAID == "DNI"]
                                                           , na.rm = TRUE) / ni_total
                                      , pt_dni = 100 * nt_dni / nt_total
                                      , .groups = "drop_last")## met.val.dni_T

    ## met.val, join----
    cols2match <- c("SAMPLEID", "INDEX_NAME", "INDEX_CLASS")
    met_dni <- c("nt_dni", "pi_dni", "pt_dni")
    met.val <- dplyr::left_join(met.val.dni_F
                                , met.val.dni_T[, c(cols2match, met_dni)])


  }## IF ~ metric_subset


  time_end2 <- Sys.time()
  # difftime(time_end2, time_start2)
  # dim(met.val)

  #
  # Clean Up ####
  if (verbose == TRUE) {
    debug_topic <- "clean up"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
  }## IF ~ verbose
  # replace NA with 0
  #met.val[is.na(met.val)] <- 0
  # but exclude SAMPLEID,  INDEX_NAME  INDEX_CLASS
  # met.val <- met.val %>% dplyr::mutate(dplyr::across(where(is.numeric)
  #                                            , tidyr::replace_na
  #                                            , 0))
  met.val <- as.data.frame(met.val)
  met.val <- met.val %>% dplyr::mutate_if(is.numeric, tidyr::replace_na, 0)
  # Crazy slow on tibble (several minutes) convert to data frame (< 2 seconds)

 # met.val <- replace(is.na(met.val), 0)

  # Marine Metrics
  MetricNames_Marine <- c("nt_Capit"
                          , "nt_Caridea"
                          , "nt_Nereid"
                          , "nt_Nudib"
                          , "nt_Poly"
                          , "nt_PolyNoSpion"
                          , "nt_Spion"
                          , "pi_Amp"
                          , "pi_AmpHaust"
                          , "pi_Capit"
                          , "pi_Cirra"
                          , "pi_Clite"
                          , "pi_Haust"
                          , "pi_Hesion"
                          , "pi_Lucin"
                          , "pi_LucinTellin"
                          , "pi_Nereid"
                          , "pi_Nudib"
                          , "pi_Orbin"
                          , "pi_Poly"
                          , "pi_Spion"
                          , "pi_Spion2Poly"
                          , "pi_Tellin"
                          , "pi_Xanth"
                          , "pt_Nudib"
                          , "pt_Poly"
                          , "pt_PolyNoSpion"
                          , "pt_Spion"
                          , "rt_Amph"
                          , "rt_Bivalve"
                          , "rt_Capit"
                          , "rt_Car"
                          , "rt_Coleo"
                          , "rt_CruMol"
                          , "rt_Deca"
                          , "rt_Ephem"
                          , "rt_EPT"
                          , "rt_Gast"
                          , "rt_Isop"
                          , "rt_Nereid"
                          , "rt_Nudib"
                          , "rt_Oligo"
                          , "rt_Pleco"
                          , "rt_Poly"
                          , "rt_PolyNoSpion"
                          , "rt_Ptero"
                          , "rt_Spion"
                          , "rt_Trich"
                          , "rt_Tubif"
                          , "x_Becks_tv2"
                          )

  ## Subset ----
  # # subset to only metrics specified by user
  if (verbose == TRUE) {
    debug_topic <- "subset"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
  }## IF ~ verbose
  if (is.null(MetricNames)) {
    # remove marine if MetrcNames not provided and boo.marine = false (default)
    if (boo.marine == FALSE) {
      met.val <- met.val[, !(names(met.val) %in% MetricNames_Marine)]
    }## IF ~ boo.marine ~ END
  } else {
    met2include <- MetricNames[!(MetricNames %in% "ni_total")]
    # remove ni_total if included as will always include it
    met.val <- met.val[, c("SAMPLEID", "INDEX_CLASS", "INDEX_NAME",
                           "ni_total", met2include)]
  }##IF~MetricNames~END

  # Add extra fields
  if (verbose == TRUE) {
    debug_topic <- "extra fields"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
  }## IF ~ verbose
  if (is.null(cols2keep)) {##IF.is.null.cols2keep.START
    df.return <- as.data.frame(met.val)
  } else {
    # create df with grouped fields
    myDF.cols2keep <- myDF %>%
      dplyr::group_by(.dots = c("SAMPLEID", cols2keep)) %>%
      dplyr::summarize(col.drop = sum(N_TAXA))
    col.drop <- ncol(myDF.cols2keep)
    myDF.cols2keep <- myDF.cols2keep[,-col.drop]
    # merge
    df.return <- merge(as.data.frame(myDF.cols2keep)
                       , as.data.frame(met.val)
                       , by = "SAMPLEID")
  }##IF.is.null.cols2keep.END

  # df to report back
  if (verbose == TRUE) {
    debug_topic <- "return result"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
  }## IF ~ verbose
  return(df.return)
}##FUNCTION.metric.values.bugs.END
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Calculate metric values, Fish
#'
#' @description Subfunction of metric.values for use with Fish.
#'
#' @details For internal use only.  Called from metric.values().
#'
#' @param myDF Data frame of taxa.
#' @param MetricNames Optional vector of metric names to be returned.
#' @param boo.Adjust Optional boolean value on whether to perform adjustments of
#' values prior to scoring.  Default = FALSE but may be TRUE for certain
#' metrics.
#' @param cols2keep Column names of fun.DF to retain in the output.  Uses
#' column names.
#' @param boo.Shiny Boolean value for if the function is accessed via Shiny.
#' Default = FALSE.
#' @param verbose Include messages to track progress.  Default = FALSE
#'
#' @return Data frame
#'
#' @keywords internal
#'
#' @export
metric.values.fish <- function(myDF
                               , MetricNames = NULL
                               , boo.Adjust = FALSE
                               , cols2keep = NULL
                               , boo.Shiny
                               , verbose) {

  # QC
  boo_QC <- FALSE
  if (boo_QC) {
    myDF <- BioMonTools::data_fish_MBSS
    MetricNames <- NULL
    boo.Adjust <- FALSE
    cols2keep <- NULL
    boo.Shiny <- FALSE
    verbose <- TRUE
  }## IF ~ boo_QC

  time_start <- Sys.time()

  # not carrying over from previous?!
  names(myDF) <- toupper(names(myDF))

  debug_sub_community <- "FISH"
  boo_debug_sub <- FALSE
  debug_sub_num <- 0
  debug_sub_num_total <- 10

  # global variable bindings ----
  SAMPLEID <- INDEX_NAME <- INDEX_CLASS <- TAXAID <- N_TAXA <- NATIVE <-
    HYBRID <- TYPE <- TROPHIC <- SILT <- TOLER <- N_ANOMALIES <- GENUS <-
    FAMILY <- SAMP_WIDTH_M <- SAMP_LENGTH_M <- NULL
  TROPHIC_GE <- TROPHIC_HB <- TROPHIC_IS <- TROPHIC_IV <- TROPHIC_OM <-
    TROPHIC_TC <- NULL
  ni_total <- x_Shan_e <- nt_total <- x_Evenness <- length_m <-
    ni_natnonhybridnonmf <- ni_natnonhybridnonmfnonlepomis <- NULL
  BCG_ATTR <- NULL
  CONNECTIVITY <- SCC <- nt_AmmEthPerc <- nt_AmmEthPerc_Cott_Notur <-
    nt_Cato <- nt_Cent <- nt_natCent <- nt_Cott <- nt_Cyprin <- nt_Ictal <-
    nt_Lepomis <- nt_native <- nt_nonnative <- nt_Notur <- nt_Salm <-
    nt_connect <- nt_scc <- TROPHIC_DE <- TROPHIC_PL <- nt_detritivore <-
    nt_herbivore <- nt_omnivore <- nt_planktivore <- nt_topcarn <- area_m2 <-
    ni_natnonhybridnonmfnonLepomis <- SAMP_BIOMASS <- ni_dom02 <- ni_dom03 <-
    ni_dom04 <- ni_dom05 <- ni_dom06 <- ni_dom07 <- ni_dom08 <- ni_dom09 <-
    ni_dom10 <- nt_BCG_att12 <- nt_BCG_att123 <- nt_BCG_att12346b <-
    nt_BCG_att1236b <- nt_BCG_att2 <- nt_BCG_att2native <- nt_BCG_att23_scc <-
    nt_BCG_att3 <- nt_BCG_att3native <- nt_BCG_att4 <- nt_BCG_att4native <-
    nt_BCG_att5 <- nt_BCG_att5native <- nt_BCG_attNA <- TI_CORECOLD <-
    TI_COLD <- TI_COOL <- TI_WARM <- TI_EURY <- TI_NA <- nt_ti_corecold <-
    nt_ti_cold <- nt_ti_cool <- nt_ti_warm <- nt_ti_eury <- nt_ti_na <-
    nt_ti_corecold_cold <- nt_ti_cool_warm <- ELEVATION_LOW <- ELEVATION_HIGH <-
    GRADIENT_LOW <- GRADIENT_MOD <- GRADIENT_HIGH <- WSAREA_S <- WSAREA_M <-
    WSAREA_L <- WSAREA_XL <- REPRO_BCAST <- REPRO_NS <- REPRO_NC <-
    REPRO_BEAR <- REPRO_MIG <- HABITAT_B <- HABITAT_W <- nt_habitat_b <-
    nt_habitat_w <- NULL

  # define pipe
  `%>%` <- dplyr::`%>%`

  # QC ####

  # Remove Non-Target Taxa
  #myDF <- myDF[myDF[,"NonTarget"]==0,] # not relevant for fish

  ## QC, Missing Cols ----
  if (verbose == TRUE) {
    # 1
    debug_topic <- "QC, required cols"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
  }## IF ~ verbose

  # QC, Required Fields
  col.req_character <- c("SAMPLEID", "TAXAID", "INDEX_NAME", "INDEX_CLASS"
                         , "FAMILY", "GENUS"
                         , "TYPE", "TOLER", "NATIVE", "TROPHIC", "SILT"
                         , "BCG_ATTR", "THERMAL_INDICATOR"
                         , "HABITAT", "ELEVATION_ATTR"
                         , "GRADIENT_ATTR", "WSAREA_ATTR"
                         , "REPRODUCTION", "HABITAT", "CONNECTIVITY", "SCC")
  col.req_logical <- c("EXCLUDE", "HYBRID")
  col.req_numeric <- c("N_TAXA", "N_ANOMALIES",  "SAMP_BIOMASS", "DA_MI2"
                       , "SAMP_WIDTH_M", "SAMP_LENGTH_M"
                       )
  col.req <- c(col.req_character, col.req_logical, col.req_numeric)
  # col.req <- c("SAMPLEID", "TAXAID", "N_TAXA", "EXCLUDE"
  #              , "N_ANOMALIES", "SAMP_BIOMASS"
  #              , "INDEX_NAME", "INDEX_CLASS"
  #              , "DA_MI2", "SAMP_WIDTH_M", "SAMP_LENGTH_M"
  #              , "TYPE", "TOLER", "NATIVE", "TROPHIC", "SILT"
  #              , "FAMILY", "GENUS", "HYBRID", "BCG_ATTR", "THERMAL_INDICATOR"
  #              , "ELEVATION_ATTR", "GRADIENT_ATTR", "WSAREA_ATTR"
  #              , "REPRODUCTION", "HABITAT", "CONNECTIVITY", "SCC"
  #              )
  col.req.missing <- col.req[!(col.req %in% toupper(names(myDF)))]
  num.col.req.missing <- length(col.req.missing)
  # Trigger prompt if any missing fields (and session is interactive)
  if (num.col.req.missing != 0) {##IF.num.col.req.missing.START
    myPrompt.01 <- paste0("There are ",num.col.req.missing," missing fields in the data:")
    myPrompt.02 <- paste(col.req.missing, collapse = ", ")
    myPrompt.03 <- "If you continue the metrics associated with these fields will be invalid."
    myPrompt.04 <- "For example, if the NATIVE field is missing all native related metrics will not be correct."
    myPrompt.05 <- "Do you wish to continue (YES or NO)?"

    myPrompt <- paste(" ", myPrompt.01, myPrompt.02, " ", myPrompt.03, myPrompt.04
                      , myPrompt.05, sep = "\n")
    #user.input <- readline(prompt=myPrompt)
    user.input <- NA

    if (interactive() == TRUE & boo.Shiny == FALSE) {
      user.input <- utils::menu(c("YES", "NO"), title = myPrompt)
    } else {
      message(myPrompt)
      message("boo.Shiny == TRUE and interactive == FALSE
              so prompt skipped and value set to '1'.")
      user.input <- 1
    }## IF ~ interactive and boo.Shiny

    # any answer other than "YES" will stop the function.
    if (user.input != 1) {##IF.user.input.START
      stop(paste("The user chose *not* to continue due to missing fields: "
                 , paste(paste0("   ", col.req.missing), collapse = "\n"), sep = "\n"))
    }##IF.user.input.END

    # Add missing fields
    myDF[, col.req.missing] <- NA
    warning(paste("Metrics related to the following fields are invalid:"
                  , paste(paste0("   ", col.req.missing), collapse = "\n"), sep = "\n"))
  }##IF.num.col.req.missing.END

  ## QC, Cols2Keep ----
  # remove duplicates with required so no errors, e.g., SAMPLEID
  cols2keep <- cols2keep[!cols2keep %in% col.req]

  ## QC, Exclude----
  # ensure TRUE/FALSE
  if (verbose == TRUE) {
    debug_topic <- "QC, cols, values, Exclude"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
    myCol <- "EXCLUDE"
    col_TF <- myCol %in% names(myDF)
    msg <- paste0("Column (", myCol, ") exists; ", col_TF)
    message(msg)
  }## IF ~ verbose
  Exclude.T <- sum(myDF$EXCLUDE == TRUE, na.rm = TRUE)
  if (Exclude.T == 0) {
    warn1 <- "EXCLUDE column does not have any TRUE values."
    warn2 <- "This is common with fish samples."
    warn3 <- "Valid values are TRUE or FALSE."
    warn4 <- "Other values are not recognized"
    msg <- paste(warn1, warn2, warn3, warn4, sep = "\n")
    message(msg)
  }##IF.Exclude.T.END

  ## QC, BCG_Attr ----
  # need as character, if complex all values fail
  if (verbose == TRUE) {
    debug_topic <- "QC, cols, complex, BCG_Attr"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
    myCol <- "BCG_ATTR"
    col_TF <- myCol %in% names(myDF)
    msg <- paste0("Column (", myCol, ") exists; ", col_TF)
    message(msg)
  }## IF ~ verbose

  BCG_Complex <- is.complex(myDF[, "BCG_ATTR"])
  # only tigger if have a complex field
  if (BCG_Complex == TRUE) {
    if (interactive() & boo.Shiny == FALSE) {
      msg <- "**BCG_ATTR is complex!**"
      msg2 <- "BCG metrics will not calculate properly."
      msg3 <- "Reimport data with column class defined."
      msg4 <- "Use either Fix1 or Fix2.  Replace 'foo.csv' with your file."
      msg5 <- ""
      msg6 <- "# Fix 1, base R"
      msg7 <- "df_data <- read.csv('foo.csv', colClass=c('BCG_Attr'='character'))"
      msg8 <- ""
      msg9 <- "# Fix 2, tidyverse"
      msg10 <- "# install package if needed and load it"
      msg11 <- "if(!require(readr)) {install.packages('readr')}"
      msg12 <- "# import file and convert from tibble to data frame"
      msg13 <- "df_data <- as.data.frame(read_csv('foo.csv'))"
      msg14 <- ""
      #
      message(paste(msg, msg2, msg3, msg4, msg5, msg6, msg7, msg8, msg9, msg10
                    , msg11, msg12, msg13, msg14, sep = "\n"))
    }## IF ~ interactive & boo.Shiny == FALSE

    if (interactive() == FALSE | boo.Shiny == TRUE) {
      # > df$BCG_Attr_char <- as.character(df$BCG_Attr)
      # > df$BCG_Attr_char <- sub("^0\\+", "", df$BCG_Attr_char)
      # > df$BCG_Attr_char <- sub("\\+0i$", "", df$BCG_Attr_char)
      # > table(df$BCG_Attr, df$BCG_Attr_char)
      myDF[, "BCG_ATTR"] <- as.character(myDF[, "BCG_ATTR"])
      myDF[, "BCG_ATTR"] <- sub("^0\\+", "", myDF[, "BCG_ATTR"])
      myDF[, "BCG_ATTR"] <- sub("\\+0i$", "", myDF[, "BCG_ATTR"])
    }## IF ~ interactive() == FALSE | boo.Shiny == TRUE

  }##IF ~ BCG_Attr ~ END

  # Data Munging ----

  if (verbose == TRUE) {
    # 2
    debug_topic <- "Munge, values to upper"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
  }## IF ~ verbose

  # Column Values to UPPER case for met.val below
  col2upper <- c("TAXAID" ,"FAMILY", "GENUS", "TYPE", "TOLER", "NATIVE"
                 , "TROPHIC", "THERMAL_INDICATOR", "ELEVATION_ATTR"
                 , "GRADIENT_ATTR", "WSAREA_ATTR", "REPRODUCTION", "HABITAT"
                 , "CONNECTIVITY", "SCC", "BCG_ATTR")
  for (i in col2upper) {
    if (i %in% names(myDF)) {
      myDF[, i] <- toupper(myDF[, i])
    }## IF ~ i %in%
  }##FOR ~ i col2upper


  # Add extra columns for some fields

  if (verbose == TRUE) {
    # 4
    debug_topic <- "Munge, TF"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
  }## IF ~ verbose

  # (need unique values for functions in summarise)
  # each will be TRUE or FALSE
  # finds any match so "GE, IV" is both "GE" and "IV"

  if (!"TROPHIC" %in% names(myDF)) {
    myDF[, "TROPHIC"] <- NA
  }## IF ~ TROPHIC
  myDF[, "TROPHIC_GE"] <- grepl("GE", myDF[, "TROPHIC"]) # Generalist
  myDF[, "TROPHIC_HB"] <- grepl("HB", myDF[, "TROPHIC"]) # Herbivore
  myDF[, "TROPHIC_IS"] <- grepl("IS", myDF[, "TROPHIC"]) # Insectivore
  myDF[, "TROPHIC_IV"] <- grepl("IV", myDF[, "TROPHIC"]) # Invertivore
  myDF[, "TROPHIC_OM"] <- grepl("OM", myDF[, "TROPHIC"]) # Omnivore
  myDF[, "TROPHIC_TC"] <- grepl("TC", myDF[, "TROPHIC"]) # Top Carnivore
  myDF[, "TROPHIC_DE"] <- grepl("DE", myDF[, "TROPHIC"]) # Detritivore
  myDF[, "TROPHIC_PL"] <- grepl("PL", myDF[, "TROPHIC"]) # Planktivore
  myDF[, "TROPHIC_PI"] <- grepl("PI", myDF[, "TROPHIC"]) # Piscivore

  if (!"THERMAL_INDICATOR" %in% names(myDF)) {
    myDF[, "THERMAL_INDICATOR"] <- NA
  }## IF ~ THERMAL_INDICATOR
  myDF[, "TI_CORECOLD"] <- grepl("COREC", myDF[,"THERMAL_INDICATOR"])
  myDF[, "TI_COLD"]     <- grepl("COLD", myDF[,"THERMAL_INDICATOR"])
  myDF[, "TI_COOL"]     <- grepl("COOL", myDF[,"THERMAL_INDICATOR"])
  myDF[, "TI_WARM"]     <- grepl("WARM", myDF[,"THERMAL_INDICATOR"])
  myDF[, "TI_EURY"]     <- grepl("EURYTHERMAL", myDF[,"THERMAL_INDICATOR"])

  if (!"REPRODUCTION" %in% names(myDF)) {
    myDF[, "REPRODUCTION"] <- NA
  }## IF ~ REPRODUCTION
  myDF[, "REPRO_BCAST"]     <- grepl("BROADCASTER", myDF[,"REPRODUCTION"])
  myDF[, "REPRO_NS"]     <- grepl("SIMPLE NEST", myDF[,"REPRODUCTION"])
  myDF[, "REPRO_NC"]     <- grepl("COMPLEX NEST", myDF[,"REPRODUCTION"])
  myDF[, "REPRO_BEAR"]     <- grepl("BEARER", myDF[,"REPRODUCTION"])
  myDF[, "REPRO_MIG"]     <- grepl("MIGRATORY", myDF[,"REPRODUCTION"])

  if (!"HABITAT" %in% names(myDF)) {
    myDF[, "HABITAT"] <- NA
  }## IF ~ HABITAT
  myDF[, "HABITAT_B"]     <- grepl("B", myDF[,"HABITAT"])
  myDF[, "HABITAT_W"]     <- grepl("W", myDF[,"HABITAT"])

  # exact matches only
  myDF[, "TI_NA"]          <- is.na(myDF[, "THERMAL_INDICATOR"])

  if (!"ELEVATION_ATTR" %in% names(myDF)) {
    myDF[, "ELEVATION_ATTR"] <- NA
  }## IF ~ ELEVATION_ATTR
  myDF[, "ELEVATION_LOW"]  <- "LOW" == myDF[, "ELEVATION_ATTR"]
  myDF[, "ELEVATION_HIGH"] <- "HIGH" == myDF[, "ELEVATION_ATTR"]

  if (!"GRADIENT_ATTR" %in% names(myDF)) {
    myDF[, "GRADIENT_ATTR"] <- NA
  }## IF ~ GRADIENT_ATTR
  myDF[, "GRADIENT_LOW"]   <- "LOW" == myDF[, "GRADIENT_ATTR"]
  myDF[, "GRADIENT_MOD"]   <- "MOD" == myDF[, "GRADIENT_ATTR"]
  myDF[, "GRADIENT_HIGH"]  <- "HIGH" == myDF[, "GRADIENT_ATTR"]

  if (!"WSAREA_ATTR" %in% names(myDF)) {
    myDF[, "WSAREA_ATTR"] <- NA
  }## IF ~ WSAREA_ATTR
  myDF[, "WSAREA_S"]       <- "SMALL" == myDF[, "WSAREA_ATTR"]
  myDF[, "WSAREA_M"]       <- "MEDIUM" == myDF[, "WSAREA_ATTR"]
  myDF[, "WSAREA_L"]       <- "LARGE" == myDF[, "WSAREA_ATTR"]
  myDF[, "WSAREA_XL"]      <- "XLARGE" == myDF[, "WSAREA_ATTR"]

  # Create Dominant N ####

  if (verbose == TRUE) {
    # 4
    debug_topic <- "Munge, Dom"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
  }## IF ~ verbose

  # DF for dom so same taxa get combined
  myDF_dom <- dplyr::summarise(dplyr::group_by(myDF, INDEX_NAME, INDEX_CLASS
                                               , SAMPLEID, TAXAID)
                               , N_TAXA = sum(N_TAXA, na.rm = TRUE)
                               , .groups = "drop_last")


  # Create df for Top N (without ties)
  #
  df.dom01 <- dplyr::arrange(myDF_dom, SAMPLEID, dplyr::desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID)  %>%
    dplyr::filter(dplyr::row_number() <= 1)
  df.dom02 <-  dplyr::arrange(myDF_dom, SAMPLEID, dplyr::desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID)  %>%
    dplyr::filter(dplyr::row_number() <= 2)
  df.dom03 <-  dplyr::arrange(myDF_dom, SAMPLEID, dplyr::desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID) %>%
    dplyr::filter(dplyr::row_number() <= 3)
  df.dom04 <-  dplyr::arrange(myDF_dom, SAMPLEID, dplyr::desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID) %>%
    dplyr::filter(dplyr::row_number() <= 4)
  df.dom05 <-  dplyr::arrange(myDF_dom, SAMPLEID, dplyr::desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID) %>%
    dplyr::filter(dplyr::row_number() <= 5)
  df.dom06 <-  dplyr::arrange(myDF_dom, SAMPLEID, dplyr::desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID) %>%
    dplyr::filter(dplyr::row_number() <= 6)
  df.dom07 <-  dplyr::arrange(myDF_dom, SAMPLEID, dplyr::desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID) %>%
    dplyr::filter(dplyr::row_number() <= 7)
  df.dom08 <-  dplyr::arrange(myDF_dom, SAMPLEID, dplyr::desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID) %>%
    dplyr::filter(dplyr::row_number() <= 8)
  df.dom09 <- dplyr::arrange(myDF_dom, SAMPLEID, dplyr::desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID) %>%
    dplyr::filter(dplyr::row_number() <= 9)
  df.dom10 <-  dplyr::arrange(myDF_dom, SAMPLEID, dplyr::desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID)  %>%
    dplyr::filter(dplyr::row_number() <= 10)

  # Summarise Top N
  df.dom01.sum <- dplyr::summarise(dplyr::group_by(df.dom01
                                                   , SAMPLEID
                                                   , INDEX_NAME
                                                   , INDEX_CLASS)
                                   , ni_dom01 = sum(N_TAXA, na.rm = TRUE)
                                   , .groups = "drop_last")
  df.dom02.sum <- dplyr::summarise(dplyr::group_by(df.dom02
                                                   , SAMPLEID
                                                   , INDEX_NAME
                                                   , INDEX_CLASS)
                                   , ni_dom02 = sum(N_TAXA, na.rm = TRUE)
                                   , .groups = "drop_last")
  df.dom03.sum <- dplyr::summarise(dplyr::group_by(df.dom03
                                                   , SAMPLEID
                                                   , INDEX_NAME
                                                   , INDEX_CLASS)
                                   , ni_dom03 = sum(N_TAXA, na.rm = TRUE)
                                   , .groups = "drop_last")
  df.dom04.sum <- dplyr::summarise(dplyr::group_by(df.dom04
                                                   , SAMPLEID
                                                   , INDEX_NAME
                                                   , INDEX_CLASS)
                                   , ni_dom04 = sum(N_TAXA, na.rm = TRUE)
                                   , .groups = "drop_last")
  df.dom05.sum <- dplyr::summarise(dplyr::group_by(df.dom05
                                                   , SAMPLEID
                                                   , INDEX_NAME
                                                   , INDEX_CLASS)
                                   , ni_dom05 = sum(N_TAXA, na.rm = TRUE)
                                   , .groups = "drop_last")
  df.dom06.sum <- dplyr::summarise(dplyr::group_by(df.dom06
                                                   , SAMPLEID
                                                   , INDEX_NAME
                                                   , INDEX_CLASS)
                                   , ni_dom06 = sum(N_TAXA, na.rm = TRUE)
                                   , .groups = "drop_last")
  df.dom07.sum <- dplyr::summarise(dplyr::group_by(df.dom07
                                                   , SAMPLEID
                                                   , INDEX_NAME
                                                   , INDEX_CLASS)
                                   , ni_dom07 = sum(N_TAXA, na.rm = TRUE)
                                   , .groups = "drop_last")
  df.dom08.sum <- dplyr::summarise(dplyr::group_by(df.dom08
                                                   , SAMPLEID
                                                   , INDEX_NAME
                                                   , INDEX_CLASS)
                                   , ni_dom08 = sum(N_TAXA, na.rm = TRUE)
                                   , .groups = "drop_last")
  df.dom09.sum <- dplyr::summarise(dplyr::group_by(df.dom09
                                                   , SAMPLEID
                                                   , INDEX_NAME
                                                   , INDEX_CLASS)
                                   , ni_dom09 = sum(N_TAXA, na.rm = TRUE)
                                   , .groups = "drop_last")
  df.dom10.sum <- dplyr::summarise(dplyr::group_by(df.dom10
                                                   , SAMPLEID
                                                   , INDEX_NAME
                                                   , INDEX_CLASS)
                                   , ni_dom10 = sum(N_TAXA, na.rm = TRUE)
                                   , .groups = "drop_last")

  # Add column of domN to main DF
  myDF <- merge(myDF, df.dom01.sum, all.x = TRUE)
  myDF <- merge(myDF, df.dom02.sum, all.x = TRUE)
  myDF <- merge(myDF, df.dom03.sum, all.x = TRUE)
  myDF <- merge(myDF, df.dom04.sum, all.x = TRUE)
  myDF <- merge(myDF, df.dom05.sum, all.x = TRUE)
  myDF <- merge(myDF, df.dom06.sum, all.x = TRUE)
  myDF <- merge(myDF, df.dom07.sum, all.x = TRUE)
  myDF <- merge(myDF, df.dom08.sum, all.x = TRUE)
  myDF <- merge(myDF, df.dom09.sum, all.x = TRUE)
  myDF <- merge(myDF, df.dom10.sum, all.x = TRUE)

  # Clean up extra Dom data frames
  rm(myDF_dom)
  rm(df.dom01)
  rm(df.dom02)
  rm(df.dom03)
  rm(df.dom04)
  rm(df.dom05)
  rm(df.dom06)
  rm(df.dom07)
  rm(df.dom08)
  rm(df.dom09)
  rm(df.dom10)
  rm(df.dom01.sum)
  rm(df.dom02.sum)
  rm(df.dom03.sum)
  rm(df.dom04.sum)
  rm(df.dom05.sum)
  rm(df.dom06.sum)
  rm(df.dom07.sum)
  rm(df.dom08.sum)
  rm(df.dom09.sum)
  rm(df.dom10.sum)

  # N_Anomalies ----

  if (verbose == TRUE) {
    # 5
    debug_topic <- "Munge, anomalies"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
  }## IF ~ verbose

  # Munge N_Anomalies, NA to 0
  myDF[, "N_ANOMALIES"] <- as.numeric(myDF[, "N_ANOMALIES"])
  myDF[is.na(myDF[, "N_ANOMALIES"]), "N_ANOMALIES"] <- 0

  # By taxon or sample total
  # Data set up to have anomalies by taxon.
  # But some report as sample total.
  # This routine redistributes values proportionally to all taxa
  #  *IF* all are the same value
  #   Cases of same number of anomalies for all taxa should be rare but possible
  stats_anom <- myDF %>%
    dplyr::group_by(SAMPLEID) %>%
    dplyr::summarize(n = dplyr::n()
              , n_distinct = dplyr::n_distinct(N_ANOMALIES, na.rm = TRUE)
              , mean = mean(N_ANOMALIES, na.rm = TRUE)
              , sd = stats::sd(N_ANOMALIES, na.rm = TRUE)
              , sum = sum(N_ANOMALIES, na.rm = TRUE))
  stats_anom[, "SUM_ANOMALIES"] <- stats_anom[, "mean"] / stats_anom[, "n"]
  # make change;  n > 1 & n_distinct == 1
  stats_anom$MOD_ANOMALIES <- ifelse(stats_anom$n > 1 &
                                       stats_anom$n_distinct == 1, TRUE, FALSE)
  # add back to myDF
  myDF <- merge(myDF, stats_anom[, c("SAMPLEID"
                                     , "SUM_ANOMALIES"
                                     , "MOD_ANOMALIES")]
             , all.x = TRUE)
  myDF[myDF$MOD_ANOMALIES == TRUE, "N_ANOMALIES"] <- myDF[myDF$MOD_ANOMALIES == TRUE
                                                        , "SUM_ANOMALIES"]

  # Metric Calc ####

  if (verbose == TRUE) {
    # 6
    debug_topic <- "Calc, metrics"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
  }## IF ~ verbose


  # code above is different than benthos
  # Calculate Metrics (could have used pipe, %>%)
  met.val <- dplyr::summarise(dplyr::group_by(myDF, SAMPLEID, INDEX_NAME
                                              , INDEX_CLASS, SAMP_WIDTH_M
                                              , SAMP_LENGTH_M)
                              , .groups = "drop_last"
                       #
                       # MBSS 2005, 11 metrics
                       # (can do metrics as one step but MBSS output has
                       # numerator so will get that as well)
                       #
                       # when invoke a "x != abc" need to include "| is.na(x)"
                       # unless all "x" are populated (e.g., TRUE or FALSE)

                       ## Individuals ####
                       # individuals, total
                       , ni_total = sum(N_TAXA, na.rm = TRUE)
                       , ni_total_notoler = sum(N_TAXA[TOLER != "TOLERANT" | is.na(TOLER)], na.rm = TRUE)
                       , ni_natnonhybridnonmf = sum(N_TAXA[NATIVE == "NATIVE" &
                                                             (HYBRID != TRUE | is.na(HYBRID)) &
                                                             (TYPE != "MOSQUITOFISH" | is.na(TYPE))], na.rm = TRUE)
                       , ni_natnonhybridnonmfnonLepomis = sum(N_TAXA[NATIVE == "NATIVE" &
                                                                       (HYBRID != TRUE | is.na(HYBRID)) &
                                                                       (TYPE != "MOSQUITOFISH" | is.na(TYPE)) &
                                                                       (GENUS != "LEPOMIS" | is.na(GENUS))], na.rm = TRUE)
                       #
                        ## Percent Individuals ####
                       , pi_AmmEthPerc = 100 * sum(N_TAXA[GENUS == "AMMOCRYPTA"
                                                          | GENUS == "ETHEOSTOMA"
                                                          | GENUS == "PERCINA"], na.rm = TRUE) / ni_total
                        , pi_AmmEthPerc_Cott_Notur = 100 * sum(N_TAXA[(GENUS == "AMMOCRYPTA"
                                                                     | GENUS == "ETHEOSTOMA"
                                                                     | GENUS == "PERCINA"
                                                                     | GENUS == "NOTURUS")
                                                                    | FAMILY == "COTTIDAE"]
                                                                , na.rm = TRUE) / ni_total
                       # % Round-Bodied Suckers
                       , pi_rbs = 100 * sum(N_TAXA[TYPE == "RBS"], na.rm = TRUE) / ni_total
                       , pi_brooktrout = 100 * sum(N_TAXA[TYPE == "BROOK TROUT"], na.rm = TRUE) / ni_total
                       , pi_brooktrout_wild = 100 * sum(N_TAXA[TAXAID == "BROOK TROUT, WILD"], na.rm = TRUE) / ni_total
                       , pi_Cato = 100 * sum(N_TAXA[FAMILY == "CATOSTOMIDAE"], na.rm = TRUE) / ni_total
                       , pi_Cent = 100 * sum(N_TAXA[FAMILY == "CENTRARCHIDAE"], na.rm = TRUE) / ni_total
                       , pi_natCent = 100 * sum(N_TAXA[NATIVE == "NATIVE" & FAMILY == "CENTRARCHIDAE"], na.rm = TRUE) / ni_total
                       , pi_Cott = 100 * sum(N_TAXA[FAMILY == "COTTIDAE"], na.rm = TRUE) / ni_total
                       , pi_Cyprin = 100 * sum(N_TAXA[FAMILY == "CYPRINIDAE"], na.rm = TRUE) / ni_total
                       , pi_Ictal = 100 * sum(N_TAXA[FAMILY == "ICTALURIDAE"], na.rm = TRUE) / ni_total
                       , pi_native = 100 * sum(N_TAXA[NATIVE == "NATIVE"], na.rm = TRUE) / ni_total
                       , pi_nonnative = 100 * sum(N_TAXA[is.na(NATIVE) | NATIVE != "NATIVE"], na.rm = TRUE) / ni_total
                       , pi_Notur = 100 * sum(N_TAXA[GENUS == "NOTURUS"], na.rm = TRUE) / ni_total
                       , pi_sculpin = 100 * sum(N_TAXA[TYPE == "SCULPIN"], na.rm = TRUE) / ni_total
                       , pi_Lepomis = 100 * sum(N_TAXA[GENUS == "LEPOMIS"], na.rm = TRUE) / ni_total
                       , pi_Salm = 100 * sum(N_TAXA[FAMILY == "SALMONIDAE"], na.rm = TRUE) / ni_total
                       , pi_trout = 100 * sum(N_TAXA["TROUT" %in% TYPE], na.rm = TRUE) / ni_total
                       , pi_brooktrout_BCG_att6 = 100 * (sum(N_TAXA[TYPE == "BROOK TROUT"], na.rm = TRUE) +
                                                           sum(N_TAXA[BCG_ATTR == "6"], na.rm = TRUE)) / ni_total
                       , pi_connect = 100 * sum(N_TAXA[CONNECTIVITY == TRUE], na.rm = TRUE) / ni_total
                       , pi_scc = 100 * sum(N_TAXA[SCC == TRUE], na.rm = TRUE) / ni_total
                       , pi_brooktrout2brooktrout_BCG_att6 = 100 *
                          sum(N_TAXA[TYPE == "BROOK TROUT"], na.rm = TRUE) /
                            (sum(N_TAXA[TYPE == "BROOK TROUT"], na.rm = TRUE) +
                               sum(N_TAXA[BCG_ATTR == "6"], na.rm = TRUE))

                       # benthic fluvial specialist
                       , pi_bfs = 100 * sum(N_TAXA[(TYPE == "BENTHIC" & TROPHIC_IV == TRUE) |
                                                   TYPE == "RBS" | TYPE == "SMM"], na.rm = TRUE) / ni_total
                        #
                       ## Number of Taxa ####
                       # account for "NONE" in nt_total, should be the only 0 N_TAXA
                       , nt_total = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & N_TAXA > 0], na.rm = TRUE)
                       #, nt_benthic=dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & TYPE == "DARTER" | TYPE == "SCULPIN" | TYPE == "MADTOM" | TYPE == "LAMPREY"])
                       , nt_benthic = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & TYPE == "BENTHIC"], na.rm = TRUE)
                       , nt_AmmEthPerc = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & (GENUS == "AMMOCRYPTA"
                                                                  | GENUS == "ETHEOSTOMA"
                                                                  | GENUS == "PERCINA")], na.rm = TRUE)
                       , nt_AmmEthPerc_Cott_Notur = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & (GENUS == "AMMOCRYPTA"
                                                                              | GENUS == "ETHEOSTOMA"
                                                                              | GENUS == "PERCINA"
                                                                              | GENUS == "NOTURUS")
                                                                             | FAMILY == "COTTIDAE"]
                                                                      , na.rm = TRUE)
                       , nt_Cato = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & FAMILY == "CATOSTOMIDAE"], na.rm = TRUE)
                       , nt_Cent = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & FAMILY == "CENTRARCHIDAE"], na.rm = TRUE)
                       , nt_natCent = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & NATIVE == "NATIVE" & FAMILY == "CENTRARCHIDAE"], na.rm = TRUE)
                       , nt_Cott = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & FAMILY == "COTTIDAE"], na.rm = TRUE)
                       , nt_Cyprin = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & FAMILY == "CYPRINIDAE"], na.rm = TRUE)
                       , nt_Lepomis = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & GENUS == "LEPOMIS"], na.rm = TRUE)
                       , nt_native = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & NATIVE == "NATIVE" & N_TAXA > 0], na.rm = TRUE)
                       , nt_nonnative = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & (is.na(NATIVE) | NATIVE != "NATIVE")
                                                                 & N_TAXA > 0], na.rm = TRUE)
                       , nt_nativenonhybrid = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & NATIVE == "NATIVE" &
                                                                         (HYBRID != TRUE | is.na(HYBRID))], na.rm = TRUE)
                       , nt_Notur = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & GENUS == "NOTURUS"], na.rm = TRUE)
                       , nt_beninvert = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & TYPE == "BENTHIC" & TROPHIC_IV == TRUE], na.rm = TRUE)
                       , nt_Ictal = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & FAMILY == "ICTALURIDAE"], na.rm = TRUE)
                       , nt_natsunfish = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & NATIVE == "NATIVE" & TYPE == "SUNFISH"], na.rm = TRUE)
                       , nt_natCent_sunfish = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & NATIVE == "NATIVE" &
                                                                 (TYPE == "SUNFISH" | TYPE == "CENTRARCHIDAE")], na.rm = TRUE)
                       , nt_natCent = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & NATIVE == "NATIVE" & FAMILY == "CENTRARCHIDAE"], na.rm = TRUE)
                       , nt_natinsctCypr = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & NATIVE == "NATIVE" &
                                                                      TROPHIC_IS == TRUE & FAMILY == "CYPRINIDAE"], na.rm = TRUE)
                       , nt_natrbs = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & NATIVE == "NATIVE" & TYPE == "RBS"], na.rm = TRUE)
                       , nt_Salm = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & FAMILY == "SALMONIDAE"], na.rm = TRUE)
                       , nt_connect = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & CONNECTIVITY == TRUE], na.rm = TRUE)
                       , nt_scc = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & SCC == TRUE], na.rm = TRUE)
                       , nt_beninsct_nows_nobg = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & TROPHIC_IS == TRUE
                                                                          & (TAXAID != "CATOSTOMUS COMMERSONII"
                                                                             | TAXAID != "LEPOMIS MACROCHIRUS"
                                                                             | is.na(TAXAID))
                                                                          ], na.rm = TRUE)
                       , nt_trout_sunfish_notoler = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & ("TROUT" %in% TYPE
                                                                              | TYPE == "SUNFISH")
                                                                              & (TOLER != "TOLERANT"
                                                                                 | is.na(TOLER))
                                                                              ], na.rm = TRUE)
                       # , nt_beninsct_nows_nobg = NA
                       # , nt_trout_sunfish_notoler = NA
                       # , pi_pisc_noae = NA


                       ## Percent of Taxa ----
                       , pt_AmmEthPerc = 100 * nt_AmmEthPerc / nt_total
                       , pt_AmmEthPerc_Cott_Notur = 100 * nt_AmmEthPerc_Cott_Notur / nt_total
                       , pt_Cato = 100 * nt_Cato / nt_total
                       , pt_Cent = 100 * nt_Cent / nt_total
                       , pt_natCent = 100 * nt_natCent / nt_total
                       , pt_Cott = 100 * nt_Cott / nt_total
                       , pt_Cyprin = 100 * nt_Cyprin / nt_total
                       , pt_Ictal = 100 * nt_Ictal / nt_total
                       , pt_Lepomis = 100 * nt_Lepomis / nt_total
                       , pt_native = 100 * nt_native / nt_total
                       , pt_nonnative = 100 * nt_nonnative / nt_total
                       , pt_Notur = 100 * nt_Notur / nt_total
                       , pt_Salm = 100 * nt_Salm / nt_total
                       , pt_connect = 100 * nt_connect / nt_total
                       , pt_scc = 100 * nt_scc / nt_total

                       ## Trophic ####
                       ### Trophic, nt----
                       , nt_detritivore = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                   & TROPHIC_DE == TRUE]
                                                            , na.rm = TRUE)
                       , nt_herbivore = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                 & TROPHIC_HB == TRUE]
                                                          , na.rm = TRUE)
                       , nt_omnivore = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                & TROPHIC_OM == TRUE]
                                                         , na.rm = TRUE)
                       , nt_planktivore = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                   & TROPHIC_PL == TRUE
                                                                   ], na.rm = TRUE)
                       , nt_topcarn = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                               & TROPHIC_TC == TRUE]
                                                        , na.rm = TRUE)
                       , nt_piscivore = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                 & TROPHIC_PI == TRUE]
                                                          , na.rm = TRUE)

                       ### Trophic, pi----
                        # % Lithophilic spawners
                       , pi_lithophil = 100 * sum(N_TAXA[SILT == TRUE], na.rm = TRUE) / ni_total
                       , pi_detritivore = 100 * sum(N_TAXA[TROPHIC_DE == TRUE], na.rm = TRUE) / ni_total
                       # % gen, omn, invert
                       , pi_genomninvrt = 100 * sum(N_TAXA[TROPHIC_GE == TRUE | TROPHIC_OM == TRUE | TROPHIC_IV == TRUE], na.rm = TRUE) / ni_total
                       , pi_herbivore = 100 * sum(N_TAXA[TROPHIC_HB == TRUE], na.rm = TRUE) / ni_total
                       , pi_insectivore = 100 * sum(N_TAXA[TROPHIC_IS == TRUE], na.rm = TRUE) / ni_total
                       , pi_insctCypr = 100 * sum(N_TAXA[TROPHIC_IS == TRUE &
                                                         FAMILY == "CYPRINIDAE"], na.rm = TRUE) / ni_total
                       , pi_gen = 100 * sum(N_TAXA[TROPHIC_GE == TRUE], na.rm = TRUE) / ni_total
                       , pi_genherb = 100 * sum(N_TAXA[TROPHIC_GE == TRUE | TROPHIC_HB == TRUE], na.rm = TRUE) / ni_total
                       , pi_omnivore = 100 * sum(N_TAXA[TROPHIC_OM == TRUE], na.rm = TRUE) / ni_total
                       , pi_planktivore = 100 * sum(N_TAXA[TROPHIC_PL == TRUE], na.rm = TRUE) / ni_total
                       , pi_topcarn = 100 * sum(N_TAXA[TROPHIC_TC == TRUE], na.rm = TRUE) / ni_total
                       , pi_trout = 100 * sum(N_TAXA["TROUT" %in% TYPE], na.rm = TRUE) / ni_total
                       , pi_pisc_noae = 100 * sum(N_TAXA[TYPE == "PISCIVORE"
                                                         & (TAXAID != "ANGUILLA ROSTRATA"
                                                            | is.na(TAXAID))], na.rm = TRUE) / ni_total
                       #
                       ### Trophic, pt ----
                       , pt_detritivore = 100 * nt_detritivore / nt_total
                       , pt_herbivore = 100 * nt_herbivore / nt_total
                       , pt_omnivore = 100 * nt_omnivore / nt_total
                       , pt_planktivore = 100 * nt_planktivore / nt_total
                       , pt_topcarn = 100 * nt_topcarn / nt_total

                       #
                       ## Tolerance ####
                       , nt_tv_intol = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & TOLER == "INTOLERANT"], na.rm = TRUE)
                       , nt_tv_intolhwi = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & (TOLER == "INTOLERANT" |
                                                                     TOLER == "HWI")], na.rm = TRUE)
                       , pi_tv_toler = 100 * sum(N_TAXA[TOLER == "TOLERANT"], na.rm = TRUE) / ni_total
                       #



                       ## Indices ####
                       # Shannon-Weiner
                       #, x_Shan_Num= -sum(log(N_TAXA / ni_total)), na.rm = TRUE)
                       #, x_Shan_e=x_Shan_Num/log(exp(1))
                       , x_Shan_e = -sum((N_TAXA / ni_total)*log((N_TAXA / ni_total)), na.rm = TRUE)
                       , x_Shan_2 = x_Shan_e/log(2)
                       , x_Shan_10 = x_Shan_e/log(10)
                       , x_Evenness = x_Shan_e/log(nt_total)
                       , x_Evenness100_ni99gt = ifelse(ni_total < 100, 1, x_Evenness * 100)
                       #
                      #  ## Other ####
                        , length_m = max(SAMP_LENGTH_M, na.rm = TRUE)
                        , area_m2 = max(SAMP_WIDTH_M, na.rm = TRUE) * length_m

                        # Abund / sq meter
                        , ni_m2 = ni_total / area_m2 #/(StWidAvg*StLength)
                        , ni_200m = 200 * ni_total / length_m
                        , ni_natnonhybridnonmf_200m = 200 * ni_natnonhybridnonmf / length_m
                        , ni_natnonhybridnonmfnonLepomis_200m = 200 * ni_natnonhybridnonmfnonLepomis / length_m
                        # biomass per square meter (assumes sample not individual biomass)
                       , biomass_m2 = max(SAMP_BIOMASS, na.rm = TRUE) / area_m2 #/(StWidAvg*StLength)
                      #Anomalies
                     , pi_anomalies = 100 * sum(N_ANOMALIES, na.rm = TRUE) / ni_total
                     , pi_delt = 100 * sum(N_ANOMALIES, na.rm = TRUE) / ni_total


                 ## Dominant N ####
                 ## uses previously defined values added to myDF
                 , pi_dom01 = 100 * max(N_TAXA, na.rm = TRUE) / ni_total
                 , pi_dom02 = 100 * max(ni_dom02, na.rm = TRUE) / ni_total
                 , pi_dom03 = 100 * max(ni_dom03, na.rm = TRUE) / ni_total
                 , pi_dom04 = 100 * max(ni_dom04, na.rm = TRUE) / ni_total
                 , pi_dom05 = 100 * max(ni_dom05, na.rm = TRUE) / ni_total
                 , pi_dom06 = 100 * max(ni_dom06, na.rm = TRUE) / ni_total
                 , pi_dom07 = 100 * max(ni_dom07, na.rm = TRUE) / ni_total
                 , pi_dom08 = 100 * max(ni_dom08, na.rm = TRUE) / ni_total
                 , pi_dom09 = 100 * max(ni_dom09, na.rm = TRUE) / ni_total
                 , pi_dom10 = 100 * max(ni_dom10, na.rm = TRUE) / ni_total

                       ## BCG ####

                  ### BCG, nt ----
                 , nt_BCG_att1 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                          & BCG_ATTR == "1"]
                                                    , na.rm = TRUE)
                  , nt_BCG_att12 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                            & (BCG_ATTR == "1"
                                                                  | BCG_ATTR == "2")]
                                                        , na.rm = TRUE)
                  , nt_BCG_att123 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & (BCG_ATTR == "1"
                                                                  | BCG_ATTR == "2"
                                                                  | BCG_ATTR == "3")]
                                                        , na.rm = TRUE)
                 , nt_BCG_att1234 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                             & (BCG_ATTR == "1"
                                                              | BCG_ATTR == "2"
                                                              | BCG_ATTR == "3"
                                                              | BCG_ATTR == "4")]
                                                      , na.rm = TRUE)
                 , nt_BCG_att1236 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                             & (BCG_ATTR == "1"
                                                               | BCG_ATTR == "2"
                                                               | BCG_ATTR == "3"
                                                               | BCG_ATTR == "6")]
                                                       , na.rm = TRUE)
                  , nt_BCG_att1236b = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                               & (BCG_ATTR == "1"
                                                               | BCG_ATTR == "2"
                                                               | BCG_ATTR == "3"
                                                               | BCG_ATTR == "6B")]
                                                       , na.rm = TRUE)
                 , nt_BCG_att12346 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                              & (BCG_ATTR == "1"
                                                              | BCG_ATTR == "2"
                                                              | BCG_ATTR == "3"
                                                              | BCG_ATTR == "4"
                                                              | BCG_ATTR == "6")]
                                                      , na.rm = TRUE)
                  , nt_BCG_att12346b = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                & (BCG_ATTR == "1"
                                                                | BCG_ATTR == "2"
                                                                | BCG_ATTR == "3"
                                                                | BCG_ATTR == "4"
                                                                | BCG_ATTR == "6B")]
                                                      , na.rm = TRUE)
                  , nt_BCG_att1i236i = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                & (BCG_ATTR == "1I"
                                                                | BCG_ATTR == "2"
                                                                | BCG_ATTR == "3"
                                                                | BCG_ATTR == "6I")]
                                                        , na.rm = TRUE)
                  , nt_BCG_att2 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                           & BCG_ATTR == "2"]
                                                    , na.rm = TRUE)
                  , nt_BCG_att2native = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                 & BCG_ATTR == "2"
                                                                 & NATIVE == "NATIVE"]
                                                          , na.rm = TRUE)
                  , nt_BCG_att23_scc = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                & (BCG_ATTR == "2"
                                                                | BCG_ATTR == "3")
                                                               & SCC == TRUE]
                                                        , na.rm = TRUE)
                  , nt_BCG_att3 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                           & BCG_ATTR == "3"]
                                                    , na.rm = TRUE)
                  , nt_BCG_att3native = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                 & BCG_ATTR == "3"
                                                                 & NATIVE == "NATIVE"]
                                                          , na.rm = TRUE)
                  , nt_BCG_att4 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                           & BCG_ATTR == "4"]
                                                    , na.rm = TRUE)
                  , nt_BCG_att4native = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                 & BCG_ATTR == "4"
                                                                 & NATIVE == "NATIVE"]
                                                          , na.rm = TRUE)
                  , nt_BCG_att5 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                           & BCG_ATTR == "5"]
                                                    , na.rm = TRUE)
                  , nt_BCG_att5native = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                 & BCG_ATTR == "5"
                                                                 & NATIVE == "NATIVE"]
                                                          , na.rm = TRUE)
                  , nt_BCG_att55a6 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                             & (BCG_ATTR == "5"
                                                               | BCG_ATTR == "5A"
                                                               | BCG_ATTR == "6")]
                                                             , na.rm = TRUE)
                  , nt_BCG_att55a6a = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                              & (BCG_ATTR == "5"
                                                                | BCG_ATTR == "5A"
                                                                | BCG_ATTR == "6A")]
                                                        , na.rm = TRUE)
                  , nt_BCG_att56t = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                             & (BCG_ATTR == "5"
                                                             | BCG_ATTR == "6T")]
                                                   , na.rm = TRUE)
                  , nt_BCG_att6i = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                            & BCG_ATTR == "6I"]
                                                    , na.rm = TRUE)
                  , nt_BCG_att6m = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                            & BCG_ATTR == "6M"]
                                                     , na.rm = TRUE)
                  , nt_BCG_att6t = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                            & BCG_ATTR == "6T"]
                                                    , na.rm = TRUE)
                  , nt_BCG_attNA = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                            & is.na(BCG_ATTR)]
                                                    , na.rm = TRUE)

                  ### BCG, pi ----

                  , pi_BCG_att12 = 100 * sum(N_TAXA[(BCG_ATTR == "1"
                                                    | BCG_ATTR == "2")]
                                            , na.rm = TRUE) / ni_total

                  , pi_BCG_att123 = 100 * sum(N_TAXA[(BCG_ATTR == "1"
                                                       | BCG_ATTR == "2"
                                                       | BCG_ATTR == "3")]
                                               , na.rm = TRUE) / ni_total
                  , pi_BCG_att1234 = 100 * sum(N_TAXA[(BCG_ATTR == "1"
                                                     | BCG_ATTR == "2"
                                                     | BCG_ATTR == "3"
                                                     | BCG_ATTR == "4")]
                                             , na.rm = TRUE) / ni_total
                  , pi_BCG_att1236 = 100 * sum(N_TAXA[(BCG_ATTR == "1"
                                                     | BCG_ATTR == "2"
                                                     | BCG_ATTR == "3"
                                                     | BCG_ATTR == "6")]
                                            , na.rm = TRUE) / ni_total
                  , pi_BCG_att1236sp = 100 * sum(N_TAXA[(BCG_ATTR == "1"
                                                      | BCG_ATTR == "2"
                                                      | BCG_ATTR == "3"
                                                      | BCG_ATTR == "6")]
                                              , na.rm = TRUE) / sum(N_TAXA[(BCG_ATTR == "1"
                                                                            | BCG_ATTR == "2"
                                                                            | BCG_ATTR == "3"
                                                                            | BCG_ATTR == "6"
                                                                            | BCG_ATTR == "5"
                                                                            | BCG_ATTR == "5A"
                                                                            | BCG_ATTR == "6A")]
                                                                    , na.rm = TRUE)
                  , pi_BCG_att1236b = 100 * sum(N_TAXA[(BCG_ATTR == "1"
                                                      | BCG_ATTR == "2"
                                                      | BCG_ATTR == "3"
                                                      | BCG_ATTR == "6B")]
                                              , na.rm = TRUE) / ni_total
                  , pi_BCG_att12346b = 100 * sum(N_TAXA[(BCG_ATTR == "1"
                                                      | BCG_ATTR == "2"
                                                      | BCG_ATTR == "3"
                                                      | BCG_ATTR == "4"
                                                      | BCG_ATTR == "6B")]
                                              , na.rm = TRUE) / ni_total
                  , pi_BCG_att1i236i = 100 * sum(N_TAXA[(BCG_ATTR == "1I"
                                                     | BCG_ATTR == "2"
                                                     | BCG_ATTR == "3"
                                                     | BCG_ATTR == "6I")]
                                             , na.rm = TRUE) / ni_total
                  , pi_BCG_att2 = 100 * sum(N_TAXA[BCG_ATTR == "2"]
                                            , na.rm = TRUE) / ni_total
                  , pi_BCG_att2native = 100 * sum(N_TAXA[BCG_ATTR == "2"
                                                         & NATIVE == "NATIVE"]
                                                  , na.rm = TRUE) / ni_total
                  , pi_BCG_att23_scc = 100 * sum(N_TAXA[(BCG_ATTR == "2"
                                                        | BCG_ATTR == "3")
                                                       & SCC == TRUE]
                                                , na.rm = TRUE) / ni_total
                  , pi_BCG_att3 = 100 * sum(N_TAXA[BCG_ATTR == "3"]
                                            , na.rm = TRUE) / ni_total
                  , pi_BCG_att3native = 100 * sum(N_TAXA[BCG_ATTR == "3"
                                                         & NATIVE == "NATIVE"]
                                                  , na.rm = TRUE) / ni_total
                  , pi_BCG_att4 = 100 * sum(N_TAXA[BCG_ATTR == "4"]
                                            , na.rm = TRUE) / ni_total
                  , pi_BCG_att4native = 100 * sum(N_TAXA[BCG_ATTR == "4"
                                                         & NATIVE == "NATIVE"]
                                                  , na.rm = TRUE) / ni_total
                  , pi_BCG_att5 =  100 * sum(N_TAXA[BCG_ATTR == "5"]
                                             , na.rm = TRUE) / ni_total
                  , pi_BCG_att5native = 100 * sum(N_TAXA[BCG_ATTR == "5"
                                                         & NATIVE == "NATIVE"]
                                                  , na.rm = TRUE) / ni_total
                  , pi_BCG_att55a6a = 100 * sum(N_TAXA[(BCG_ATTR == "5"
                                                     | BCG_ATTR == "5A"
                                                    | BCG_ATTR == "6A")]
                                            , na.rm = TRUE) / ni_total
                  , pi_BCG_att56a = 100 * sum(N_TAXA[(BCG_ATTR == "5"
                                                       | BCG_ATTR == "6A")]
                                               , na.rm = TRUE) / ni_total
                  , pi_BCG_att5a6a = 100 * sum(N_TAXA[(BCG_ATTR == "5A"
                                                    | BCG_ATTR == "6A")]
                                            , na.rm = TRUE) / ni_total
                  , pi_BCG_att56t = 100 * sum(N_TAXA[(BCG_ATTR == "5"
                                                   | BCG_ATTR == "6T")]
                                           , na.rm = TRUE) / ni_total
                  , pi_BCG_att6 =  100 * sum(N_TAXA[BCG_ATTR == "6"]
                                              , na.rm = TRUE) / ni_total
                  , pi_BCG_att6i =  100 * sum(N_TAXA[BCG_ATTR == "6I"]
                                            , na.rm = TRUE) / ni_total
                  , pi_BCG_att6m =  100 * sum(N_TAXA[BCG_ATTR == "6M"]
                                             , na.rm = TRUE) / ni_total
                  , pi_BCG_att6t =  100 * sum(N_TAXA[BCG_ATTR == "6T"]
                                              , na.rm = TRUE) / ni_total
                  , pi_BCG_att66a = 100 * sum(N_TAXA[(BCG_ATTR == "6"
                                                    | BCG_ATTR == "6A")]
                                            , na.rm = TRUE) / ni_total
                  , pi_BCG_att66a6b = 100 * sum(N_TAXA[(BCG_ATTR == "6"
                                                    | BCG_ATTR == "6A"
                                                    | BCG_ATTR == "6B")]
                                            , na.rm = TRUE) / ni_total
                  , pi_BCG_att66s6t =  100 * sum(N_TAXA[BCG_ATTR == "6"
                                                       | BCG_ATTR == "6S"
                                                       | BCG_ATTR == "6T"]
                                                , na.rm = TRUE) / ni_total
                  , pi_BCG_attNA = 100 * sum(N_TAXA[is.na(BCG_ATTR)]
                                        , na.rm = TRUE) / ni_total

                  ### BCG, pt----
                  , pt_BCG_att12 = 100 * nt_BCG_att12 / nt_total
                  , pt_BCG_att123 = 100 * nt_BCG_att123 / nt_total
                  , pt_BCG_att1234 = 100 * nt_BCG_att1234 / nt_total
                  , pt_BCG_att1236 = 100 * nt_BCG_att1236 / nt_total
                  , pt_BCG_att1236b = 100 * nt_BCG_att1236b / nt_total

                  , pt_BCG_att1236sp = 100 * nt_BCG_att1236 / (nt_BCG_att1236 +
                                                                 nt_BCG_att55a6a)

                  , pt_BCG_att12346b = 100 * nt_BCG_att12346b / nt_total
                  , pt_BCG_att1i236i = 100 * nt_BCG_att1i236i / nt_total
                  , pt_BCG_att2 = 100 * nt_BCG_att2 / nt_total
                  , pt_BCG_att2native = 100 * nt_BCG_att2native / nt_total
                  , pt_BCG_att23_scc = 100 * nt_BCG_att23_scc / nt_total
                  , pt_BCG_att3 = 100 * nt_BCG_att3 / nt_total
                  , pt_BCG_att3native = 100 * nt_BCG_att3native / nt_total
                  , pt_BCG_att4 = 100 * nt_BCG_att4 / nt_total
                  , pt_BCG_att4native = 100 * nt_BCG_att4native / nt_total
                  , pt_BCG_att5 = 100 * nt_BCG_att5 / nt_total
                  , pt_BCG_att5native = 100 * nt_BCG_att5native / nt_total
                  , pt_BCG_att55a6a = 100 * nt_BCG_att55a6 / nt_total
                  , pt_BCG_att56t = 100 * nt_BCG_att56t / nt_total
                  , pt_BCG_att6i = 100 * nt_BCG_att6i / nt_total
                  , pt_BCG_att6m = 100 * nt_BCG_att6m / nt_total
                  , pt_BCG_att6t = 100 * nt_BCG_att6t / nt_total
                  , pt_BCG_attNA = 100 * nt_BCG_attNA / nt_total

                  ### BCG, pi_dom----

                  , pi_dom01_BCG_att4 = 100 * max(0, max(N_TAXA[(BCG_ATTR == "4")]
                                                        , na.rm = TRUE)
                                                  , na.rm = TRUE) / ni_total
                 , pi_dom01_BCG_att45 = 100 * max(0, max(N_TAXA[(BCG_ATTR == "4"
                                                                   | BCG_ATTR == "5")]
                                                           , na.rm = TRUE)
                                                    , na.rm = TRUE) / ni_total

                  , pi_dom01_BCG_att5 = 100 * max(0, max(N_TAXA[(BCG_ATTR == "5")]
                                                        , na.rm = TRUE)
                                                  , na.rm = TRUE) / ni_total
                 , pi_dom01_BCG_att5a = 100 * max(0, max(N_TAXA[(BCG_ATTR == "5A")]
                                                        , na.rm = TRUE)
                                                 , na.rm = TRUE) / ni_total
                 , pi_dom01_BCG_att5a6a = 100 * max(0, max(N_TAXA[(BCG_ATTR == "5A"
                                                                   | BCG_ATTR == "6A")]
                                                           , na.rm = TRUE)
                                                    , na.rm = TRUE) / ni_total
                  , pi_dom01_BCG_att566a = 100 * max(0, max(N_TAXA[(BCG_ATTR == "5"
                                                          | BCG_ATTR == "6"
                                                          | BCG_ATTR == "6A")]
                                                          , na.rm = TRUE)
                                                , na.rm = TRUE) / ni_total

                  ## Thermal Indicators ####
                  ## nt_ti
                  , nt_ti_corecold = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                              & TI_CORECOLD == TRUE]
                                                       , na.rm = TRUE)
                  , nt_ti_cold = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                          & TI_COLD == TRUE]
                                                   , na.rm = TRUE)
                  , nt_ti_cool = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                          & TI_COOL == TRUE]
                                                   , na.rm = TRUE)
                  , nt_ti_warm = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                          & TI_WARM == TRUE]
                                                   , na.rm = TRUE)
                  , nt_ti_eury = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                          & TI_EURY == TRUE]
                                                   , na.rm = TRUE)
                  , nt_ti_na = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                        & TI_NA == TRUE]
                                                 , na.rm = TRUE)
                  , nt_ti_corecold_cold = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                   & (TI_CORECOLD == TRUE
                                                                      | TI_COLD == TRUE)]
                                                            , na.rm = TRUE)
                  , nt_ti_cool_warm = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                               & (TI_COOL == TRUE
                                                                  | TI_WARM == TRUE)]
                                                        , na.rm = TRUE)

                  ## pi_ti
                  , pi_ti_corecold = 100 * sum(N_TAXA[TI_CORECOLD == TRUE]
                                             , na.rm = TRUE) / ni_total
                  , pi_ti_cold = 100 * sum(N_TAXA[TI_COLD == TRUE]
                                         , na.rm = TRUE) / ni_total
                  , pi_ti_cool = 100 * sum(N_TAXA[TI_COOL == TRUE]
                                         , na.rm = TRUE) / ni_total
                  , pi_ti_warm = 100 * sum(N_TAXA[TI_WARM == TRUE]
                                         , na.rm = TRUE) / ni_total
                  , pi_ti_eury = 100 * sum(N_TAXA[TI_EURY == TRUE]
                                         , na.rm = TRUE) / ni_total
                  , pi_ti_na = 100 * sum(N_TAXA[TI_NA == TRUE]
                                       , na.rm = TRUE) / ni_total
                  , pi_ti_corecold_cold = 100 * sum(N_TAXA[TI_CORECOLD == TRUE |
                                                           TI_COLD == TRUE]
                                                  , na.rm = TRUE) / ni_total
                  , pi_ti_cool_warm = 100 * sum(N_TAXA[TI_COOL == TRUE |
                                                       TI_WARM == TRUE]
                                              , na.rm = TRUE) / ni_total


                  ## pt_ti
                  , pt_ti_corecold = 100 * nt_ti_corecold / nt_total
                  , pt_ti_cold = 100 * nt_ti_cold / nt_total
                  , pt_ti_cool = 100 * nt_ti_cool / nt_total
                  , pt_ti_warm = 100 * nt_ti_warm / nt_total
                  , pt_ti_eury = 100 * nt_ti_eury / nt_total
                  , pt_ti_na = 100 * nt_ti_na / nt_total
                  , pt_ti_corecold_cold = 100 * nt_ti_corecold_cold / nt_total
                  , pt_ti_cool_warm = 100 * nt_ti_cool_warm / nt_total


                  ## Elevation ----
                  , nt_elev_low = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & ELEVATION_LOW == TRUE]
                                                    , na.rm = TRUE)
                  , nt_elev_high = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & ELEVATION_HIGH == TRUE]
                                                     , na.rm = TRUE)

                  ## Gradient ----
                  , nt_grad_low = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & GRADIENT_LOW == TRUE]
                                                    , na.rm = TRUE)
                  , nt_grad_mod = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & GRADIENT_MOD == TRUE]
                                                    , na.rm = TRUE)
                  , nt_grad_high = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & GRADIENT_HIGH == TRUE]
                                                     , na.rm = TRUE)

                  ## WS_Area ----
                  , nt_wsarea_small = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & WSAREA_S == TRUE]
                                                        , na.rm = TRUE)
                  , nt_wsarea_medium = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & WSAREA_M == TRUE]
                                                         , na.rm = TRUE)
                  , nt_wsarea_large = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & WSAREA_L == TRUE]
                                                        , na.rm = TRUE)
                  , nt_wsarea_xlarge = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & WSAREA_XL == TRUE]
                                                         , na.rm = TRUE)

                  ## Reproduction ----
                  , nt_repro_broadcaster = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & REPRO_BCAST == TRUE]
                                                        , na.rm = TRUE)
                  , nt_repro_nestsimp = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & REPRO_NS == TRUE]
                                                         , na.rm = TRUE)
                  , nt_repro_nestcomp = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & REPRO_NC == TRUE]
                                                        , na.rm = TRUE)
                  , nt_repro_bearer = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & REPRO_BEAR == TRUE]
                                                         , na.rm = TRUE)
                  , nt_repro_migratory = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & REPRO_MIG == TRUE]
                                                        , na.rm = TRUE)


                  ## Habitat ####
                  # BCG Great Plains 2021
                  # W = water column
                  # B = benthic
                  #
                  # nt_habitat
                  , nt_habitat_b = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & HABITAT_B == TRUE]
                                                        , na.rm = TRUE)
                  , nt_habitat_w = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & HABITAT_W == TRUE]
                                                        , na.rm = TRUE)
                  ## pi_habitat
                  , pi_habitat_b = 100 * sum(N_TAXA[HABITAT_B == TRUE]
                                              , na.rm = TRUE) / ni_total
                  , pi_habitat_w = 100 * sum(N_TAXA[HABITAT_W == TRUE]
                                              , na.rm = TRUE) / ni_total
                  ## pt_habitat
                  , pt_habitat_b = 100 * nt_habitat_b / nt_total
                  , pt_habitat_w = 100 * nt_habitat_w / nt_total

                 ## SPECIAL ----
                 # New Mexico Fish BCG
                 , nt_piscivore_BCG_att66s6t = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                        & TROPHIC_PI == TRUE
                                                                        & (BCG_ATTR == "6"
                                                                           | BCG_ATTR == "6S"
                                                                           | BCG_ATTR == "6T")]
                                                                 , na.rm = TRUE)
                 , nt_LLNLB = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                       & TYPE == "LLNLB"]
                                                , na.rm = TRUE)
                 , nt_Cyprin_BCG_att1234 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                                    & FAMILY == "CYPRINIDAE"
                                                                    & (BCG_ATTR == "1"
                                                                       | BCG_ATTR == "2"
                                                                       | BCG_ATTR == "3"
                                                                       | BCG_ATTR == "4")]
                                                             , na.rm = TRUE)
                 , ni_Hybognathus_amarus = sum(N_TAXA[TAXAID == "HYBOGNATHUS AMARUS"]
                                               , na.rm = TRUE)
                 , x_TrophicCats = dplyr::n_distinct(TROPHIC, na.rm = TRUE)

                       #
                       # name changes ####
                       # # MBSS metric names
                       # , STRMAREA  = area
                       # , TOTCNT    = ni_total
                       # , ABUNSQM   = ni_m2
                       # , PABDOM    = pi_dom01
                       # , TOTBIOM   = x_biomass_total
                       # , BIOM_MSQ  = x_biomass_m2
                       # , NUMBENTSP = nt_benthic
                       # # , NUMBROOK  = ni_brooktrout
                       # , PBROOK    = pi_brooktrout
                       # # , NUMGEOMIV = ni_genomninvrt
                       # , PGEOMIV   = pi_genomninvrt
                       # # , NUMIS     = ni_insectivore
                       # , P_IS      = pi_insectivore
                       # # , NUMLITH   = ni_lithophil
                       # , P_LITH    = pi_lithophil
                       # # , NUMROUND  = ni_rbs
                       # , PROUND    = pi_rbs
                       # # , NUMSCULP  = ni_sculpin
                       # , PSCULP    = pi_sculpin
                       # # , NUMTOL    = ni_tv_toler
                       # , PTOL      = pi_tv_toler
                       #
        )## met.val.END

  if (verbose == TRUE) {
    # 7
    debug_topic <- "clean up"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
  }## IF ~ verbose

  #
  # replace NA with 0
  met.val[is.na(met.val)] <- 0
  #
  # # # subset to only metrics specified by user
  # # if (!is.null(MetricNames)) {
  # #   met.val <- met.val[,c(Index_Name, SITE, INDEX_CLASS, ACREAGE, LEN_SAMP, MetricNames)]
  # # }
  # myFlds_Remove <- c("ni_total", "pi_rbs",
  #                    , "pi_brooktrout", "pi_sculpin", "nt_total"
  #                    , "nt_benthic", "pi_lithophil",
  #                    , "pi_genomninvrt", "pi_insectivore",
  #                    , "pi_tv_toler", "pi_dom01", "area", "ni_m2"
  #                    , "x_biomass_total", "x_biomass_m2")
  # met.val <- met.val[,-match(myFlds_Remove,names(met.val))]

  # # subset to only metrics specified by user

  if (verbose == TRUE) {
    # 8
    debug_topic <- "subset"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
  }## IF ~ verbose

  if (is.null(MetricNames)) {
    #met.val <- met.val
  } else {
    met2include <- MetricNames[!(MetricNames %in% "ni_total")]
    # remove ni_total if included as will always include it
    met.val <- met.val[, c("SAMPLEID", "INDEX_CLASS", "INDEX_NAME", met2include)]
  }##IF~MetricNames~END

  # Add extra fields

  if (verbose == TRUE) {
    # 9
    debug_topic <- "extra fields"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
  }## IF ~ verbose

  if (is.null(cols2keep)) {##IF.is.null.cols2keep.START
    df.return <- as.data.frame(met.val)
  } else {
    # create df with grouped fields
    myDF.cols2keep <- myDF %>%
      dplyr::group_by(.dots = c("SAMPLEID", cols2keep)) %>%
      dplyr::summarize(col.drop = sum(N_TAXA))
    col.drop <- ncol(myDF.cols2keep)
    myDF.cols2keep <- myDF.cols2keep[,-col.drop]
    # merge
    df.return <- merge(as.data.frame(myDF.cols2keep)
                       , as.data.frame(met.val)
                       , by = "SAMPLEID")
  }##IF.is.null.cols2keep.END

    # adjust ----
  # #
  # # Adjust metrics (MBSS always adjust so remove IF/THEN)
  # # added as extra columns to output
  # #if (boo.Adjust==TRUE) {##IF.boo.Ajust.START
  #   # MBSS.2005.Fish
  #   # nt_benthic
  #     met.val[,"NUMBENTSP_Obs"] <- met.val[,"NUMBENTSP"]
  #     # Expected constants
  #     ## m
  #     met.val[,"NUMBENTSP_m"] <- NA
  #     met.val[,"NUMBENTSP_m"][met.val[,"INDEX_CLASS"]=="COASTAL"]   <- 1.69
  #     met.val[,"NUMBENTSP_m"][met.val[,"INDEX_CLASS"]=="EPIEDMONT"] <- 1.25
  #     met.val[,"NUMBENTSP_m"][met.val[,"INDEX_CLASS"]=="HIGHLAND"]  <- 1.23
  #     ## b
  #     met.val[,"NUMBENTSP_b"] <- NA
  #     met.val[,"NUMBENTSP_b"][met.val[,"INDEX_CLASS"]=="COASTAL"]   <- -3.33
  #     met.val[,"NUMBENTSP_b"][met.val[,"INDEX_CLASS"]=="EPIEDMONT"] <- -2.36
  #     met.val[,"NUMBENTSP_b"][met.val[,"INDEX_CLASS"]=="HIGHLAND"]  <- -2.35
  #     # Calc Expected
  #     met.val[,"NUMBENTSP_Exp"] <- (met.val[,"NUMBENTSP_m"] * log10(met.val[,"ACREAGE"])) + met.val[,"NUMBENTSP_b"]
  #     # Calc Adjusted
  #     met.val[,"NUMBENTSP_Adj"] <- met.val[,"NUMBENTSP_Obs"] / met.val[,"NUMBENTSP_Exp"]
  #     # Rename base metric with adjusted value
  #     met.val[,"NUMBENTSP"] <- met.val[,"NUMBENTSP_Adj"]
  #     # NA to zero
  #     met.val[,"NUMBENTSP"][is.na(met.val[,"NUMBENTSP"])] <- 0
  #
  # #}##IF.boo.Ajust.END
  #
  # df to report back

  if (verbose == TRUE) {
    # 10
    debug_topic <- "return results"
    debug_sub_num <- debug_sub_num + 1
    msg <- paste0("debug_metval_sub, "
                  , debug_sub_community
                  , ", "
                  , debug_sub_num
                  , "/"
                  , debug_sub_num_total
                  , ", "
                  , debug_topic)
    message(msg)
  }## IF ~ verbose

  return(df.return)
}##FUNCTION.metric.values.fish.END
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Calculate metric values, Algae
#'
#' @description Subfunction of metric.values for use with Algae.
#'
#' @details For internal use only.  Called from metric.values().
#'
#' @param myDF Data frame of taxa.
#' @param MetricNames Optional vector of metric names to be returned.
#' @param boo.Adjust Optional boolean value on whether to perform adjustments of
#' values prior to scoring.  Default = FALSE but may be TRUE for certain
#' metrics.
#' @param cols2keep Column names of fun.DF to retain in the output.  Uses
#' column names.
#' @param MetricSort How metric names should be sort; NA = as is
#' , AZ = alphabetical.  Default = NULL.
#' @param boo.Shiny Boolean value for if the function is accessed via Shiny.
#' Default = FALSE.
#' @param verbose Include messages to track progress.  Default = FALSE
#'
#' @return Data frame
#'
#' @keywords internal
#'
#' @export
metric.values.algae <- function(myDF
                                , MetricNames = NULL
                                , boo.Adjust = FALSE
                                , cols2keep = NULL
                                , MetricSort = NA
                                , boo.Shiny = FALSE
                                , verbose) {

  time_start <- Sys.time()

  # global variable bindings ----
  N_TAXA <- NULL
  NONTARGET <- SAMPLEID <- INDEX_NAME <- INDEX_CLASS <- ni_total <- TAXAID <-
    EXCLUDE <- GENUS <- LOW_N <- HIGH_N <- LOW_P <- HIGH_P <- BC_1 <- BC_2 <-
    BC_3 <- BC_4 <- BC_5 <- PT_1 <- PT_2 <- PT_3 <- PT_4 <- PT_5 <-
    SALINITY_1 <- SALINITY_2 <- SALINITY_3 <- SALINITY_4 <- O_1 <- O_2 <- O_3 <-
    O_4 <- O_5 <- SESTONIC_HABIT <- BENTHIC_HABIT <- BAHLS_1 <- BAHLS_2 <-
    BAHLS_3 <- TROPHIC_1 <- TROPHIC_2 <- TROPHIC_3 <- TROPHIC_4 <- TROPHIC_5 <-
    TROPHIC_6 <- TROPHIC_7 <- SAP_1 <- SAP_2 <- SAP_3 <- SAP_4 <- SAP_5 <-
    NON_N_FIXER <- N_FIXER <- HIGHLY_MOTILE <- MODERATELY_MOTILE <-
    NON_MOTILE <- SLIGHTLY_MOTILE <- WEAKLY_MOTILE <- BIG <- SMALL <- MEDIUM <-
    VERY_BIG <- VERY_SMALL <- ADNATE <- STALKED <- HIGHLY_MOTILE.1 <- ARAPHID <-
    DIAT_CL <- BEN_SES <- DIAT_CA <- DIAT_COND <- DIATAS_TN <- DIATAS_TP <-
    MOTILITY <- NF <- pi_DIAT_CL_1 <- nt_Achnan_Navic <- nt_total <-
    nt_HIGH_N <- nt_LOW_N <- nt_HIGH_P <- nt_LOW_P <- nt_BC_1 <- nt_BC_2 <-
    nt_BC_3 <- nt_BC_4 <- nt_BC_5 <- nt_BC_12 <- nt_BC_45 <- nt_PT_1 <-
    nt_PT_2 <- nt_PT_3 <- nt_PT_4 <- nt_PT_5 <- nt_PT_12 <- nt_SALINITY_1 <-
    nt_SALINITY_2 <- nt_SALINITY_3 <- nt_SALINITY_4 <- nt_SALINITY_34 <-
    nt_O_1 <- nt_O_2 <- nt_O_3 <- nt_O_4 <- nt_O_5 <- nt_O_345 <-
    nt_SESTONIC_HABIT <- nt_BENTHIC_HABIT <- nt_BAHLS_1 <- nt_BAHLS_2 <-
    nt_BAHLS_3 <- nt_TROPHIC_1 <- nt_TROPHIC_2 <- nt_TROPHIC_3 <-
    nt_TROPHIC_4 <- nt_TROPHIC_5 <- nt_TROPHIC_6 <- nt_TROPHIC_7 <-
    nt_TROPHIC_456 <- nt_SAP_1 <- nt_SAP_2 <- nt_SAP_3 <- nt_SAP_4 <-
    nt_SAP_5 <- nt_NON_N_FIXER <- nt_N_FIXER <- nt_HIGHLY_MOTILE <-
    nt_MODERATELY_MOTILE <- nt_NON_MOTILE <- nt_SLIGHTLY_MOTILE <-
    nt_WEAKLY_MOTILE <- nt_BIG <- nt_SMALL <- nt_MEDIUM <- nt_VERY_BIG <-
    nt_VERY_SMALL <- nt_ADNATE <- nt_STALKED <- nt_HIGHLY_MOTILE.1 <-
    nt_ARAPHID <- nt_DIAT_CL_1 <- nt_DIAT_CL_2 <- nt_BEN_SES_1 <-
    nt_BEN_SES_2 <- nt_DIAT_CA_1 <- nt_DIAT_CA_2 <- nt_DIAT_COND_1 <-
    nt_DIAT_COND_2 <- nt_DIATAS_TN_1 <- nt_DIATAS_TN_2 <- nt_DIATAS_TP_1 <-
    nt_DIATAS_TP_2 <- nt_MOTILITY_1 <- nt_MOTILITY_2 <- nt_NF_1 <- nt_NF_2 <-
    TOLVAL <- REF_INDICATORS <- nt_Sens_810 <- nt_RefIndicators <- nt_Tol_13 <-
    POLL_TOL <- NULL
  nt_TROPHIC_12 <- nt_TROPHIC_56 <- pi_BC_12 <- pt_TROPHIC_12 <-
    pt_TROPHIC_56 <- NULL


  # define pipe
  `%>%` <- dplyr::`%>%`

  # QC----
  # QC, Required Fields
  ## QC, Missing Cols ----
  # col.req_character <- c("SAMPLEID", "TAXAID", "INDEX_NAME", "INDEX_CLASS"
  #                        , "PHYLUM", "ORDER", "FAMILY", "GENUS"
  #                        )
  # col.req_logical <- c("EXCLUDE", "NONTARGET")
  # col.req_numeric <- c("N_TAXA")
  # col.req <- c(col.req_character, col.req_logical, col.req_numeric)
  col.req <- c("INDEX_NAME", "INDEX_CLASS", "SAMPLEID","TAXAID","N_TAXA"
               ,"EXCLUDE","NONTARGET"
               ,"PHYLUM","ORDER","FAMILY","GENUS","BC_USGS"
               ,"TROPHIC_USGS","SAP_USGS","PT_USGS","O_USGS","SALINITY_USGS"
               ,"BAHLS_USGS","P_USGS","N_USGS","HABITAT_USGS","N_FIXER_USGS"
               ,"MOTILITY_USGS","SIZE_USGS","HABIT_USGS","MOTILE2_USGS"
               ,"TOLVAL","DIATOM_ISA","DIAT_CL","POLL_TOL","BEN_SES"
               ,"DIATAS_TP","DIATAS_TN","DIAT_COND","DIAT_CA","MOTILITY"
               ,"NF")
  col.req.missing <- col.req[!(col.req %in% toupper(names(myDF)))]
  num.col.req.missing <- length(col.req.missing)
  # Trigger prompt if any missing fields (and session is interactive)
  if (num.col.req.missing != 0) {##IF.num.col.req.missing.START
    myPrompt.01 <- paste0("There are ",num.col.req.missing," missing fields in the data:")
    myPrompt.02 <- paste(col.req.missing, collapse = ", ")
    myPrompt.03 <- "If you continue the metrics associated with these fields will be invalid."
    myPrompt.04 <- "For example, if the HABIT field is missing all habit related metrics will not be correct."
    myPrompt.05 <- "Do you wish to continue (YES or NO)?"

    myPrompt <- paste(" ", myPrompt.01, myPrompt.02, " ", myPrompt.03, myPrompt.04
                      , myPrompt.05, sep = "\n")
    #user.input <- readline(prompt=myPrompt)
    user.input <- NA
    # special condition for Shiny
    #Shiny counts as interactive()==TRUE but cannot access this prompt in Shiny.
    if (boo.Shiny == FALSE) {
      user.input <- utils::menu(c("YES", "NO"), title = myPrompt)
    } else {
      message(myPrompt)
      message("boo.Shiny == TRUE so prompt skipped and value set to '1'.")
      user.input <- 1
    }## IF ~ boo.Shiny ~ END

    # any answer other than "YES" will stop the function.
    if (user.input != 1) {##IF.user.input.START
      stop(paste("The user chose *not* to continue due to missing fields: "
                 , paste(paste0("   ",col.req.missing), collapse = "\n"), sep = "\n"))
    }##IF.user.input.END
    # Add missing fields
    myDF[,col.req.missing] <- NA
    warning(paste("Metrics related to the following fields are invalid:"
                  , paste(paste0("   ", col.req.missing), collapse = "\n"), sep = "\n"))
  }##IF.num.col.req.missing.END

  ## QC, Cols2Keep ----
  # remove duplicates with required so no errors, e.g., SAMPLEID
  cols2keep <- cols2keep[!cols2keep %in% col.req]

  ## QC, Exclude ----
  # as TRUE/FALSE
  Exclude.T <- sum(myDF$EXCLUDE == TRUE, na.rm = TRUE)
  if (Exclude.T == 0) {##IF.Exclude.T.START
    warning("EXCLUDE column does not have any TRUE values. \n  Valid values are TRUE or FALSE.  \n  Other values are not recognized.")
  }##IF.Exclude.T.END

  ## QC, NonTarget ----
  # as TRUE/FALSE
  NonTarget.F <- sum(myDF$NONTARGET == FALSE, na.rm = TRUE)
  if (NonTarget.F == 0) {##IF.Exclude.T.START
    warning("NONTARGET column does not have any FALSE values. \n  Valid values are TRUE or FALSE.  \n  Other values are not recognized.")
  }##IF.Exclude.T.END

  ## QC, TolVal----
  # need as numeric, if have "NA" as character it fails
  TolVal_Char_NA <- myDF[, "TOLVAL"] == "NA"
  if (sum(TolVal_Char_NA, na.rm = TRUE) > 0) {
    myDF[TolVal_Char_NA, "TOLVAL"] <- NA
    myDF[, "TOLVAL"] <- as.numeric(myDF[, "TOLVAL"])
  }##IF ~ TOLVAL ~ END

  # Data Munging----
  # Remove NonTarget Taxa (added back 20200715, missing since 20200224)
  # Function fails if all NA (e.g., column was missing) (20200724)
  myDF <- myDF %>% dplyr::filter(NONTARGET != TRUE | is.na(NONTARGET))

  # Convert values to upper case (FFG, Habit, Life_Cycle)
  myDF[, "BC_USGS"] <- toupper(myDF[, "BC_USGS"])
  myDF[, "PT_USGS"] <- toupper(myDF[, "PT_USGS"])
  myDF[, "O_USGS"] <- toupper(myDF[, "O_USGS"])
  myDF[, "SALINITY_USGS"] <- toupper(myDF[, "SALINITY_USGS"])
  myDF[, "P_USGS"] <- toupper(myDF[, "P_USGS"])
  myDF[, "N_USGS"] <- toupper(myDF[, "N_USGS"])
  myDF[, "HABITAT_USGS"] <- toupper(myDF[, "HABITAT_USGS"])
  myDF[, "BAHLS_USGS"] <- toupper(myDF[, "BAHLS_USGS"])
  myDF[, "TROPHIC_USGS"] <- toupper(myDF[, "TROPHIC_USGS"])
  myDF[, "DIATOM_ISA"] <- toupper(myDF[, "DIATOM_ISA"])
  myDF[, "SAP_USGS"] <- toupper(myDF[, "SAP_USGS"])
  myDF[, "N_FIXER_USGS"] <- toupper(myDF[, "N_FIXER_USGS"])
  myDF[, "MOTILITY_USGS"] <- toupper(myDF[, "MOTILITY_USGS"])
  myDF[, "SIZE_USGS"] <- toupper(myDF[, "SIZE_USGS"])
  myDF[, "HABIT_USGS"] <- toupper(myDF[, "HABIT_USGS"])
  myDF[, "MOTILE2_USGS"] <- toupper(myDF[, "MOTILE2_USGS"])
  myDF[, "DIATOM_ISA"] <- toupper(myDF[, "DIATOM_ISA"])

  # Add extra columns for some fields
  # (need unique values for functions in summarise)
  # each will be TRUE or FALSE
  # finds any match so "CN, CB" is both "CN" and "CB"
  myDF[, "BC_1"] <- grepl("BC_1", myDF[, "BC_USGS"])
  myDF[, "BC_2"] <- grepl("BC_2", myDF[, "BC_USGS"])
  myDF[, "BC_3"] <- grepl("BC_3", myDF[, "BC_USGS"])
  myDF[, "BC_4"] <- grepl("BC_4", myDF[, "BC_USGS"])
  myDF[, "BC_5"] <- grepl("BC_5", myDF[, "BC_USGS"])
  myDF[, "PT_1"] <- grepl("PT_1", myDF[, "PT_USGS"])
  myDF[, "PT_2"] <- grepl("PT_2", myDF[, "PT_USGS"])
  myDF[, "PT_3"] <- grepl("PT_3", myDF[, "PT_USGS"])
  myDF[, "PT_4"] <- grepl("PT_4", myDF[, "PT_USGS"])
  myDF[, "PT_5"] <- grepl("PT_5", myDF[, "PT_USGS"])
  myDF[, "O_1"]  <- grepl("O_1", myDF[, "O_USGS"])
  myDF[, "O_2"]  <- grepl("O_2", myDF[, "O_USGS"])
  myDF[, "O_3"]  <- grepl("O_3", myDF[, "O_USGS"])
  myDF[, "O_4"]  <- grepl("O_4", myDF[, "O_USGS"])
  myDF[, "O_5"]  <- grepl("O_5", myDF[, "O_USGS"])
  myDF[, "SALINITY_1"] <- grepl("SALINITY_1", myDF[, "SALINITY_USGS"])
  myDF[, "SALINITY_2"] <- grepl("SALINITY_2", myDF[, "SALINITY_USGS"])
  myDF[, "SALINITY_3"] <- grepl("SALINITY_3", myDF[, "SALINITY_USGS"])
  myDF[, "SALINITY_4"] <- grepl("SALINITY_4", myDF[, "SALINITY_USGS"])
  myDF[, "HIGH_P"]     <- grepl("HIGH_P", myDF[, "P_USGS"])
  myDF[, "LOW_P"]      <- grepl("LOW_P", myDF[, "P_USGS"])
  myDF[, "HIGH_N"]     <- grepl("HIGH_N", myDF[, "N_USGS"])
  myDF[, "LOW_N"]      <- grepl("LOW_N", myDF[, "N_USGS"])
  myDF[, "BENTHIC_HABIT"]  <- grepl("BENTHIC_HABIT", myDF[, "HABITAT_USGS"])
  myDF[, "SESTONIC_HABIT"] <- grepl("SESTONIC_HABIT", myDF[, "HABITAT_USGS"])
  myDF[, "BAHLS_1"]   <- grepl("BAHLS_1", myDF[, "BAHLS_USGS"])
  myDF[, "BAHLS_2"]   <- grepl("BAHLS_2", myDF[, "BAHLS_USGS"])
  myDF[, "BAHLS_3"]   <- grepl("BAHLS_3", myDF[, "BAHLS_USGS"])
  myDF[, "TROPHIC_1"] <- grepl("TROPHIC_1", myDF[, "TROPHIC_USGS"])
  myDF[, "TROPHIC_2"] <- grepl("TROPHIC_2", myDF[, "TROPHIC_USGS"])
  myDF[, "TROPHIC_3"] <- grepl("TROPHIC_3", myDF[, "TROPHIC_USGS"])
  myDF[, "TROPHIC_4"] <- grepl("TROPHIC_4", myDF[, "TROPHIC_USGS"])
  myDF[, "TROPHIC_5"] <- grepl("TROPHIC_5", myDF[, "TROPHIC_USGS"])
  myDF[, "TROPHIC_6"] <- grepl("TROPHIC_6", myDF[, "TROPHIC_USGS"])
  myDF[, "TROPHIC_7"] <- grepl("TROPHIC_7", myDF[, "TROPHIC_USGS"])
  myDF[, "SAP_1"]     <- grepl("SAP_1", myDF[, "SAP_USGS"])
  myDF[, "SAP_2"]     <- grepl("SAP_2", myDF[, "SAP_USGS"])
  myDF[, "SAP_3"]     <- grepl("SAP_3", myDF[, "SAP_USGS"])
  myDF[, "SAP_4"]     <- grepl("SAP_4", myDF[, "SAP_USGS"])
  myDF[, "SAP_5"]     <- grepl("SAP_5", myDF[, "SAP_USGS"])
  myDF[, "NON_N_FIXER"]       <- grepl("NON_N_FIXER", myDF[, "N_FIXER_USGS"])
  myDF[, "N_FIXER"]           <- grepl("\\bN_FIXER\\b", myDF[, "N_FIXER_USGS"])
  myDF[, "HIGHLY_MOTILE"]     <- grepl("HIGHLY_MOTILE", myDF[, "MOTILITY_USGS"])
  myDF[, "MODERATELY_MOTILE"] <- grepl("MODERATELY_MOTILE", myDF[, "MOTILITY_USGS"])
  myDF[, "NON_MOTILE"]        <- grepl("NON_MOTILE", myDF[, "MOTILITY_USGS"])
  myDF[, "SLIGHTLY_MOTILE"]   <- grepl("SLIGHTLY_MOTILE", myDF[, "MOTILITY_USGS"])
  myDF[, "WEAKLY_MOTILE"]     <- grepl("WEAKLY_MOTILE", myDF[, "MOTILITY_USGS"])
  myDF[, "BIG"]        <- grepl("\\bBIG\\b", myDF[, "SIZE_USGS"])
  myDF[, "MEDIUM"]     <- grepl("MEDIUM", myDF[, "SIZE_USGS"])
  myDF[, "SMALL"]      <- grepl("\\bSMALL\\b", myDF[, "SIZE_USGS"])
  myDF[, "VERY_BIG"]   <- grepl("VERY_BIG", myDF[, "SIZE_USGS"])
  myDF[, "VERY_SMALL"] <- grepl("VERY_SMALL", myDF[, "SIZE_USGS"])
  myDF[, "ADNATE"]     <- grepl("ADNATE", myDF[, "HABIT_USGS"])
  myDF[, "STALKED"]    <- grepl("STALKED", myDF[, "HABIT_USGS"])
  myDF[, "HIGHLY_MOTILE.1"] <- grepl("HIGHLY_MOTILE.1", myDF[, "MOTILE2_USGS"])
  myDF[, "ARAPHID"]         <- grepl("ARAPHID", myDF[, "MOTILE2_USGS"])
  myDF[, "REF_INDICATORS"]  <- grepl("^REF", myDF[, "DIATOM_ISA"])


  # Metric Calc----

  # Calculate Metrics (could have used pipe, %>%)
    met.val <- dplyr::summarise(dplyr::group_by(myDF, SAMPLEID, INDEX_NAME
                                                , INDEX_CLASS)
                #
                ## Individuals ----
                , ni_total = sum(N_TAXA, na.rm = TRUE)
                , li_total = log(ni_total)

                ## Number of Taxa ----
                , nt_total = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                      & N_TAXA > 0]
                                               , na.rm = TRUE)

                ### Phylo----
                , nt_Achnan_Navic = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                    & (GENUS == "Achnanthidium"
                                                        | GENUS == "Navicula")]
                                                      , na.rm = TRUE)
                ### N_USGS----
                , nt_LOW_N = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                      & LOW_N == TRUE]
                                               , na.rm = TRUE)
                , nt_HIGH_N = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                      & HIGH_N == TRUE]
                                               , na.rm = TRUE)
                ### P_USGS----
                , nt_LOW_P = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                      & LOW_P == TRUE]
                                               , na.rm = TRUE)
                , nt_HIGH_P = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                       & HIGH_P == TRUE]
                                                , na.rm = TRUE)
                ### BC_USGS----
                , nt_BC_1 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                      & BC_1 == TRUE]
                                               , na.rm = TRUE)
                , nt_BC_2 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                     & BC_2 == TRUE]
                                              , na.rm = TRUE)
                , nt_BC_3 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                     & BC_3 == TRUE]
                                              , na.rm = TRUE)
                , nt_BC_4 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                     & BC_4 == TRUE]
                                              , na.rm = TRUE)
                , nt_BC_5 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                     & BC_5 == TRUE]
                                              , na.rm = TRUE)
                , nt_BC_12 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                      & (BC_1 == TRUE
                                                          | BC_2 == TRUE)]
                                               , na.rm = TRUE)
                , nt_BC_45 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                      & (BC_4 == TRUE
                                                          | BC_5 == TRUE)]
                                               , na.rm = TRUE)

                ### PT_USGS----
                , nt_PT_1 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                      & PT_1 == TRUE]
                                               , na.rm = TRUE)
                , nt_PT_2 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                     & PT_2 == TRUE]
                                              , na.rm = TRUE)
                , nt_PT_3 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                     & PT_3 == TRUE]
                                              , na.rm = TRUE)
                , nt_PT_4 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                     & PT_4 == TRUE]
                                              , na.rm = TRUE)
                , nt_PT_5 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                     & PT_5 == TRUE]
                                              , na.rm = TRUE)
                , nt_PT_12 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                      & (PT_1 == TRUE
                                                          | PT_2 == TRUE)]
                                               , na.rm = TRUE)
                ### SALINITY_USGS----
                , nt_SALINITY_1 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                          & SALINITY_1 == TRUE]
                                                     , na.rm = TRUE)
                , nt_SALINITY_2 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                          & SALINITY_2 == TRUE]
                                                    , na.rm = TRUE)
                , nt_SALINITY_3 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                          & SALINITY_3 == TRUE]
                                                    , na.rm = TRUE)
                , nt_SALINITY_4 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                          & SALINITY_4 == TRUE]
                                                    , na.rm = TRUE)
                , nt_SALINITY_12 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                          & (SALINITY_1 == TRUE
                                                          | SALINITY_2 == TRUE)]
                                                     , na.rm = TRUE)
                , nt_SALINITY_34 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                        & (SALINITY_3 == TRUE
                                                           | SALINITY_4 == TRUE)]
                                                     , na.rm = TRUE)
                ### O_USGS----
                , nt_O_1 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                      & O_1 == TRUE]
                                               , na.rm = TRUE)
                , nt_O_2 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                    & O_2 == TRUE]
                                             , na.rm = TRUE)
                , nt_O_3 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                    & O_3 == TRUE]
                                             , na.rm = TRUE)
                , nt_O_4 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                    & O_4 == TRUE]
                                             , na.rm = TRUE)
                , nt_O_5 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                    & O_5 == TRUE]
                                             , na.rm = TRUE)
                , nt_O_345 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                      & (O_3 == TRUE
                                                          | O_4 == TRUE
                                                          | O_5 == TRUE)]
                                               , na.rm = TRUE)
                ### HABITAT_USGS----
                , nt_SESTONIC_HABIT = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                      & SESTONIC_HABIT == TRUE]
                                                        , na.rm = TRUE)
                , nt_BENTHIC_HABIT = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                        & BENTHIC_HABIT == TRUE]
                                                        , na.rm = TRUE)
                ### BAHLS_USGS----
                , nt_BAHLS_1 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                            & (BAHLS_1 == TRUE)]
                                                        , na.rm = TRUE)
                , nt_BAHLS_2 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                        & (BAHLS_2 == TRUE)]
                                                 , na.rm = TRUE)
                , nt_BAHLS_3 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                        & (BAHLS_3 == TRUE)]
                                                 , na.rm = TRUE)
                ### TROPHIC_USGS----
                , nt_TROPHIC_1 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                            & TROPHIC_1 == TRUE]
                                                     , na.rm = TRUE)
                , nt_TROPHIC_2 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                          & TROPHIC_2 == TRUE]
                                                   , na.rm = TRUE)
                , nt_TROPHIC_3 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                          & TROPHIC_3 == TRUE]
                                                   , na.rm = TRUE)
                , nt_TROPHIC_4 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                          & TROPHIC_4 == TRUE]
                                                   , na.rm = TRUE)
                , nt_TROPHIC_5 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                          & TROPHIC_5 == TRUE]
                                                   , na.rm = TRUE)
                , nt_TROPHIC_6 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                          & TROPHIC_6 == TRUE]
                                                   , na.rm = TRUE)
                , nt_TROPHIC_7 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                          & TROPHIC_7 == TRUE]
                                                   , na.rm = TRUE)
                , nt_TROPHIC_12 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                           & (TROPHIC_1 == TRUE
                                                               | TROPHIC_2 == TRUE)]
                                                    , na.rm = TRUE)

                , nt_TROPHIC_456 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                      & (TROPHIC_4 == TRUE
                                                          | TROPHIC_5 == TRUE
                                                          | TROPHIC_6 == TRUE)]
                                               , na.rm = TRUE)
                , nt_TROPHIC_56 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                           & (TROPHIC_5 == TRUE
                                                               | TROPHIC_6 == TRUE)]
                                                    , na.rm = TRUE)

                ### SAP_USGS----
                , nt_SAP_1 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                          & SAP_1 == TRUE]
                                                   , na.rm = TRUE)
                , nt_SAP_2 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                      & SAP_2 == TRUE]
                                               , na.rm = TRUE)
                , nt_SAP_3 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                      & SAP_3 == TRUE]
                                               , na.rm = TRUE)
                , nt_SAP_4 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                      & SAP_4 == TRUE]
                                               , na.rm = TRUE)
                , nt_SAP_5 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                      & SAP_5 == TRUE]
                                               , na.rm = TRUE)
                ### N_FIXER_USGS----
                , nt_NON_N_FIXER = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                      & NON_N_FIXER == TRUE]
                                               , na.rm = TRUE)
                , nt_N_FIXER = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                            & N_FIXER == TRUE]
                                                     , na.rm = TRUE)
                ### MOTILITY_USGS----
                , nt_HIGHLY_MOTILE = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                        & HIGHLY_MOTILE == TRUE]
                                                      , na.rm = TRUE)
                , nt_MODERATELY_MOTILE = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                    & MODERATELY_MOTILE == TRUE]
                                                       , na.rm = TRUE)
                , nt_NON_MOTILE = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                          & NON_MOTILE == TRUE]
                                                           , na.rm = TRUE)
                , nt_SLIGHTLY_MOTILE = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                      & SLIGHTLY_MOTILE == TRUE]
                                                    , na.rm = TRUE)
                , nt_WEAKLY_MOTILE = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                        & WEAKLY_MOTILE == TRUE]
                                                         , na.rm = TRUE)

                ### SIZE_USGS----
                , nt_BIG = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                     & BIG == TRUE]
                                              , na.rm = TRUE)
                , nt_SMALL = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                     & SMALL == TRUE]
                                              , na.rm = TRUE)
                , nt_MEDIUM = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                     & MEDIUM == TRUE]
                                              , na.rm = TRUE)
                , nt_VERY_BIG = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                    & VERY_BIG == TRUE]
                                             , na.rm = TRUE)
                , nt_VERY_SMALL = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                      & VERY_SMALL == TRUE]
                                               , na.rm = TRUE)
                ### HABIT_USGS----
                , nt_ADNATE = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                       & ADNATE == TRUE]
                                                , na.rm = TRUE)
                , nt_STALKED = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                        & STALKED == TRUE]
                                                 , na.rm = TRUE)

                ### MOTILE2_USGS----
                , nt_HIGHLY_MOTILE.1 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                      & HIGHLY_MOTILE.1 == TRUE]
                                                         , na.rm = TRUE)
                , nt_ARAPHID = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                        & ARAPHID == TRUE]
                                                 , na.rm = TRUE)

                ### DIAT_CL----
                , nt_DIAT_CL_1 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                          & DIAT_CL == 1]
                                                   , na.rm = TRUE)
                , nt_DIAT_CL_2 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                          & DIAT_CL == 2]
                                                   , na.rm = TRUE)

                ### BEN_SES----
                , nt_BEN_SES_1 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                          & BEN_SES == 1]
                                                   , na.rm = TRUE)
                , nt_BEN_SES_2 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                          & BEN_SES == 2]
                                                   , na.rm = TRUE)
                ### DIAT_CA----
                , nt_DIAT_CA_1 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                          & DIAT_CA == 1]
                                                   , na.rm = TRUE)
                , nt_DIAT_CA_2 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                          & DIAT_CA == 2]
                                                   , na.rm = TRUE)
                ### DIAT_COND----
                , nt_DIAT_COND_1 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                          & DIAT_COND == 1]
                                                   , na.rm = TRUE)
                , nt_DIAT_COND_2 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                            & DIAT_COND == 2]
                                                     , na.rm = TRUE)
                ### DIATAS----
                , nt_DIATAS_TN_1 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                            & DIATAS_TN == 1]
                                                     , na.rm = TRUE)
                , nt_DIATAS_TN_2 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                            & DIATAS_TN == 2]
                                                     , na.rm = TRUE)
                , nt_DIATAS_TP_1 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                            & DIATAS_TP == 1]
                                                     , na.rm = TRUE)
                , nt_DIATAS_TP_2 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                            & DIATAS_TP == 2]
                                                     , na.rm = TRUE)
                ### MOTILITY----
                , nt_MOTILITY_1 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                            & MOTILITY == 1]
                                                     , na.rm = TRUE)
                , nt_MOTILITY_2 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                           & MOTILITY == 2]
                                                    , na.rm = TRUE)
                ### NF----
                , nt_NF_1 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                           & NF == 1]
                                                    , na.rm = TRUE)
                , nt_NF_2 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                     & NF == 2]
                                              , na.rm = TRUE)


                ## Percent Individuals----
                ### Phylo----
                , pi_Achnan_Navic = 100 * sum(N_TAXA[GENUS == "Achnanthidium"
                                                   | GENUS == "Navicula"]
                                            , na.rm = TRUE) / ni_total
                ### N_USGS---
                , pi_HIGH_N = 100 * sum(N_TAXA[HIGH_N == TRUE]
                                      , na.rm = TRUE) / ni_total
                , pi_LOW_N = 100 * sum(N_TAXA[LOW_N == TRUE]
                                      , na.rm = TRUE) / ni_total
                ### P_USGS---
                , pi_HIGH_P = 100 * sum(N_TAXA[HIGH_P == TRUE]
                                      , na.rm = TRUE) / ni_total
                , pi_LOW_P = 100 * sum(N_TAXA[LOW_P == TRUE]
                                      , na.rm = TRUE) / ni_total
                ### BC_USGS---
                , pi_BC_1 = 100 * sum(N_TAXA[BC_1 == TRUE]
                                    , na.rm = TRUE) / ni_total
                , pi_BC_2 = 100 * sum(N_TAXA[BC_2 == TRUE]
                                    , na.rm = TRUE) / ni_total
                , pi_BC_3 = 100 * sum(N_TAXA[BC_3 == TRUE]
                                    , na.rm = TRUE) / ni_total
                , pi_BC_4 = 100 * sum(N_TAXA[BC_4 == TRUE]
                                    , na.rm = TRUE) / ni_total
                , pi_BC_5 = 100 * sum(N_TAXA[BC_5 == TRUE]
                                    , na.rm = TRUE) / ni_total
                , pi_BC_12 = 100 * sum(N_TAXA[BC_1 == TRUE
                                            | BC_2 == TRUE]
                                     , na.rm = TRUE) / ni_total
                ### PT_USGS---
                , pi_PT_1 = 100 * sum(N_TAXA[PT_1 == TRUE]
                                     , na.rm = TRUE) / ni_total
                , pi_PT_2 = 100 * sum(N_TAXA[PT_2 == TRUE]
                                    , na.rm = TRUE) / ni_total
                , pi_PT_3 = 100 * sum(N_TAXA[PT_3 == TRUE]
                                    , na.rm = TRUE) / ni_total
                , pi_PT_4 = 100 * sum(N_TAXA[PT_4 == TRUE]
                                    , na.rm = TRUE) / ni_total
                , pi_PT_5 = 100 * sum(N_TAXA[PT_5 == TRUE]
                                    , na.rm = TRUE) / ni_total
                , pi_PT_45 = 100 * sum(N_TAXA[PT_4 == TRUE
                                            | PT_5 == TRUE]
                                     , na.rm = TRUE) / ni_total
                ### SALINITY_USGS---
                , pi_SALINITY_1 = 100 * sum(N_TAXA[SALINITY_1 == TRUE]
                                          , na.rm = TRUE) / ni_total
                , pi_SALINITY_2 = 100 * sum(N_TAXA[SALINITY_2 == TRUE]
                                          , na.rm = TRUE) / ni_total
                , pi_SALINITY_3 = 100 * sum(N_TAXA[SALINITY_3 == TRUE]
                                          , na.rm = TRUE) / ni_total
                , pi_SALINITY_4 = 100 * sum(N_TAXA[SALINITY_4 == TRUE]
                                     , na.rm = TRUE) / ni_total
                ### O_USGS---
                , pi_O_1 = 100 * sum(N_TAXA[O_1 == TRUE]
                                          , na.rm = TRUE) / ni_total
                , pi_O_2 = 100 * sum(N_TAXA[O_2 == TRUE]
                                   , na.rm = TRUE) / ni_total
                , pi_O_3 = 100 * sum(N_TAXA[O_3 == TRUE]
                                   , na.rm = TRUE) / ni_total
                , pi_O_4 = 100 * sum(N_TAXA[O_4 == TRUE]
                                   , na.rm = TRUE) / ni_total
                , pi_O_5 = 100 * sum(N_TAXA[O_5 == TRUE]
                                   , na.rm = TRUE) / ni_total
                ### HABITAT_USGS---
                , pi_SESTONIC_HABIT = 100 * sum(N_TAXA[SESTONIC_HABIT == TRUE]
                                              , na.rm = TRUE) / ni_total
                , pi_BENTHIC_HABIT = 100 * sum(N_TAXA[BENTHIC_HABIT == TRUE]
                                             , na.rm = TRUE) / ni_total
                ### BAHLS_USGS---
                , pi_BAHLS_1 = 100 * sum(N_TAXA[BAHLS_1 == TRUE]
                                   , na.rm = TRUE) / ni_total
                , pi_BAHLS_2 = 100 * sum(N_TAXA[BAHLS_2 == TRUE]
                                       , na.rm = TRUE) / ni_total
                , pi_BAHLS_3 = 100 * sum(N_TAXA[BAHLS_3 == TRUE]
                                       , na.rm = TRUE) / ni_total
                ### TROPHIC_USGS---
                , pi_TROPHIC_1 = 100 * sum(N_TAXA[TROPHIC_1 == TRUE]
                                       , na.rm = TRUE) / ni_total
                , pi_TROPHIC_2 = 100 * sum(N_TAXA[TROPHIC_2 == TRUE]
                                         , na.rm = TRUE) / ni_total
                , pi_TROPHIC_3 = 100 * sum(N_TAXA[TROPHIC_3 == TRUE]
                                         , na.rm = TRUE) / ni_total
                , pi_TROPHIC_4 = 100 * sum(N_TAXA[TROPHIC_4 == TRUE]
                                         , na.rm = TRUE) / ni_total
                , pi_TROPHIC_5 = 100 * sum(N_TAXA[TROPHIC_5 == TRUE]
                                         , na.rm = TRUE) / ni_total
                , pi_TROPHIC_6 = 100 * sum(N_TAXA[TROPHIC_6 == TRUE]
                                         , na.rm = TRUE) / ni_total
                , pi_TROPHIC_7 = 100 * sum(N_TAXA[TROPHIC_7 == TRUE]
                                         , na.rm = TRUE) / ni_total
                ### SAP_USGS----
                , pi_SAP_1 = 100 * sum(N_TAXA[SAP_1 == TRUE]
                                         , na.rm = TRUE) / ni_total
                , pi_SAP_2 = 100 * sum(N_TAXA[SAP_2 == TRUE]
                                     , na.rm = TRUE) / ni_total
                , pi_SAP_3 = 100 * sum(N_TAXA[SAP_3 == TRUE]
                                     , na.rm = TRUE) / ni_total
                , pi_SAP_4 = 100 * sum(N_TAXA[SAP_4 == TRUE]
                                     , na.rm = TRUE) / ni_total
                , pi_SAP_5 = 100 * sum(N_TAXA[SAP_5 == TRUE]
                                     , na.rm = TRUE) / ni_total
                ### N_FIXER_USGS----
                , pi_NON_N_FIXER = 100 * sum(N_TAXA[NON_N_FIXER == TRUE]
                                     , na.rm = TRUE) / ni_total
                , pi_N_FIXER = 100 * sum(N_TAXA[N_FIXER == TRUE]
                                           , na.rm = TRUE) / ni_total

                ### MOTILITY_USGS----
                , pi_HIGHLY_MOTILE = 100 * sum(N_TAXA[HIGHLY_MOTILE == TRUE]
                                           , na.rm = TRUE) / ni_total
                , pi_MODERATELY_MOTILE = 100 * sum(N_TAXA[MODERATELY_MOTILE == TRUE]
                                             , na.rm = TRUE) / ni_total
                , pi_NON_MOTILE = 100 * sum(N_TAXA[NON_MOTILE == TRUE]
                                                 , na.rm = TRUE) / ni_total
                , pi_SLIGHTLY_MOTILE = 100 * sum(N_TAXA[SLIGHTLY_MOTILE == TRUE]
                                          , na.rm = TRUE) / ni_total
                , pi_WEAKLY_MOTILE = 100 * sum(N_TAXA[WEAKLY_MOTILE == TRUE]
                                               , na.rm = TRUE) / ni_total

                ### SIZE_USGS----
                , pi_BIG = 100 * sum(N_TAXA[BIG == TRUE]
                                             , na.rm = TRUE) / ni_total
                , pi_SMALL = 100 * sum(N_TAXA[SMALL == TRUE]
                                   , na.rm = TRUE) / ni_total
                , pi_MEDIUM = 100 * sum(N_TAXA[MEDIUM == TRUE]
                                     , na.rm = TRUE) / ni_total
                , pi_VERY_BIG = 100 * sum(N_TAXA[VERY_BIG == TRUE]
                                   , na.rm = TRUE) / ni_total
                , pi_VERY_SMALL = 100 * sum(N_TAXA[VERY_SMALL == TRUE]
                                     , na.rm = TRUE) / ni_total

                ### HABIT_USGS----
                , pi_ADNATE = 100 * sum(N_TAXA[ADNATE == TRUE]
                                          , na.rm = TRUE) / ni_total
                , pi_STALKED = 100 * sum(N_TAXA[STALKED == TRUE]
                                      , na.rm = TRUE) / ni_total

                ### MOTILE2_USGS----
                , pi_HIGHLY_MOTILE.1 = 100 * sum(N_TAXA[HIGHLY_MOTILE.1 == TRUE]
                                       , na.rm = TRUE) / ni_total
                , pi_ARAPHID = 100 * sum(N_TAXA[ARAPHID == TRUE]
                                       , na.rm = TRUE) / ni_total

                ### DIAT_CL----
                , pi_DIAT_CL_1 = 100 * sum(N_TAXA[DIAT_CL == 1]
                                         , na.rm = TRUE) / ni_total
                , pi_DIAT_CL_1_ASSR = 100 * (asin(sqrt(pi_DIAT_CL_1 / 100)))
                , pi_DIAT_CL_2 = 100 * sum(N_TAXA[DIAT_CL == 2]
                                         , na.rm = TRUE) / ni_total

                ### BEN_SES----
                , pi_BEN_SES_1 = 100 * sum(N_TAXA[BEN_SES == 1]
                                         , na.rm = TRUE) / ni_total
                , pi_BEN_SES_2 = 100 * sum(N_TAXA[BEN_SES == 2]
                                         , na.rm = TRUE) / ni_total
                ### DIAT_CA----
                , pi_DIAT_CA_1 = 100 * sum(N_TAXA[DIAT_CA == 1]
                                         , na.rm = TRUE) / ni_total
                , pi_DIAT_CA_2 = 100 * sum(N_TAXA[DIAT_CA == 2]
                                         , na.rm = TRUE) / ni_total
                ### DIAT_COND----
                , pi_DIAT_COND_1 = 100 * sum(N_TAXA[DIAT_COND == 1]
                                         , na.rm = TRUE) / ni_total
                , pi_DIAT_COND_2 = 100 * sum(N_TAXA[DIAT_COND == 2]
                                           , na.rm = TRUE) / ni_total
                ### DIATAS----
                , pi_DIATAS_TN_1 = 100 * sum(N_TAXA[DIATAS_TN == 1]
                                           , na.rm = TRUE) / ni_total
                , pi_DIATAS_TN_2 = 100 * sum(N_TAXA[DIATAS_TN == 2]
                                           , na.rm = TRUE) / ni_total
                , pi_DIATAS_TP_1 = 100 * sum(N_TAXA[DIATAS_TP == 1]
                                           , na.rm = TRUE) / ni_total
                , pi_DIATAS_TP_2 = 100 * sum(N_TAXA[DIATAS_TP == 2]
                                           , na.rm = TRUE) / ni_total
                ### MOTILITY----
                , pi_MOTILITY_1 = 100 * sum(N_TAXA[MOTILITY == 1]
                                           , na.rm = TRUE) / ni_total
                , pi_MOTILITY_2 = 100 * sum(N_TAXA[MOTILITY == 2]
                                          , na.rm = TRUE) / ni_total
                ### NF----
                , pi_NF_1 = 100 * sum(N_TAXA[NF == 1]
                                          , na.rm = TRUE) / ni_total
                , pi_NF_2 = 100 * sum(N_TAXA[NF == 2]
                                    , na.rm = TRUE) / ni_total

                ## Percent of Taxa----
                ### Phylo----
                , pt_Achnan_Navic = 100 * nt_Achnan_Navic / nt_total
                ### N_USGS----
                , pt_HIGH_N = 100 * nt_HIGH_N / nt_total
                , pt_LOW_N = 100 * nt_LOW_N / nt_total
                ### P_USGS----
                , pt_HIGH_P = 100 * nt_HIGH_P / nt_total
                , pt_LOW_P = 100 * nt_LOW_P / nt_total
                ### BC_USGS----
                , pt_BC_1 = 100 * nt_BC_1 / nt_total
                , pt_BC_2 = 100 * nt_BC_2 / nt_total
                , pt_BC_3 = 100 * nt_BC_3 / nt_total
                , pt_BC_4 = 100 * nt_BC_4 / nt_total
                , pt_BC_5 = 100 * nt_BC_5 / nt_total
                , pt_BC_12 = 100 * nt_BC_12 / nt_total
                , pt_BC_12_adj = NA_real_
                , pt_BC_45 = 100 * nt_BC_45 / nt_total

                ### PT_USGS----
                , pt_PT_1 = 100 * nt_PT_1 / nt_total
                , pt_PT_2 = 100 * nt_PT_2 / nt_total
                , pt_PT_3 = 100 * nt_PT_3 / nt_total
                , pt_PT_4 = 100 * nt_PT_4 / nt_total
                , pt_PT_5 = 100 * nt_PT_5 / nt_total
                , pt_PT_12 = 100 * nt_PT_12 / nt_total

                ### SALINITY_USGS----
                , pt_SALINITY_1 = 100 * nt_SALINITY_1 / nt_total
                , pt_SALINITY_2 = 100 * nt_SALINITY_2 / nt_total
                , pt_SALINITY_3 = 100 * nt_SALINITY_3 / nt_total
                , pt_SALINITY_4 = 100 * nt_SALINITY_4 / nt_total
                , pt_SALINITY_34 = 100 * nt_SALINITY_34 / nt_total

                ### O_USGS----
                , pt_O_1 = 100 * nt_O_1 / nt_total
                , pt_O_2 = 100 * nt_O_2 / nt_total
                , pt_O_3 = 100 * nt_O_3 / nt_total
                , pt_O_4 = 100 * nt_O_4 / nt_total
                , pt_O_5 = 100 * nt_O_5 / nt_total
                , pt_O_345 = 100 * nt_O_345 / nt_total

                ### HABITAT_USGS----
                , pt_SESTONIC_HABIT = 100 * nt_SESTONIC_HABIT / nt_total
                , pt_BENTHIC_HABIT = 100 * nt_BENTHIC_HABIT / nt_total

                ### BAHLS_USGS----
                , pt_BAHLS_1 = 100 * nt_BAHLS_1 / nt_total
                , pt_BAHLS_2 = 100 * nt_BAHLS_2 / nt_total
                , pt_BAHLS_3 = 100 * nt_BAHLS_3 / nt_total

                ### TROPHIC_USGS----
                , pt_TROPHIC_1 = 100 * nt_TROPHIC_1 / nt_total
                , pt_TROPHIC_2 = 100 * nt_TROPHIC_2 / nt_total
                , pt_TROPHIC_3 = 100 * nt_TROPHIC_3 / nt_total
                , pt_TROPHIC_4 = 100 * nt_TROPHIC_4 / nt_total
                , pt_TROPHIC_5 = 100 * nt_TROPHIC_5 / nt_total
                , pt_TROPHIC_6 = 100 * nt_TROPHIC_6 / nt_total
                , pt_TROPHIC_7 = 100 * nt_TROPHIC_7 / nt_total
                , pt_TROPHIC_12 = 100 * nt_TROPHIC_12 / nt_total
                , pt_TROPHIC_456 = 100 * nt_TROPHIC_456 / nt_total
                , pt_TROPHIC_56 = 100 * nt_TROPHIC_56 / nt_total

                ### SAP_USGS----
                , pt_SAP_1 = 100 * nt_SAP_1 / nt_total
                , pt_SAP_2 = 100 * nt_SAP_2 / nt_total
                , pt_SAP_3 = 100 * nt_SAP_3 / nt_total
                , pt_SAP_4 = 100 * nt_SAP_4 / nt_total
                , pt_SAP_5 = 100 * nt_SAP_5 / nt_total

                ### N_FIXER_USGS----
                , pt_NON_N_FIXER = 100 * nt_NON_N_FIXER / nt_total
                , pt_N_FIXER = 100 * nt_N_FIXER / nt_total

                ### MOTILITY_USGS----
                , pt_HIGHLY_MOTILE = 100 * nt_HIGHLY_MOTILE / nt_total
                , pt_MODERATELY_MOTILE = 100 * nt_MODERATELY_MOTILE / nt_total
                , pt_NON_MOTILE = 100 * nt_NON_MOTILE / nt_total
                , pt_SLIGHTLY_MOTILE = 100 * nt_SLIGHTLY_MOTILE / nt_total
                , pt_WEAKLY_MOTILE = 100 * nt_WEAKLY_MOTILE / nt_total

                ### SIZE_USGS----
                , pt_BIG = 100 * nt_BIG / nt_total
                , pt_SMALL = 100 * nt_SMALL / nt_total
                , pt_MEDIUM = 100 * nt_MEDIUM / nt_total
                , pt_VERY_BIG = 100 * nt_VERY_BIG / nt_total
                , pt_VERY_SMALL = 100 * nt_VERY_SMALL / nt_total

                ### HABIT_USGS----
                , pt_ADNATE = 100 * nt_ADNATE / nt_total
                , pt_STALKED = 100 * nt_STALKED / nt_total

                ### MOTILE2_USGS----
                , pt_HIGHLY_MOTILE.1 = 100 * nt_HIGHLY_MOTILE.1 / nt_total
                , pt_ARAPHID = 100 * nt_ARAPHID / nt_total

                ### DIAT_CL----
                , pt_DIAT_CL_1 = 100 * nt_DIAT_CL_1 / nt_total
                , pt_DIAT_CL_2 = 100 * nt_DIAT_CL_2 / nt_total

                ### BEN_SES----
                , pt_BEN_SES_1 = 100 * nt_BEN_SES_1 / nt_total
                , pt_BEN_SES_2 = 100 * nt_BEN_SES_2 / nt_total

                ### DIAT_CA----
                , pt_DIAT_CA_1 = 100 * nt_DIAT_CA_1 / nt_total
                , pt_DIAT_CA_2 = 100 * nt_DIAT_CA_2 / nt_total

                ### DIAT_COND----
                , pt_DIAT_COND_1 = 100 * nt_DIAT_COND_1 / nt_total
                , pt_DIAT_COND_2 = 100 * nt_DIAT_COND_2 / nt_total

                ### DIATAS----
                , pt_DIATAS_TN_1 = 100 * nt_DIATAS_TN_1 / nt_total
                , pt_DIATAS_TN_2 = 100 * nt_DIATAS_TN_2 / nt_total
                , pt_DIATAS_TP_1 = 100 * nt_DIATAS_TP_1 / nt_total
                , pt_DIATAS_TP_2 = 100 * nt_DIATAS_TP_2 / nt_total

                ### MOTILITY----
                , pt_MOTILITY_1 = 100 * nt_MOTILITY_1 / nt_total
                , pt_MOTILITY_2 = 100 * nt_MOTILITY_2 / nt_total

                ### NF----
                , pt_NF_1 = 100 * nt_NF_1 / nt_total
                , pt_NF_2 = 100 * nt_NF_2 / nt_total


                ## Tolerance----
                ### Number of Taxa----
                , nt_Sens_810 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE# DOES NOT FOLLOW NORMAL TOLVAL CONVENTION
                                                         & TOLVAL >= 8 # LOWER VALUES MORE TOLERANT (Indiana)
                                                         & TOLVAL <= 10]
                                                  , na.rm = TRUE)
                , nt_RefIndicators = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE # Diatom Indicator Species Analysis
                                                          & REF_INDICATORS == TRUE]
                                                       , na.rm = TRUE)
                , nt_Tol_13 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                       & TOLVAL >= 1# DOES NOT FOLLOW NORMAL TOLVAL CONVENTION
                                                       & TOLVAL <= 3]# LOWER VALUES MORE TOLERANT (Indiana)
                                                , na.rm = TRUE)

                ### Percent of Individuals----
                , pi_Sens_810 = 100 * sum(N_TAXA[TOLVAL >= 8
                                               & TOLVAL <= 10]
                                        , na.rm = TRUE) / sum(
                                          N_TAXA[!is.na(TOLVAL)], na.rm = TRUE)
                , pi_RefIndicators = 100 * sum(N_TAXA[REF_INDICATORS == TRUE], # Diatom Indicator Species Analysis
                                             na.rm = TRUE) / ni_total
                , pi_Tol_13 = 100 * sum(N_TAXA[TOLVAL >= 1 # DOES NOT FOLLOW NORMAL TOLVAL CONVENTION (Indiana)
                                        & TOLVAL <= 3], na.rm = TRUE) / sum(
                                          N_TAXA[!is.na(TOLVAL)], na.rm = TRUE) # LOWER VALUES MORE TOLERANT

                ### Percent of Taxa----
                , pt_Sens_810 = 100 * nt_Sens_810 / nt_total # DOES NOT FOLLOW NORMAL TOLVAL CONVENTION (Indiana)
                , pt_RefIndicators = 100 * nt_RefIndicators / nt_total
                , pt_Tol_13 = 100 * nt_Tol_13 / nt_total

                ### Weighted Average ----
                , wa_POLL_TOL = sum(N_TAXA[!is.na(POLL_TOL)]
                                     * POLL_TOL[!is.na(POLL_TOL)
                                              ]) / sum(N_TAXA[!is.na(POLL_TOL)])



                , .groups = "drop_last")##met.val.END

  # Clean Up ####
    # replace NA with 0
    met.val[is.na(met.val)] <- 0

  # subset to only metrics specified by user
    if (is.null(MetricNames)) {
      met.val <- met.val
    } else {
      met2include <- MetricNames[!(MetricNames %in% "ni_total")]
      # remove ni_total if included as will always include it
      met.val <- met.val[, c("SAMPLEID", "INDEX_CLASS", "INDEX_NAME",
                             "ni_total", met2include)]
    }##IF~MetricNames~END

    # Add extra fields
    if (is.null(cols2keep)) {##IF.is.null.cols2keep.START
      df.return <- as.data.frame(met.val)
    } else {
      # create df with grouped fields
      myDF.cols2keep <- myDF %>% dplyr::group_by(.dots = c("SAMPLEID"
                                                         , cols2keep)) %>%
        dplyr::summarize(col.drop = sum(N_TAXA))
      col.drop <- ncol(myDF.cols2keep)
      myDF.cols2keep <- myDF.cols2keep[,-col.drop]
      # merge
      df.return <- merge(as.data.frame(myDF.cols2keep)
                         , as.data.frame(met.val), by = "SAMPLEID")
    }##IF.is.null.cols2keep.END

    # Run Time
    time_end <- Sys.time()
    msg <- difftime(time_end, time_start)
    message(msg)

    # df to report back
    return(df.return)

}##FUNCTION.metric.values.algae.END
