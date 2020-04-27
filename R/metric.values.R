#' @title Calculate metric values
#'
#' @description This function calculates metric values for bugs and fish.
#' Inputs are a data frame with SampleID and taxa with phylogenetic and autecological information
#' (see below for required fields by community).  The dplyr package is used to generate the metric values.
#'
#' @details All percent metric results are 0-100.
#'
#' No manipulations of the taxa are performed by this routine.
#' All benthic macroinvertebrate taxa should be identified to the appropriate
#' operational taxonomic unit (OTU).
#'
#' Any non-count taxa should be identified in the "Exclude" field as "TRUE".
#' These taxa will be excluded from taxa richness metrics (but will count for
#' all others).  #' Any non-target taxa should be identified in the "NonTarget"
#' field as "TRUE".  Non-target taxa are those that are not part of your intended
#' capture list; e.g., fish,  herps, water column taxa, or water surface taxa in
#' a benthic sample.  The target list will vary by program.  The non-target taxa
#' will be removed prior to any calculations.
#'
#' Excluded taxa are ambiguous taxa (on a sample basis), i.e.,
#' the parent taxa when child taxa are present.  For example, the parent taxa
#' Chironomidae would be xcluded when the child taxa Tanytarsini is present.
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
#' Required Fields:
#'
#' * SAMPLEID (character or number, must be unique)
#'
#' * TAXAID (character or number, must be unique)
#'
#' * N_TAXA
#'
#' * EXCLUDE (valid values are TRUE and FALSE)
#'
#' * INDEX_NAME
#'
#' * INDEX_REGION (BCG or MMI site category; e.g., for BCG PacNW valid values are "hi" or "lo")
#'
#' * NONTARGET (valid values are TRUE and FALSE)
#'
#' * PHYLUM, SUBPHYLUM, CLASS, INFRAORDER, ORDER, FAMILY, SUBFAMILY, TRIBE, GENUS
#'
#' * FFG, HABIT, LIFE_CYCLE, TOLVAL, BCG_ATTR, THERMAL_INDICATOR, FFG2, TOLVAL2
#'
#' Valid values for FFG: CG, CF, PR, SC, SH
#'
#' Valid values for HABIT: BU, CB, CN, SP, SW
#'
#' Valid values for LIFE_CYCLE: UNI, SEMI, MULTI
#'
#' Valid values for THERMAL_INDICATOR: COLD, COLD_COOL, COOL_WARM, WARM
#'
#' Columns to keep are additional fields in the input file that the user wants
#' retained in the output.  Fields need to be those that are unique per sample
#' and not associated with the taxa.  For example, the fields used in qc.check();
#' Area_mi2, SurfaceArea, Density_m2, and Density_ft2.
#'
#' If fun.MetricNames is provided only those metrics will be returned in the provided order.
#' This variable can be used to sort the metrics per the user's preferences..
#' By default the metric names will be returned in the groupings that were used for calculation.
#'
#' The fields TOLVAL2 and FFG2 are provided to allow the user to calculate metrics
#' based on alternative scenarios.  For example, HBI and NCBI where the NCBI uses
#' a different set of tolerance values (TOLVAL2).
#'
#' If TAXAID is 'NONE' and N_TAXA is "0" then metrics **will** be calculated.
#' Other values for TAXAID with N_TAXA = 0 will be removed before calculations.
#'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @param fun.DF Data frame of taxa (list required fields)
#' @param fun.Community Community name for which to calculate metric values (bugs, fish, or algae)
#' @param fun.MetricNames Optional vector of metric names to be returned.
#' If none are supplied then all will be returned.  Default=NULL
#' @param boo.Adjust Optional boolean value on whether to perform adjustments of
#' values prior to scoring.  Default = FALSE but may be TRUE for certain metrics.
#' @param fun.cols2keep Column names of fun.DF to retain in the output.  Uses column names.
# @param MetricSort How metric names should be sort; NA = as is, AZ = alphabetical.  Default = NULL.
#' @param boo.marine Should estuary/marine metrics be included.
#' Ignored if fun.MetricNames is not null. Default = FALSE.
#'
#' @return data frame of SampleID and metric values
#' @examples
#' # Example data
#'
#' library(readxl)
#' library(reshape2)
#'
#' df_samps_bugs <- read_excel(system.file("extdata/Data_Benthos.xlsx"
#'                                        , package="BioMonTools")
#'                             , guess_max = 10^6)
#'
#' # Columns to keep
#' myCols <- c("Area_mi2", "SurfaceArea", "Density_m2", "Density_ft2")
#'
#' # Run Function
#' df_metric_values_bugs <- metric.values(df_samps_bugs, "bugs", fun.cols2keep=myCols)
#'
#'\dontrun{
#' # View Results
#' View(df_metric_values_bugs)
#' }
#'
#' # Get data in long format so can QC results more easily
#' df_long <- melt(df_metric_values_bugs, id.vars=c("SAMPLEID", "INDEX_NAME"
#'                                                  , "INDEX_REGION", toupper(myCols))
#'                           , variable.name="METRIC_NAME", value.name="METRIC_VALUE")
#'
#'\dontrun{
#' # Save Results
#' write.table(df_long, "metric.values.tsv", col.names=TRUE, row.names=FALSE, sep="\t")
#'
#' # DataExplorer Report
#' library(DataExplorer)
#' create_report(df_metric_values_bugs, "DataExplorer_Report_MetricValues.html")
#' create_report(df_samps_bugs, "DataExplorer_Report_BugSamples.html")
#' }
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Example 2, specific metrics or metrics in a specific order
#' ## reuse df_samps_bugs from above
#'
#' # metric names to keep (in this order)
#' myMetrics <- c("ni_total", "nt_EPT", "nt_Ephem", "pi_tv_intol", "pi_Ephem"
#'                , "nt_ffg_scrap", "pi_habit_climb")
#'
#' # Run Function
#' df_metric_values_bugs_myMetrics <- metric.values(df_samps_bugs, "bugs"
#'                                                 , fun.MetricNames = myMetrics)
#'\dontrun{
#' # View Results
#' View(df_metric_values_bugs_myMetrics)
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
# df.long <- melt(df.metric.values.bugs, id.vars=c("SAMPLEID", "INDEX_NAME", "INDEX_REGION")
#                           , variable.name="METRIC_NAME", value.name="METRIC_VALUE")
# # Save Results
# write.table(df.long, "metric.values.tsv", col.names=TRUE, row.names=FALSE, sep="\t")
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
# fun.DF[,"INDEX_REGION"] <- toupper(fun.DF[,"INDEX_REGION"])
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
# x <- dplyr::filter(myDF, SAMPLEID==mySamp, (BCG_ATTR == "4" | BCG_ATTR == "5" | BCG_ATTR == "6"))
# # 22 taxa (good)
# x <- dplyr::filter(myDF, SAMPLEID==mySamp
#                    , (is.na(CLASS)==TRUE | (CLASS != "Insecta" & CLASS != "Arachnida"))
#                    , (is.na(ORDER) == TRUE | (ORDER != "Decapoda" & ORDER!="Rissooidea"))
#                    , (is.na(GENUS) == TRUE | GENUS!="Juga")
#                    , (BCG_ATTR == "4" | BCG_ATTR == "5" | BCG_ATTR == "6")
#                    )
# # filter works here
# # 5 taxa and 202 ind.
#
# met.val <- dplyr::summarise(dplyr::group_by(x, SAMPLEID, INDEX_NAME, INDEX_REGION)
#                  # individuals #
#                  , ni_total=sum(N_TAXA)
#                  #
#                  , nt_NonInsArachDecaClump_BCG_att456 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
#                                & (is.na(CLASS)==TRUE | (CLASS != "Insecta" & CLASS != "Arachnida"))
#                                & (is.na(ORDER) == TRUE | (ORDER != "Decapoda" & ORDER!="Rissooidea"))
#                                & (is.na(GENUS) == TRUE | GENUS!="Juga")
#                                & (BCG_ATTR == "4" | BCG_ATTR == "5" | BCG_ATTR == "6")]
#                         , na.rm = TRUE)
#                  #
#                  , pi_NonInsArachDecaClump_BCG_att456 = sum(N_TAXA[
#                               (is.na(CLASS)==TRUE | (CLASS != "Insecta" & CLASS != "Arachnida"))
#                              & (is.na(ORDER) == TRUE | (ORDER != "Decapoda" & ORDER!="Rissooidea"))
#                              & (is.na(GENUS) == TRUE | GENUS!="Juga")
#                              & (BCG_ATTR == "4" | BCG_ATTR == "5" | BCG_ATTR == "6")]
#                       , na.rm=TRUE)
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
                          , fun.MetricNames=NULL
                          , boo.Adjust=FALSE
                          , fun.cols2keep=NULL
                          , boo.marine=FALSE){##FUNCTION.metric.values.START
  # define pipe
  `%>%` <- dplyr::`%>%`
  # Munge ####
  # Data Munging (common to all data types)
  # Convert to data.frame.  Code breaks if myDF is a tibble.
  fun.DF <- as.data.frame(fun.DF)
  # convert Field Names to UPPER CASE
  names(fun.DF) <- toupper(names(fun.DF))
  # convert cols2keep to UPPER CASE
  if(!is.null(fun.cols2keep)){
    #names(fun.cols2keep) <- toupper(fun.cols2keep)
    fun.cols2keep <- toupper(fun.cols2keep)
  }##IF~!is.null(fun.cols2keep)~END
  # Remove Count = 0 taxa unless TaxaID = NONE
  #fun.DF <- fun.DF[fun.DF[,"N_TAXA"]>0, ]
  fun.DF <- fun.DF %>% dplyr::filter(N_TAXA > 0 | TAXAID == "NONE")
  # non-target taxa removed in community function, if appropriate
  #
  # SiteType to upper case
  fun.DF[,"INDEX_REGION"] <- toupper(fun.DF[,"INDEX_REGION"])
  # convert community to upper case
  fun.Community <- toupper(fun.Community)
  # run the proper sub function
  if (fun.Community=="BUGS") {##IF.START
    metric.values.bugs(fun.DF, fun.MetricNames, boo.Adjust, fun.cols2keep, NA, boo.marine)
  } else if(fun.Community=="FISH"){
    metric.values.fish(fun.DF, fun.MetricNames, boo.Adjust, fun.cols2keep)
  # } else if(fun.Community=="ALGAE"){
  #   metric.values.algae(fun.DF, fun.MetricNames, boo.Adjust)
  }##IF.END
}##FUNCTION.metric.values.START
#
#
#' @export
metric.values.bugs <- function(myDF
                               , MetricNames=NULL
                               , boo.Adjust=FALSE
                               , cols2keep=NULL
                               , MetricSort=NA
                               , boo.marine=FALSE){##FUNCTION.metric.values.bugs.START
  #
  #names(myDF) <- toupper(names(myDF))
  # not carrying over from previous?!
  #
  # define pipe
  `%>%` <- dplyr::`%>%`
  # QC ####
  # QC, Required Fields
  col.req <- c("SAMPLEID", "TAXAID", "N_TAXA", "EXCLUDE", "INDEX_NAME"
              , "INDEX_REGION", "NONTARGET", "PHYLUM", "SUBPHYLUM", "CLASS", "INFRAORDER"
              , "ORDER", "FAMILY", "SUBFAMILY", "TRIBE", "GENUS", "FFG", "HABIT"
              , "LIFE_CYCLE", "TOLVAL", "BCG_ATTR", "THERMAL_INDICATOR"
              , "LONGLIVED", "NOTEWORTHY", "FFG2", "TOLVAL2")
  col.req.missing <- col.req[!(col.req %in% toupper(names(myDF)))]
  num.col.req.missing <- length(col.req.missing)
  # Trigger prompt if any missing fields (and session is interactive)
  if(num.col.req.missing!=0 & interactive()==TRUE){##IF.num.col.req.missing.START
    myPrompt.01 <- paste0("There are ",num.col.req.missing," missing fields in the data:")
    myPrompt.02 <- paste(col.req.missing, collapse=", ")
    myPrompt.03 <- "If you continue the metrics associated with these fields will be invalid."
    myPrompt.04 <- "For example, if the HABIT field is missing all habit related metrics will not be correct."
    myPrompt.05 <- "Do you wish to continue (YES or NO)?"

    myPrompt <- paste(" ", myPrompt.01, myPrompt.02, " ", myPrompt.03, myPrompt.04
                      , myPrompt.05, sep="\n")
    #user.input <- readline(prompt=myPrompt)
    user.input <- NA
    user.input <- utils::menu(c("YES", "NO"), title=myPrompt)
    # any answer other than "YES" will stop the function.
    if(user.input!=1){##IF.user.input.START
      stop(paste("The user chose *not* to continue due to missing fields: "
                  , paste(paste0("   ",col.req.missing), collapse="\n"),sep="\n"))
    }##IF.user.input.END
    # Add missing fields
    myDF[,col.req.missing] <- NA
    warning(paste("Metrics related to the following fields are invalid:"
                  , paste(paste0("   ", col.req.missing), collapse="\n"), sep="\n"))
  }##IF.num.col.req.missing.END

  # QC, Exclude as TRUE/FALSE
  Exclude.T <- sum(myDF$EXCLUDE==TRUE, na.rm=TRUE)
  if(Exclude.T==0){##IF.Exclude.T.START
    warning("EXCLUDE column does not have any TRUE values. \n  Valid values are TRUE or FALSE.  \n  Other values are not recognized.")
  }##IF.Exclude.T.END

  # QC, NonTarget as TRUE/FALSE
  NonTarget.F <- sum(myDF$NONTARGET==FALSE, na.rm=TRUE)
  if(NonTarget.F==0){##IF.Exclude.T.START
    warning("NONTARGET column does not have any FALSE values. \n  Valid values are TRUE or FALSE.  \n  Other values are not recognized.")
  }##IF.Exclude.T.END

  # QC, TolVal
  # need as numeric, if have "NA" as character it fails
  TolVal_Char_NA <- myDF[, "TOLVAL"]=="NA"
  if(sum(TolVal_Char_NA, na.rm=TRUE)>0){
    myDF[TolVal_Char_NA, "TOLVAL"] <- NA
    myDF[, "TOLVAL"] <- as.numeric(myDF[, "TOLVAL"])
  }

  # QC, TolVal
  # need as numeric, if have "NA" as character it fails
  TolVal2_Char_NA <- myDF[, "TOLVAL2"]=="NA"
  if(sum(TolVal2_Char_NA, na.rm=TRUE)>0){
    myDF[TolVal2_Char_NA, "TOLVAL2"] <- NA
    myDF[, "TOLVAL2"] <- as.numeric(myDF[, "TOLVAL2"])
  }

  # Data Munging ####
  # Convert values to upper case (FFG, Habit, Life_Cycle)
  myDF[, "HABIT"] <- toupper(myDF[, "HABIT"])
  myDF[, "FFG"] <- toupper(myDF[, "FFG"])
  myDF[, "LIFE_CYCLE"] <- toupper(myDF[, "LIFE_CYCLE"])
  myDF[, "THERMAL_INDICATOR"] <- toupper(myDF[, "THERMAL_INDICATOR"])
  myDF[, "FFG2"] <- toupper(myDF[, "FFG2"])
  # Add extra columns for FFG and Habit
  # (need unique values for functions in summarise)
  # each will be TRUE or FALSE
  # finds any match so "CN, CB" is both "CN" and "CB"
  myDF[, "HABIT_BU"] <- grepl("BU", myDF[, "HABIT"])
  myDF[, "HABIT_CB"] <- grepl("CB", myDF[, "HABIT"])
  myDF[, "HABIT_CN"] <- grepl("CN", myDF[, "HABIT"])
  myDF[, "HABIT_SP"] <- grepl("SP", myDF[, "HABIT"])
  myDF[, "HABIT_SW"] <- grepl("SW", myDF[, "HABIT"])
  myDF[, "FFG_COL"]  <- grepl("CG", myDF[, "FFG"])
  myDF[, "FFG_FIL"]  <- grepl("CF", myDF[, "FFG"])
  myDF[, "FFG_PRE"]  <- grepl("PR", myDF[, "FFG"])
  myDF[, "FFG_SCR"]  <- grepl("SC", myDF[, "FFG"])
  myDF[, "FFG_SHR"]  <- grepl("SH", myDF[, "FFG"])
  myDF[, "LC_MULTI"] <- grepl("MULTI", myDF[, "LIFE_CYCLE"])
  myDF[, "LC_SEMI"]  <- grepl("SEMI", myDF[, "LIFE_CYCLE"])
  myDF[, "LC_UNI"]   <- grepl("UNI", myDF[, "LIFE_CYCLE"])
  myDF[, "FFG2_PRE"]  <- grepl("PR", myDF[, "FFG2"])
  # exact matches only
  myDF[, "TI_COLD"]     <- "COLD"      == myDF[, "THERMAL_INDICATOR"]
  myDF[, "TI_COLDCOOL"] <- "COLD_COOL" == myDF[, "THERMAL_INDICATOR"]
  myDF[, "TI_COOLWARM"] <- "COOL_WARM" == myDF[, "THERMAL_INDICATOR"]
  myDF[, "TI_WARM"]     <- "WARM"      == myDF[, "THERMAL_INDICATOR"]
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
  #                 dplyr::group_by(SAMPLEID, INDEX_NAME, INDEX_REGION) %>%
  #                   dplyr::summarise(ni_total=sum(N_TAXA)
  #                         , nt_total=dplyr::n_distinct(TAXAID[EXCLUDE != TRUE], na.rm = TRUE)
  #                         , ni_max= max(N_TAXA)
  #                         , ni_dom01=dplyr::top_n(n=1, wt=N_TAXA)
  #                   )
  #https://stackoverflow.com/questions/45365484/how-to-find-top-n-descending-values-in-group-in-dplyr
  # may have to create a 2nd output with domX metrics then join together.
  # dom.val <- myDF %>%
  #               group_by(SAMPLEID, INDEX_NAME, INDEX_REGION) %>%
  #                 summarise(N_TAXA=n()) %>%
  #                   top_n(n=3, wt=N_TAXA) %>%
  #                     arrange()
  # https://groups.google.com/forum/#!topic/manipulatr/ZzohinbNsJc

  # X <- myDF %>% arrange(SampleID, desc(N_Taxa)) %>%
  #   group_by(SampleID) %>%
  #   filter(row_number()<=5)


  # Create Dominant N ####
  # Create df for Top N (without ties)
  #
  df.dom01 <- dplyr::arrange(myDF, SAMPLEID, desc(N_TAXA)) %>%
                            dplyr::group_by(SAMPLEID)  %>%
                                dplyr::filter(dplyr::row_number()<=1)
  df.dom02 <-  dplyr::arrange(myDF, SAMPLEID, desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID)  %>%
    dplyr::filter(dplyr::row_number()<=2)
  df.dom03 <-  dplyr::arrange(myDF, SAMPLEID, desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID) %>%
    dplyr::filter(dplyr::row_number()<=3)
  df.dom04 <-  dplyr::arrange(myDF, SAMPLEID, desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID) %>%
    dplyr::filter(dplyr::row_number()<=4)
  df.dom05 <-  dplyr::arrange(myDF, SAMPLEID, desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID) %>%
    dplyr::filter(dplyr::row_number()<=5)
  df.dom06 <-  dplyr::arrange(myDF, SAMPLEID, desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID) %>%
    dplyr::filter(dplyr::row_number()<=6)
  df.dom07 <-  dplyr::arrange(myDF, SAMPLEID, desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID) %>%
    dplyr::filter(dplyr::row_number()<=7)
  df.dom08 <-  dplyr::arrange(myDF, SAMPLEID, desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID) %>%
    dplyr::filter(dplyr::row_number()<=8)
  df.dom09 <- dplyr::arrange(myDF, SAMPLEID, desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID) %>%
    dplyr::filter(dplyr::row_number()<=9)
  df.dom10 <-  dplyr::arrange(myDF, SAMPLEID, desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID)  %>%
    dplyr::filter(dplyr::row_number()<=10)
  df.dom02_NoJugaRiss_BCG_att456 <-  dplyr::arrange(myDF, SAMPLEID, desc(N_TAXA)) %>%
    dplyr::group_by(SAMPLEID) %>%
    dplyr::filter((is.na(GENUS) == TRUE | GENUS!="Juga")
                  & (is.na(ORDER)==TRUE | ORDER!="Rissooidea")
                  & (BCG_ATTR == "4" | BCG_ATTR == "5" | BCG_ATTR == "6")) %>%
    dplyr::filter(dplyr::row_number()<=2)

  # Summarise Top N
  df.dom01.sum <- dplyr::summarise(dplyr::group_by(df.dom01, SAMPLEID, INDEX_NAME, INDEX_REGION)
                            , ni_dom01=sum(N_TAXA, na.rm = TRUE))
  df.dom02.sum <- dplyr::summarise(dplyr::group_by(df.dom02, SAMPLEID, INDEX_NAME, INDEX_REGION)
                            , ni_dom02=sum(N_TAXA, na.rm = TRUE))
  df.dom03.sum <- dplyr::summarise(dplyr::group_by(df.dom03, SAMPLEID, INDEX_NAME, INDEX_REGION)
                            , ni_dom03=sum(N_TAXA, na.rm = TRUE))
  df.dom04.sum <- dplyr::summarise(dplyr::group_by(df.dom04, SAMPLEID, INDEX_NAME, INDEX_REGION)
                            , ni_dom04=sum(N_TAXA, na.rm = TRUE))
  df.dom05.sum <- dplyr::summarise(dplyr::group_by(df.dom05, SAMPLEID, INDEX_NAME, INDEX_REGION)
                            , ni_dom05=sum(N_TAXA, na.rm = TRUE))
  df.dom06.sum <- dplyr::summarise(dplyr::group_by(df.dom06, SAMPLEID, INDEX_NAME, INDEX_REGION)
                            , ni_dom06=sum(N_TAXA, na.rm = TRUE))
  df.dom07.sum <- dplyr::summarise(dplyr::group_by(df.dom07, SAMPLEID, INDEX_NAME, INDEX_REGION)
                            , ni_dom07=sum(N_TAXA, na.rm = TRUE))
  df.dom08.sum <- dplyr::summarise(dplyr::group_by(df.dom08, SAMPLEID, INDEX_NAME, INDEX_REGION)
                            , ni_dom08=sum(N_TAXA, na.rm = TRUE))
  df.dom09.sum <- dplyr::summarise(dplyr::group_by(df.dom09, SAMPLEID, INDEX_NAME, INDEX_REGION)
                            , ni_dom09=sum(N_TAXA, na.rm = TRUE))
  df.dom10.sum <- dplyr::summarise(dplyr::group_by(df.dom10, SAMPLEID, INDEX_NAME, INDEX_REGION)
                            , ni_dom10=sum(N_TAXA, na.rm = TRUE))
  df.dom02_NoJugaRiss_BCG_att456.sum <- dplyr::summarise(dplyr::group_by(df.dom02_NoJugaRiss_BCG_att456
                                                                       , SAMPLEID, INDEX_NAME, INDEX_REGION)
                                                       , ni_dom02_NoJugaRiss_BCG_att456=sum(N_TAXA))
  # Add column of domN to main DF
  myDF <- merge(myDF, df.dom01.sum, all.x=TRUE)
  myDF <- merge(myDF, df.dom02.sum, all.x=TRUE)
  myDF <- merge(myDF, df.dom03.sum, all.x=TRUE)
  myDF <- merge(myDF, df.dom04.sum, all.x=TRUE)
  myDF <- merge(myDF, df.dom05.sum, all.x=TRUE)
  myDF <- merge(myDF, df.dom06.sum, all.x=TRUE)
  myDF <- merge(myDF, df.dom07.sum, all.x=TRUE)
  myDF <- merge(myDF, df.dom08.sum, all.x=TRUE)
  myDF <- merge(myDF, df.dom09.sum, all.x=TRUE)
  myDF <- merge(myDF, df.dom10.sum, all.x=TRUE)
  myDF <- merge(myDF, df.dom02_NoJugaRiss_BCG_att456.sum, all.x=TRUE)

  # Clean up extra Dom data frames
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

  # Metric Calc ####
  met.val <- dplyr::summarise(dplyr::group_by(myDF, SAMPLEID, INDEX_NAME, INDEX_REGION)
             #
             # one metric per line
             #
             # Individuals ####
             , ni_total = sum(N_TAXA, na.rm = TRUE)
             , li_total = log(ni_total)
             , ni_Chiro = sum(N_TAXA[FAMILY == "Chironomidae"], na.rm=TRUE)
             , ni_EPT = sum(N_TAXA[ORDER == "Ephemeroptera" |
                                     ORDER == "Trichoptera" |
                                     ORDER == "Plecoptera"], na.rm=TRUE)
             , ni_Trich = sum(N_TAXA[ORDER == "Trichoptera"], na.rm=TRUE)
             , ni_Americo = sum(N_TAXA[GENUS == "Americorophium"], na.rm=TRUE)
             , ni_Gnorimo = sum(N_TAXA[GENUS == "Gnorimosphaeroma"], na.rm=TRUE)
             , ni_brackish= ni_Americo + ni_Gnorimo
             , ni_Ramello = sum(N_TAXA[GENUS == "Ramellogammarus"], na.rm=TRUE)

             # Number of Taxa ####
             # account for "NONE" in nt_total, should be the only 0 N_TAXA
             , nt_total = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & N_TAXA > 0], na.rm = TRUE)
             , nt_Amph = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & ORDER == "Amphipoda"], na.rm = TRUE)
             , nt_Bival = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & CLASS == "Bivalvia"], na.rm = TRUE)
             , nt_Capit = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & FAMILY == "Capitellidae"], na.rm = TRUE)
             , nt_Caridea= dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & INFRAORDER == "Caridea"], na.rm = TRUE)
             #, nt_Chiro ## in special Chironomidae section
             , nt_Coleo = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & ORDER == "Coleoptera"], na.rm = TRUE)
             , nt_CruMol = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & PHYLUM == "Mollusca"], na.rm = TRUE) +
               dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & SUBPHYLUM == "Crustacea"], na.rm = TRUE)
             , nt_Deca = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & ORDER == "Decapoda"], na.rm = TRUE)
             , nt_Dipt = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & ORDER == "Diptera"], na.rm = TRUE)
             , nt_Ephem = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & ORDER == "Ephemeroptera"], na.rm = TRUE)
             , nt_Ephemerellid = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & FAMILY == "Ephemerellidae"], na.rm = TRUE)
             , nt_EPT = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                 & (ORDER == "Ephemeroptera"
                                                    | ORDER == "Trichoptera"
                                                    | ORDER == "Plecoptera")], na.rm = TRUE)
             , nt_Gast = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & CLASS == "Gastropoda"], na.rm = TRUE)
             , nt_Hepta = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & FAMILY == "Heptageniidae"], na.rm = TRUE)
             , nt_Insect = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & CLASS == "Insecta"], na.rm = TRUE)
             , nt_Isop = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & ORDER == "Isopoda"], na.rm = TRUE)
             , nt_Mega = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & ORDER == "Megaloptera"], na.rm = TRUE)
             , nt_Nereid = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & FAMILY == "Nereididae"], na.rm = TRUE)
             , nt_Nemour = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & FAMILY == "Nemouridae"], na.rm = TRUE)
             , nt_NonIns = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                    & (CLASS != "Insecta" | is.na(CLASS))], na.rm = TRUE)
             , nt_Nudib = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & ORDER == "Nudibranchia"], na.rm = TRUE)
             , nt_Odon = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & ORDER == "Odonata"], na.rm = TRUE)
             , nt_Oligo = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & CLASS == "Oligochaeta"], na.rm = TRUE)
             , nt_Perlid = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & FAMILY == "Perlidae"], na.rm = TRUE)
             , nt_Pleco = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & ORDER == "Plecoptera"], na.rm = TRUE)
             , nt_POET = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                                  & (ORDER == "Ephemeroptera"
                                                     | ORDER == "Trichoptera"
                                                     | ORDER == "Plecoptera"
                                                     | ORDER == "Odonata")], na.rm = TRUE)
             , nt_Poly = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & CLASS == "Polychaeta"], na.rm = TRUE)
             , nt_PolyNoSpion = 100*sum(N_TAXA[CLASS == "Polychaeta"
                                                & (is.na(FAMILY)==TRUE | FAMILY != "Spionidae")], na.rm=TRUE)
             , nt_Ptero = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & GENUS == "Pteronarcys"], na.rm = TRUE)
             , nt_Rhya = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & GENUS == "Rhyacophila"], na.rm = TRUE)
             , nt_Spion = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & FAMILY == "Spionidae"], na.rm = TRUE)
             , nt_Tipulid = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & FAMILY == "Tipulidae"], na.rm = TRUE)
             , nt_Trich = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & ORDER == "Trichoptera"], na.rm = TRUE)
             , nt_Tromb = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & ORDER == "Trombidiformes"], na.rm = TRUE)
             , nt_Tubif = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & FAMILY == "Tubificidae"], na.rm = TRUE)
             # ,intolMol, ,

             # Percent Individuals####
             , pi_Amp = NA #pi_Ampeliscidae
             , pi_AmpHaust = NA
             , pi_Amph = 100*sum(N_TAXA[ORDER == "Amphipoda"], na.rm=TRUE)/ni_total

             , pi_Baet = 100*sum(N_TAXA[FAMILY == "Baetidae"], na.rm=TRUE)/ni_total
             #, pi_Baet2Ephem
             , pi_Bival = 100*sum(N_TAXA[CLASS == "Bivalvia"], na.rm=TRUE)/ni_total
             , pi_Caen = 100*sum(N_TAXA[FAMILY == "Caenidae"], na.rm=TRUE)/ni_total
             , pi_Capit = 100*sum(N_TAXA[FAMILY == "Capitellidae"], na.rm=TRUE)/ni_total
             , pi_Cirra = 100*sum(N_TAXA[FAMILY == "Cirratulidae"], na.rm=TRUE)/ni_total
             , pi_Clite = 100*sum(N_TAXA[CLASS == "Clitellata"], na.rm=TRUE)/ni_total
             , pi_Coleo = 100*sum(N_TAXA[ORDER == "Coleoptera"], na.rm=TRUE)/ni_total
             , pi_Corb = 100*sum(N_TAXA[GENUS == "Corbicula"], na.rm=TRUE)/ni_total
             , pi_Cru = 100*sum(N_TAXA[SUBPHYLUM == "Crustacea"], na.rm=TRUE)/ni_total
             , pi_CruMol = 100*sum(N_TAXA[PHYLUM == "Mollusca" | SUBPHYLUM == "Crustacea"], na.rm=TRUE)/ni_total
             , pi_Deca = 100*sum(N_TAXA[ORDER == "Decapoda"], na.rm=TRUE)/ni_total
             , pi_Dipt = 100*sum(N_TAXA[ORDER == "Diptera"], na.rm=TRUE)/ni_total
             , pi_Ephem = 100*sum(N_TAXA[ORDER == "Ephemeroptera"], na.rm=TRUE)/ni_total
             , pi_EphemNoCae = 100*sum(N_TAXA[ORDER == "Ephemeroptera"
                                          & (is.na(FAMILY)==TRUE | FAMILY != "Caenidae")], na.rm=TRUE)/ni_total
             , pi_EphemNoCaeBae = 100*sum(N_TAXA[ORDER == "Ephemeroptera"
                                             & (is.na(FAMILY)==TRUE | FAMILY != "Caenidae")
                                             & (is.na(FAMILY)==TRUE | FAMILY != "Baetidae")]
                                      , na.rm=TRUE)/ni_total
             , pi_EPT = 100*sum(N_TAXA[ORDER == "Ephemeroptera" |
                                     ORDER == "Trichoptera" | ORDER == "Plecoptera"], na.rm=TRUE)/ni_total
             , pi_Gast = 100*sum(N_TAXA[CLASS == "Gastropoda"], na.rm=TRUE)/ni_total
             , pi_Haust = 100*sum(N_TAXA[FAMILY == "Haustoriidae"], na.rm=TRUE)/ni_total
             , pi_Hesion = 100*sum(N_TAXA[FAMILY == "Hesionidae"], na.rm=TRUE)/ni_total
             , pi_Hydro = 100*sum(N_TAXA[FAMILY == "Hydropsychidae"], na.rm=TRUE)/ni_total
             , pi_Hydro2EPT = 100*sum(N_TAXA[FAMILY == "Hydropsychidae"], na.rm=TRUE)/ni_EPT
             , pi_Hydro2Trich = 100*sum(N_TAXA[FAMILY == "Hydropsychidae"], na.rm=TRUE)/ni_Trich
             , pi_Insect = 100*sum(N_TAXA[CLASS == "Insecta"], na.rm=TRUE)/ni_total
             , pi_Isop = 100*sum(N_TAXA[ORDER == "Isopoda"], na.rm=TRUE)/ni_total
             , pi_Lucin = 100*sum(N_TAXA[FAMILY == "Lucinidae"], na.rm=TRUE)/ni_total
             , pi_LucinTellin = 100*sum(N_TAXA[FAMILY == "Lucinidae" | FAMILY == "Tellinidae"], na.rm=TRUE)/ni_total
             , pi_Mega = 100*sum(N_TAXA[ORDER == "Megaloptera"], na.rm=TRUE)/ni_total
             , pi_Mol = 100*sum(N_TAXA[PHYLUM == "Mollusca"], na.rm=TRUE)/ni_total
             , pi_Nereid = 100*sum(N_TAXA[FAMILY == "Nereididae"], na.rm=TRUE)/ni_total
             , pi_Nudib = 100*sum(N_TAXA[ORDER == "Nudibranchia"], na.rm=TRUE)/ni_total
             , pi_NonIns = 100*sum(N_TAXA[CLASS != "Insecta" | is.na(CLASS)], na.rm=TRUE)/ni_total
             , pi_Odon = 100*sum(N_TAXA[ORDER == "Odonata"], na.rm=TRUE)/ni_total
             , pi_Oligo = 100*sum(N_TAXA[CLASS == "Oligochaeta"], na.rm=TRUE)/ni_total
             , pi_Orbin = 100*sum(N_TAXA[FAMILY == "Orbiniidae"], na.rm=TRUE)/ni_total
             , pi_Pleco = 100*sum(N_TAXA[ORDER == "Plecoptera"], na.rm=TRUE)/ni_total
             , pi_Poly = 100*sum(N_TAXA[CLASS == "Polychaeta"], na.rm=TRUE)/ni_total
             , pi_Spion = 100*sum(N_TAXA[FAMILY == "Spionidae"], na.rm=TRUE)/ni_total
             , pi_Spion2Poly = 100*sum(N_TAXA[CLASS == "Polychaeta" | FAMILY == "Spionidae"], na.rm=TRUE)/ni_total
             , pi_Tellin = 100*sum(N_TAXA[FAMILY == "Tellinidae"], na.rm=TRUE)/ni_total
             , pi_Trich = 100*sum(N_TAXA[ORDER == "Trichoptera"], na.rm=TRUE)/ni_total
             , pi_TricNoHydro = 100*sum(N_TAXA[ORDER == "Trichoptera"
                                           & (is.na(FAMILY)==TRUE | FAMILY != "Hydropsychidae")]
                                    , na.rm=TRUE)/ni_total
             , pi_Tubif = 100*sum(N_TAXA[FAMILY == "Tubificidae"], na.rm=TRUE)/ni_total
             , pi_Xanth = 100*sum(N_TAXA[FAMILY == "Xanthidae"], na.rm=TRUE)/ni_total
             # Cole2Odon,
             #EPTsenstive


             # Percent of Taxa ####
             , pt_Amph = 100*nt_Amph/nt_total
             , pt_Bival = 100*nt_Bival/nt_total
             , pt_Coleo = 100*nt_Coleo/nt_total
             , pt_Deca = 100*nt_Deca/nt_total
             , pt_Dipt = 100*nt_Dipt/nt_total
             , pt_Ephem = 100*nt_Ephem/nt_total
             , pt_EPT = 100*nt_EPT/nt_total
             , pt_Gast = 100*nt_Gast/nt_total
             , pt_Insect = 100*nt_Insect/nt_total
             , pt_Isop = 100*nt_Isop/nt_total
             , pt_Mega = 100*nt_Mega/nt_total
             , pt_NonIns = 100*nt_NonIns/nt_total
             , pt_Nudib = 100*nt_Nudib/nt_total
             , pt_Odon = 100*nt_Odon/nt_total
             , pt_Oligo = 100*nt_Oligo/nt_total
             , pt_Pleco = 100*nt_Pleco/nt_total
             , pt_POET = 100*nt_POET/nt_total
             , pt_Poly = 100*nt_Poly/nt_total
             , pt_PolyNoSpion = 100*nt_PolyNoSpion/nt_total
             , pt_Spion = 100*nt_Spion/nt_total
             , pt_Trich = 100*nt_Trich/nt_total

             # Ratio of Taxa ####
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


             # Midges ####
             # Family    = Chironomidae
             # subfamily = Chironominae
             # subfamily = Orthocladiinae
             # subfamily = Tanypodinae
             # Tribe     = Tanytarsini
             , nt_Chiro = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & FAMILY == "Chironomidae"], na.rm = TRUE)
             , pi_Chiro = 100*ni_Chiro/ni_total
             , pt_Chiro = 100*nt_Chiro/nt_total
             , pi_Tanyt = 100*sum(N_TAXA[TRIBE == "Tanytarsini"], na.rm=TRUE)/ni_total
             , pi_Tanyp = 100*sum(N_TAXA[SUBFAMILY == "Tanypodinae"], na.rm=TRUE)/ni_total
             , pi_COC2Chi = 100*sum(N_TAXA[GENUS == "Chironomus"
                                       | GENUS=="Cricotopus"
                                       | GENUS=="Cricotopus/Orthocladius"
                                       | GENUS=="Orthocladius/Cricotopus"
                                       | GENUS=="Orthocladius"], na.rm=TRUE)/ni_Chiro

             , pi_ChCr2Chi = 100*sum(N_TAXA[GENUS == "Chironomus"
                                            | GENUS=="Cricotopus"], na.rm=TRUE)/ni_Chiro
             , pi_Orth2Chi = 100*sum(N_TAXA[SUBFAMILY == "Orthocladiinae"], na.rm=TRUE)/ni_Chiro
             , pi_Tanyp2Chi = 100*sum(N_TAXA[SUBFAMILY == "Tanypodinae"], na.rm=TRUE)/ni_Chiro
             #,nt_Ortho (Marine)
             #MB_pi_OrthocladiinaeCricotopusChironomus2Chironomidae
             # rt_Chiro, Ortho, Tanyt





             # SPECIAL ####
             # oddball or specialized metrics
             # , ni_NonIns = sum(N_TAXA[CLASS==NA | CLASS!="Insecta"], na.rm=TRUE)
             # , ni_NonArach = sum(N_TAXA[CLASS==NA | CLASS!="Arachnida"], na.rm=TRUE)
             # , ni_NonDeca = sum(N_TAXA[ORDER==NA | ORDER!="Decapoda"], na.rm=TRUE)
             #
             # , ni_clumpy = sum(N_TAXA[GENUS=="Juga" & GENUS=="Rissoidea"], na.rm=TRUE)
             # , ni_Nonclumpy = sum(N_TAXA[GENUS!="Juga" & GENUS!="Rissoidea"], na.rm=TRUE)
             #
             # PacNW, NonIns_select
             #This metric excludes Class Insecta, Class Arachnida and Order Decapoda;
             # and only includes Attribute IV, V, VI taxa.
             , nt_NonInsArachDeca_BCG_att456 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                      & (is.na(CLASS)==TRUE | (CLASS != "Insecta" & CLASS != "Arachnida"))
                                      & (is.na(ORDER) == TRUE | ORDER != "Decapoda")
                                      & (BCG_ATTR == "4" | BCG_ATTR == "5" | BCG_ATTR == "6")]
                                     , na.rm = TRUE)
             , pi_NonInsArachDeca_BCG_att456 = 100*sum(N_TAXA[
                                    (is.na(CLASS)==TRUE | (CLASS != "Insecta" & CLASS != "Arachnida"))
                                    & (is.na(ORDER) == TRUE | ORDER != "Decapoda")
                                    & (BCG_ATTR == "4" | BCG_ATTR == "5" | BCG_ATTR == "6")]
                                    , na.rm=TRUE)/ni_total
             , pt_NonInsArachDeca_BCG_att456 = 100*nt_NonInsArachDeca_BCG_att456/nt_total
             # PacNW, NonIns_select_NonClump
             # above but also non-clumpy
             #clumpy' taxa (Juga [genus] and Rissooidea [superfamily as Order] in PacNW);
             #and it only includes Attribute IV, V, VI taxa.
             , nt_NonInsArachDecaJugaRiss_BCG_att456 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                      & (is.na(CLASS)==TRUE | (CLASS != "Insecta" & CLASS != "Arachnida"))
                                      & (is.na(ORDER) == TRUE | (ORDER != "Decapoda" & ORDER!="Rissooidea"))
                                      & (is.na(GENUS) == TRUE | GENUS!="Juga")
                                      & (BCG_ATTR == "4" | BCG_ATTR == "5" | BCG_ATTR == "6")]
                                      , na.rm = TRUE)
             , pi_NonInsArachDecaJugaRiss_BCG_att456 = 100*sum(N_TAXA[
                                     (is.na(CLASS)==TRUE | (CLASS != "Insecta" & CLASS != "Arachnida"))
                                     & (is.na(ORDER) == TRUE | (ORDER != "Decapoda" & ORDER!="Rissooidea"))
                                     & (is.na(GENUS) == TRUE | GENUS!="Juga")
                                     & (BCG_ATTR == "4" | BCG_ATTR == "5" | BCG_ATTR == "6")]
                                    , na.rm=TRUE)/ni_total
             , pt_NonInsArachDecaJugaRiss_BCG_att456 = 100*nt_NonInsArachDecaJugaRiss_BCG_att456/nt_total
             # dominant
             , pi_dom02_BCG_att456_NoJugaRiss = 100*max(ni_dom02_NoJugaRiss_BCG_att456)/ni_total
             #
             # 20180608, rework PacNW
             # NonInsecta, Attribute 456
             , nt_NonIns_BCG_att456 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                     & (is.na(CLASS)==TRUE | CLASS != "Insecta")
                                     & (BCG_ATTR == "4" | BCG_ATTR == "5" | BCG_ATTR == "6")]
                                     , na.rm = TRUE)
             , pi_NonIns_BCG_att456 = 100*sum(N_TAXA[
                                     (is.na(CLASS)==TRUE | CLASS != "Insecta")
                                     & (BCG_ATTR == "4" | BCG_ATTR == "5" | BCG_ATTR == "6")]
                                     , na.rm=TRUE)/ni_total
             , pt_NonIns_BCG_att456 = 100*nt_NonIns_BCG_att456/nt_total
             # NonInsectaJugaRiss, Attribute 456
             , nt_NonInsJugaRiss_BCG_att456 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE
                                     & (is.na(CLASS)==TRUE | CLASS != "Insecta")
                                     & (is.na(ORDER) == TRUE | ORDER!="Rissooidea")
                                     & (is.na(GENUS) == TRUE | GENUS!="Juga")
                                     & (BCG_ATTR == "4" | BCG_ATTR == "5" | BCG_ATTR == "6")]
                                     , na.rm = TRUE)
             , pi_NonInsJugaRiss_BCG_att456 = 100*sum(N_TAXA[
                                     (is.na(CLASS)==TRUE | CLASS != "Insecta")
                                     & (is.na(ORDER) == TRUE | ORDER!="Rissooidea")
                                     & (is.na(GENUS) == TRUE | GENUS!="Juga")
                                     & (BCG_ATTR == "4" | BCG_ATTR == "5" | BCG_ATTR == "6")]
                                     , na.rm=TRUE)/ni_total
             , pt_NonInsJugaRiss_BCG_att456 = 100*nt_NonInsJugaRiss_BCG_att456/nt_total
             # 20180815, Percent Baetis tricaudatus complex + Simuliidae individual
             , pi_SimBtri = 100*(sum(N_TAXA[FAMILY == "Simuliidae"], na.rm=TRUE)
                            + sum(N_TAXA[TAXAID == "Baetis tricaudatus complex"]
                              , na.rm=TRUE))/ni_total
             # 20181018, MS, sensitive Coleoptera & (Family is Null or not Hydrophyilidae)
             , pi_Colesens = 100*sum(N_TAXA[ORDER == "Coleoptera"
                             & (FAMILY!="Hydrophilidae" | is.na(FAMILY)==TRUE)], na.rm=TRUE)/ni_total
             # 20181207, BCG PacNW, Level 1 Signal metrics
             , nt_longlived=dplyr::n_distinct(TAXAID[EXCLUDE!=TRUE & LONGLIVED==TRUE], na.rm=TRUE)
             , nt_noteworthy=dplyr::n_distinct(TAXAID[EXCLUDE!=TRUE & NOTEWORTHY==TRUE], na.rm=TRUE)
             , nt_ffg2_pred=dplyr::n_distinct(TAXAID[EXCLUDE!=TRUE & FFG2_PRE==TRUE], na.rm=TRUE)


             # Thermal Indicators ####
             ## nt_ti
             , nt_ti_c = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & TI_COLD == TRUE], na.rm = TRUE)
             , nt_ti_cc = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & TI_COLDCOOL == TRUE], na.rm = TRUE)
             , nt_ti_cw = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & TI_COOLWARM == TRUE], na.rm = TRUE)
             , nt_ti_w = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & TI_WARM == TRUE], na.rm = TRUE)
              ## pi_ti
             , pi_ti_c = 100*sum(N_TAXA[TI_COLD == TRUE], na.rm=TRUE)/ni_total
             , pi_ti_cc = 100*sum(N_TAXA[TI_COLDCOOL == TRUE], na.rm=TRUE)/ni_total
             , pi_ti_cw = 100*sum(N_TAXA[TI_COOLWARM == TRUE], na.rm=TRUE)/ni_total
             , pi_ti_w = 100*sum(N_TAXA[TI_WARM == TRUE], na.rm=TRUE)/ni_total
             ## pt_ti
             , pt_ti_c = 100*nt_ti_c/nt_total
             , pt_ti_cc = 100*nt_ti_cc/nt_total
             , pt_ti_cw = 100*nt_ti_cw/nt_total
             , pt_ti_w = 100*nt_ti_w/nt_total


             # Tolerance ####
             , nt_tv_intol = dplyr::n_distinct(TAXAID[EXCLUDE!=TRUE & TOLVAL>=0 & TOLVAL<=3], na.rm=TRUE)
             , nt_tv_toler = dplyr::n_distinct(TAXAID[EXCLUDE!=TRUE & TOLVAL>=7 & TOLVAL<=10], na.rm=TRUE)
             , pi_tv_intol = 100*sum(N_TAXA[TOLVAL>=0  & TOLVAL<=3], na.rm=TRUE)/ni_total
             , pi_tv_toler = 100*sum(N_TAXA[TOLVAL>=7  & TOLVAL<=10], na.rm=TRUE)/ni_total
             , pt_tv_intol = 100*nt_tv_intol/nt_total
             , pt_tv_toler = 100*nt_tv_toler/nt_total
             #,nt_tvfam_intol = dplyr::n_distinct(TAXAID[EXCLUDE!=TRUE & FAM_TV<=3 & !is.na(FAM_TV)])
             # pi_Baet2Eph, pi_Hyd2EPT, pi_Hyd2Tri, , , ,
             # nt_intMol (for marine)

             , nt_tv_ntol = dplyr::n_distinct(TAXAID[EXCLUDE!=TRUE & TOLVAL>=0 & TOLVAL<6], na.rm=TRUE)
             , nt_tv_stol = dplyr::n_distinct(TAXAID[EXCLUDE!=TRUE & TOLVAL>=8 & TOLVAL<=10], na.rm=TRUE)
             , pi_tv_ntol = 100*sum(N_TAXA[TOLVAL>=0  & TOLVAL<6], na.rm=TRUE)/ni_total
             , pi_tv_stol = 100*sum(N_TAXA[TOLVAL>=8  & TOLVAL<=10], na.rm=TRUE)/ni_total
             , pt_tv_ntol = 100*nt_tv_ntol/nt_total
             , pt_tv_stol = 100*nt_tv_stol/nt_total




             # USEPA, WSA and NRSA
             ## ntol is not tolerant
             ## stol is

             # Tolerance2 ####
             ## special condition tolerance values
             # MBSS
             , pi_tv2_intol=sum(N_TAXA[TOLVAL2<=3 & !is.na(TOLVAL2)])/sum(N_TAXA[!is.na(TOLVAL2)])
             #, pi_tv_intolurb=pi_tv2_intol
             , pi_tv2_toler_ISA_SalHi_xFL = NA
             , pi_tv2_intol_ISA_SalHi_xFL = NA
             , pt_tv2_intol_ISA_SalHi_xFL = NA

             # FFG #####
             ## nt_ffg
             , nt_ffg_col = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & FFG_COL == TRUE], na.rm = TRUE)
             , nt_ffg_filt = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & FFG_FIL == TRUE], na.rm = TRUE)
             , nt_ffg_pred = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & FFG_PRE == TRUE], na.rm = TRUE)
             , nt_ffg_scrap = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & FFG_SCR == TRUE], na.rm = TRUE)
             , nt_ffg_shred = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & FFG_SHR == TRUE], na.rm = TRUE)
             ## pi_ffg
             , pi_ffg_col = 100*sum(N_TAXA[FFG_COL == TRUE], na.rm=TRUE)/ni_total
             , pi_ffg_filt = 100*sum(N_TAXA[FFG_FIL == TRUE], na.rm=TRUE)/ni_total
             , pi_ffg_pred = 100*sum(N_TAXA[FFG_PRE == TRUE], na.rm=TRUE)/ni_total
             , pi_ffg_scrap = 100*sum(N_TAXA[FFG_SCR == TRUE], na.rm=TRUE)/ni_total
             , pi_ffg_shred = 100*sum(N_TAXA[FFG_SHR == TRUE], na.rm=TRUE)/ni_total
             ## pt_ffg
             , pt_ffg_col = 100*nt_ffg_col/nt_total
             , pt_ffg_filt = 100*nt_ffg_filt/nt_total
             , pt_ffg_pred = 100*nt_ffg_pred/nt_total
             , pt_ffg_scrap = 100*nt_ffg_scrap/nt_total
             , pt_ffg_shred = 100*nt_ffg_shred/nt_total

             #, pi_ffg_infc
             #, rt_ffg_infc, converborbelt, scavbrow, subsurf, watercol
             #, rt_ffg_pred
             # carnivoreomnivore, deepdeposit, suspension
             # FFG2 ####
             # marine
             ## nt_ffg2
             , nt_ffg2_intface = NA
             , nt_ffg2_subsurf = NA
             ## pi_ffg2
             , pi_ffg2_scavburr = NA
             ## pt_ffg2
             # = conveyorbelt, interface, scavengerbrowser, subsurface, watercolumn, predator

             # Habit ####
             #(need to be wild card. that is, counts both CN,CB and CB as climber)
             ## nt_habit
             , nt_habit_burrow = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & HABIT_BU == TRUE], na.rm = TRUE)
             , nt_habit_climb = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & HABIT_CB == TRUE], na.rm = TRUE)
             , nt_habit_cling = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & HABIT_CN == TRUE], na.rm = TRUE)
             , nt_habit_sprawl = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & HABIT_SP == TRUE], na.rm = TRUE)
             , nt_habit_swim = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & HABIT_SW == TRUE], na.rm = TRUE)
             ## pi_habit
             , pi_habit_burrow = 100*sum(N_TAXA[HABIT_BU == TRUE], na.rm=TRUE)/ni_total
             , pi_habit_climb = 100*sum(N_TAXA[HABIT_CB == TRUE], na.rm=TRUE)/ni_total
             , pi_habit_cling = 100*sum(N_TAXA[HABIT_CN == TRUE], na.rm=TRUE)/ni_total
             , pi_habit_sprawl = 100*sum(N_TAXA[HABIT_SP == TRUE], na.rm=TRUE)/ni_total
             , pi_habit_swim = 100*sum(N_TAXA[HABIT_SW == TRUE], na.rm=TRUE)/ni_total
              # pt for each
             , pt_habit_burrow = 100*nt_habit_burrow/nt_total
             , pt_habit_climb = 100*nt_habit_climb/nt_total
             , pt_habit_cling = 100*nt_habit_cling/nt_total
             , pt_habit_sprawl = 100*nt_habit_sprawl/nt_total
             , pt_habit_swim = 100*nt_habit_swim/nt_total


             # Life Cycle ####
             # pi and nt for mltvol, semvol, univol
             ## nt_LifeCycle
             , nt_volt_multi=dplyr::n_distinct(TAXAID[EXCLUDE!=TRUE & LC_MULTI==TRUE], na.rm = TRUE)
             , nt_volt_semi=dplyr::n_distinct(TAXAID[EXCLUDE!=TRUE & LC_SEMI==TRUE], na.rm = TRUE)
             , nt_volt_uni=dplyr::n_distinct(TAXAID[EXCLUDE!=TRUE & LC_UNI==TRUE], na.rm = TRUE)
             ## pi_LifeCycle
             , pi_volt_multi = 100*sum(N_TAXA[LC_MULTI==TRUE], na.rm=TRUE) / ni_total
             , pi_volt_semi = 100*sum(N_TAXA[LC_SEMI==TRUE], na.rm=TRUE) / ni_total
             , pi_volt_uni = 100*sum(N_TAXA[LC_UNI==TRUE], na.rm=TRUE) / ni_total
             ## pt_LifeCycle
             , pt_volt_multi = 100*nt_volt_multi/nt_total
             , pt_volt_semi = 100*nt_volt_semi/nt_total
             , pt_volt_uni= 100*nt_volt_uni/nt_total


             # Dominant N ####
             ## uses previously defined values added to myDF
             , pi_dom01 = 100*max(N_TAXA)/ni_total
             , pi_dom02 = 100*max(ni_dom02)/ni_total
             , pi_dom03 = 100*max(ni_dom03)/ni_total
             , pi_dom04 = 100*max(ni_dom04)/ni_total
             , pi_dom05 = 100*max(ni_dom05)/ni_total
             , pi_dom06 = 100*max(ni_dom06)/ni_total
             , pi_dom07 = 100*max(ni_dom07)/ni_total
             , pi_dom08 = 100*max(ni_dom08)/ni_total
             , pi_dom09 = 100*max(ni_dom09)/ni_total
             , pi_dom10 = 100*max(ni_dom10)/ni_total

            # , pi_dom01alt= dplyr::top_n(N_TAXA, n=1)/ni_total
            #https://stackoverflow.com/questions/27766054/getting-the-top-values-by-group
            # top_n uses ties so can't use it

            # Indices ####
            #, x_AMBI - may need extra function or like "top" functions do some precalc
             #,x_Becks.CLASS1=n_distinct(N_TAXA[EXCLUDE!=TRUE & TolVal>=0 & TolVal<=2.5])
             #,x_Becks.CLASS2=n_distinct(N_TAXA[EXCLUDE!=TRUE & TolVal>=2.5 & TolVal<=4])
             , x_Becks = (2 * dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & TOLVAL >= 0 & TOLVAL <= 1.5], na.rm = TRUE)) +
                          (1 * dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & TOLVAL > 1.5 & TOLVAL <= 4], na.rm = TRUE))
             #,x_HBI_numer=sum(N_TAXA*TOLVAL, na.rm=TRUE)
             #,x_HBI_denom=sum(N_TAXA[!is.na(TOLVAL) & TOLVAL>=0], na.rm=TRUE)
             , x_HBI = sum(N_TAXA * TOLVAL, na.rm=TRUE)/sum(N_TAXA[!is.na(TOLVAL) & TOLVAL >= 0], na.rm=TRUE)
             , x_NCBI = sum(N_TAXA * TOLVAL2, na.rm=TRUE)/sum(N_TAXA[!is.na(TOLVAL2) & TOLVAL2 >= 0], na.rm=TRUE)
             # Shannon-Weiner
             #, x_Shan_Num= -sum(log(N_TAXA/ni_total)), na.rm=TRUE)
             #, x_Shan_e=x_Shan_Num/log(exp(1))
             , x_Shan_e = -sum((N_TAXA/ni_total)*log((N_TAXA/ni_total)), na.rm=TRUE)
             , x_Shan_2 = x_Shan_e/log(2)
             , x_Shan_10 = x_Shan_e/log(10)
             #, x_D Simpson
             , x_D = 1-sum((N_TAXA/ni_total)^2, na.rm = TRUE)
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

            # Density ####
            # Numbers per area sampled

            # Estuary-Marine ####
            # Mixed in with other metrics
            , x_Becks_tv2 <- NA

            # Habitat ####
            # BCG PacNW group 2020
            # BRAC brackish
            # DEPO depositional
            # GENE generalist
            # HEAD headwater
            # RHEO rheophily
            # RIVE riverine
            # SPEC specialist
            # UNKN unknown
            ## nt_Habitat
            ## pi_Habitat
            ## pt_Habitat

             # BCG ####
             # 1i, 1m, 1t
             # Xi, Xm, Xt
             # 5i, 5m, 5t
             # 6i, 6m, 6t
            ## nt_BCG
            , nt_BCG_att1 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & BCG_ATTR == "1"], na.rm = TRUE)
            , nt_BCG_att1i = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & BCG_ATTR == "1i"], na.rm = TRUE)
            , nt_BCG_att1m = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & BCG_ATTR == "1m"], na.rm = TRUE)
            , nt_BCG_att12 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & (BCG_ATTR == "1" | BCG_ATTR == "2")], na.rm = TRUE)
            , nt_BCG_att1i2 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & (BCG_ATTR == "1i" | BCG_ATTR == "2")], na.rm = TRUE)
            , nt_BCG_att123 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE &
                                                       (BCG_ATTR == "1" | BCG_ATTR == "2" | BCG_ATTR == "3")], na.rm = TRUE)
            , nt_BCG_att1i23 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE &
                                                         (BCG_ATTR == "1i" | BCG_ATTR == "2" | BCG_ATTR == "3")], na.rm = TRUE)
            , nt_BCG_att2 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & (BCG_ATTR == "2")], na.rm = TRUE)
            , nt_BCG_att23 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & (BCG_ATTR == "2" | BCG_ATTR == "3")], na.rm = TRUE)
            , nt_BCG_att234 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & (BCG_ATTR == "2" | BCG_ATTR == "3" | BCG_ATTR == "4")], na.rm = TRUE)
            , nt_BCG_att3 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & (BCG_ATTR == "3")], na.rm = TRUE)
            , nt_BCG_att4 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & (BCG_ATTR == "4")], na.rm = TRUE)
            , nt_BCG_att45 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & (BCG_ATTR == "4" | BCG_ATTR == "5")], na.rm = TRUE)
            , nt_BCG_att5 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & (BCG_ATTR == "5")], na.rm = TRUE)
            , nt_BCG_att56 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & (BCG_ATTR == "5" | BCG_ATTR == "6")], na.rm = TRUE)
            , nt_BCG_att6 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE &  (BCG_ATTR == "6")], na.rm = TRUE)
           # , nt_BCG_attNAT = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE &  is.na(BCG_ATTR)], na.rm = TRUE)
           # , nt_BCG_attNAF = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE &  is.na(BCG_ATTR)], na.rm = FALSE)

            ## EPT
            , nt_EPT_BCG_att123 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & (ORDER == "Ephemeroptera" | ORDER == "Trichoptera" | ORDER == "Plecoptera")
                                                           & (BCG_ATTR == "1" | BCG_ATTR == "2" | BCG_ATTR == "3")], na.rm = TRUE)
            , nt_EPT_BCG_att1i23 = dplyr::n_distinct(TAXAID[EXCLUDE != TRUE & (ORDER == "Ephemeroptera" | ORDER == "Trichoptera" | ORDER == "Plecoptera")
                                                           & (BCG_ATTR == "1i" | BCG_ATTR == "2" | BCG_ATTR == "3")], na.rm = TRUE)
            ## pi_BCG
            , pi_BCG_att1 = 100*sum(N_TAXA[(BCG_ATTR == "1")], na.rm=TRUE)/ni_total
            , pi_BCG_att1i = 100*sum(N_TAXA[(BCG_ATTR == "1i")], na.rm=TRUE)/ni_total
            , pi_BCG_att1m = 100*sum(N_TAXA[(BCG_ATTR == "1m")], na.rm=TRUE)/ni_total
            , pi_BCG_att12 = 100*sum(N_TAXA[(BCG_ATTR == "1" | BCG_ATTR == "2")], na.rm=TRUE)/ni_total
            , pi_BCG_att1i2 = 100*sum(N_TAXA[(BCG_ATTR == "1i" | BCG_ATTR == "2")], na.rm=TRUE)/ni_total
            , pi_BCG_att123 = 100*sum(N_TAXA[(BCG_ATTR == "1" | BCG_ATTR == "2" | BCG_ATTR == "3")], na.rm=TRUE)/ni_total
            , pi_BCG_att1i23 = 100*sum(N_TAXA[(BCG_ATTR == "1i" | BCG_ATTR == "2" | BCG_ATTR == "3")], na.rm=TRUE)/ni_total
            , pi_BCG_att2 = 100*sum(N_TAXA[(BCG_ATTR == "2")], na.rm=TRUE)/ni_total
            , pi_BCG_att23 = 100*sum(N_TAXA[(BCG_ATTR == "2" | BCG_ATTR == "3")], na.rm=TRUE)/ni_total
            , pi_BCG_att234 = 100*sum(N_TAXA[(BCG_ATTR == "2" | BCG_ATTR == "3" | BCG_ATTR == "4")], na.rm=TRUE)/ni_total
            , pi_BCG_att3 = 100*sum(N_TAXA[(BCG_ATTR == "3")], na.rm=TRUE)/ni_total
            , pi_BCG_att4 = 100*sum(N_TAXA[(BCG_ATTR == "4")], na.rm=TRUE)/ni_total
            , pi_BCG_att45 = 100*sum(N_TAXA[(BCG_ATTR == "4" | BCG_ATTR == "5")], na.rm=TRUE)/ni_total
            , pi_BCG_att5 = 100*sum(N_TAXA[(BCG_ATTR == "5")], na.rm=TRUE)/ni_total
            , pi_BCG_att56 = 100*sum(N_TAXA[(BCG_ATTR == "5" | BCG_ATTR == "6")], na.rm=TRUE)/ni_total
            , pi_BCG_att6 = 100*sum(N_TAXA[(BCG_ATTR == "6")], na.rm=TRUE)/ni_total

            ## EPT
            , pi_EPT_BCG_att123 = 100*sum(N_TAXA[(ORDER == "Ephemeroptera" | ORDER == "Trichoptera" | ORDER == "Plecoptera")
                                             & (BCG_ATTR == "1" | BCG_ATTR == "2" | BCG_ATTR == "3")], na.rm=TRUE)/ni_total
            ## pt_BCG
            , pt_BCG_att1 = 100*nt_BCG_att1/nt_total
            , pt_BCG_att1i = 100*nt_BCG_att1i/nt_total
            , pt_BCG_att1m = 100*nt_BCG_att1m/nt_total
            , pt_BCG_att12 = 100*nt_BCG_att12/nt_total
            , pt_BCG_att1i2 = 100*nt_BCG_att1i2/nt_total
            , pt_BCG_att123 = 100*nt_BCG_att123/nt_total
            , pt_BCG_att1i23 = 100*nt_BCG_att1i23/nt_total
            , pt_BCG_att2 = 100*nt_BCG_att2/nt_total
            , pt_BCG_att23 = 100*nt_BCG_att23/nt_total
            , pt_BCG_att234 = 100*nt_BCG_att234/nt_total
            , pt_BCG_att3 = 100*nt_BCG_att3/nt_total
            , pt_BCG_att4 = 100*nt_BCG_att4/nt_total
            , pt_BCG_att45 = 100*nt_BCG_att45/nt_total
            , pt_BCG_att5 = 100*nt_BCG_att5/nt_total
            , pt_BCG_att56 = 100*nt_BCG_att56/nt_total
            , pt_BCG_att6 = 100*nt_BCG_att6/nt_total

            , pt_EPT_BCG_att123 = 100*nt_EPT_BCG_att123/nt_total


             # pi_dom01_att 4, 5, 56
             # pi_dom05_att 123, not 456


             #
          )## met.val.END
  #
  # Clean Up ####
  # replace NA with 0
  met.val[is.na(met.val)] <- 0

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


  # # subset to only metrics specified by user
  if (is.null(MetricNames)) {
    # removed marine only if MetrcNames not provided
    met.val <- met.val[, !(names(met.val) %in% MetricNames_Marine)]
  } else {
    met2include <- MetricNames[!(MetricNames %in% "ni_total")]
    # remove ni_total if included as will always include it
    met.val <- met.val[, c("SAMPLEID", "INDEX_REGION", "INDEX_NAME",
                           "ni_total", met2include)]
  }##IF~MetricNames~END

  # Add extra fields
  if(is.null(cols2keep)){##IF.is.null.cols2keep.START
    df.return <- as.data.frame(met.val)
  } else {
    # create df with grouped fields
    myDF.cols2keep <- myDF %>% dplyr::group_by_(.dots=c("SAMPLEID", cols2keep)) %>%
      dplyr::summarize(col.drop=sum(N_TAXA))
    col.drop <- ncol(myDF.cols2keep)
    myDF.cols2keep <- myDF.cols2keep[,-col.drop]
    # merge
    df.return <- merge(as.data.frame(myDF.cols2keep), as.data.frame(met.val), by="SAMPLEID")
  }##IF.is.null.cols2keep.END

  # df to report back
  return(df.return)
}##FUNCTION.metric.values.bugs.END
#
#
#' @export
metric.values.fish <- function(myDF
                               , MetricNames=NULL
                               , boo.Adjust=FALSE
                               , cols2keep=NULL){##FUNCTION.metric.values.fish.START
  # define pipe
  `%>%` <- dplyr::`%>%`
  # QC ####
  # Remove Non-Target Taxa
  #myDF <- myDF[myDF[,"NonTarget"]==0,] # not relevant for fish
  # QC, Required Fields
  col.req <- c("SAMPLEID", "TAXAID", "N_TAXA", "N_ANOMALIES", "SAMP_BIOMASS"
               , "INDEX_NAME", "INDEX_REGION"
               , "DA_MI2", "SAMP_WIDTH_M", "SAMP_LENGTH_M"
               , "TYPE", "TOLER", "NATIVE", "TROPHIC", "SILT"
               , "FAMILY", "GENUS", "HYBRID"
               )
  col.req.missing <- col.req[!(col.req %in% toupper(names(myDF)))]
  num.col.req.missing <- length(col.req.missing)
  # Trigger prompt if any missing fields (and session is interactive)
  if(num.col.req.missing!=0 & interactive()==TRUE){##IF.num.col.req.missing.START
    myPrompt.01 <- paste0("There are ",num.col.req.missing," missing fields in the data:")
    myPrompt.02 <- paste(col.req.missing, collapse=", ")
    myPrompt.03 <- "If you continue the metrics associated with these fields will be invalid."
    myPrompt.04 <- "For example, if the NATIVE field is missing all native related metrics will not be correct."
    myPrompt.05 <- "Do you wish to continue (YES or NO)?"

    myPrompt <- paste(" ", myPrompt.01, myPrompt.02, " ", myPrompt.03, myPrompt.04
                      , myPrompt.05, sep="\n")
    #user.input <- readline(prompt=myPrompt)
    user.input <- NA
    user.input <- utils::menu(c("YES", "NO"), title=myPrompt)
    # any answer other than "YES" will stop the function.
    if(user.input!=1){##IF.user.input.START
      stop(paste("The user chose *not* to continue due to missing fields: "
                 , paste(paste0("   ",col.req.missing), collapse="\n"),sep="\n"))
    }##IF.user.input.END
    # Add missing fields
    myDF[, col.req.missing] <- NA
    warning(paste("Metrics related to the following fields are invalid:"
                  , paste(paste0("   ", col.req.missing), collapse="\n"), sep="\n"))
  }##IF.num.col.req.missing.END

  # Column Values to UPPER case for met.val below
  col2upper <- c("FAMILY", "GENUS", "TYPE", "TOLER", "NATIVE", "TROPHIC")
  for (i in col2upper){
    myDF[, i] <- toupper(myDF[, i])
  }##FOR~i~END


  # N_Anomalies
  # By taxon or sample total
  # Data set up to be by taxon.  But some report as sample total.
  stats_anom <- myDF %>%
    group_by(SAMPLEID) %>%
    summarize(n =n()
              , n_distinct = n_distinct(N_ANOMALIES, na.rm = TRUE)
              , mean = mean(N_ANOMALIES, na.rm = TRUE)
              , sd = sd(N_ANOMALIES, na.rm = TRUE)
              , sum = sum(N_ANOMALIES, na.rm = TRUE))
  stats_anom[, "SUM_ANOMALIES"] <- stats_anom[, "mean"] / stats_anom[, "n"]
  # make change;  n > 1 & n_distinct == 1
  stats_anom$MOD_ANOMALIES <- ifelse(stats_anom$n > 1 & stats_anom$n_distinct == 1, TRUE, FALSE)
  # add back to myDF
  myDF <- merge(myDF, stats_anom[, c("SAMPLEID", "SUM_ANOMALIES", "MOD_ANOMALIES")]
             , all.x = TRUE)
  myDF[myDF$MOD_ANOMALIES==TRUE, "N_ANOMALIES"] <- myDF[myDF$MOD_ANOMALIES==TRUE, "SUM_ANOMALIES"]

  # Metric Calc ####
  # code above is different than benthos
  # Calculate Metrics (could have used pipe, %>%)
  met.val <- dplyr::summarise(dplyr::group_by(myDF, SAMPLEID, INDEX_NAME, INDEX_REGION, SAMP_WIDTH_M, SAMP_LENGTH_M)
                       #
                       # MBSS 2005, 11 metrics
                       # (can do metrics as one step but MBSS output has numerator so will get that as well)
                       #
                       # when invoke a "x != abc" need to include "| is.na(x)"
                       # unless all "x" are populated (e.g., TRUE or FALSE)

                       # Individuals ####
                       # individuals, total
                       , ni_total=sum(N_TAXA, na.rm = TRUE)
                       , ni_natnonhybridnonmf = sum(N_TAXA[NATIVE == "NATIVE" &
                                                             (HYBRID != TRUE | is.na(HYBRID)) &
                                                             (TYPE != "MOSQUITOFISH" | is.na(TYPE))], na.rm = TRUE)
                       , ni_natnonhybridnonmfnonlepomis = sum(N_TAXA[NATIVE == "NATIVE" &
                                                                       (HYBRID != TRUE | is.na(HYBRID)) &
                                                                       (TYPE != "MOSQUITOFISH" | is.na(TYPE)) &
                                                                       (GENUS != "LEPOMIS" | is.na(GENUS))], na.rm = TRUE)
                       #
                       # Percent Individuals ####
                       # % RBS
                       , pi_rbs=100*sum(N_TAXA[TYPE == "RBS"], na.rm = TRUE)/ni_total
                       # Pct Brook Trout
                       , pi_brooktrout=100*sum(N_TAXA[TYPE == "BROOK TROUT"], na.rm = TRUE)/ni_total
                       # Pct Sculpins
                       , pi_sculpin=100*sum(N_TAXA[TYPE == "SCULPIN"], na.rm = TRUE)/ni_total
                       , pi_lepomis = 100*sum(N_TAXA[GENUS == "LEPOMIS"], na.rm = TRUE)/ni_total

                       # benthic fluvial specialist
                       , pi_bfs = 100*sum(N_TAXA[(TYPE == "BENTHIC" & TROPHIC == "IV") |
                                                   TYPE == "RBS" | TYPE == "SMM"], na.rm = TRUE)/ni_total
                        #
                       # Number of Taxa ####
                       # account for "NONE" in nt_total, should be the only 0 N_TAXA
                       , nt_total=dplyr::n_distinct(TAXAID[N_TAXA > 0], na.rm = TRUE)
                       #, nt_benthic=dplyr::n_distinct(TAXAID[TYPE == "DARTER" | TYPE == "SCULPIN" | TYPE == "MADTOM" | TYPE == "LAMPREY"])
                       , nt_benthic=dplyr::n_distinct(TAXAID[TYPE == "BENTHIC"], na.rm = TRUE)
                       , nt_native = dplyr::n_distinct(TAXAID[NATIVE == "NATIVE" & N_TAXA > 0], na.rm = TRUE)
                       , nt_nativenonhybrid = dplyr::n_distinct(TAXAID[NATIVE == "NATIVE" &
                                                                         (HYBRID != TRUE | is.na(HYBRID))], na.rm = TRUE)
                       , nt_beninvert = dplyr::n_distinct(TAXAID[TYPE == "BENTHIC" & TROPHIC == "IV"], na.rm = TRUE)
                       , nt_natsunfish = dplyr::n_distinct(TAXAID[NATIVE == "NATIVE" & TYPE == "SUNFISH"], na.rm = TRUE)
                       , nt_natcent = dplyr::n_distinct(TAXAID[NATIVE == "NATIVE" &
                                                                 (TYPE == "SUNFISH" | TYPE == "CENTRARCHIDAE")], na.rm = TRUE)
                       , nt_natinsctcypr = dplyr::n_distinct(TAXAID[TROPHIC == "IS" & FAMILY == "CYPRINIDAE"], na.rm = TRUE)
                       , nt_natrbs = dplyr::n_distinct(TAXAID[NATIVE == "NATIVE" & TYPE == "RBS"], na.rm = TRUE)
                       #
                       # Feeding ####
                       # % Lithophilic spawners
                       , pi_lithophil=100*sum(N_TAXA[SILT == TRUE], na.rm = TRUE)/ni_total
                       # % gen, omn, invert
                       , pi_genomninvrt=100*sum(N_TAXA[TROPHIC == "GE" | TROPHIC == "OM" | TROPHIC == "IV"], na.rm = TRUE)/ ni_total
                       # % insectivore
                       , pi_insectivore=100*sum(N_TAXA[TROPHIC == "IS"], na.rm = TRUE)/ ni_total
                       , pi_insctcypr = 100*sum(N_TAXA[TROPHIC == "IS" & FAMILY == "CYPRINIDAE"], na.rm = TRUE)/ ni_total
                       , pi_genherb = 100*sum(N_TAXA[TROPHIC == "GE" | TROPHIC == "HB"], na.rm = TRUE)/ ni_total
                       , pi_topcarn = 100*sum(N_TAXA[TROPHIC == "TC"], na.rm = TRUE)/ ni_total
                       #
                       # Tolerance ####
                       , nt_tv_intol = dplyr::n_distinct(TAXAID[TOLER == "INTOLERANT"], na.rm = TRUE)
                       , nt_tv_intolhwi = dplyr::n_distinct(TAXAID[TOLER == "INTOLERANT" | TOLER == "HWI"], na.rm = TRUE)
                       , pi_tv_toler= 100*sum(N_TAXA[TOLER=="TOLERANT"], na.rm = TRUE)/ni_total
                       #
                       #
                       # Indices ####
                       #,pi_dom01/2/3/5 #last? or nth
                       , pi_dom01=100*max(N_TAXA, na.rm = TRUE)/ni_total
                       # Shannon-Weiner
                       #, x_Shan_Num= -sum(log(N_TAXA/ni_total)), na.rm=TRUE)
                       #, x_Shan_e=x_Shan_Num/log(exp(1))
                       , x_Shan_e = -sum((N_TAXA/ni_total)*log((N_TAXA/ni_total)), na.rm=TRUE)
                       , x_Shan_2 = x_Shan_e/log(2)
                       , x_Shan_10 = x_Shan_e/log(10)
                       , x_Evenness = x_Shan_e/log(nt_total)
                       , x_Evenness100_ni99gt = ifelse(ni_total < 100, 1, x_Evenness * 100)
                       #
                       # Other ####
                       #, area_m = max(SAMP_WIDTH_M, na.rm = TRUE)*max(SAMP_LENGTH_M, na.rm = TRUE)
                       , length_m = max(SAMP_LENGTH_M, na.rm = TRUE)
                       # Abund / sq meter
                       #, ni_m2=ni_total/area_m #/(StWidAvg*StLength)
                       , ni_200m = 200 * ni_total / length_m
                       , ni_natnonhybridnonmf_200m = 200 * ni_natnonhybridnonmf / length_m
                       , ni_natnonhybridnonmfnonlepomis_200m = 200 * ni_natnonhybridnonmfnonlepomis / length_m
                       # biomass per square meter
                  #     , biomass_total=max(SAMP_BIOMASS, na.rm = TRUE)
                  #     , biomass_m2=biomass_total/area_m #/(StWidAvg*StLength)
                       # Anomalies
                       , pi_anomalies = 100 * sum(N_ANOMALIES, na.rm = TRUE)/ni_total
                       # #
                       # BCG ####
                       # ,nt_BCG_att123=n_distinct(Count[EXCLUDE!=TRUE & (BCG_Atr=="1" | BCG_Atr=="2" | BCG_Atr=="3")])
                       #
                       # Clean Up ####
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
  #
  # replace NA with 0
  met.val[is.na(met.val)] <- 0
  #
  # # # subset to only metrics specified by user
  # # if (!is.null(MetricNames)){
  # #   met.val <- met.val[,c(Index_Name, SITE, INDEX_REGION, ACREAGE, LEN_SAMP, MetricNames)]
  # # }
  # myFlds_Remove <- c("ni_total", "pi_rbs",
  #                    , "pi_brooktrout", "pi_sculpin", "nt_total"
  #                    , "nt_benthic", "pi_lithophil",
  #                    , "pi_genomninvrt", "pi_insectivore",
  #                    , "pi_tv_toler", "pi_dom01", "area", "ni_m2"
  #                    , "x_biomass_total", "x_biomass_m2")
  # met.val <- met.val[,-match(myFlds_Remove,names(met.val))]

  # # subset to only metrics specified by user
  if (is.null(MetricNames)) {
    #met.val <- met.val
  } else {
    met2include <- MetricNames[!(MetricNames %in% "ni_total")]
    # remove ni_total if included as will always include it
    met.val <- met.val[, c("SAMPLEID", "INDEX_REGION", "INDEX_NAME", met2include)]
  }##IF~MetricNames~END

  # Add extra fields
  if(is.null(cols2keep)){##IF.is.null.cols2keep.START
    df.return <- as.data.frame(met.val)
  } else {
    # create df with grouped fields
    myDF.cols2keep <- myDF %>% dplyr::group_by_(.dots=c("SAMPLEID", cols2keep)) %>%
      dplyr::summarize(col.drop=sum(N_TAXA))
    col.drop <- ncol(myDF.cols2keep)
    myDF.cols2keep <- myDF.cols2keep[,-col.drop]
    # merge
    df.return <- merge(as.data.frame(myDF.cols2keep), as.data.frame(met.val), by="SAMPLEID")
  }##IF.is.null.cols2keep.END





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
  #     met.val[,"NUMBENTSP_m"][met.val[,"INDEX_REGION"]=="COASTAL"]   <- 1.69
  #     met.val[,"NUMBENTSP_m"][met.val[,"INDEX_REGION"]=="EPIEDMONT"] <- 1.25
  #     met.val[,"NUMBENTSP_m"][met.val[,"INDEX_REGION"]=="HIGHLAND"]  <- 1.23
  #     ## b
  #     met.val[,"NUMBENTSP_b"] <- NA
  #     met.val[,"NUMBENTSP_b"][met.val[,"INDEX_REGION"]=="COASTAL"]   <- -3.33
  #     met.val[,"NUMBENTSP_b"][met.val[,"INDEX_REGION"]=="EPIEDMONT"] <- -2.36
  #     met.val[,"NUMBENTSP_b"][met.val[,"INDEX_REGION"]=="HIGHLAND"]  <- -2.35
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
  return(df.return)
}##FUNCTION.metric.values.fish.END
#
#
#' @export
metric.values.algae <- function(myDF, MetricNames=NULL
                                , boo.Adjust=FALSE
                                , cols2keep=NULL){##FUNCTION.metric.values.algae.START
  # define pipe
  `%>%` <- dplyr::`%>%`
  # Calculate Metrics (could have used pipe, %>%)
    met.val <- dplyr::summarise(dplyr::group_by(myDF, SampleID, "Index_Name", "Index_Type")
                #
                # individuals, total
                ,ni_total=sum(N_TAXA)
                #
    )##met.val.END
    # replace NA with 0
    met.val[is.na(met.val)] <- 0
    # subset to only metrics specified by user
    if (!is.null(MetricNames)){
      met.val <- met.val[,c(SampleID, "Index_Name", "Index_Type", "ni_total", MetricNames)]
    }
    # df to report back
    return(met.val)
}##FUNCTION.metric.values.algae.END
