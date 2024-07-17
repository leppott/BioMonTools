#' Score metrics
#'
#' This function calculates metric scores based on a Thresholds data frame.
#' Can generate scores for categories n=3 (e.g., 1/3/5, ScoreRegime="Cat_135")
#' or n=4 (e.g., 0/2/4/6, ScoreRegime="Cat_0246")
#' or continuous (e.g., 0-100, ScoreRegime="Cont_0100").
#'
#' The R library dplyr is needed for this function.
#'
#' For all ScoreRegime cases at the index level a "sum_Index" field is computed
#' that is the sum of all metric scores.  Valid "ScoreRegime" values are:
#'
#' * SUM = all metric scores added together.
#'
#' * AVERAGE = all metric scores added and divided by the number of metrics.
#' The index is on the same scale as the individual metric scores.
#'
#' * AVERAGE_100 = AVERAGE is scaled 0 to 100.
#'
#' FIX, 2024-01-29, v1.0.0.9060
#' Rename col_IndexRegion to col_IndexClass
#' Add col_IndexRegion as variable at end to avoid breaking existing code
#' Later remove it as an input variable but add code in the function to accept
#
#' @param DF_Metrics Data frame of metric values (as columns), Index Name, and
#' Index Region (strata).
#' @param col_MetricNames Names of columns of metric values.
#' @param col_IndexName Name of column with index (e.g., MBSS.2005.Bugs)
#' @param col_IndexClass Name of column with relevant bioregion or site class
#' (e.g., COASTAL).
#' @param DF_Thresh_Metric Data frame of Scoring Thresholds for metrics
#' (INDEX_NAME, INDEX_CLASS,
#' METRIC_NAME, Direction, Thresh_Lo, Thresh_Mid, Thresh_Hi, ScoreRegime
#' , SingleValue_Add, NormDist_Tail_Lo, NormDist_Tail_Hi, CatGrad_xvar
#' , CatGrad_InfPt, CatGrad_Lo_m,	CatGrad_Lo_b,	CatGrad_Mid_m,	CatGrad_Mid_b
#' ,	CatGrad_Hi_m,	CatGrad_Hi_b).
#' @param DF_Thresh_Index Data frame of Scoring Thresholds for indices
#' (INDEX_NAME, INDEX_CLASS,METRIC_NAME, ScoreRegime, Thresh01, Thresh02
#' , Thresh03, Thresh04, Thresh05, Thresh06, Thresh07
#' , Nar01, Nar02, Nar03, Nar04, Nar05, Nar06).
#' @param col_ni_total Name of column with total number of individuals.  Used
#' for cases where sample was collected but no organisms collected.
#' Default = ni_total.#'
#' @param col_IndexRegion Name of column with relevant bioregion or site class
#' (e.g., COASTAL). Default = NULL. DEPRECATED
# @param col_Xvar Name of column with additional variable needed to calculate
# scores.  For example, log10 drainage area.
#'
#' @return vector of scores
#
#' @examples
#' # Example data
#'
#' library(readxl)
#' library(reshape2)
#'
#' # Thresholds
#' fn_thresh <- file.path(system.file(package = "BioMonTools")
#'                        , "extdata"
#'                        , "MetricScoring.xlsx")
#' df_thresh_metric <- read_excel(fn_thresh, sheet = "metric.scoring")
#' df_thresh_index <- read_excel(fn_thresh, sheet = "index.scoring")
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~
#' # Pacific Northwest, BCG Level 1 Indicator Taxa Index
#' df_samps_bugs <- read_excel(system.file("extdata/Data_Benthos.xlsx"
#'                                         , package = "BioMonTools")
#'                             , guess_max = 10^6)
#'
#' myIndex <- "BCG_PacNW_L1"
#' df_samps_bugs$Index_Name   <- myIndex
#' df_samps_bugs$Index_Class <- "ALL"
#' (myMetrics.Bugs <- unique(as.data.frame(df_thresh_metric)[df_thresh_metric[
#'                           , "INDEX_NAME"] == myIndex, "METRIC_NAME"]))
#' # Run Function
#' df_metric_values_bugs <- metric.values(df_samps_bugs
#'                                        , "bugs"
#'                                        , fun.MetricNames = myMetrics.Bugs)
#'
#' # index to BCG.PacNW.L1
#' df_metric_values_bugs$INDEX_NAME <- myIndex
#' df_metric_values_bugs$INDEX_CLASS <- "ALL"
#'
#' # SCORE Metrics
#' df_metric_scores_bugs <- metric.scores(df_metric_values_bugs
#'                                        , myMetrics.Bugs
#'                                        , "INDEX_NAME"
#'                                        , "INDEX_CLASS"
#'                                        , df_thresh_metric
#'                                        , df_thresh_index)
#'
#'\dontrun{
#' # View Results
#' View(df_metric_scores_bugs)
#'}
#' # QC, table
#' table(df_metric_scores_bugs$Index, df_metric_scores_bugs$Index_Nar)
#' # QC, plot
#' hist(df_metric_scores_bugs$Index
#'      , main = "PacNW BCG Example Data"
#'      , xlab = "Level 1 Indicator Taxa Index Score")
#' abline(v=c(21,30), col = "blue")
#' text(21 + c(-2, +2), 200, c("Low", "Medium"), col = "blue")
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~
#' # Metrics, Index, Benthic Macroinvertebrates, genus
#' # (generate values then scores)
#' myIndex <- "MBSS_2005_Bugs"
#' # Thresholds
#' # imported above
#' # get metric names for myIndex
#' (myMetrics_Bugs_MBSS <- unique(df_thresh_metric[df_thresh_metric[
#' , "INDEX_NAME"]
#'                                == myIndex, "METRIC_NAME", TRUE]))
#' # Taxa Data
#' myDF_Bugs_MBSS <- data_benthos_MBSS
#' myDF_Bugs_MBSS$NONTARGET <- FALSE
#' myDF_Bugs_MBSS$INDEX_CLASS <- toupper(myDF_Bugs_MBSS$INDEX_CLASS)
#' myDF_Bugs_MBSS$SAMPLEID <- myDF_Bugs_MBSS$SITE
#' myDF_Bugs_MBSS$INDEX_NAME <- myIndex
#' myDF_Bugs_MBSS$TAXAID <- myDF_Bugs_MBSS$TAXON
#' myDF_Bugs_MBSS$SubPhylum <- NA
#' myDF_Bugs_MBSS$SubFamily <- NA
#' myDF_Bugs_MBSS$TOLVAL <- myDF_Bugs_MBSS$FinalTolVal07
#' myDF_Bugs_MBSS$TOLVAL2 <- myDF_Bugs_MBSS$FinalTolVal08
#' myDF_Bugs_MBSS$EXCLUDE <- myDF_Bugs_MBSS$EXCLUDE=="Y"
#'
#' myMetric_Values_Bugs_MBSS <- metric.values(myDF_Bugs_MBSS
#'                                            , "bugs"
#'                                            , myMetrics_Bugs_MBSS)
#'
#'\dontrun{
#' View(myMetric_Values_Bugs_MBSS)
#' }
#' # SCORE
#' myMetric_Values_Bugs_MBSS$INDEX_CLASS <- toupper(myMetric_Values_Bugs_MBSS$INDEX_CLASS)
#' Metrics_Bugs_Scores_MBSS <- metric.scores(myMetric_Values_Bugs_MBSS
#'                                           , myMetrics_Bugs_MBSS
#'                                           , "INDEX_NAME"
#'                                           , "INDEX_CLASS"
#'                                           , df_thresh_metric
#'                                           , df_thresh_index)
#' \dontrun{
#' # View Results
#' View(Metrics_Bugs_Scores_MBSS)
#' }
#'
#' # QC Index Scores and Narratives
#' # Set Narrative as Ordered Factor
#' Nar_MBSS <- c("Very Poor", "Poor", "Fair", "Good")
#' Metrics_Bugs_Scores_MBSS$Index_Nar <- factor(Metrics_Bugs_Scores_MBSS$Index_Nar
#'                                             , levels=Nar_MBSS
#'                                             , labels=Nar_MBSS
#'                                             , ordered=TRUE)
#' table(Metrics_Bugs_Scores_MBSS$Index, Metrics_Bugs_Scores_MBSS$Index_Nar, useNA="ifany")
#'
#' # QC bug count (manual)
#' Metrics_Bugs_Scores_MBSS[Metrics_Bugs_Scores_MBSS[,"ni_total"]>120,
#' "QC_Count"] <- "LARGE"
#' Metrics_Bugs_Scores_MBSS[Metrics_Bugs_Scores_MBSS[,"ni_total"]<60,
#' "QC_Count"] <- "SMALL"
#' Metrics_Bugs_Scores_MBSS[is.na(Metrics_Bugs_Scores_MBSS[,"QC_Count"]),
#' "QC_Count"] <- "OK"
#' # table of QC_Count
#' table(Metrics_Bugs_Scores_MBSS$QC_Count)
#'
#' # QC bug count (with function)
#' # Import Checks
#' #df_checks <- read_excel(system.file("./extdata/MetricFlags.xlsx"
#' #                                           , package="BioMonTools"), sheet="Flags")
#' # Run Function
#' #df_flags <- qc.checks(Metrics_Bugs_Scores_MBSS, df_checks)
#' # Summarize Results
#' # table(df_flags[,"CHECKNAME"], df_flags[,"FLAG"], useNA="ifany")
#'
#' @export
metric.scores <- function(DF_Metrics
                          , col_MetricNames
                          , col_IndexName
                          , col_IndexClass
                          , DF_Thresh_Metric
                          , DF_Thresh_Index
                          , col_ni_total = "ni_total"
                          , col_IndexRegion = NULL) {
  #
  boo.QC <- FALSE
  if (boo.QC == TRUE) {
    # DF_Metrics <- df_metric_values_bugs
    # col_MetricNames <- myMetrics.Bugs
    # col_IndexName <- "INDEX_NAME"
    # col_IndexRegion <-  "INDEX_CLASS"
    # DF_Thresh <- df_thresh
    #~~~~~~~~~~~~~~~~~~~~~
    # DF_Metrics       <- df_metric_values_bugs
    # col_MetricNames  <- myMetrics.Bugs
    # col_IndexName    <- "INDEX_NAME"
    # col_IndexRegion  <- "INDEX_CLASS"
    # DF_Thresh_Metric <- df_thresh_metric
    # DF_Thresh_Index  <- df_thresh_index
    (a <- unique(as.matrix(DF_Metrics[, col_IndexName]))[1])
    (b <- toupper(unique(as.matrix(DF_Metrics[, col_IndexClass]))[2]))
    (c <- col_MetricNames[8])
    (aa <- unique(as.matrix(DF_Metrics[, col_IndexName]))[1])
    (bb <- toupper(unique(as.matrix(DF_Metrics[, col_IndexClass]))[2]))
  }##IF~boo.QC~END

  # global variable bindings ----
  INDEX_NAME <- INDEX_CLASS <- METRIC_NAME <- NULL

  # define pipe
  `%>%` <- dplyr::`%>%`

  # IndexClass not IndexRegion ----
  if (is.na(col_IndexClass)) {
    col_IndexClass <- col_IndexRegion
  }##


  # QC ####
  #
  # Ensure have a data frame not a tibble
  DF_Metrics       <- as.data.frame(DF_Metrics)
  DF_Thresh_Metric <- as.data.frame(DF_Thresh_Metric)
  DF_Thresh_Index  <- as.data.frame(DF_Thresh_Index)
  #
  # QC, Column Names
  # Error check on fields (thresh metric)
  myFlds <- c("INDEX_NAME", "INDEX_CLASS", "METRIC_NAME", "Thresh_Lo", "Thresh_Mid", "Thresh_Hi"
              , "Direction", "ScoreRegime", "SingleValue_Add", "NormDist_Tail_Lo", "NormDist_Tail_Hi"
              , "CatGrad_xvar", "CatGrad_InfPt", "CatGrad_Lo_m",	"CatGrad_Lo_b",	"CatGrad_Mid_m"
              ,	"CatGrad_Mid_b",	"CatGrad_Hi_m", "CatGrad_Hi_b")
  if (length(myFlds) != sum(myFlds %in% names(DF_Thresh_Metric))) {
    myMsg <- paste0("Fields missing from DF_Thresh_Metric input data frame (v0.3.3.9011 and after). Expecting: \n"
                    , paste(myFlds[!(myFlds %in% names(DF_Thresh_Metric))]
                            , sep = ""
                            , collapse = ", ")
                    , collapse = "")
    stop(myMsg)
  }
  # Error check on fields (metrics)
  myFlds_2 <- c("INDEX_NAME", "INDEX_CLASS")
  if (length(myFlds_2) != sum(myFlds_2 %in% names(DF_Metrics))) {
    myMsg <- paste0("Fields missing from DF_Metrics input data frame.  Expecting: \n"
                    , paste(myFlds_2, sep = "", collapse = ", ")
                    , collapse = "")
    stop(myMsg)
  }
  # Error check on fields (thresh Index)
  myFlds_Index <- c("INDEX_NAME", "INDEX_CLASS", "NumMetrics", "ScoreRegime"
                    , paste0("Thresh0",1:6), paste0("Nar0",1:5))
  if (length(myFlds_Index) != sum(myFlds_Index %in% names(DF_Thresh_Index))) {
    myMsg <- paste0("Fields missing from DF_Metrics input data frame.  Expecting: \n"
                    , paste(myFlds_Index, sep = "", collapse = ", ")
                    , collapse = "")
    stop(myMsg)
  }

  # Munge ####

  # Index Region Field to upper case
  DF_Metrics[,"INDEX_CLASS"]       <- toupper(as.matrix(DF_Metrics[,"INDEX_CLASS"]))
  DF_Thresh_Metric[,"INDEX_CLASS"] <- toupper(as.matrix(DF_Thresh_Metric[,"INDEX_CLASS"]))
  DF_Thresh_Index[,"INDEX_CLASS"]  <- toupper(as.matrix(DF_Thresh_Index[,"INDEX_CLASS"]))

  # Add "SCORE" columns for each metric
  Score.MetricNames <- paste0("SC_", col_MetricNames)
  DF_Metrics[, Score.MetricNames] <- NA

  # SCORING ####
  # Need to cycle based on Index (a), Region (b), and Metric (c)
  for (a in unique(as.matrix(DF_Metrics[, col_IndexName]))) {
    for (b in unique(as.matrix(DF_Metrics[, col_IndexClass]))) {
      for (c in col_MetricNames) {
        #
        # Thresholds (filter with dplyr)
        fun.Thresh.myMetric <- as.data.frame(dplyr::filter(DF_Thresh_Metric
                                                           , INDEX_NAME == a
                                                           & INDEX_CLASS == b
                                                           & METRIC_NAME == c))
        # QC
        #stopifnot(nrow(fun.Thresh.myMetric)==1)
        if (nrow(fun.Thresh.myMetric) != 1) {
          #return(0)
          next
        }##IF~nrow~END
        #
        # Debug
        if (boo.QC) {
          myMsg <- paste0("\nIndex = ", a, ", Region = ", b, ", Metric = ", c)
          message(myMsg)
        }##IF~boo.QC~END

        #
        # thresholds
        ## suppress warnings as some will be "NA".
        # use drop = TRUE so a Tibble behaves more like a data frame and returns a single value
        fun.Lo          <- suppressWarnings(as.numeric(fun.Thresh.myMetric[, "Thresh_Lo", drop = TRUE]))
        fun.Mid         <- suppressWarnings(as.numeric(fun.Thresh.myMetric[, "Thresh_Mid", drop = TRUE]))
        fun.Hi          <- suppressWarnings(as.numeric(fun.Thresh.myMetric[, "Thresh_Hi", drop = TRUE]))
        fun.Direction   <- toupper(fun.Thresh.myMetric[, "Direction", drop = TRUE])
        fun.ScoreRegime <- toupper(fun.Thresh.myMetric[, "ScoreRegime", drop = TRUE])
        fun.SV_Add      <- suppressWarnings(as.numeric(fun.Thresh.myMetric[, "SingleValue_Add", drop = TRUE]))
        fun.ND_Lo       <- suppressWarnings(as.numeric(fun.Thresh.myMetric[, "NormDist_Tail_Lo", drop = TRUE]))
        fun.ND_Hi       <- suppressWarnings(as.numeric(fun.Thresh.myMetric[, "NormDist_Tail_Hi", drop = TRUE]))
        fun.CG_xvar     <- suppressWarnings(toupper(fun.Thresh.myMetric[, "CatGrad_xvar", drop = TRUE]))
        fun.CG_IP       <- suppressWarnings(as.numeric(fun.Thresh.myMetric[, "CatGrad_InfPt", drop = TRUE]))
        fun.CG_Lo_m     <- suppressWarnings(as.numeric(fun.Thresh.myMetric[, "CatGrad_Lo_m", drop = TRUE]))
        fun.CG_Lo_b     <- suppressWarnings(as.numeric(fun.Thresh.myMetric[, "CatGrad_Lo_b", drop = TRUE]))
        fun.CG_Mid_m    <- suppressWarnings(as.numeric(fun.Thresh.myMetric[, "CatGrad_Mid_m", drop = TRUE]))
        fun.CG_Mid_b    <- suppressWarnings(as.numeric(fun.Thresh.myMetric[, "CatGrad_Mid_b", drop = TRUE]))
        fun.CG_Hi_m     <- suppressWarnings(as.numeric(fun.Thresh.myMetric[, "CatGrad_Hi_m", drop = TRUE]))
        fun.CG_Hi_b     <- suppressWarnings(as.numeric(fun.Thresh.myMetric[, "CatGrad_Hi_b", drop = TRUE]))
        #
        fun.SelMet_Name  <- suppressWarnings(fun.Thresh.myMetric[, "SelMet_Master_Name", drop = TRUE])
        fun.SelMet_Score <- suppressWarnings(as.numeric(fun.Thresh.myMetric[, "SelMet_Master_Score", drop = TRUE]))
        fun.SelMet_Cond  <- suppressWarnings(fun.Thresh.myMetric[, "SelMet_Master_Condition", drop = TRUE])
        #
        fun.ScMet_Name  <- suppressWarnings(fun.Thresh.myMetric[, "ScMet_Master_Name", drop = TRUE])
        fun.ScMet_Value <- suppressWarnings(as.numeric(fun.Thresh.myMetric[, "ScMet_Master_Value", drop = TRUE]))
        fun.ScMet_Cond  <- suppressWarnings(fun.Thresh.myMetric[, "ScMet_Master_Condition", drop = TRUE])
        fun.ScMet_ScMet <- suppressWarnings(as.numeric(fun.Thresh.myMetric[, "ScMet_Score", drop = TRUE]))
        #
        fun.S0_Met <- suppressWarnings(fun.Thresh.myMetric[, "ScoreZero_Metric", drop = TRUE])
        fun.S0_Thr <- suppressWarnings(as.numeric(fun.Thresh.myMetric[, "ScoreZero_Thresh", drop = TRUE]))
        fun.S0_Dir <- suppressWarnings(toupper(fun.Thresh.myMetric[, "ScoreZero_Direction", drop = TRUE]))
        #
        # default value
        fun.Value <- DF_Metrics[, c]
        fun.Result <- fun.Value * NA  #default value of NA
        #
        if (fun.ScoreRegime == "CONT_0100") {
          ## Cont_0100 ####
          fun.Result_Mult <- 1 # multiplier
          score_max <- 100
          if (fun.Direction == "DECREASE") {
            fun.calc <- score_max*((fun.Value - fun.Lo) / (fun.Hi - fun.Lo))
          }else if (fun.Direction == "INCREASE") {
            fun.calc <- score_max*((fun.Hi - fun.Value) / (fun.Hi - fun.Lo))
          }
          fun.Result <- sapply(fun.calc, function(x) {stats::median(c(0, score_max, x))})
        } else if (fun.ScoreRegime == "CONT_0010") {
          ## Cont_0010 ####
          score_max <- 10
          if (fun.Direction == "DECREASE") {
            fun.calc <- score_max*((fun.Value - fun.Lo) / (fun.Hi - fun.Lo))
          }else if (fun.Direction == "INCREASE") {
            fun.calc <- score_max*((fun.Hi - fun.Value) / (fun.Hi - fun.Lo))
          }
          fun.Result <- sapply(fun.calc, function(x) {stats::median(c(0, score_max, x))})
        } else if (fun.ScoreRegime == "CONT_0010_SC0") {
          ## Cont_0010_Sc0----
          # leave as Cont_0010
          score_max <- 10
          if (fun.Direction == "DECREASE") {
            fun.calc <- score_max*((fun.Value - fun.Lo) / (fun.Hi - fun.Lo))
          }else if (fun.Direction == "INCREASE") {
            fun.calc <- score_max*((fun.Hi - fun.Value) / (fun.Hi - fun.Lo))
          }
          fun.Result <- sapply(fun.calc, function(x) {stats::median(c(0, score_max, x))})

       #   S0 <- fun.S0_Met[1]
        #  if (!is.na(S0)) {
            # Add other metric
            # fun.Thresh.myMetric <- dplyr::filter(DF_Thresh_Metric
            #                                      , INDEX_NAME == a
            #                                      & INDEX_CLASS == b
            #                                      & METRIC_NAME %in% c(c, S0)) %>%
            #   tidyr::pivot_wider(DF_Thresh_Metric
            #                      , id_cols = c(INDEX_NAME, INDEX_CLASS)
            #                      , names_from = METRIC_NAME) %>%
            #   as.data.frame()
            # # direction
            # if (fun.S0_Dir == "LESSTHAN") {
            #   # check if less than
            #
            # } else if (fun.S0_Dir == "GREATERTHAN") {
            #   fun.Result <- NA
            # } else {
            #   fun.Result <- NA
            # }## IF ~ fun.S0_Dir
            #
            #
            # # fun.S0_Met
            # # fun.S0_thr
            # # fun.S0_dir
            #

         # # } else {
         #    score_max <- 10
         #    if (fun.Direction == "DECREASE") {
         #      fun.calc <- score_max*((fun.Value - fun.Lo) / (fun.Hi - fun.Lo))
         #    }else if (fun.Direction == "INCREASE") {
         #      fun.calc <- score_max*((fun.Hi - fun.Value) / (fun.Hi - fun.Lo))
         #    }
         #    fun.Result <- fun.Result_Mult * sapply(fun.calc, function(x) {stats::median(c(0, score_max, x))})
         #  }## IF ~ ScoreZero
        } else if (fun.ScoreRegime == "CAT_135") {
          ## Cat_135 ####
          if (fun.Direction == "DECREASE") {
            fun.Result <- ifelse(fun.Value >= fun.Hi
                                 , 5
                                 ,ifelse(fun.Value < fun.Lo, 1, 3))
            if (boo.QC == TRUE) {
              message(paste0("\nMetric="
                             , c
                             , ", Value="
                             , fun.Value
                             , ", Result="
                             , fun.Result))
              #utils::flush.console()
            }##IF.boo.QC.END
          } else if (fun.Direction == "INCREASE") {
            fun.Result <- ifelse(fun.Value <= fun.Lo
                                 , 5
                                 , ifelse(fun.Value > fun.Hi, 1, 3))
          }##IF~fun.Direction~END
        } else if (fun.ScoreRegime == "CAT_0246" | fun.ScoreRegime == "CAT_0123") {
          ## Cat_0246 ####
          if (fun.Direction == "DECREASE") {
            fun.Result <- ifelse(fun.Value >= fun.Hi
                                 , 6
                                 ,ifelse(fun.Value >= fun.Mid
                                         , 4
                                         , ifelse(fun.Value >= fun.Lo, 2, 0)))
            if (fun.ScoreRegime == "CAT_0123") {
              fun.Result <- fun.Result / 2
            }##CAT_0123.END
          } else if (fun.Direction == "INCREASE") {
            fun.Result <- ifelse(fun.Value <= fun.Lo
                                 , 6
                                 , ifelse(fun.Value <= fun.Mid
                                          , 4
                                          , ifelse(fun.Value <= fun.Hi, 2, 0)))
            if (fun.ScoreRegime == "CAT_0123") {
              fun.Result <- fun.Result / 2
            }##CAT_0123.END
          } else if (fun.Direction == "SCOREVALUE") {
            fun.Result <- fun.Value
          }##SCOREVALUE.END
        } else if (fun.ScoreRegime == "NORMDIST_135") {
          ## NormDist_135 ####
          fun.Result <- ifelse(fun.Value < fun.ND_Lo | fun.Value > fun.ND_Hi, 1
                               , ifelse(fun.Value >= fun.ND_Lo & fun.Value < fun.Lo, 3
                                        , ifelse(fun.Value <= fun.ND_Hi & fun.Value > fun.Hi, 3
                                                 , ifelse(fun.Value >= fun.Lo & fun.Value <= fun.Hi, 5, NA))))
        } else if (fun.ScoreRegime == "SINGLEVALUE") {
          ## SingleValue ####
          if (!is.na(fun.Hi)) {
            fun.Result <- ifelse(fun.Value > fun.Hi, fun.SV_Add, 0)
          }##SingleValue_Hi
          #
          if (!is.na(fun.Lo)) {
            fun.Result <- ifelse(fun.Value < fun.Lo, fun.SV_Add, 0)
          }##SingleValue_Lo
        } else if (fun.ScoreRegime == "CATGRAD_135") {
          ## ContGrad135 ####
          #
          # get xvar and calc Expected Score
          # QC to ensure xvar is present in data
          boo_CG_xvar <- fun.CG_xvar %in% names(DF_Metrics)
          if (boo_CG_xvar == FALSE) {
            myMsg <- paste0("\nField missing from DF_Metric input data frame. Expecting: \n"
                            , fun.CG_xvar)
            stop(myMsg)
          }##IF~boo_CG_xvar~END
          #
          fun.CG_xval     <- suppressWarnings(as.numeric(DF_Metrics[, fun.CG_xvar]))
          #x_var <- fun.CG_xvar
          #x_Obs <- fun.Value
          # Xvar specific thresholds
          # y = mx + b
          x_Exp_Lo  <- (fun.CG_Lo_m * fun.CG_xval) + fun.CG_Lo_b
          x_Exp_Mid <- (fun.CG_Mid_m * fun.CG_xval) + fun.CG_Mid_b
          x_Exp_Hi  <- (fun.CG_Hi_m * fun.CG_xval) + fun.CG_Hi_b
          #
          # Check for inflection point, then score based on Gradient
          # Gradient is only a decrease scoring regime
          if (is.na(fun.CG_IP)) {
            # ContGrad_135 w/o IP
            fun.Result <- ifelse(fun.Value >= x_Exp_Hi
                                 , 5
                                 , ifelse(fun.Value < x_Exp_Lo, 1, 3))
          } else {
            fun.Result <- ifelse(fun.CG_xval >= fun.CG_IP
                                 , ifelse(fun.Value >= fun.Hi
                                          , 5
                                          , ifelse(fun.Value < fun.Lo, 1, 3))
                                 , ifelse(fun.Value >= x_Exp_Hi
                                          , 5
                                          , ifelse(fun.Value < x_Exp_Lo, 1, 3)))
          }##IF~is.na(fun.CG_IP)~END



          # use dplyr::mutate ?
          #~~~~~~~~~~

        } else if (fun.ScoreRegime == "CAT_135_SELMET") {
          ## Cat_135_SelMet ####
          #
          # QC, check for name
          c_master_boo <- fun.SelMet_Name %in% col_MetricNames
          if (c_master_boo == FALSE) {
            myMsg <- paste0("\nField missing from DF_Metrics input data frame for master metric.  Expecting: \n"
                            , fun.SelMet_Name)
            stop(myMsg)
          }
          # Check to see if "master" already scored
          # Get Master Metric Score
          c_master_score <- DF_Metrics[, paste0("SC_", fun.SelMet_Name)]
          #
          if (sum(is.na(c_master_score)) == length(c_master_score)) {
            myMsg <- paste0("\nMaster metric ("
                            , fun.SelMet_Name
                            , ") not scored before dependent metric ("
                            , c
                            , ").\n"
                            , "Reorder columns before proceeding.")
            stop(myMsg)
            # Scores for testing
            if (boo.QC == TRUE) {
              c_master_score <- rep_len(c(NA, 1, 3, 5), length(fun.Value))
            }##IF~boo.QC~END
          }##IF~c_master_Score~END
          #
          # Default Value
          fun.Result <- NA
          #
          # Keep only relevant values
          #fun.Value_orig <- fun.Value # for testing only
          if (fun.SelMet_Cond == "greaterthan") {
            fun.Value <- ifelse(c_master_score > fun.SelMet_Score, fun.Value, NA)
          } else if (fun.SelMet_Cond == "equal") {
            fun.Value <- ifelse(c_master_score == fun.SelMet_Score, fun.Value, NA)
          } else {
            fun.Value <- NA
          }##IF~SelMetMaster~END
          #
          # Score (only non NA)
          # Same Cat135 code as above
          # Cat_135 #
          if (fun.Direction == "DECREASE") {
            fun.Result <- ifelse(fun.Value >= fun.Hi
                                 , 5
                                 ,ifelse(fun.Value < fun.Lo, 1, 3))
            if (boo.QC == TRUE) {
              myMsg <- paste0("\nMetric="
                              , c
                              , ", Value="
                              , fun.Value
                              , ", Result="
                              , fun.Result)
              message(myMsg)
              #utils::flush.console()
            }##IF.boo.QC.END
          } else if (fun.Direction == "INCREASE") {
            fun.Result <- ifelse(fun.Value <= fun.Lo, 5
                                 ,ifelse(fun.Value > fun.Hi, 1, 3))
          }##IF~fun.direction~END
          #
        } else if (fun.ScoreRegime == "CAT_135_SCMET") {
          ## Cat_135_ScMet ####
          #
          # QC, check for name
          c_master_boo <- fun.ScMet_Name %in% col_MetricNames
          if (c_master_boo == FALSE) {
            myMsg <- paste0("\nField missing from DF_Metrics input data frame for master metric.  Expecting: \n"
                            , fun.ScMet_Name)
            stop(myMsg)
          }
          # Check to see if "master" already scored
          # Get Master Metric Score
          c_master_score <- DF_Metrics[, paste0("SC_", fun.ScMet_Name)]
          c_master_value <- DF_Metrics[, fun.ScMet_Name]
          #
          # if (sum(is.na(c_master_value)) == length(c_master_value)) {
          #   myMsg <- paste0("\nMaster metric (", fun.ScMet_Name, ") not included before dependent metric (", c, ").\n",
          #                   "Reorder columns before proceeding.")
          #   stop(myMsg)
          #   # # Scores for testing
          #   # if (boo.QC == TRUE) {
          #   #   c_master_score <- rep_len(c(NA, 1, 3, 5), length(fun.Value))
          #   # }##IF~boo.QC~END
          # }##IF~c_master_Score~END
          # shouldn't matter, just needs to be included.
          #
          # Default Value
          fun.Result <- NA
          #

          #
          # Score (only non NA)
          # Same Cat135 code as above
          # Cat_135 #
          if (fun.Direction == "DECREASE") {
            fun.Result <- ifelse(fun.Value >= fun.Hi
                                 , 5
                                 , ifelse(fun.Value < fun.Lo, 1, 3))
            if (boo.QC == TRUE) {
              myMsg <- paste0("\nMetric="
                              , c
                              , ", Value="
                              , fun.Value
                              , ", Result="
                              , fun.Result)
              message(myMsg)
              #utils::flush.console()
            }##IF.boo.QC.END
          } else if (fun.Direction == "INCREASE") {
            fun.Result <- ifelse(fun.Value <= fun.Lo
                                 , 5
                                 ,ifelse(fun.Value > fun.Hi, 1, 3))
          }##IF~fun.direction~END

          # Change scores if meet conditions
          if (fun.ScMet_Cond == "greaterthan") {
            fun.Result <- ifelse(c_master_value > fun.ScMet_Value
                                 , fun.ScMet_ScMet
                                 , fun.Result)
          } else if (fun.ScMet_Cond == "equal") {
            fun.Result <- ifelse(c_master_value == fun.ScMet_Value
                                 , fun.ScMet_ScMet
                                 , fun.Result)
          } else {
            # something not right.  Give "ERROR" rather than score so know is an issue.
            # Don't want to "stop" as wouldn't be able to get results.
            fun.Result <- "ERROR"
          }##IF~SelMetMaster~END

        } else if (fun.ScoreRegime == "Cat_101") {
          ## Cat_101 ----
          fun.Result <- ifelse(fun.Value > fun.Hi
                               , 1
                               , ifelse(fun.Value < fun.Lo, 1, 0))
          if (boo.QC == TRUE) {
            message(paste0("\nMetric="
                           , c
                           , ", Value="
                           , fun.Value
                           , ", Result="
                           , fun.Result))
            #utils::flush.console()
          }##IF.boo.QC.END

        } else if (fun.ScoreRegime == "CAT_0510") {
          ## Cat_0510----
          # MN IBI Fish
          if (fun.Direction == "INCREASE") {
            fun.Result <- ifelse(fun.Value >= fun.Hi
                                 , 0
                                 ,ifelse(fun.Value < fun.Lo, 10, 5))
            if (boo.QC == TRUE) {
              message(paste0("\nMetric="
                             , c
                             , ", Value="
                             , fun.Value
                             , ", Result="
                             , fun.Result))
              #utils::flush.console()
            }##IF.boo.QC.END
          } else if (fun.Direction == "DECREASE") {
            fun.Result <- ifelse(fun.Value < fun.Lo
                                 , 0
                                 , ifelse(fun.Value >= fun.Hi, 10, 5))
          }##IF~fun.Direction~END

        } else if (fun.ScoreRegime == "CAT_0510_SC0") {
          ## Cat_0510_Sc0----
          # leave as Cat_0510
          # MN IBI Fish
          if (fun.Direction == "INCREASE") {
            fun.Result <- ifelse(fun.Value >= fun.Hi
                                 , 0
                                 ,ifelse(fun.Value < fun.Lo, 10, 5))
            if (boo.QC == TRUE) {
              message(paste0("\nMetric="
                             , c
                             , ", Value="
                             , fun.Value
                             , ", Result="
                             , fun.Result))
              #utils::flush.console()
            }##IF.boo.QC.END
          } else if (fun.Direction == "DECREASE") {
            fun.Result <- ifelse(fun.Value < fun.Lo
                                 , 0
                                 , ifelse(fun.Value >= fun.Hi, 10, 5))
          }##IF~fun.Direction~END
        } else if (is.na(fun.ScoreRegime)) {
          ## No Score Regime ####
          fun.Result <- NA
        } else {
          ## OTHER ####
          fun.Result <- NA
        }##IF.scoring.END
        #
        # Update input DF with matching values
        myTF <- DF_Metrics[, col_IndexName] == a & DF_Metrics[, col_IndexClass] == b
        DF_Metrics[myTF, paste0("SC_", c)] <- fun.Result[myTF]
      }##FOR.c.END
    }##FOR.a.END
  }##FOR.b.END
  #

  # INDEX----
  DF_Metrics[,"sum_Index"] <- NA_real_
  DF_Metrics[,"Index"]     <- NA_real_
  DF_Metrics[,"Index_Nar"] <- NA_character_

  ## INDEX, Sum----
  # sum all metrics
  DF_Metrics[,"sum_Index"] <- rowSums(DF_Metrics[, Score.MetricNames]
                                      , na.rm = TRUE)

  ## INDEX, Value----
  # Need to cycle based on Index (aa) and Region (bb)
  for (aa in unique(as.matrix(DF_Metrics[,col_IndexName]))) {
    for (bb in unique(as.matrix(DF_Metrics[,col_IndexClass]))) {

      # Thresholds (filter with dplyr)
      fun.Thresh.myIndex <- as.data.frame(dplyr::filter(DF_Thresh_Index
                                                        , INDEX_NAME == aa
                                                          & INDEX_CLASS == bb))
      # QC
      if (nrow(fun.Thresh.myIndex) != 1) {
        #return(0)
        next
      }
      # thresholds
      fun.NumMetrics       <- as.numeric(fun.Thresh.myIndex[, "NumMetrics"])
      fun.ScoreRegime      <- fun.Thresh.myIndex[, "ScoreRegime"]
      fun.Scale            <- fun.Thresh.myIndex[, "ScoreScaling"]
      fun.Index.Nar.Thresh <- fun.Thresh.myIndex[, c(paste0("Thresh0"
                                                            , seq_len(7)))]
      fun.Index.Nar.Nar    <- fun.Thresh.myIndex[, c(paste0("Nar0", seq_len(6)))]

      fun.Index.Nar.Numb <- sum(!is.na(fun.Index.Nar.Nar), na.rm = TRUE)

      fun.ZeroInd_Use <- fun.Thresh.myIndex[, "Use_ZeroInd"]
      fun.ZeroInd_Sc  <- fun.Thresh.myIndex[, "ZeroInd_Score"]
      fun.ZeroInd_Nar <- fun.Thresh.myIndex[, "ZeroInd_Narrative"]

      # default value
      fun.Value <- DF_Metrics[, "sum_Index"]
      fun.Result <- fun.Value * NA  #default value of NA

      ## INDEX, Score Regime ----
      # Scoring
      if (fun.ScoreRegime == "AVERAGE") {
        ### AVERAGE ----
        fun.Result <- DF_Metrics[, "sum_Index"] / fun.NumMetrics
        # } else if (fun.ScoreRegime == "AVERAGE_10") {
        #   sr_mult    <- 10
        #   fun.Result <- sr_mult * DF_Metrics[, "sum_Index"] / fun.NumMetrics
        # } else if (fun.ScoreRegime == "AVERAGE_20") {
        #   sr_mult    <- 20
        #   fun.Result <- sr_mult * DF_Metrics[, "sum_Index"] / fun.NumMetrics
      } else if (fun.ScoreRegime == "AVERAGE_100") {
        ### AVERAGE_100 ----
        sr_mult    <- 100 / fun.NumMetrics
        fun.Result <- sr_mult * DF_Metrics[, "sum_Index"] / fun.NumMetrics
      } else if (fun.ScoreRegime == "AVERAGESCALE_100") {
        ### AVERAGESCALE_100 ----
        sr_mult    <- 100 / fun.NumMetrics / fun.Scale
        fun.Result <- sr_mult * DF_Metrics[, "sum_Index"]
      } else if (fun.ScoreRegime == "AVERAGE_010") {
        ### AVERAGE_010 ----
        sr_mult    <- 10 / fun.NumMetrics
        fun.Result <- sr_mult * DF_Metrics[, "sum_Index"] / fun.NumMetrics
      } else if (fun.ScoreRegime == "AVERAGE_100_M10_R2") {
        ### AVERAGE_100_M10_R2 ----
        # MPCA (MN)
        sr_mult    <- round(10 / fun.NumMetrics, 2)
        fun.Result <- sr_mult * DF_Metrics[, "sum_Index"]
      } else if (fun.ScoreRegime == "AVERAGE_100_M10_R3") {
        ### AVERAGE_100_M10_R3 ----
        # MPCA (MN)
        sr_mult    <- round(10 / fun.NumMetrics, 3)
        fun.Result <- sr_mult * DF_Metrics[, "sum_Index"]
      } else {
        ### SUM ----
        fun.Result <- DF_Metrics[, "sum_Index"]
      }##IF.scoring.END
      #
      # Narrative
      myBreaks <- as.numeric(paste(fun.Index.Nar.Thresh[1, 1:(fun.Index.Nar.Numb + 1)]))
      myLabels <- paste(fun.Index.Nar.Nar[1, 1:fun.Index.Nar.Numb])
      fun.Result.Nar <- as.vector(cut(fun.Result
                                      , breaks = myBreaks
                                      , labels = myLabels
                                      , include.lowest = TRUE
                                      , right = FALSE
                                      , ordered_result = TRUE))

      # Update for zero individuals
      if (fun.ZeroInd_Use == TRUE) {
        boo_zero_ni_total <- DF_Metrics[, col_ni_total] == 0
        fun.Result[boo_zero_ni_total]     <- fun.ZeroInd_Sc
        fun.Result.Nar[boo_zero_ni_total] <- fun.ZeroInd_Nar
      }##IF~fun.zeroind_use~END


      # Update input DF with matching values
      myTF <- DF_Metrics[, col_IndexName] == aa & DF_Metrics[, col_IndexClass] == bb
      DF_Metrics[myTF, "Index"]     <- fun.Result[myTF]
      DF_Metrics[myTF, "Index_Nar"] <- fun.Result.Nar[myTF]

      # Add factor levels for Index_Nar
      ## only works if doing a single index or multiple indices with the same narrative categories
      # DF_Metrics[myTF, "Index_Nar"] <- factor(DF_Metrics[myTF, "Index_Nar"]
      #                                         , levels = fun.Index.Nar.Nar
      #                                         , labels = fun.Index.Nar.Nar
      #                                         , ordered = TRUE)

    }##FOR.bb.END
  }##FOR.aa.END


  # Return original DF with added columns
  return(DF_Metrics)
  #
}##FUNCTION.metric.score.END
