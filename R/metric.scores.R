#' Score metrics
#'
#' This function calculates metric scores based on a Thresholds data frame.
#' Can generate scores for categories n=3 (e.g., 1/3/5, ScoreRegime="Cat_135")
#' or n=4 (e.g., 0/2/4/6, ScoreRegime="Cat_0246")
#' or continuous (e.g., 0-100, ScoreRegime="Cont_0100").
#'
#' The R library dplyr is needed for this function.
#'
#' ScoreRegime for metrics is as above.  ScoreRegime for an index is "SUM" or "AVERAGE".
#' That is, for SUM all metric scores are added together.  For AVERAGE all metric scores are averaged.
#' In both cases a "sum_Index" field will be computed.
#
#' @param DF_Metrics Data frame of metric values (as columns), Index Name, and
#' Index Region (strata).
#' @param col_MetricNames Names of columns of metric values.
#' @param col_IndexName Name of column with index (e.g., MBSS.2005.Bugs)
#' @param col_IndexRegion Name of column with relevant bioregion or site class
#' (e.g., COASTAL).
#' @param DF_Thresh_Metric Data frame of Scoring Thresholds for metrics (INDEX_NAME, INDEX_REGION,
#' METRIC_NAME, Direction, Thresh_Lo, Thresh_Mid, Thresh_Hi, ScoreRegime)
#' @param DF_Thresh_Index Data frame of Scoring Thresholds for indices (INDEX_NAME, INDEX_REGION,
#' METRIC_NAME, Direction, Thresh_Lo, Thresh_Mid, Thresh_Hi, ScoreRegime)
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
#' fn_thresh <- file.path(system.file(package="BioMonTools"), "extdata", "MetricScoring.xlsx")
#' df_thresh_metric <- read_excel(fn_thresh, sheet="metric.scoring")
#' df_thresh_index <- read_excel(fn_thresh, sheet="index.scoring")
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~
#' # Pacific Northwest, BCG Level 1 Indicator Taxa Index
#' df_samps_bugs <- read_excel(system.file("extdata/Data_Benthos.xlsx"
#'                                         , package="BioMonTools")
#'                             , guess_max = 10^6)
#'
#' myIndex <- "BCG.PacNW.L1"
#' df_samps_bugs$INDEX_NAME   <- myIndex
#' df_samps_bugs$INDEX_REGION <- "ALL"
#' (myMetrics.Bugs <- unique(as.data.frame(df_thresh_metric)[df_thresh_metric[,"INDEX_NAME"]==myIndex,"METRIC_NAME"]))
#' # Run Function
#' df_metric_values_bugs <- metric.values(df_samps_bugs, "bugs", fun.MetricNames = myMetrics.Bugs)
#'
#' # index to BCG.PacNW.L1
#' df_metric_values_bugs$INDEX_NAME <- myIndex
#' df_metric_values_bugs$INDEX_REGION <- "ALL"
#'
#' # SCORE Metrics
#' df_metric_scores_bugs <- metric.scores(df_metric_values_bugs
#'                                        , myMetrics.Bugs
#'                                        , "INDEX_NAME"
#'                                        , "INDEX_REGION"
#'                                        , df_thresh_metric
#'                                        , df_thresh_index)
#' # View Results
#' View(df_metric_scores_bugs)
#' # QC, table
#' table(df_metric_scores_bugs$Index, df_metric_scores_bugs$Index_Nar)
#' # QC, plot
#' hist(df_metric_scores_bugs$Index, main="PacNW BCG Example Data", xlab="Level 1 Indicator Taxa Index Score")
#' abline(v=c(21,30), col="blue")
#' text(21+c(-2,+2), 200, c("Low","Medium"), col="blue")
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~
#' # Metrics, Index, Benthic Macroinvertebrates, genus
#' # (generate values then scores)
#' myIndex <- "MBSS.2005.Bugs"
#' # Thresholds
#' # imported above
#' # get metric names for myIndex
#' (myMetrics.Bugs.MBSS <- unique(as.data.frame(df_thresh_metric)[df_thresh_metric[,"INDEX_NAME"]==myIndex,"METRIC_NAME"]))
#' # Taxa Data
#' myDF.Bugs.MBSS <- MBSStools::taxa_bugs_genus
#' myDF.Bugs.MBSS$NONTARGET <- FALSE
#' myDF.Bugs.MBSS$INDEX_REGION <- toupper(myDF.Bugs.MBSS$strata_r)
#' myDF.Bugs.MBSS$SAMPLEID <- myDF.Bugs.MBSS$SITE
#' myDF.Bugs.MBSS$INDEX_NAME <- myDF.Bugs.MBSS$Index.Name
#' myDF.Bugs.MBSS$TAXAID <- myDF.Bugs.MBSS$TAXON
#' myDF.Bugs.MBSS$SubPhylum <- NA
#' myDF.Bugs.MBSS$SubFamily <- NA
#' myDF.Bugs.MBSS$TOLVAL <- myDF.Bugs.MBSS$FinalTolVal07
#' myDF.Bugs.MBSS$TOLVAL2 <- myDF.Bugs.MBSS$FinalTolVal08
#' myDF.Bugs.MBSS$EXCLUDE <- myDF.Bugs.MBSS$EXCLUDE=="Y"
#' myMetric.Values.Bugs.MBSS <- metric.values(myDF.Bugs.MBSS, "bugs", myMetrics.Bugs.MBSS)
#' #
#' View(myMetric.Values.Bugs.MBSS)
#' # SCORE
#' myMetric.Values.Bugs.MBSS$INDEX_REGION <- toupper(myMetric.Values.Bugs.MBSS$INDEX_REGION)
#' Metrics.Bugs.Scores.MBSS <- metric.scores(myMetric.Values.Bugs.MBSS, myMetrics.Bugs.MBSS
#'                             , "INDEX_NAME", "INDEX_REGION", df_thresh_metric, df_thresh_index)
#' # View Results
#' View(Metrics.Bugs.Scores.MBSS)
#'
#' # QC Index Scores and Narratives
#' # Set Narrative as Ordered Factor
#' Nar.MBSS <- c("Very Poor", "Poor", "Fair", "Good")
#' Metrics.Bugs.Scores.MBSS$Index_Nar <- factor(Metrics.Bugs.Scores.MBSS$Index_Nar, levels=Nar.MBSS, labels=Nar.MBSS, ordered=TRUE)
#' table(Metrics.Bugs.Scores.MBSS$Index, Metrics.Bugs.Scores.MBSS$Index_Nar, useNA="ifany")
#'
#' # QC bug count (manual)
#' Metrics.Bugs.Scores.MBSS[Metrics.Bugs.Scores.MBSS[,"ni_total"]>120,
#' "QC_Count"] <- "LARGE"
#' Metrics.Bugs.Scores.MBSS[Metrics.Bugs.Scores.MBSS[,"ni_total"]<60,
#' "QC_Count"] <- "SMALL"
#' Metrics.Bugs.Scores.MBSS[is.na(Metrics.Bugs.Scores.MBSS[,"QC_Count"]),
#' "QC_Count"] <- "OK"
#' # table of QC_Count
#' table(Metrics.Bugs.Scores.MBSS$QC_Count)
#'
#' # QC bug count (with function)
#' # Import Checks
#' df.checks <- read_excel(system.file("./extdata/MetricFlags.xlsx"
#'                                           , package="BioMonTools"), sheet="Flags")
#' # Run Function
#' df.flags <- qc.checks(Metrics.Bugs.Scores.MBSS, df.checks)
#' # Summarize Results
#' table(df.flags[,"CHECKNAME"], df.flags[,"FLAG"], useNA="ifany")
#'
#' @export
metric.scores <- function(DF_Metrics, col_MetricNames, col_IndexName, col_IndexRegion
                          , DF_Thresh_Metric, DF_Thresh_Index) {##FUNCTION.metric.score.START
  #
  boo.QC <- FALSE
  if(boo.QC==TRUE){
    # DF_Metrics <- df_metric_values_bugs
    # col_MetricNames <- myMetrics.Bugs
    # col_IndexName <- "INDEX_NAME"
    # col_IndexRegion <-  "INDEX_REGION"
    # DF_Thresh <- df_thresh
    #~~~~~~~~~~~~~~~~~~~~~
    DF_Metrics <- df_metric_values_bugs
    col_MetricNames <- myMetrics.Bugs
    col_IndexName <- "INDEX_NAME"
    col_IndexRegion <- "INDEX_REGION"
    DF_Thresh_Metric <- df_thresh_metric
    DF_Thresh_Index <- df_thresh_index
    (a <- unique(as.matrix(DF_Metrics[, col_IndexName]))[1])
    (b <- unique(as.matrix(DF_Metrics[, col_IndexRegion]))[1])
    (c <- col_MetricNames[1])
    (aa <- unique(as.matrix(DF_Metrics[, col_IndexName]))[1])
    (bb <- unique(as.matrix(DF_Metrics[, col_IndexRegion]))[1])
  }
  #
  # QC, Column Names
  # Error check on fields (thresh metric)
  myFlds <- c("INDEX_NAME", "INDEX_REGION", "METRIC_NAME", "Thresh_Lo", "Thresh_Mid", "Thresh_Hi", "Direction", "ScoreRegime")
  if (length(myFlds)!=sum(myFlds %in% names(DF_Thresh_Metric))) {
    myMsg <- paste0("Fields missing from DF_Thresh_Metric input data frame.  Expecting: \n",paste(myFlds,sep="",collapse=", "),collapse="")
    stop(myMsg)
  }
  # Error check on fields (metrics)
  myFlds_2 <- c("INDEX_NAME", "INDEX_REGION")
  if (length(myFlds_2)!=sum(myFlds_2 %in% names(DF_Metrics))) {
    myMsg <- paste0("Fields missing from DF_Metrics input data frame.  Expecting: \n",paste(myFlds_2,sep="",collapse=", "),collapse="")
    stop(myMsg)
  }
  # Error check on fields (thresh Index)
  myFlds_Index <- c("INDEX_NAME", "INDEX_REGION", "NumMetrics", "ScoreRegime", paste0("Thresh0",1:6), paste0("Nar0",1:5))
  if (length(myFlds_Index)!=sum(myFlds_Index %in% names(DF_Thresh_Index))) {
    myMsg <- paste0("Fields missing from DF_Metrics input data frame.  Expecting: \n",paste(myFlds_Index,sep="",collapse=", "),collapse="")
    stop(myMsg)
  }
  #
  # Add "SCORE" columns for each metric
  Score.MetricNames <- paste0("SC_", col_MetricNames)
  DF_Metrics[, Score.MetricNames] <- 0
  #
  # Need to cycle based on Index (a), Region (b), and Metric (c)
  for (a in unique(as.matrix(DF_Metrics[, col_IndexName]))){##FOR.a.START
    for (b in unique(as.matrix(DF_Metrics[, col_IndexRegion]))) {##FOR.b.START
      for (c in col_MetricNames){##FOR.c.START
        # Thresholds (filter with dplyr)
        fun.Thresh.myMetric <- as.data.frame(dplyr::filter(DF_Thresh_Metric, INDEX_NAME==a & INDEX_REGION==b & METRIC_NAME==c))
        # QC
        #stopifnot(nrow(fun.Thresh.myMetric)==1)
        if(nrow(fun.Thresh.myMetric)!=1){
          #return(0)
          next
        }
        # thresholds
        fun.Lo          <- suppressWarnings(as.numeric(fun.Thresh.myMetric[, "Thresh_Lo"]))
        fun.Mid         <- suppressWarnings(as.numeric(fun.Thresh.myMetric[, "Thresh_Mid"]))
        fun.Hi          <- suppressWarnings(as.numeric(fun.Thresh.myMetric[, "Thresh_Hi"]))
        fun.Direction   <- toupper(fun.Thresh.myMetric[, "Direction"])
        fun.ScoreRegime <- toupper(fun.Thresh.myMetric[, "ScoreRegime"])
        #
        # default value
        fun.Value <- DF_Metrics[, c]
        fun.Result <- fun.Value * 0  #default value of zero
        #
        if(fun.ScoreRegime=="CONT_0100"){##IF.scoring.START
          if(fun.Direction=="DECREASE"){
            fun.calc <- 100*((fun.Value-fun.Lo)/(fun.Hi-fun.Lo))
          }else if (fun.Direction=="INCREASE") {
            fun.calc <- 100*((fun.Hi-fun.Value)/(fun.Hi-fun.Lo))
          }
          fun.Result <- sapply(fun.calc, function(x) {median(c(0, 100, x))})
        } else if(fun.ScoreRegime=="CAT_135"){
          if(fun.Direction=="DECREASE") {
            fun.Result <- ifelse(fun.Value>=fun.Hi,5
                                 ,ifelse(fun.Value<fun.Lo,1,3))
            if(boo.QC==TRUE){##IF.boo.QC.START
              print(paste0("Metric=",c,", Value=",fun.Value,", Result=", fun.Result))
              flush.console()
            }##IF.boo.QC.END
          } else if (fun.Direction=="INCREASE") {
            fun.Result <- ifelse(fun.Value<=fun.Lo,5
                                 ,ifelse(fun.Value>fun.Hi,1,3))
          }
        } else if(fun.ScoreRegime=="CAT_0246" | fun.ScoreRegime=="CAT_0123") {
          if(fun.Direction=="DECREASE") {
            fun.Result <- ifelse(fun.Value>=fun.Hi,6
                                 ,ifelse(fun.Value>=fun.Mid,4
                                         ,ifelse(fun.Value>=fun.Lo,2,0)))
            if(fun.ScoreRegime=="CAT_0123"){##CAT_0123.START
              fun.Result <- fun.Result / 2
            }##CAT_0123.END
          } else if (fun.Direction=="INCREASE") {
            fun.Result <- ifelse(fun.Value<=fun.Lo,6
                                 ,ifelse(fun.Value<=fun.Mid,4
                                         ,ifelse(fun.Value<=fun.Hi,2,0)))
            if(fun.ScoreRegime=="CAT_0123"){##CAT_0123.START
              fun.Result <- fun.Result / 2
            }##CAT_0123.END
          } else if (fun.Direction=="SCOREVALUE"){##SCOREVALUE.START
            fun.Result <- fun.Value
          }##SCOREVALUE.END
        } else if(is.na(fun.ScoreRegime)) {
          fun.Result <- 0
        } else {
          fun.Result <- 0
        }##IF.scoring.END
        #
        # Update input DF with matching values
        myTF <- DF_Metrics[, col_IndexName]==a & DF_Metrics[, col_IndexRegion]==b
        DF_Metrics[myTF,paste0("SC_", c)] <- fun.Result[myTF]
      }##FOR.c.END
    }##FOR.a.END
  }##FOR.b.END
  #

  # INDEX ####
  DF_Metrics[,"sum_Index"] <- 0
  DF_Metrics[,"Index"]     <- 0
  DF_Metrics[,"Index_Nar"] <- NA

  # Index, Sum
  # sum all metrics
  DF_Metrics[,"sum_Index"] <- rowSums(DF_Metrics[,Score.MetricNames])

  # Index, Value
    # Need to cycle based on Index (a), Region (b), and Metric (c)
  for (aa in unique(as.matrix(DF_Metrics[,col_IndexName]))){##FOR.a.START
    for (bb in unique(as.matrix(DF_Metrics[,col_IndexRegion]))) {##FOR.b.START
    # Thresholds (filter with dplyr)
    fun.Thresh.myIndex <- as.data.frame(dplyr::filter(DF_Thresh_Index, INDEX_NAME==aa & INDEX_REGION==bb))
    # QC
    if(nrow(fun.Thresh.myIndex)!=1){
      #return(0)
      next
    }
    # thresholds
    fun.NumMetrics  <- fun.Thresh.myIndex[, "NumMetrics"]
    fun.ScoreRegime <- fun.Thresh.myIndex[, "ScoreRegime"]
    fun.Index.Nar.Thresh <- fun.Thresh.myIndex[, c(paste0("Thresh0", 1:6))]
    fun.Index.Nar.Nar    <- fun.Thresh.myIndex[, c(paste0("Nar0", 1:5))]

    fun.Index.Nar.Numb <- sum(!is.na(fun.Index.Nar.Nar))

    # default value
    fun.Value <- DF_Metrics[, "sum_Index"]
    fun.Result <- fun.Value * 0  #default value of zero
    #
    # Scoring
    if(fun.ScoreRegime=="AVERAGE"){##IF.scoring.START
      fun.Result <- DF_Metrics[, "sum_Index"] / fun.NumMetrics
    } else {
      # SUM
      fun.Result <- DF_Metrics[, "sum_Index"]
    }##IF.scoring.END
    #
    # Narrative
    myBreaks <- as.numeric(paste(fun.Index.Nar.Thresh[1, 1:(fun.Index.Nar.Numb+1)]))
    myLabels <- paste(fun.Index.Nar.Nar[1, 1:fun.Index.Nar.Numb])
    fun.Result.Nar <- as.vector(cut(fun.Result, breaks=myBreaks, labels=myLabels
                          , include.lowest=TRUE, right=FALSE, ordered_result = TRUE))
    #
    # Update input DF with matching values
    myTF <- DF_Metrics[,col_IndexName]==aa & DF_Metrics[,col_IndexRegion]==bb
    DF_Metrics[myTF, "Index"]     <- fun.Result[myTF]
    DF_Metrics[myTF, "Index_Nar"] <- fun.Result.Nar[myTF]

    }##FOR.a.END
  }##FOR.b.END





  # Return original DF with added columns
  return(DF_Metrics)
  #
}##FUNCTION.metric.score.END
