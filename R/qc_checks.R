#' @title QC checks on metric values
#'
#' @description Apply "QC checks" on calculated metrics and station/sample
#' attributes to "flag" samples for the user. Examples include watershed size or
#'  total number of individuals.  Can have checks for both high and low values.
#' Checks are stored in separate file.  For structure see df.checks in example.
#'
#' @details used reshape2 package
#'
#' @param df.metrics Wide data frame with metric values to be evaluated.
#' @param df.checks  Data frame of metric thresholds to check.
#' @param input.shape Shape of df.metrics; wide or long.  Default is wide.
#'
#' @return Returns a data frame of SampleID checks and results; Pass and Fail.
#'
#' @examples
#' library(readxl)
#'
#' # Calculate Metrics
#' df.samps.bugs <- read_excel(system.file("./extdata/Data_Benthos.xlsx"
#'                                        , package="BioMonTools")
#'                            , guess_max = 10^6)
#'
#' # Columns to keep
#' myCols <- c("Area_mi2", "SurfaceArea", "Density_m2", "Density_ft2")
#'
#' # Run Function
#' myDF <- df.samps.bugs
#' df.metric.values.bugs <- metric.values(myDF, "bugs", fun.cols2keep=myCols)
#'
#' # Import Checks
#' df.checks <- read_excel(system.file("./extdata/MetricFlags.xlsx"
#'                                           , package="BioMonTools")
#'                                           , sheet="Flags")
#'\dontrun{
#' # View Checks
#' View(df.checks)
#'}
#'
#' # Run Function
#' df.flags <- qc.checks(df.metric.values.bugs, df.checks)
#'
#' # Summarize Results
#' table(df.flags[,"CHECKNAME"], df.flags[,"FLAG"], useNA="ifany")
#~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
qc.checks <- function(df.metrics, df.checks, input.shape="wide") {
  ##FUNCTION.START
  #
  # global variable bindings ----
  df.metric.values.bugs <- NULL
  boo_DEBUG <- FALSE
  if(boo_DEBUG==TRUE){
    # # Rename UPPER CASE myCols in metric.values output
    # for (i in myCols){##IF.i.START
    #   i.match <- match(toupper(i), names(df.metric.values.bugs))
    #   if(!is.na(i.match)){##IF.is.na.START
    #     names(df.metric.values.bugs)[i.match] <- i
    #   }##IF.is.na.END
    # }##IF.i.END
    #~~~~~~~~~~~~~~~~~~~~~~~~~~
    #  QC
    df.metrics <- df.metric.values.bugs
    df.checks <- df.checks
    input.shape <- "wide"
  }##IF~boo_DEBUG~END

  # to data frame (from Tibble)
  df.metrics <- as.data.frame(df.metrics)
  df.checks <- as.data.frame(df.checks)
  #
  # Names to Upper Case
  names(df.metrics) <- toupper(names(df.metrics))
  names(df.checks) <- toupper(names(df.checks))
  #
  # # upper case metrics
  # df.checks$METRIC_NAME <- toupper(df.checks$METRIC_NAME)
  # #
  # # metrics to check
  # checks.metrics <- unique(df.checks$Metric_Name)

  #
  # Metrics to long
  if (input.shape=="wide") {##IF.input.shape.START
    df.long <- reshape2::melt(df.metrics, id.vars=c("SAMPLEID"
                                                    , "INDEX_NAME"
                                                    , "INDEX_CLASS")
                             , variable.name="METRIC_NAME"
                             , value.name="METRIC_VALUE")
    # # compare to input
    # checks.metrics.input.col <- checks.metrics[checks.metrics %in%
    # toupper(names(df.metrics))]
    # # add to df.long
    # #df.metrics.long <- reshape2::melt(df.metrics)

  } else {
    df.long <- df.metrics
  }##IF.input.shape.END
  #

  # lower case metrics
  df.long$METRIC_NAME <- tolower(df.long$METRIC_NAME)
  df.checks$METRIC_NAME <- tolower(df.checks$METRIC_NAME)

  #
  df.long[,"INDEX_CLASS"] <- tolower(df.long[,"INDEX_CLASS"])
  df.checks[,"INDEX_CLASS"] <- tolower(df.checks[,"INDEX_CLASS"])
  #
  # merge metrics and checks
  df.merge <- merge(df.long, df.checks
                    , by.x=c("INDEX_NAME", "INDEX_CLASS", "METRIC_NAME")
                    , by.y=c("INDEX_NAME", "INDEX_CLASS", "METRIC_NAME"))
  #
  # perform evaluation (adds Pass/Fail, default is NA)

  # >
  # <
  # >=
  # <=
  # ==
  # !=

  df.merge[,"EXPR"] <- eval(expression(paste(df.merge[,"METRIC_VALUE"]
                                             , df.merge[,"SYMBOL"]
                                             , df.merge[,"VALUE"])))

  # y <- apply(df.merge$Expr, 1, function(x) eval(parse(text=x)))
  #
  # x <- "1 < 2"
  # eval(parse(text=x))

  # temporary (quick and dirty)
  for (i in seq_len(nrow(df.merge))){
    df.merge[i, "EVAL"] <- eval(parse(text=df.merge[i, "EXPR"]))
  }

  # apply?
  # deparse etc

  ## default to NA
  df.merge[,"FLAG"] <- as.character(NA)
  #df.merge[df.merge[,"EVAL"]==FALSE, "FLAG"] <- "PASS"
  #df.merge[df.merge[,"EVAL"]==TRUE,  "FLAG"] <- "FAIL"
  df.merge[,"FLAG"][df.merge[,"EVAL"]==FALSE] <- "PASS"
  df.merge[,"FLAG"][df.merge[,"EVAL"]==TRUE] <- "FAIL"

  # QC
  #table(df.merge$FLAG, df.merge$EVAL, useNA="always")

  #
  # create output
  return(as.data.frame(df.merge))
  #
}##FUNCTION.END
