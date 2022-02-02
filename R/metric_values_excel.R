#' @title Metric values Groups to Excel
#'
#' @description The output of metric.values() is saved to Excel with different
#' groups of metrics on different worksheets.
#'
#' @details This function will save the output of metric.values() into groups
#' by worksheet as defined by the user.
#'
#' The Excel file MetricNames.xlsx provided in the extdata folder has a column
#' named 'Groups' that can be used as default groupings. If no groupings are
#' provided (the default) all metrics are saved to a single worksheet.  Within
#' each group the 'sort_order' is used to sort the metrics.  If this column
#' is blank then the metrics are sorted in the order they appear in the
#' output from metric.values() (i.e., in fun.DF).
#'
#' The MetricNames data frame must include the following fields:
#'
#' * Metric_Name
#'
#' * Community
#'
#' * Sort_Group
#'
#' * Sort_Order
#'
#' @param fun.DF.MetVal Data frame of metric values.
#' @param fun.DF.xlMetNames Data frame of metric names and groups.
#' Default (NULL) will use the verion of MetricNames.xlsx that is in the
#' BioMonTools package.
#' @param fun.Community Community name of calculated metric values
#' (bugs, fish, or algae)
#' @param fun.MetVal.Col2Keep Column names in metric values to keep.
#' Default = c("SAMPLEID", "INDEX_NAME", "INDEX_REGION")
#' @param file.out Output file name.  Default (NULL) will generate a file name
#' based on the data and time (e.g., MetricValuesGroups_bugs_20220201.xlsx)

#'
#' @return Saves Excel file with metrics grouped by worksheet
#'
#' @examples
#' # Example 1, bugs
#' ## Community
#' comm <- "bugs"
#' ## Calculate Metrics
#' df_metval <- metric.values(data_benthos_PacNW, comm)
#' ## Metric Names and Groups
#' df_metnames <- readxl::read_excel(system.file("extdata/MetricNames.xlsx"
#'                                                   , package="BioMonTools")
#'                                       , guess_max = 10^6
#'                                       , sheet = "MetricMetadata"
#'                                       , skip = 4)
#' ## Columns to Keep
#' col2keep <- c("SAMPLEID", "INDEX_NAME", "INDEX_REGION")
#' ## File Name
#' file.out <- file.path(tempdir(), paste0("MetValGrps_", comm, ".xlsx"))
#' ## Run Function
#' metvalgrpxl(df_metval, df_metnames, comm, col2keep, file.out)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Example 2, fish
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Example 2, periphyton (algae)
#'
#' @export
metvalgrpxl <- function(fun.DF.MetVal
            , fun.DF.xlMetNames = NULL
            , fun.Community
            , fun.MetVal.Col2Keep = c("SAMPLEID", "INDEX_NAME", "INDEX_REGION")
            , file.out = NULL) {

  # DEBUG
  boo_debug <- FALSE
  if(boo_debug == TRUE) {
    fun.Community <- "bugs"
    fun.DF.MetVal <- metric.values(data_benthos_PacNW, fun.Community)
  fun.DF.xlMetNames <- readxl::read_excel(system.file("extdata/MetricNames.xlsx"
                                                      , package="BioMonTools")
                                          , guess_max = 10^6
                                          , sheet = "MetricMetadata"
                                          , skip = 4)
    fun.Community <- "bugs"
    fun.MetVal.Col2Keep = c("SAMPLEID", "INDEX_NAME", "INDEX_REGION")
    file.out <- file.path(tempdir(), paste0("MetValGrps_"
                                            , fun.Community
                                            , ".xlsx"))
  }## IF ~ boo_debug == TRUE

  # Set default values
  if(is.null(fun.DF.xlMetNames)) {
    fun.DF.xlMetNames <- readxl::read_excel(system.file("extdata/MetricNames.xlsx"
                                                        , package="BioMonTools")
                                            , sheet = "MetricMetadata"
                                            , skip = 4
                                            , guess_max = 10^6)
  }## IF ~ is.null(fun.DF.xlMetNames)

  if(is.null(file.out)){
    datetime <- format(Sys.time(), "_%Y%m%d_%H%M%S")
    file.out <- paste0("MetricValuesGroups_"
                       , fun.Community
                       , date.time
                       , ".xlsx")
  }## IF ~ is.null(file.out)

  # QC stop if community blank



  # QC, MetricNames To Upper
  names(fun.DF.xlMetNames) <- toupper(names(fun.DF.xlMetNames))

  # QC, convert DF to DF
  ## just in case inputs are tibbles
  df_metval <- as.data.frame(fun.DF.MetVal)
  df_metnames <- as.data.frame(fun.DF.xlMetNames)

  # QC, check for colnames
  ## Quit with message if missing columns
  ## See metric.values() code line 568
  # QC, Required Fields
  col.req <- c("METRIC_NAME", "Community", "Sort_Group")
  col.req.missing <- col.req[!(toupper(col.req) %in% toupper(names(df_metnames)))]
  num.col.req.missing <- length(col.req.missing)
  if(num.col.req.missing > 0) {
    msg <- paste("Columns missing from Metric Names:"
                  , paste(col.req.missing, collapse = "\n  ")
                  , sep = "\n  ")
    stop(msg)
  }## IF ~ col missing

  # Filter for community
  df_metnames <- df_metnames[df_metnames[, "COMMUNITY"] == fun.Community, ]

  # writexl can only create hyperlinks or formulas on entire columns
  # can see with str()
  # To include formulas set entire column to '=""'
  # then have to use formulas to insert text

  ## Create Notes
  nrow_Notes   <- 15
  Notes        <- data.frame(matrix(ncol = 3, nrow = nrow_Notes))
  Notes[, 2] <- writexl::xl_formula('=""') # set column as formula
  Notes[, 3] <- writexl::xl_formula('=""') # set column as formula
  Notes[1, 1]  <- "BioMonTools, Metric Values Groups"
  Notes[3, 1]  <- "Path and FileName"
  Notes[3, 2]  <- "=LEFT(@CELL(\"filename\",A1),FIND(\"]\",@CELL(\"filename\",A1)))"
  Notes[4, 1]  <- "FileName"
  Notes[4, 2]  <- "=MID(@CELL(\"filename\",B8),FIND(\"[\",@CELL(\"filename\",B8)),
            (FIND(\"]\",@CELL(\"filename\",B8))-FIND(\"[\",@CELL(\"filename\",B8)))+1)"
  Notes[5, 1]  <- "Worksheet"
  Notes[5, 2]  <- "=MID(@CELL(\"filename\",B10),FIND(\"]\",@CELL(\"filename\",
B10))+1,LEN(@CELL(\"filename\",B10))-FIND(\"]\",@CELL(\"filename\",B10)))"
  Notes[7, 1]  <- "Description of Work"
  Notes[8, 1]  <- "Metric value calculations from the R package BioMonTools."
  Notes[9, 1] <- "Metrics are sorted by common groups. Groupings defined in MetricNames"
  Notes[11, 1] <- "Input File Name"
  Notes[11, 2] <- paste0('="', deparse(substitute(fun.DF.MetricNames)), '"')
  Notes[12, 1] <- "Community"
  Notes[12, 2] <- paste0('="', fun.Community, '"')
  Notes[13, 1] <- "Date"
  Notes[13, 2] <- paste0('="', as.character(Sys.Date()), '"')
  Notes[15, 1] <- "Worksheet"
  Notes[15, 2] <- '="Description"'
  Notes[15, 3] <- '="Link"'

  # Remove Formula
  Notes[1, 2:3] <- NA

  # Add worksheets
  Notes[16, 1] <- "Notes"
  Notes[16, 2] <- '="File metadata"'
  Notes[16, 3] <- paste0("=HYPERLINK($B$5&$A"
                        , 16 + 1
                        , "&\"!A1\",$A"
                        , 16 + 1
                        , ")")
  Notes[17, 1] <- "MetricNames"
  Notes[17, 2] <- '="Metric Name metadata"'
  Notes[17, 3] <- paste0("=HYPERLINK($B$5&$A"
                         , 17 + 1
                         , "&\"!A1\",$A"
                         , 17 + 1
                         , ")")
  Notes[18, 1] <- "MetricValues"
  Notes[18, 2] <- '="Metric Values, Group = ALL"'
  Notes[18, 3] <- paste0("=HYPERLINK($B$5&$A"
                         , 18 + 1
                         , "&\"!A1\",$A"
                         , 18 + 1
                         , ")")

  # Future update add in links to each worksheet

  # Notes, MetricNames, MetricValues, Groups


  # Set up Lists

  sheet <- list()
  result <- list()

  #
  # notesheet <- data.frame(Reduce(rbind, sheet))
  # names(notesheet) <- "V1"
  # Notes <- rbind(Notes,notesheet)

  #### need to make these 2 work, outside loop preferably

  #### very unsure if this works or just prints fun.DF.MetricNames
 # Notes[11,2]<- deparse(substitute(fun.DF.MetricNames))

  result[["Notes"]] <- Notes
  result[["MetricNames"]] <- df_metnames
  result[["MetricValues"]] <- df_metval


  # Create List objects for each metric group
  ## Save each set of metrics to Excel on a worksheet named by sort_group
  sort_grps <- unique(df_metnames[, "SORT_GROUP"])
  for (i in sort_grps) {
    i_metrics <- df_metnames[df_metnames[, "SORT_GROUP"] == i, "METRIC_NAME"]
    col_base <- c(fun.MetVal.Col2Keep, i_metrics)
    col_keep <- col_base[col_base %in% names(df_metval)]
    df_i <- df_metval[, col_keep]
    result[[i]] <- df_i
    # update NOTES with hyperlink
    i_num <- match(i, sort_grps)
    i_num_Notes <- i_num + nrow_Notes + 3
    Notes[i_num_Notes, 1] <- i
    Notes[i_num_Notes, 2] <- paste0('="Metric Values, Group = ', i, '"')
    Notes[i_num_Notes, 3] <- paste0("=HYPERLINK($B$5&$A"
                                    , i_num_Notes + 1
                                    , "&\"!A1\",$A"
                                    , i_num_Notes + 1
                                    , ")")
    # B4 = path and file name
    # + 1 for header row added by write_xlsx

  }## FOR ~ i

  # Update Notes
  result[["Notes"]] <- Notes

  # Save to Excel
  writexl::write_xlsx(result
                     , path = file.out
                     , col_names = TRUE
                     , format_headers = TRUE
                     , use_zip64 = FALSE)




}## FUNCTION ~ metval.excel
