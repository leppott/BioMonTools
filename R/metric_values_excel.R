#' @title Metric values to Excel in Groups
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
#' @param fun.DF Data frame of metric values.
#' @param fun.Community Community name of calculated metric values
#' (bugs, fish, or algae)
#' @param fun.xlFileName Optional boolean value on whether to perform adjustments of
#' values prior to scoring.  Default = FALSE but may be TRUE for certain
#' metrics.
#' @param fun.DF.MetricNames Optional data frame of metric names and worksheet
#' groups.  If NULL then all metrics will be exported to a single worksheet.
#' Default = NULL
#'
#' @return Excel file
#'
#' @examples
#' Example 1, bugs
#' # Calc Values
#' com <- "bugs"
#' df_metval <- metric.values(data_benthos_PacNW, fun.community = com)
#'
#' # Sorting
df_metric_names <- readxl::read_excel(system.file("extdata/MetricNames.xlsx"
                                      , package="BioMonTools")
                                      , guess_max = 10^6
                                      , sheet = "MetricMetadata"
                                      , skip = 4)
#' # Save to Excel
#' fn_xl <- file.path(tempdir(), paste0("metval_", com, ".xlsx"))
#' metval.xl(df_metval
#'           , com
#'           ,  fn_xl
#'           , df_metric_names)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Example 2, fish
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Example 2, periphyton (algae)
#'
#' @export
#df_metnames<-data.frame()
#fun.Community<-data.frame()

metval.xl <- function(fun.DF
                     , fun.Community
                     , fun.xlFileName
                     , fun.DF.MetricNames = NULL) {

  # QC, convert DF to DF
  ## just in case inputs are tibbles
  df_metval <- as.data.frame(fun.DF)
  df_metnames <- as.data.frame(fun.DF.MetricNames)

  # QC, check for colnames
  ## Quit with message if missing columns
  ## See metric.values() code line 568

  # Filter for community
  df_metnames <- df_metnames[df_metnames[, "Community"] == fun.Community, ]
sheet<-list()
result<- list()
  # Loop through df_metnames$Sort_Group
  ## Save each set of metrics to Excel on a worksheet named by sort_group
  for (i in df_metnames$Sort_Group) {
    df_sorted<- subset(df_metnames, df_metnames$Sort_Group %in% i)
    result[[i]]<-df_sorted
    #cnt<-length(unique(result))

    sheet[[i]]<-paste0(i)

   # write_xlsx(list(sheet[[i]] = result[[i]]),path = file.path("C:/Users/Matt.Hedin/Documents/R-Code/BIoMon"),col_names = TRUE,format_headers = TRUE, use_zip64 = FALSE)

  }

## Create Notes, gotta fix metnames and community
Notes<- data.frame()
Notes[1,1]<- "BioMonTools"
Notes[2,1]<-"metric.values"
Notes[4,1]<-"Path and FileName"
Notes[5,1]<-"FileName"
Notes[6,1]<-"TabName"
Notes[8,1]<-"Description of Work"
Notes[9,1]<-"Metric value calculations from the R package BioMonTools."
Notes[10,1]<- "Metrics are sorted by common groups. Groupings defined in MetricNames.xlsx"
Notes[11,1]<-"Input File Name"
Notes[12,1]<-"Community"
Notes[13,1]<-"Date"
Notes[15,1]<-"Worksheet"
Notes[16,1]<-"Notes"
Notes[17,1]<-"Input Data"
notesheet<-data.frame(Reduce(rbind,sheet))
names(notesheet)<- "V1"
Notes<-rbind(Notes,notesheet)

Notes[13,2]<- as.character(Sys.Date())
#### need to make these 2 work, outside loop preferalby
Notes[12,2]<- fun.Community
#### very unsure if this works or just prints fun.DF.MetricNames
Notes[11,2]<- deparse(substitute(fun.DF.MetricNames))

result[["Notes"]]<-Notes
result[["MetricNames"]]<-df_metnames

listorder<- c(length(result)-1,length(result),1:(length(result)-2))

result<-result[listorder]
#fun.xlFileName<-result


write_xlsx(result,path = file.path(tempdir()),col_names = TRUE,format_headers = TRUE, use_zip64 = FALSE,fileext= "xlsx")




}
############################# to do
 #### hopefully I'm not missing something but do we actually need df.metval?
### need to add col QC check





}## FUNCTION ~ metval.excel

