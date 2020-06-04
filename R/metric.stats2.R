#' @title Secondary metric statistics
#'
#' @description This function calculates secondary statistics (DE and z-score) on metric statistics for use with developing a
#' multi-metric index.
#'
#'
#' @details Secondary metrics statistics for the data are calculated.
#'
#' Inputs are metric values and metric stats outputs.
#'
#' Metric values is a wide format with columns for each metric.
#' Assumes only a single Subset.
#'
#' Metrics stats is a wide format with columns for each statistic with metrics in a single column.
#' Assumes only a single Subset.
#'
#' Required fields are RefStatus, DataType, and Index_Region.  The user is allowed to enter their own
#' values for these fields for each input file.
#'
#' The two statistics calculated are z-score and discrimination efficiency (DE) for each metric
#' within each DataType (cal / val).
#'
#' Z-scores are calculated using the calibration (or development) data set
#' for a given Index_Region (or Site Class).
#'
#' * (mean Ref - mean Str) / sd Ref
#'
#' DE is calculated without knowing the expected direction of response for each metric
#' for a given Index_Region (or Site Class).  DE is the percentage (0-100) of **stressed**
#' samples that fall **below** the **25th** quantile (for decreaser metrics, e.g., total taxa)
#' or **above** the **75th** quantile (for increaser metrics, e.g., HBI) of the **reference** samples.
#'
#' A data frame of the metric.stats input is returned with new columns
#' (z_score, DE25 and DE75).  The z-score is added for each Ref_Status.  DE25 and DE75 are only added
#' where Ref_Status is labeled as Stressed.
#'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @param data_metval Data frame of metric values.
#' @param data_metstat Data frame of metric statistics
#' @param col_metval_RefStatus Column name for Reference Status.  Default = "Ref_Status"
#' @param col_metval_DataType Column name for Data Type – Validation vs. Calibration.  Default = "Data_Type"
#' @param col_metstat_Subset Column name for Index_Region in data_metstats.  Default = xx.
#' @param col_metstat_RefStatus Column name for Reference Status.  Default = "Ref_Status"
#' @param col_metstat_DataType Column name for Data Type – Validation vs. Calibration.  Default = "Data_Type"
#' @param col_metstat_Subset Column name for Index_Region in data_metstats.  Default = xx.
#' @param RefStatus_Ref RefStatus value for Reference.  Default = "Ref"
#' @param RefStatus_Str RefStatus value for Stressed.  Default = "Str"
#' @param RefStatus_Oth RefStatus value for Other. Default = "Oth"
#' @param DataType_Cal DataType value for Calibration. Default = "Cal"
#' @param DataType_Ver DataType value for Verification. Default = "Ver"
#' @param Subset_Value Subset value of Index_Region (site class).  Default = NULL
#'
#' @return A data frame of the metric.stats input is returned with new columns
#' (z_score, DE25 and DE75).
#'
#' @examples
#' # data, benthos
#' df_bugs <- data_mmi_dev
#'
#' # Munge Names
#' names(df_bugs)[names(df_bugs) %in% "BenSampID"] <- "SAMPLEID"
#' names(df_bugs)[names(df_bugs) %in% "TaxaID"] <- "TAXAID"
#' names(df_bugs)[names(df_bugs) %in% "Individuals"] <- "N_TAXA"
#' names(df_bugs)[names(df_bugs) %in% "Exclude"] <- "EXCLUDE"
#' names(df_bugs)[names(df_bugs) %in% "Class"] <- "INDEX_REGION"
#' names(df_bugs)[names(df_bugs) %in% "Unique_ID"] <- "SITEID"
#'
#' # Calc Metrics
#' cols_keep <- c("Ref_v1", "CalVal_Class4", "SITEID", "CollDate", "CollMeth")
#' # INDEX_NAME and INDEX_REGION kept by default
#' df_metval <- metric.values(df_bugs, "bugs", fun.cols2keep = cols_keep)
#'
#' # Calc Stats
#' col_metrics   <- names(df_metval)[9:ncol(df_metval)]
#' col_SampID    <- "SAMPLEID"
#' col_RefStatus <- "REF_V1"
#' RefStatus_Ref <- "Ref"
#' RefStatus_Str <- "Strs"
#' RefStatus_Oth <- "Other"
#' col_DataType  <- "CALVAL_CLASS4"
#' DataType_Cal  <- "cal"
#' DataType_Ver  <- "verif"
#' col_Subset    <- "INDEX_REGION"
#' Subset_Value  <- "CENTRALHILLS"
#' df_stats <- metric.stats(df_metval, col_metrics, col_SampID
#'                          , col_RefStatus, RefStatus_Ref, RefStatus_Str, RefStatus_Oth
#'                          , col_DataType, DataType_Cal, DataType_Ver
#'                          , col_Subset, Subset_Value)
#'
#' # Calc Stats2 (z-scores and DE)
#' data_metval <- df_metval
#' data_metstat <- df_stats
#' col_metval_RefStatus <- "REF_V1"
#' col_metval_DataType <- "CALVAL_CLASS4"
#' col_metval_Subset <- "INDEX_REGION"
#' col_metstat_RefStatus <- "REF_V1"
#' col_metstat_DataType <- "CALVAL_CLASS4"
#' col_metstat_Subset <- "INDEX_REGION"
#' RefStatus_Ref = "Ref"
#' RefStatus_Str = "Strs"
#' RefStatus_Oth = "Other"
#' DataType_Cal = "cal"
#' DataType_Ver = "verif"
#' Subset_Value = "CENTRALHILLS"
#' df_stats2 <- metric.stats2(data_metval
#'                            , data_metstat
#'                            , col_metval_RefStatus
#'                            , col_metval_DataType
#'                            , col_metval_Subset
#'                            , col_metstat_RefStatus
#'                            , col_metstat_DataType
#'                            , col_metstat_Subset
#'                            , RefStatus_Ref
#'                            , RefStatus_Str
#'                            , RefStatus_Oth
#'                            , DataType_Cal
#'                            , DataType_Ver
#'                            , Subset_Value)
#'
#' \dontrun{
#' # Save Results
#' write.table(df_stats2, "metric.stats2.tsv", col.names=TRUE, row.names=FALSE, sep="\t")
#' }
#~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
metric.stats2 <- function(data_metval
                          , data_metstat
                          , col_metval_RefStatus = "RefStatus"
                          , col_metval_DataType = "DataType"
                          , col_metval_Subset = "INDEX_REGION"
                          , col_metstat_RefStatus = "RefStatus"
                          , col_metstat_DataType = "DataType"
                          , col_metstat_Subset = "INDEX_REGION"
                          , RefStatus_Ref = "Ref"
                          , RefStatus_Str = "Str"
                          , RefStatus_Oth = "Oth"
                          , DataType_Cal = "Cal"
                          , DataType_Ver = "Ver"
                          , Subset_Value = NULL
                         ){##FUNCTION.metric.values.START
  # debug
  boo_debug <- FALSE
  if (boo_debug == TRUE){
    ## Create Metric Values
    # data, benthos
    df_bugs <- data_mmi_dev
    # Munge Names
    names(df_bugs)[names(df_bugs) %in% "BenSampID"] <- "SAMPLEID"
    names(df_bugs)[names(df_bugs) %in% "TaxaID"] <- "TAXAID"
    names(df_bugs)[names(df_bugs) %in% "Individuals"] <- "N_TAXA"
    names(df_bugs)[names(df_bugs) %in% "Exclude"] <- "EXCLUDE"
    names(df_bugs)[names(df_bugs) %in% "Class"] <- "INDEX_REGION"
    names(df_bugs)[names(df_bugs) %in% "Unique_ID"] <- "SITEID"
    # Calc metrics
    cols_keep <- c("Ref_v1", "CalVal_Class4", "SITEID", "CollDate", "CollMeth")
    # INDEX_NAME and INDEX_REGION kept by default
    df_metval <- metric.values(df_bugs, "bugs", fun.cols2keep = cols_keep)
    #
    ## Calc stats
    # input
    fun.DF <- df_metval
    col_metrics <- names(df_metval)[9:ncol(df_metval)]
    col_SampID  <- "SAMPLEID"
    col_RefStatus = "REF_V1"
    RefStatus_Ref = "Ref"
    RefStatus_Str = "Strs"
    RefStatus_Oth = "Other"
    col_DataType = "CALVAL_CLASS4"
    DataType_Cal = "cal"
    DataType_Ver = "verif"
    col_Subset = "INDEX_REGION"
    Subset_Value = "CENTRALHILLS"
    df_stats <- metric.stats(df_metval, col_metrics, col_SampID
                             , col_RefStatus, RefStatus_Ref, RefStatus_Str, RefStatus_Oth
                             , col_DataType, DataType_Cal, DataType_Ver
                             , col_Subset, Subset_Value)

    # Stats2
    data_metval <- df_metval
    data_metstat <- df_stats
    col_metval_RefStatus <- "REF_V1"
    col_metval_DataType <- "CALVAL_CLASS4"
    col_metval_Subset <- "INDEX_REGION"
    col_metstat_RefStatus <- "REF_V1"
    col_metstat_DataType <- "CALVAL_CLASS4"
    col_metstat_Subset <- "INDEX_REGION"
    RefStatus_Ref = "Ref"
    RefStatus_Str = "Strs"
    RefStatus_Oth = "Other"
    DataType_Cal = "cal"
    DataType_Ver = "verif"
    Subset_Value = "CENTRALHILLS"

  }## IF ~ boo_debug ~ END


  # define pipe
  `%>%` <- dplyr::`%>%`
  # Munge ####
  # Data Munging (common to all data types)
  # Convert to data.frame.  Code breaks if myDF is a tibble.
  data_metval  <- as.data.frame(data_metval)
  data_metstat <- as.data.frame(data_metstat)

  # convert Field Names to UPPER CASE
  # names(fun.DF) <- toupper(names(fun.DF))
  # # convert cols2keep to UPPER CASE
  # if(!is.null(fun.cols2keep)){
  #   #names(fun.cols2keep) <- toupper(fun.cols2keep)
  #   fun.cols2keep <- toupper(fun.cols2keep)
  # }##IF~!is.null(fun.cols2keep)~END

  # QC
  # # Check for columns and values
  # ## Columns
  # qc_col <- c(col_RefStatus, col_DataType, col_Subset)
  # qc_col_check <- qc_col %in% names(fun.DF)
  # if(length(qc_col) != sum(qc_col_check)){
  #   cols_missing <- qc_col[!qc_col_check]
  #   msg <- paste0("Columns missing from data; ", paste(cols_missing, collapse = ", "))
  #   stop(msg)
  # }##IF ~ check columns ~ END
  # #
  # #
  # ## Values
  # ### RefStatus
  # qc_val_col <- col_RefStatus
  # qc_val     <- c(RefStatus_Ref, RefStatus_Str, RefStatus_Oth)
  # qc_val_check <- qc_val %in% unique(fun.DF[, qc_val_col])
  # if(length(qc_val) != sum(qc_val_check)){
  #   vals_missing <- qc_val[!qc_val_check]
  #   msg <- paste0("Values missing from column '", qc_val_col,"'; "
  #                 , paste(vals_missing, collapse = ", "))
  #   stop(msg)
  # }##IF ~ check columns ~ END
  # #
  # ### DataType
  # qc_val_col <- col_DataType
  # qc_val     <- c(DataType_Cal, DataType_Ver)
  # qc_val_check <- qc_val %in% unique(fun.DF[, qc_val_col])
  # if(length(qc_val) != sum(qc_val_check)){
  #   vals_missing <- qc_val[!qc_val_check]
  #   msg <- paste0("Values missing from column '", qc_val_col,"'; "
  #                 , paste(vals_missing, collapse = ", "))
  #   stop(msg)
  # }##IF ~ check columns ~ END
  # #
  # ### Subset
  # qc_val_col <- col_Subset
  # qc_val     <- Subset_Value
  # qc_val_check <- qc_val %in% unique(fun.DF[, qc_val_col])
  # if(length(qc_val) != sum(qc_val_check)){
  #   vals_missing <- qc_val[!qc_val_check]
  #   msg <- paste0("Values missing from column '", qc_val_col,"'; "
  #                 , paste(vals_missing, collapse = ", "))
  #   stop(msg)
  # }##IF ~ check columns ~ END


  # Filter for user specified subset
  if (is.null(col_Subset)){
    data_metval  <- data_metval
    data_metstat <- data_metstat
  } else {
    data_metval <- data_metval[data_metval[, col_Subset] == Subset_Value, ]
    data_metstat <- data_metstat[data_metstat[, col_Subset] == Subset_Value, ]
  }##IF ~ is.null(col_subset) ~ END



  # # Create further subsets and calc stats
  # combos <- c(t(outer(c(RefStatus_Ref, RefStatus_Str, RefStatus_Oth)
  #                     , c(DataType_Cal, DataType_Ver)
  #                     , FUN=paste, sep = "___")))
  #         # "___" just so not likely to be in the data
  # n_combos <- length(combos)
  # # Should be 6 (RefStatus = 3 * DataType = 2) but might have fewer
  # # col_RefStatus
  # # col_DataType


  # z-score, ref mean - str mean / ref sd

  df_stats_c <- data_metstat[data_metstat[, col_metval_DataType] == DataType_Cal, ]
  df_stats_v <- data_metstat[data_metstat[, col_metval_DataType] == DataType_Ver, ]

  df_stats_c_ref <- df_stats_c[df_stats_c[, col_metval_RefStatus] == RefStatus_Ref, ]
  df_stats_c_str <- df_stats_c[df_stats_c[, col_metval_RefStatus] == RefStatus_Str, ]

  df_stats_v_ref <- df_stats_v[df_stats_v[, col_metval_RefStatus] == RefStatus_Ref, ]
  df_stats_v_str <- df_stats_v[df_stats_v[, col_metval_RefStatus] == RefStatus_Str, ]

  # z-score
  ## calc
  z_c <- (df_stats_c_ref[, "mean"] - df_stats_c_str[, "mean"]) / df_stats_c_ref[, "sd"]
  z_v <- (df_stats_v_ref[, "mean"] - df_stats_v_str[, "mean"]) / df_stats_v_ref[, "sd"]
  ### Add back to df_stats
  col_metnam <- df_stats_c_ref[, "Metric_Name"]
  col_z <- c(col_metval_DataType, col_metval_Subset, "Metric_Name", "z_score")
  df_z_c <- data.frame(col_metval_DataType = DataType_Cal
                       , col_metval_Subset = Subset_Value
                       , "Metric_Name" = col_metnam
                       , "z_score" = z_c)
  df_z_v <- data.frame(col_metval_DataType = DataType_Ver
                       ,  col_metval_Subset = Subset_Value
                       , "Metric_Name" = col_metnam
                       , "z_score" = z_v)
  df_z <- rbind(df_z_c, df_z_v)
  names(df_z) <- col_z
  #df_stats <- merge(df_stats, df_z_cv)

  # DE
  col_de <- c(col_metval_DataType, col_metval_Subset, "Metric_Name"
              , "q25_Ref", "q75_Ref", "n_Str")
  df_de_c <- data.frame(col_metval_DataType = DataType_Cal
                       , col_metval_Subset = Subset_Value
                       , "Metric_Name" = col_metnam
                       , "q25_Ref" = df_stats_c_ref[, "q25"]
                       , "q75_Ref" = df_stats_c_ref[, "q75"]
                       , "n_Str" = df_stats_c_str[, "n"])
  df_de_v <- data.frame(col_metval_DataType = DataType_Ver
                       , col_metval_Subset = Subset_Value
                       , "Metric_Name" = col_metnam
                       , "q25_Ref" = df_stats_v_ref[, "q25"]
                       , "q75_Ref" = df_stats_v_ref[, "q75"]
                       , "n_Str" = df_stats_v_str[, "n"])
  df_de_cv <- rbind(df_de_c, df_de_v)
  names(df_de_cv) <- col_de
  # metrics
  # df_metval_c <- data_metval[data_metval[, col_metval_DataType] == DataType_Cal, ]
  # df_metval_v <- data_metval[data_metval[, col_metval_DataType] == DataType_Ver, ]
  #
  # df_metval_c_str <- df_metval_c[df_metval_c[, col_metval_RefStatus] == RefStatus_Str, ]
  #
  # df_metval_v_str <- df_metval_v[df_metval_v[, col_metval_RefStatus] == RefStatus_Str, ]
  #
  #
  # df_metval_cv_str_de <- rbind(df_metval_c_str, df_metval_v_str)


  # Str metric values in long format
  data_metval_str_longer <- tidyr::pivot_longer(data_metval[data_metval[, col_RefStatus] == RefStatus_Str, ]
                                            , cols = all_of(col_metnam)
                                            , names_to = "Metric_Name"
                                            , values_to = "Metric_Value")
  # merge
  df_merge4de <- merge(data_metval_str_longer, df_de_cv
              , by.x = c(col_metval_DataType, col_metval_Subset, "Metric_Name")
              , by.y = c(col_metstat_DataType, col_metstat_Subset, "Metric_Name"))
  # calc
  df_merge4de[, "q25_lt"]  <- df_merge4de[, "Metric_Value"] < df_merge4de[, "q25_Ref"]
  df_merge4de[, "q75_gt"]  <- df_merge4de[, "Metric_Value"] > df_merge4de[, "q75_Ref"]
  #df_merge4de[, "q25_lte"] <- df_merge4de[, "Metric_Value"] <= df_merge4de[, "q25_Ref"]
  #df_merge4de[, "q75_gte"] <- df_merge4de[, "Metric_Value"] >= df_merge4de[, "q75_Ref"]

  # summarize
  df_de <- df_merge4de %>%
    dplyr::group_by_(col_metval_Subset, col_metval_DataType, col_metval_RefStatus, "Metric_Name") %>%
    dplyr::summarize(DE25 = 100 * sum(q25_lt / n_Str)
                     , DE75 = 100 * sum(q75_gt / n_Str))
  df_de <- data.frame(df_de)
  # Need variables not the names
  # use group_by_ even though deprecated.  using enquo(!!variable)  adds extra quotes

  # add back
  #df_z and df_de
  data_metstat_merge_z <- merge(data_metstat, df_z)
  data_metstat_merge_z_de <- merge(data_metstat_merge_z, df_de, all.x = TRUE)


  # Return
  df_results <- data_metstat_merge_z_de
  return(df_results)

}##FUNCTION ~ metric.stats2 ~ END
#
