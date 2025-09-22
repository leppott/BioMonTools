#' @title Calculate metric statistics
#'
#' @description This function calculates metric statistics for use with
#' developing a multi-metric index.
#'
#' Inputs are a data frame with
#'
#' @details Summary statistics for the data are calculated.
#'
#' The data is filtered by the column Subset for only a single value given by
#' the user.  If need further subsets re-run the function.  If no subset is
#' given the entire data set is used.
#'
#' Statistics will be generated for up to 6 combinations for RefStatus (Ref,
#' Oth, Str) and DataType (Cal, Ver).
#'
#' The resulting dataframe will have the statistics in columns with the first 4
#' columns as:  INDEX_CLASS (if col_Subset not provided), col_RefStatus,
#' col_DataType, and Metric_Name.
#'
#' The following statistics are generated with na.rm = TRUE.
#'
#' * n = number
#'
#' * min = minimum
#'
#' * max = maximum
#'
#' * mean = mean
#'
#' * median = median
#'
#' * range = range (max - min)
#'
#' * sd = standard deviation
#'
#' * cv = coefficient of variation (sd/mean)
#'
#' * q05 = quantile, 5%
#'
#' * q10 = quantile, 10%
#'
#' * q25 = quantile, 25%
#'
#' * q50 = quantile, 50%
#'
#' * q75 = quantile, 75%
#'
#' * q90 = quantile, 90%
#'
#' * q95 = quantile, 95%

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @param fun.DF Data frame.
#' @param col_metrics Column names for metrics.
#' @param col_SampID Column name for unique sample identifier.
#' Default = "SAMPLEID".
#' @param col_RefStatus Column name for Reference Status.
#' Default = "Ref_Status"
#' @param RefStatus_Ref Reference Status name for Reference used in
#' col_ RefStatus.  Default = “Ref”.
#' Use NULL if you don't use this value.
#' @param RefStatus_Str Reference Status name for Stressed used in
#' col_ RefStatus.  Default = “Str”.
#' Use NULL if you don't use this value.
#' @param RefStatus_Oth Reference Status name for Other used in col_ RefStatus.
#' Default = “Oth”.
#' Use NULL if you don't use this value.
#' @param col_DataType Column name for Data Type – Validation vs. Calibration.
#' Default = "Data_Type"
#' @param DataType_Cal Datatype name for Calibration used in col_DataType.
#' Default = “Cal”.
#' Use NULL if you don't use this value.
#' @param DataType_Ver Datatype name for Verification used in col_DataType.
#' Default = “Ver”.
#' Use NULL if you don't use this value.
#' @param col_Subset Column name to subset the data and run on each subset.
#' Default = NULL.
#' If NULL then no subset will be generated.
#' @param Subset_Value Subset name to be used for creating subset.
#' Default = NULL.
#'
#' @return data frame of metrics (rows) and statistics (columns).
#' This is in long format with columns for INDEX_CLASS, RefStatus, and
#' DataType.
#'
#' @examples
#' # data, benthos
#' df_bugs <- data_mmi_dev_small
#'
#' # Munge Names
#' names(df_bugs)[names(df_bugs) %in% "BenSampID"] <- "SAMPLEID"
#' names(df_bugs)[names(df_bugs) %in% "TaxaID"] <- "TAXAID"
#' names(df_bugs)[names(df_bugs) %in% "Individuals"] <- "N_TAXA"
#' names(df_bugs)[names(df_bugs) %in% "Exclude"] <- "EXCLUDE"
#' names(df_bugs)[names(df_bugs) %in% "Class"] <- "INDEX_CLASS"
#' names(df_bugs)[names(df_bugs) %in% "Unique_ID"] <- "SITEID"
#'
#' # Add Missing Columns
#' df_bugs$ELEVATION_ATTR <- NA_character_
#' df_bugs$GRADIENT_ATTR <- NA_character_
#' df_bugs$WSAREA_ATTR <- NA_character_
#' df_bugs$HABSTRUCT <- NA_character_
#' df_bugs$BCG_ATTR2 <- NA_character_
#' df_bugs$AIRBREATHER <- NA
#' df_bugs$UFC <- NA_real_
#'
#' # Calc Metrics
#' cols_keep <- c("Ref_v1", "CalVal_Class4", "SITEID", "CollDate", "CollMeth")
#' # INDEX_NAME and INDEX_CLASS kept by default
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
#' col_Subset    <- "INDEX_CLASS"
#' Subset_Value  <- "CentralHills"
#'
#' df_stats <- metric.stats(df_metval
#'                          , col_metrics
#'                          , col_SampID
#'                          , col_RefStatus
#'                          , RefStatus_Ref
#'                          , RefStatus_Str
#'                          , RefStatus_Oth
#'                          , col_DataType
#'                          , DataType_Cal
#'                          , DataType_Ver
#'                          , col_Subset
#'                          , Subset_Value)
#'
#' \donttest{
#' # Save Results
#' write.table(df_stats
#'             , file.path(tempdir(), "metric.stats.tsv")
#'             , col.names = TRUE
#'             , row.names = FALSE
#'             , sep = "\t")
#' }
#~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
metric.stats <- function(fun.DF,
                         col_metrics,
                         col_SampID = "SAMPLEID",
                         col_RefStatus = "Ref_Status",
                         RefStatus_Ref = "Ref",
                         RefStatus_Str = "Str",
                         RefStatus_Oth = "Oth",
                         col_DataType = "Data_Type",
                         DataType_Cal = "Cal",
                         DataType_Ver = "Ver",
                         col_Subset = NULL,
                         Subset_Value = NULL
                         ){
  # global variable bindings ----
  data_mmi_dev <- NULL

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
    names(df_bugs)[names(df_bugs) %in% "Class"] <- "INDEX_CLASS"
    names(df_bugs)[names(df_bugs) %in% "Unique_ID"] <- "SITEID"
    # Calc metrics
    cols_keep <- c("Ref_v1", "CalVal_Class4", "SITEID", "CollDate", "CollMeth")
    # INDEX_NAME and INDEX_CLASS kept by default
    df_metval <- metric.values(df_bugs, "bugs", fun.cols2keep = cols_keep)
    #
    ## Calc stats
    # input
    fun.DF <- df_metval
    col_metrics <- names(df_metval)[9:ncol(df_metval)]
    col_SampID  <- "SAMPLEID"
    col_RefStatus <- "REF_V1"
    RefStatus_Ref <- "Ref"
    RefStatus_Str <- "Strs"
    RefStatus_Oth <- "Other"
    col_DataType <- "CALVAL_CLASS4"
    DataType_Cal <- "cal"
    DataType_Ver <- "verif"
    col_Subset <- "INDEX_CLASS"
    Subset_Value <- "CENTRALHILLS"
    #
  }## IF ~ boo_debug ~ END

  # define pipe
  `%>%` <- dplyr::`%>%`

  # Munge ####
  # Data Munging (common to all data types)
  # Convert to data.frame.  Code breaks if myDF is a tibble.
  fun.DF <- as.data.frame(fun.DF)
  # convert Field Names to UPPER CASE
  # names(fun.DF) <- toupper(names(fun.DF))
  # # convert cols2keep to UPPER CASE
  # if(!is.null(fun.cols2keep)){
  #   #names(fun.cols2keep) <- toupper(fun.cols2keep)
  #   fun.cols2keep <- toupper(fun.cols2keep)
  # }##IF~!is.null(fun.cols2keep)~END

  # QC
  # Check for columns and values
  ## Columns
  qc_col <- c(col_RefStatus, col_DataType, col_Subset)
  qc_col_check <- qc_col %in% names(fun.DF)
  if(length(qc_col) != sum(qc_col_check)){
    cols_missing <- qc_col[!qc_col_check]
    msg <- paste0("Columns missing from data; ", paste(cols_missing
                                                       , collapse = ", "))
    stop(msg)
  }##IF ~ check columns ~ END
  #
  #
  ## Values
  ### RefStatus
  qc_val_col <- col_RefStatus
  qc_val     <- c(RefStatus_Ref, RefStatus_Str, RefStatus_Oth)
  qc_val_check <- qc_val %in% unique(fun.DF[, qc_val_col])
  if(length(qc_val) != sum(qc_val_check)){
    vals_missing <- qc_val[!qc_val_check]
    msg <- paste0("Values missing from column '", qc_val_col,"'; "
                  , paste(vals_missing, collapse = ", "))
    stop(msg)
  }##IF ~ check columns ~ END
  #
  ### DataType
  qc_val_col <- col_DataType
  qc_val     <- c(DataType_Cal, DataType_Ver)
  qc_val_check <- qc_val %in% unique(fun.DF[, qc_val_col])
  if(length(qc_val) != sum(qc_val_check)){
    vals_missing <- qc_val[!qc_val_check]
    msg <- paste0("Values missing from column '", qc_val_col,"'; "
                  , paste(vals_missing, collapse = ", "))
    stop(msg)
  }##IF ~ check columns ~ END
  #
  ### Subset
  qc_val_col <- col_Subset
  qc_val     <- Subset_Value
  qc_val_check <- qc_val %in% unique(fun.DF[, qc_val_col])
  if(length(qc_val) != sum(qc_val_check)){
    vals_missing <- qc_val[!qc_val_check]
    msg <- paste0("Values missing from column '", qc_val_col,"'; "
                  , paste(vals_missing, collapse = ", "))
    stop(msg)
  }##IF ~ check columns ~ END

  # Filter for user specified subset
  if (is.null(col_Subset)){
    df_subset <- fun.DF
  } else {
    df_subset <- fun.DF[fun.DF[, col_Subset] == Subset_Value, ]
  }##IF ~ is.null(col_subset) ~ END

  # Create further subsets and calc stats
  combos <- c(t(outer(c(RefStatus_Ref, RefStatus_Str, RefStatus_Oth)
                      , c(DataType_Cal, DataType_Ver)
                      , FUN=paste, sep = "___")))
          # "___" just so not likely to be in the data
  n_combos <- length(combos)
  # Should be 6 (RefStatus = 3 * DataType = 2) but might have fewer
  # col_RefStatus
  # col_DataType
  for(i in 1:n_combos){
    # User feedback
    msg <- paste0("Working on item ", i, "/", n_combos, "; ", combos[i])
    message(msg)
    # Combo
    combo_RefStatus <- unlist(strsplit(combos[i], "___"))[1]
    combo_DataType  <- unlist(strsplit(combos[i], "___"))[2]
    # Filter
    df_i <- df_subset[df_subset[, col_RefStatus] == combo_RefStatus &
                        df_subset[, col_DataType] == combo_DataType, ]

    # calc
    metrics_n     <- sapply(df_i[, col_metrics], length)
    metrics_min   <- sapply(df_i[, col_metrics], min, na.rm = TRUE)
    metrics_max   <- sapply(df_i[, col_metrics], max, na.rm = TRUE)
    metrics_mean  <- sapply(df_i[, col_metrics], mean, na.rm = TRUE)
    metrics_median  <- sapply(df_i[, col_metrics], stats::median, na.rm = TRUE)
    metrics_range <- metrics_max - metrics_min
    metrics_sd    <- sapply(df_i[, col_metrics], stats::sd, na.rm = TRUE)
    metrics_cv    <- metrics_sd / metrics_mean
    #
    # percentiles to run
    p <- c(5, 10, 25, 50, 75, 90, 95)/100
    # percentiles
    metrics_quantiles <- sapply(df_i[, col_metrics], stats::quantile, probs = p
                                , na.rm = TRUE)
    #
    metrics_all <- rbind(metrics_n, metrics_min, metrics_max, metrics_mean
                         , metrics_range, metrics_sd, metrics_cv
                         , metrics_quantiles)
    # rename
    col_StatNames <- c("n", "min", "max", "mean", "range", "sd", "cv"
                       , paste0("q", sprintf("%02d", p*100)))
    rownames(metrics_all) <- col_StatNames
    # transpose
    df_i_stats <- data.frame(t(metrics_all))
    #
    # Munge
    # Add Columns
    col_stats <- colnames(df_i_stats)
    if(!is.null(col_Subset)){
      df_i_stats[, col_Subset] <- Subset_Value
    } else {
      df_i_stats[, "Subset"] <- NULL
    }##IF ~ !is.null(col_Subset) ~ END
    df_i_stats[, col_RefStatus] <- combo_RefStatus
    df_i_stats[, col_DataType] <- combo_DataType
    df_i_stats[, "Metric_Name"] <- rownames(df_i_stats)
    rownames(df_i_stats) <- NULL # c()
    # reorder columns
    col_ordered <- c((length(col_StatNames)+1):length(colnames(df_i_stats))
                     # , seq_len(length(col_StatNames)))
                     , seq_along(col_StatNames))
    df_i_stats <- df_i_stats[, col_ordered]

    # Create DF to hold results
    if(i == 1){
      df_results <- df_i_stats
    } else {
      df_results <- rbind(df_results, df_i_stats)
    }## IF ~ i DF ~ END
  }## FOR ~ i n_combos ~ END

  # Return
  return(df_results)

}##FUNCTION ~ metric.stats ~ END
#
