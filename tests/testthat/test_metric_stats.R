# metric_stats and metric_stats2 ####
test_that("metric_stats & metric_stats2", {

  #' # data, benthos
  df_bugs <- BioMonTools::data_mmi_dev

  # Add UFC, 2021-11-02
  df_bugs$UFC <- NA_integer_
  df_bugs$ELEVATION_ATTR <- NA_character_
  df_bugs$GRADIENT_ATTR <- NA_character_
  df_bugs$WSAREA_ATTR <- NA_character_

  # Munge Names
  names(df_bugs)[names(df_bugs) %in% "BenSampID"] <- "SAMPLEID"
  names(df_bugs)[names(df_bugs) %in% "TaxaID"] <- "TAXAID"
  names(df_bugs)[names(df_bugs) %in% "Individuals"] <- "N_TAXA"
  names(df_bugs)[names(df_bugs) %in% "Exclude"] <- "EXCLUDE"
  names(df_bugs)[names(df_bugs) %in% "Class"] <- "INDEX_REGION"
  names(df_bugs)[names(df_bugs) %in% "Unique_ID"] <- "SITEID"

  # Calc Metrics
  cols_keep <- c("Ref_v1", "CalVal_Class4", "SITEID", "CollDate", "CollMeth")
  # INDEX_NAME and INDEX_REGION kept by default
  df_metval <- BioMonTools::metric.values(df_bugs
                                          , "bugs"
                                          , fun.cols2keep = cols_keep)

  # Calc Stats
  col_metrics   <- names(df_metval)[9:ncol(df_metval)]
  col_SampID    <- "SAMPLEID"
  col_RefStatus <- "REF_V1"
  RefStatus_Ref <- "Ref"
  RefStatus_Str <- "Strs"
  RefStatus_Oth <- "Other"
  col_DataType  <- "CALVAL_CLASS4"
  DataType_Cal  <- "cal"
  DataType_Ver  <- "verif"
  col_Subset    <- "INDEX_REGION"
  Subset_Value  <- "CentralHills"
  df_stats <- BioMonTools::metric.stats(df_metval
                                       , col_metrics
                                       , col_SampID
                                       , col_RefStatus
                                       , RefStatus_Ref
                                       , RefStatus_Str
                                       , RefStatus_Oth
                                       , col_DataType
                                       , DataType_Cal
                                       , DataType_Ver
                                       , col_Subset
                                       , Subset_Value)

  df_numbers <- df_stats[, -(1:4)]
  # 2022-02-22, replace Inf and -Inf with NA
  df_num_inf <- sapply(df_numbers, is.infinite)
  df_numbers[df_num_inf] <- NA
  #
  sum_calc <- sum(df_numbers, na.rm = TRUE)

  #sum_qc <- 315244.8
  #sum_qc <- 331996.8 # new value, 2021-04-14
  sum_qc <- 367118.8 # new value, 2022-02-22, new metrics

  # test
  testthat::expect_equal(sum_calc, sum_qc, tolerance = 0.02)



  # # metricstats2
  #
  # # Calc Stats2 (z-scores and DE)
  # data_metval <- df_metval
  # data_metstat <- df_stats
  # col_metval_RefStatus <- "REF_V1"
  # col_metval_DataType <- "CALVAL_CLASS4"
  # col_metval_Subset <- "INDEX_REGION"
  # col_metstat_RefStatus <- "REF_V1"
  # col_metstat_DataType <- "CALVAL_CLASS4"
  # col_metstat_Subset <- "INDEX_REGION"
  # RefStatus_Ref = "Ref"
  # RefStatus_Str = "Strs"
  # RefStatus_Oth = "Other"
  # DataType_Cal = "cal"
  # DataType_Ver = "verif"
  # Subset_Value = "CENTRALHILLS"
  # df_stats2 <- BioMonTools::metric.stats2(data_metval
  #                                  , data_metstat
  #                                  , col_metval_RefStatus
  #                                  , col_metval_DataType
  #                                  , col_metval_Subset
  #                                  , col_metstat_RefStatus
  #                                  , col_metstat_DataType
  #                                  , col_metstat_Subset
  #                                  , RefStatus_Ref
  #                                  , RefStatus_Str
  #                                  , RefStatus_Oth
  #                                  , DataType_Cal
  #                                  , DataType_Ver
  #                                  , Subset_Value)
  #
  #
  # df_numbers2 <- df_stats2[, -(1:4)]
  # # -Inf in CV column, replace with NA
  # df_numbers2[df_numbers2 == -Inf] <- NA
  # df_numbers2 <- df_numbers2[, 1:15]
  #
  # sum2_calc <- sum(df_numbers2, na.rm = TRUE)
  #
  # sum2_qc <- 315376
  #
  # # test
  # testthat::expect_equal(sum2_calc, sum2_qc, tolerance = 0.02)

})## Test ~ qc_checks ~ END

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
