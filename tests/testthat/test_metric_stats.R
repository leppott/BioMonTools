# qc_checks ####
test_that("metric_stats", {

  #' # data, benthos
  df_bugs <- BioMonTools::data_mmi_dev

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
  Subset_Value  <- "CENTRALHILLS"
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
  sum_calc <- sum(df_numbers, na.rm = TRUE)

  sum_qc <- 315244.8

  # test
  testthat::expect_equal(sum_calc, sum_qc)
})## Test ~ qc_checks ~ END

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
