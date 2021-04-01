#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# metval, Names, bugs, Function, xlNames ####
test_that("metric.values, names, bugs, Function, xlNames", {
  # Packages
  #library(readxl) # part of BioMonTools

  # Data
  fn_metnam_xlNames   <- file.path(system.file(package="BioMonTools")
                                   , "extdata"
                                   , "MetricNames.xlsx")
  # Import
  df_metnam_xlNames   <- readxl::read_excel(fn_metnam_xlNames
                                            , sheet="MetricMetadata"
                                            , skip = 4)
  df_metnam_xlNames <- as.data.frame(df_metnam_xlNames)
  # Metric Names
  metnam_xlNames <- df_metnam_xlNames[df_metnam_xlNames[, "Community"]=="bugs"
                                      , "METRIC_NAME"
                                      , drop = TRUE]

  # Benthic data
  df_benthos <- BioMonTools::data_benthos_PacNW
  df_benthos$SUBCLASS <- NA

  # Function
  df_metval <- BioMonTools::metric.values(df_benthos
                                          , "bugs"
                                          , boo.marine = TRUE
                                          , boo.Shiny = TRUE)
  metnam_fun <- colnames(df_metval)[-c(1:3)] # remove first 3 columns

  # Check
  metnam_len <- length(metnam_fun)
  metnam_match <- sum(metnam_fun %in% metnam_xlNames)

  # Show non-matches
  metnam_fun[!(metnam_fun %in% metnam_xlNames)]

  # test
  testthat::expect_equal(metnam_len, metnam_match)

})## Test ~ metric names, bugs, Function, Names ~ END

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# metval, names, bugs, xlNames, xlScoring ####
test_that("metric.values, names, bugs, xlNames, xlScoring", {
  # Packages
  #library(readxl) # part of BioMonTools

  # Data
  fn_metnam_xlNames   <- file.path(system.file(package="BioMonTools")
                                   , "extdata"
                                   , "MetricNames.xlsx")
  fn_metnam_xlScoring <- file.path(system.file(package="BioMonTools")
                                   , "extdata"
                                   , "MetricScoring.xlsx")
  # Import
  df_metnam_xlNames   <- readxl::read_excel(fn_metnam_xlNames
                                            , sheet="MetricMetadata"
                                            , skip = 4)
  df_metnam_xlScoring <- readxl::read_excel(fn_metnam_xlScoring
                                            , sheet="metric.scoring")
  # Convert to data frames
  df_metnam_xlNames <- as.data.frame(df_metnam_xlNames)
  df_metnam_xlScoring <- as.data.frame(df_metnam_xlScoring)

  # Names
  metnam_xlNames <- df_metnam_xlNames[df_metnam_xlNames[, "Community"]=="bugs"
                                      , "METRIC_NAME", drop = TRUE]
  metnam_xlScoring <- unique(df_metnam_xlScoring[df_metnam_xlScoring[
    , "Community"]=="bugs", "METRIC_NAME", drop = TRUE])

  # Check
  metnam_xlScoring_match <- sum(metnam_xlScoring %in% metnam_xlNames)
  metnam_xlScoring_len <- length(metnam_xlScoring)

  # Show non-matches
  metnam_xlScoring[!(metnam_xlScoring %in% metnam_xlNames)]

  # test
  testthat::expect_equal(metnam_xlScoring_match, metnam_xlScoring_len)
})## Test - metric.values, names, Excel, Scoring ~ END

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# metval, names, bugs, Function, Names ####
test_that("metric.values, names, bugs, Function, Names", {
  # Packages
  #library(readxl) # part of BioMonTools

  # Data
  fn_metnam_xlScoring <- file.path(system.file(package="BioMonTools")
                                   , "extdata"
                                   , "MetricScoring.xlsx")
  # Import
  df_metnam_xlScoring <- readxl::read_excel(fn_metnam_xlScoring
                                            , sheet="metric.scoring")
  df_metnam_xlScoring <- as.data.frame(df_metnam_xlScoring)

  # Metric Names
  metnam_xlScoring <- unique(df_metnam_xlScoring[df_metnam_xlScoring[
    , "Community"]=="bugs", "METRIC_NAME", drop = TRUE])

  # Benthic Data
  df_benthos <- BioMonTools::data_benthos_PacNW
  df_benthos$SUBCLASS <- NA

  # Function
  df_metval <- BioMonTools::metric.values(df_benthos
                                          , "bugs"
                                          , boo.marine = TRUE
                                          , boo.Shiny = TRUE)
  metnam_fun <- colnames(df_metval)[-c(1:3)] # remove first 3 columns

  # Check
  metnam_len <- length(metnam_xlScoring)
  metnam_match <- sum(metnam_xlScoring %in% metnam_fun)

  # Show non-matches
  metnam_xlScoring[!(metnam_xlScoring %in% metnam_fun)]

  # test
  testthat::expect_equal(metnam_len, metnam_match)  # fails due to structure
})## Test ~ metric.values, names, bugs, Function, Names ~ END

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# metsc, num metrics ####
test_that("metric.scores, index, number metrics", {
  # Packages
  #library(readxl) # part of BioMonTools
  #library(dplyr)
  `%>%` <- dplyr::`%>%`

  # Data File
  fn_xlScoring <- file.path(system.file(package="BioMonTools")
                            , "extdata"
                            , "MetricScoring.xlsx")

  # METRICS (metric.scoring)
  # Import
  df_metsc <- readxl::read_excel(fn_xlScoring
                                 , sheet="metric.scoring"
                                 , na = c("", "NA", NA))
  # Number of metrics by index name and region
  df_metsc_cnt_met <- df_metsc %>%
    dplyr::group_by(INDEX_NAME, INDEX_REGION) %>%
    dplyr::summarize(n_met_all = dplyr::n()
              , n_met_single = sum(!is.na(SingleValue_Add))
              , n_met_total = n_met_all - n_met_single
              , .groups = "drop_last")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Need to take out GA single value metrics
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # METRICS (index.scoring)
  # Import
  df_indsc<- readxl::read_excel(fn_xlScoring, sheet="index.scoring")
  # Number of metrics by index name and region
  df_indsc_cnt_met <- df_indsc %>%
    dplyr::filter(ScoreRegime == "AVERAGE" | ScoreRegime == "SUM"
                  | ScoreRegime == "AVERAGE_100") %>%
    dplyr::select(INDEX_NAME, INDEX_REGION, NumMetrics)

  # Merge
  df_nummet_merge <- merge(df_metsc_cnt_met, df_indsc_cnt_met
                           , all = TRUE)

  # test vectors
  nummet_metsc <- df_nummet_merge[, "n_met_total"]
  nummet_indsc <- df_nummet_merge[, "NumMetrics"]

  # Need to take out GA single value metrics
  matches <- nummet_metsc == nummet_indsc
  df_nummet_merge[!(matches %in% TRUE), ]

  # test
  testthat::expect_equal(nummet_metsc, nummet_indsc)  # fails due to structure
})## Test ~ metric.scores, index, number metrics ~ END

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
