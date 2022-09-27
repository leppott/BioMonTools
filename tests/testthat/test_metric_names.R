# Bugs----
## metval, names, bugs, Function, xlNames ####
testthat::test_that("metric.values, names, bugs, Function, xlNames", {
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

  # Function
  df_metval <- BioMonTools::metric.values(df_benthos
                                          , "bugs"
                                          , boo.marine = TRUE
                                          , boo.Shiny = TRUE)
  metnam_fun <- colnames(df_metval)[-c(1:3)] # remove first 3 columns

  # Check
  metnam_len <- length(metnam_fun)
  metnam_match <- sum(metnam_fun %in% metnam_xlNames)
#
#   # Show non-matches
#   metnam_fun[!(metnam_fun %in% metnam_xlNames)]
#
#   # test
#   testthat::expect_equal(metnam_len, metnam_match)
#
#
  # Show non-matches
  ## A to B
  metnam_fun[!(metnam_fun %in% metnam_xlNames)]
  ## B to A
  metnam_xlNames[!(metnam_xlNames %in% metnam_fun)]

  # test A to B
  testthat::expect_equal(metnam_len, metnam_match)

  # test B to A
  testthat::expect_equal(metnam_match, metnam_len)

})## Test ~ metric names, bugs, Function, Names ~ END

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## metval, names, bugs, xlScoring, xlNames ####
testthat::test_that("metric.values, names, bugs, xlNames, xlScoring", {
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
## metval, names, bugs, xlScoring, Function  ####
testthat::test_that("metric.values, names, bugs, Function, xlScoring", {
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
  #df_benthos$SUBCLASS <- NA

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
  testthat::expect_equal(metnam_len, metnam_match)
})## Test ~ metric.values, names, bugs, Function, Names ~ END

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## metsc, num metrics ####
testthat::test_that("metric.scores, index, number metrics", {
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
  # Fix NJ A/B
  df_metsc_cnt_met[df_metsc_cnt_met$INDEX_NAME == "NJ_NorthernFish_2005"
                   , "n_met_total"] <- 10

  # METRICS (index.scoring)
  # Import
  df_indsc<- readxl::read_excel(fn_xlScoring, sheet="index.scoring")
  # Number of metrics by index name and region
  valid_ScoreRegime <- c("AVERAGE", "SUM", "AVERAGE_100", "AVERAGESCALE_100")
  df_indsc_cnt_met <- df_indsc %>%
    dplyr::filter(ScoreRegime %in% valid_ScoreRegime) %>%
    dplyr::select(INDEX_NAME, INDEX_REGION, NumMetrics)

  # Merge
  df_nummet_merge <- merge(df_metsc_cnt_met, df_indsc_cnt_met
                           , all = TRUE)

  # test vectors
  nummet_metsc <- df_nummet_merge[, "n_met_total"]
  nummet_indsc <- as.numeric(df_nummet_merge[, "NumMetrics"])

  # Need to take out GA single value metrics
  matches <- nummet_metsc == nummet_indsc
  df_nummet_merge[!(matches %in% TRUE), ]

  # test
  testthat::expect_equal(nummet_metsc, nummet_indsc)
})## Test ~ metric.scores, index, number metrics ~ END

# Fish ----
## metval, names, fish, Function, xlNames ####
# Need fish data
testthat::test_that("metric.values, names, fish, Function, xlNames", {
  #Packages
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
  metnam_xlNames <- df_metnam_xlNames[df_metnam_xlNames[, "Community"]=="fish"
                                      , "METRIC_NAME"
                                      , drop = TRUE]

  # fish data
  df_fish <- BioMonTools::data_fish_MBSS

  # Function
  df_metval <- BioMonTools::metric.values(df_fish
                                          , "fish"
                                          , boo.Shiny = TRUE)
  metnam_fun <- colnames(df_metval)[-c(1:5)] # remove first few columns

  # Check
  metnam_len <- length(metnam_fun)
  metnam_match <- sum(metnam_fun %in% metnam_xlNames)

  # Show non-matches
  ## A to B
  metnam_fun[!(metnam_fun %in% metnam_xlNames)]
  ## B to A
  metnam_xlNames[!(metnam_xlNames %in% metnam_fun)]

  # test A to B
  testthat::expect_equal(metnam_len, metnam_match)

  # test B to A
  testthat::expect_equal(metnam_match, metnam_len)

})## Test ~ metric names, fish, Function, Names ~ END

## metval, names, fish, xlScoring, xlNames ####
testthat::test_that("metric.values, names, fish, xlNames, xlScoring", {
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
  metnam_xlNames <- df_metnam_xlNames[df_metnam_xlNames[, "Community"]=="fish"
                                      , "METRIC_NAME", drop = TRUE]
  metnam_xlScoring <- unique(df_metnam_xlScoring[df_metnam_xlScoring[
    , "Community"]=="fish", "METRIC_NAME", drop = TRUE])

  # Check
  metnam_xlScoring_match <- sum(metnam_xlScoring %in% metnam_xlNames)
  metnam_xlScoring_len <- length(metnam_xlScoring)

  # Show non-matches
  metnam_xlScoring[!(metnam_xlScoring %in% metnam_xlNames)]

  # test
  testthat::expect_equal(metnam_xlScoring_match, metnam_xlScoring_len)
})## Test - metric.values, names, Excel, Scoring ~ END

## metval, names, fish, xlScoring, Function  ####
testthat::test_that("metric.values, names, fish, Function, xlScoring", {
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
    , "Community"]=="fish", "METRIC_NAME", drop = TRUE])

  # Benthic Data
  df_fish <- BioMonTools::data_fish_MBSS
  #df_benthos$SUBCLASS <- NA

  # Function
  df_metval <- BioMonTools::metric.values(df_fish
                                          , "fish"
                                          , boo.Shiny = TRUE)
  metnam_fun <- colnames(df_metval)[-c(1:5)] # remove first few columns

  # Check
  metnam_len <- length(metnam_xlScoring)
  metnam_match <- sum(metnam_xlScoring %in% metnam_fun)

  # Show non-matches
  metnam_xlScoring[!(metnam_xlScoring %in% metnam_fun)]

  # test
  testthat::expect_equal(metnam_len, metnam_match)
})## Test ~ metric.values, names, bugs, Function, Names ~ END


# Algae ----
## metval, names, algae, Function, xlNames ####
testthat::test_that("metric.values, names, algae, Function, xlNames", {
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
  metnam_xlNames <- df_metnam_xlNames[df_metnam_xlNames[, "Community"]=="algae"
                                      , "METRIC_NAME"
                                      , drop = TRUE]

  # Algae data
  df_diatoms <- BioMonTools::data_diatom_mmi_dev

  # Function
  df_metval <- BioMonTools::metric.values(df_diatoms
                                          , "algae"
                                          , boo.Shiny = TRUE)
  metnam_fun <- colnames(df_metval)[-c(1:3)] # remove first 3 columns

  # Check
  metnam_len <- length(metnam_fun)
  metnam_match <- sum(metnam_fun %in% metnam_xlNames)

  # Show non-matches
  ## A to B
  metnam_fun[!(metnam_fun %in% metnam_xlNames)]
  ## B to A
  metnam_xlNames[!(metnam_xlNames %in% metnam_fun)]

  # test A to B
  testthat::expect_equal(metnam_len, metnam_match)

  # test B to A
  testthat::expect_equal(metnam_match, metnam_len)

})## Test ~ metric names, algae, Function, Names ~ END

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## metval, names, algae, xlScoring, xlNames ####
testthat::test_that("metric.values, names, algae, xlScoring, xlNames", {
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
  metnam_xlNames <- df_metnam_xlNames[df_metnam_xlNames[, "Community"]=="algae"
                                      , "METRIC_NAME", drop = TRUE]
  metnam_xlScoring <- unique(df_metnam_xlScoring[df_metnam_xlScoring[
    , "Community"]=="algae", "METRIC_NAME", drop = TRUE])

  # Check
  metnam_xlScoring_match <- sum(metnam_xlScoring %in% metnam_xlNames)
  metnam_xlScoring_len <- length(metnam_xlScoring)

  # Show non-matches
  metnam_xlScoring[!(metnam_xlScoring %in% metnam_xlNames)]

  # test
  testthat::expect_equal(metnam_xlScoring_match, metnam_xlScoring_len)
})## Test - metric.values, names, Excel, Scoring ~ END

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## metval, names, algae, xlScoring, Function ####
testthat::test_that("metric.values, names, algae, xlScoring, Function", {
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
    , "Community"]=="algae", "METRIC_NAME", drop = TRUE])

  # Algae data
  df_diatoms <- BioMonTools::data_diatom_mmi_dev

  # Function
  df_metval <- BioMonTools::metric.values(df_diatoms
                                          , "algae"
                                          , boo.Shiny = TRUE)
  metnam_fun <- colnames(df_metval)[-c(1:3)] # remove first 3 columns

  # Check
  metnam_len <- length(metnam_xlScoring)
  metnam_match <- sum(metnam_xlScoring %in% metnam_fun)

  # Show non-matches
  metnam_xlScoring[!(metnam_xlScoring %in% metnam_fun)]

  metnam_xlScoring[!(metnam_xlScoring %in% metnam_fun)]

  # test
  testthat::expect_equal(metnam_len, metnam_match)  # fails due to structure
})## Test ~ metric.values, names, algae, Function, Names ~ END


# Excel ----
## metval, xlNames, NA ####
testthat::test_that("metric.values, xlNames, description", {
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

  # Num Metrics, Name
  met_name_len <- length(df_metnam_xlNames$METRIC_NAME)

  # Description, NA
  num_NA_desc <- sum(is.na(df_metnam_xlNames$Description))

  ### test, NA, DESCRIPTION ----
  testthat::expect_equal(num_NA_desc, 0)

  # Description, NA
  num_NA_comm <- sum(is.na(df_metnam_xlNames$Community))

  ## test, NA, COMMENTS ----
  testthat::expect_equal(num_NA_comm, 0)

})## Test ~ metval, xlNames, NA ~ END
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
