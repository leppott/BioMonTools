# Bugs----
## metval, names, bugs, Function, duplicates ####
testthat::test_that("metric.values, names, bugs, Function, xlNames", {
  # Packages
  #library(readxl) # part of BioMonTools

  # Benthic data
  df_benthos <- BioMonTools::data_benthos_PacNW

  # Function
  df_metval <- BioMonTools::metric.values(df_benthos
                                          , "bugs"
                                          , boo.marine = TRUE
                                          , boo.Shiny = TRUE)
  metnam_fun <- colnames(df_metval)[-(1:3)] # remove first 3 columns

  # dups
  names_dup <- metnam_fun[duplicated(metnam_fun)]

  metnam_len <- length(metnam_fun)
  metnam_len_unique <- length(unique(metnam_fun))

  # test
  testthat::expect_equal(metnam_len, metnam_len_unique)

})## Test ~ metric names, bugs, Function, duplicates

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## metval, names, bugs, Function, xlNames ####
testthat::test_that("metric.values, names, bugs, Function, xlNames", {
  # Packages
  #library(readxl) # part of BioMonTools

  # Data
  fn_metnam_xlNames   <- file.path(system.file(package = "BioMonTools")
                                   , "extdata"
                                   , "MetricNames.xlsx")
  # Import
  df_metnam_xlNames   <- readxl::read_excel(fn_metnam_xlNames
                                            , sheet = "MetricMetadata"
                                            , skip = 4)
  df_metnam_xlNames <- as.data.frame(df_metnam_xlNames)
  # Metric Names
  metnam_xlNames <- df_metnam_xlNames[df_metnam_xlNames[, "Community"] == "bugs"
                                      , "METRIC_NAME"
                                      , drop = TRUE]

  # Benthic data
  df_benthos <- BioMonTools::data_benthos_PacNW

  # Function
  df_metval <- BioMonTools::metric.values(df_benthos
                                          , "bugs"
                                          , boo.marine = TRUE
                                          , boo.Shiny = TRUE)
  metnam_fun <- colnames(df_metval)[-(1:3)] # remove first 3 columns

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

})## Test ~ metric names, bugs, Function, Names

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## metval, names, bugs, xlScoring, xlNames ####
testthat::test_that("metric.values, names, bugs, xlNames, xlScoring", {
  # Packages
  #library(readxl) # part of BioMonTools

  # Data
  fn_metnam_xlNames   <- file.path(system.file(package = "BioMonTools")
                                   , "extdata"
                                   , "MetricNames.xlsx")
  fn_metnam_xlScoring <- file.path(system.file(package = "BioMonTools")
                                   , "extdata"
                                   , "MetricScoring.xlsx")
  # Import
  df_metnam_xlNames   <- readxl::read_excel(fn_metnam_xlNames
                                            , sheet = "MetricMetadata"
                                            , skip = 4)
  df_metnam_xlScoring <- readxl::read_excel(fn_metnam_xlScoring
                                            , sheet = "metric.scoring")
  # Convert to data frames
  df_metnam_xlNames <- as.data.frame(df_metnam_xlNames)
  df_metnam_xlScoring <- as.data.frame(df_metnam_xlScoring)

  # Names
  metnam_xlNames <- df_metnam_xlNames[df_metnam_xlNames[, "Community"] == "bugs"
                                      , "METRIC_NAME", drop = TRUE]
  metnam_xlScoring <- unique(df_metnam_xlScoring[df_metnam_xlScoring[
    , "Community"] == "bugs", "METRIC_NAME", drop = TRUE])

  # Check
  metnam_xlScoring_match <- sum(metnam_xlScoring %in% metnam_xlNames)
  metnam_xlScoring_len <- length(metnam_xlScoring)

  # Show non-matches
  metnam_xlScoring[!(metnam_xlScoring %in% metnam_xlNames)]

  # test
  testthat::expect_equal(metnam_xlScoring_match, metnam_xlScoring_len)
})## Test - metric.values, names, Excel, Scoring

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## metval, names, bugs, xlScoring, Function  ####
testthat::test_that("metric.values, names, bugs, Function, xlScoring", {
  # Packages
  #library(readxl) # part of BioMonTools

  # Data
  fn_metnam_xlScoring <- file.path(system.file(package = "BioMonTools")
                                   , "extdata"
                                   , "MetricScoring.xlsx")
  # Import
  df_metnam_xlScoring <- readxl::read_excel(fn_metnam_xlScoring
                                            , sheet = "metric.scoring")
  df_metnam_xlScoring <- as.data.frame(df_metnam_xlScoring)

  # Metric Names
  metnam_xlScoring <- unique(df_metnam_xlScoring[df_metnam_xlScoring[
    , "Community"] == "bugs", "METRIC_NAME", drop = TRUE])

  # Benthic Data
  df_benthos <- BioMonTools::data_benthos_PacNW
  #df_benthos$SUBCLASS <- NA

  # Function
  df_metval <- BioMonTools::metric.values(df_benthos
                                          , "bugs"
                                          , boo.marine = TRUE
                                          , boo.Shiny = TRUE)
  metnam_fun <- colnames(df_metval)[-(1:3)] # remove first 3 columns

  # Check
  metnam_len <- length(metnam_xlScoring)
  metnam_match <- sum(metnam_xlScoring %in% metnam_fun)

  # Show non-matches
  metnam_xlScoring[!(metnam_xlScoring %in% metnam_fun)]

  # test
  testthat::expect_equal(metnam_len, metnam_match)
})## Test ~ metric.values, names, bugs, Function, Names

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## metsc, num metrics ####
testthat::test_that("metric.scores, index, number metrics", {
  # Packages
  #library(readxl) # part of BioMonTools
  #library(dplyr)

  # Data File
  fn_xlScoring <- file.path(system.file(package = "BioMonTools")
                            , "extdata"
                            , "MetricScoring.xlsx")

  # METRICS (metric.scoring)
  # Import
  df_metsc <- readxl::read_excel(fn_xlScoring
                                 , sheet = "metric.scoring"
                                 , na = c("", "NA", NA))
  # Number of metrics by index name and region
  df_metsc_cnt_met <- df_metsc |>
    dplyr::group_by(INDEX_NAME, INDEX_CLASS) |>
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
  df_indsc <- readxl::read_excel(fn_xlScoring, sheet = "index.scoring")
  # Number of metrics by index name and region
  valid_ScoreRegime <- c("AVERAGE"
                         , "SUM"
                         , "AVERAGE_100"
                         , "AVERAGE_010"
                         , "AVERAGESCALE_100"
                         , "AVERAGE_100_M10_R2"
                         , "AVERAGE_100_M10_R3")
  df_indsc_cnt_met <- df_indsc |>
    dplyr::filter(ScoreRegime %in% valid_ScoreRegime) |>
    dplyr::select(INDEX_NAME, INDEX_CLASS, NumMetrics)

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
})## Test ~ metric.scores, index, number metrics

# Fish ----
## metval, names, fish, Function, duplicates ####
testthat::test_that("metric.values, names, fish, Function, xlNames", {
  # Packages
  #library(readxl) # part of BioMonTools

  # fish data
  df_fish <- BioMonTools::data_fish_MBSS
  # Munge (v1.0.2.9015, 2024-04-29)
  df_fish$TOLVAL2 <- NA_integer_
  df_fish$BCG_ATTR2 <- NA_character_
  # 2025-12-26
  df_fish$AGECLASS <- c(NA, 1:10)

  # Function
  df_metval <- BioMonTools::metric.values(df_fish
                                          , "fish"
                                          , boo.Shiny = TRUE)
  metnam_fun <- colnames(df_metval)[-(1:5)] # remove first few columns

  # dups
  names_dup <- metnam_fun[duplicated(metnam_fun)]

  metnam_len <- length(metnam_fun)
  metnam_len_unique <- length(unique(metnam_fun))

  # test
  testthat::expect_equal(metnam_len, metnam_len_unique)

})## Test ~ metric names, fish, Function, duplicates

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## metval, names, fish, Function, xlNames ####
# Need fish data
testthat::test_that("metric.values, names, fish, Function, xlNames", {
  #Packages
  #library(readxl) # part of BioMonTools

  # Data
  fn_metnam_xlNames   <- file.path(system.file(package = "BioMonTools")
                                   , "extdata"
                                   , "MetricNames.xlsx")
  # Import
  df_metnam_xlNames   <- readxl::read_excel(fn_metnam_xlNames
                                            , sheet = "MetricMetadata"
                                            , skip = 4)
  df_metnam_xlNames <- as.data.frame(df_metnam_xlNames)
  # Metric Names
  metnam_xlNames <- df_metnam_xlNames[df_metnam_xlNames[, "Community"] == "fish"
                                      , "METRIC_NAME"
                                      , drop = TRUE]

  # fish data
  df_fish <- BioMonTools::data_fish_MBSS
  # Munge (v1.0.2.9015, 2024-04-29)
  df_fish$TOLVAL2 <- NA_integer_
  df_fish$BCG_ATTR2 <- NA_character_
  # 2025-12-26
  df_fish$AGECLASS <- c(NA, 1:10)

  # Function
  df_metval <- BioMonTools::metric.values(df_fish
                                          , "fish"
                                          , boo.Shiny = TRUE)
  metnam_fun <- colnames(df_metval)[-(1:5)] # remove first few columns

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

})## Test ~ metric names, fish, Function, Names

## metval, names, fish, xlScoring, xlNames ####
testthat::test_that("metric.values, names, fish, xlNames, xlScoring", {
  # Packages
  #library(readxl) # part of BioMonTools

  # Data
  fn_metnam_xlNames   <- file.path(system.file(package = "BioMonTools")
                                   , "extdata"
                                   , "MetricNames.xlsx")
  fn_metnam_xlScoring <- file.path(system.file(package = "BioMonTools")
                                   , "extdata"
                                   , "MetricScoring.xlsx")
  # Import
  df_metnam_xlNames   <- readxl::read_excel(fn_metnam_xlNames
                                            , sheet = "MetricMetadata"
                                            , skip = 4)
  df_metnam_xlScoring <- readxl::read_excel(fn_metnam_xlScoring
                                            , sheet = "metric.scoring")
  # Convert to data frames
  df_metnam_xlNames <- as.data.frame(df_metnam_xlNames)
  df_metnam_xlScoring <- as.data.frame(df_metnam_xlScoring)

  # Names
  metnam_xlNames <- df_metnam_xlNames[df_metnam_xlNames[, "Community"] == "fish"
                                      , "METRIC_NAME", drop = TRUE]
  metnam_xlScoring <- unique(df_metnam_xlScoring[df_metnam_xlScoring[
    , "Community"] == "fish", "METRIC_NAME", drop = TRUE])

  # Check
  metnam_xlScoring_match <- sum(metnam_xlScoring %in% metnam_xlNames)
  metnam_xlScoring_len <- length(metnam_xlScoring)

  # Show non-matches
  metnam_xlScoring[!(metnam_xlScoring %in% metnam_xlNames)]

  # test
  testthat::expect_equal(metnam_xlScoring_match, metnam_xlScoring_len)
})## Test - metric.values, names, Excel, Scoring

## metval, names, fish, xlScoring, Function  ####
testthat::test_that("metric.values, names, fish, Function, xlScoring", {
  # Packages
  #library(readxl) # part of BioMonTools

  # Data
  fn_metnam_xlScoring <- file.path(system.file(package = "BioMonTools")
                                   , "extdata"
                                   , "MetricScoring.xlsx")
  # Import
  df_metnam_xlScoring <- readxl::read_excel(fn_metnam_xlScoring
                                            , sheet = "metric.scoring")
  df_metnam_xlScoring <- as.data.frame(df_metnam_xlScoring)

  # Metric Names
  metnam_xlScoring <- unique(df_metnam_xlScoring[df_metnam_xlScoring[
    , "Community"] == "fish", "METRIC_NAME", drop = TRUE])

  # Fish Data
  df_fish <- BioMonTools::data_fish_MBSS
  #df_benthos$SUBCLASS <- NA
  # Munge (v1.0.2.9015, 2024-04-29)
  df_fish$TOLVAL2 <- NA_integer_
  df_fish$BCG_ATTR2 <- NA_character_
  # 2025-12-26
  df_fish$AGECLASS <- c(NA, 1:10)

  # Function
  df_metval <- BioMonTools::metric.values(df_fish
                                          , "fish"
                                          , boo.Shiny = TRUE)
  metnam_fun <- colnames(df_metval)[-(1:5)] # remove first few columns

  # Check
  metnam_len <- length(metnam_xlScoring)
  metnam_match <- sum(metnam_xlScoring %in% metnam_fun)

  # Show non-matches
  metnam_xlScoring[!(metnam_xlScoring %in% metnam_fun)]

  # test
  testthat::expect_equal(metnam_len, metnam_match)
})## Test ~ metric.values, names, bugs, Function, Names


# Algae ----
## metval, names, algae, Function, duplicates ####
testthat::test_that("metric.values, names, algae, Function, xlNames", {
  # Packages
  #library(readxl) # part of BioMonTools

  # Algae data
  df_diatoms <- BioMonTools::data_diatom_mmi_dev

  # 20250908
  # Add Exclude = TRUE to avoid warning
  # checking names only not values
  # ok if data not correct
  df_diatoms[1, "EXCLUDE"] <- TRUE

  # Function
  df_metval <- BioMonTools::metric.values(df_diatoms
                                          , "algae"
                                          , boo.Shiny = TRUE)
  metnam_fun <- colnames(df_metval)[-(1:3)] # remove first 3 columns

  # dups
  names_dup <- metnam_fun[duplicated(metnam_fun)]

  metnam_len <- length(metnam_fun)
  metnam_len_unique <- length(unique(metnam_fun))

  # test
  testthat::expect_equal(metnam_len, metnam_len_unique)

})## Test ~ metric names, algae, Function, duplicates

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## metval, names, algae, Function, xlNames ####
testthat::test_that("metric.values, names, algae, Function, xlNames", {
  # Packages
  #library(readxl) # part of BioMonTools

  # Data
  fn_metnam_xlNames   <- file.path(system.file(package = "BioMonTools")
                                   , "extdata"
                                   , "MetricNames.xlsx")
  # Import
  df_metnam_xlNames   <- readxl::read_excel(fn_metnam_xlNames
                                            , sheet = "MetricMetadata"
                                            , skip = 4)
  df_metnam_xlNames <- as.data.frame(df_metnam_xlNames)
  # Metric Names
  metnam_xlNames <- df_metnam_xlNames[df_metnam_xlNames[, "Community"] == "algae"
                                      , "METRIC_NAME"
                                      , drop = TRUE]

  # Algae data
  df_diatoms <- BioMonTools::data_diatom_mmi_dev

  # 20250908
  # Add Exclude = TRUE to avoid warning
  # checking names only not values
  # ok if data not correct
  df_diatoms[1, "EXCLUDE"] <- TRUE

  # Function
  df_metval <- BioMonTools::metric.values(df_diatoms
                                          , "algae"
                                          , boo.Shiny = TRUE)
  metnam_fun <- colnames(df_metval)[-(1:3)] # remove first 3 columns

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

})## Test ~ metric names, algae, Function, Names

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## metval, names, algae, xlScoring, xlNames ####
testthat::test_that("metric.values, names, algae, xlScoring, xlNames", {
  # Packages
  #library(readxl) # part of BioMonTools

  # Data
  fn_metnam_xlNames   <- file.path(system.file(package = "BioMonTools")
                                   , "extdata"
                                   , "MetricNames.xlsx")
  fn_metnam_xlScoring <- file.path(system.file(package = "BioMonTools")
                                   , "extdata"
                                   , "MetricScoring.xlsx")
  # Import
  df_metnam_xlNames   <- readxl::read_excel(fn_metnam_xlNames
                                            , sheet = "MetricMetadata"
                                            , skip = 4)
  df_metnam_xlScoring <- readxl::read_excel(fn_metnam_xlScoring
                                            , sheet = "metric.scoring")
  # Convert to data frames
  df_metnam_xlNames <- as.data.frame(df_metnam_xlNames)
  df_metnam_xlScoring <- as.data.frame(df_metnam_xlScoring)

  # Names
  metnam_xlNames <- df_metnam_xlNames[df_metnam_xlNames[, "Community"] == "algae"
                                      , "METRIC_NAME", drop = TRUE]
  metnam_xlScoring <- unique(df_metnam_xlScoring[df_metnam_xlScoring[
    , "Community"] == "algae", "METRIC_NAME", drop = TRUE])

  # Check
  metnam_xlScoring_match <- sum(metnam_xlScoring %in% metnam_xlNames)
  metnam_xlScoring_len <- length(metnam_xlScoring)

  # Show non-matches
  metnam_xlScoring[!(metnam_xlScoring %in% metnam_xlNames)]

  # test
  testthat::expect_equal(metnam_xlScoring_match, metnam_xlScoring_len)
})## Test - metric.values, names, Excel, Scoring

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## metval, names, algae, xlScoring, Function ####
testthat::test_that("metric.values, names, algae, xlScoring, Function", {
  # Packages
  #library(readxl) # part of BioMonTools

  # Data
  fn_metnam_xlScoring <- file.path(system.file(package = "BioMonTools")
                                   , "extdata"
                                   , "MetricScoring.xlsx")
  # Import
  df_metnam_xlScoring <- readxl::read_excel(fn_metnam_xlScoring
                                            , sheet = "metric.scoring")
  df_metnam_xlScoring <- as.data.frame(df_metnam_xlScoring)

  # Metric Names
  metnam_xlScoring <- unique(df_metnam_xlScoring[df_metnam_xlScoring[
    , "Community"] == "algae", "METRIC_NAME", drop = TRUE])

  # Algae data
  df_diatoms <- BioMonTools::data_diatom_mmi_dev

  # 20250908
  # Add Exclude = TRUE to avoid warning
  # checking names only not values
  # ok if data not correct
  df_diatoms[1, "EXCLUDE"] <- TRUE

  # Function
  df_metval <- BioMonTools::metric.values(df_diatoms
                                          , "algae"
                                          , boo.Shiny = TRUE)
  metnam_fun <- colnames(df_metval)[-(1:3)] # remove first 3 columns

  # Check
  metnam_len <- length(metnam_xlScoring)
  metnam_match <- sum(metnam_xlScoring %in% metnam_fun)

  # Show non-matches
  metnam_xlScoring[!(metnam_xlScoring %in% metnam_fun)]

  metnam_xlScoring[!(metnam_xlScoring %in% metnam_fun)]

  # test
  testthat::expect_equal(metnam_len, metnam_match)  # fails due to structure
})## Test ~ metric.values, names, algae, Function, Names

# Coral ----
## metval, names, coral, Function, duplicates ####
testthat::test_that("metric.values, names, coral, Function, xlNames", {
  # Packages
  #library(readxl) # part of BioMonTools

  # coral data
  df_corals <- BioMonTools::data_coral_bcg_metric_dev

  # Function
  df_metval <- BioMonTools::metric.values(df_corals
                                          , "coral"
                                          , boo.Shiny = TRUE)
  metnam_fun <- colnames(df_metval)[-(1:3)] # remove first 3 columns

  # dups
  names_dup <- metnam_fun[duplicated(metnam_fun)]

  metnam_len <- length(metnam_fun)
  metnam_len_unique <- length(unique(metnam_fun))

  # test
  testthat::expect_equal(metnam_len, metnam_len_unique)

})## Test ~ metric names, coral, Function, duplicates

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## metval, names, coral, Function, xlNames ####
testthat::test_that("metric.values, names, coral, Function, xlNames", {
  # Packages
  #library(readxl) # part of BioMonTools

  # Data
  fn_metnam_xlNames   <- file.path(system.file(package = "BioMonTools")
                                   , "extdata"
                                   , "MetricNames.xlsx")
  # Import
  df_metnam_xlNames   <- readxl::read_excel(fn_metnam_xlNames
                                            , sheet = "MetricMetadata"
                                            , skip = 4)
  df_metnam_xlNames <- as.data.frame(df_metnam_xlNames)
  # Metric Names
  metnam_xlNames <- df_metnam_xlNames[df_metnam_xlNames[, "Community"] == "coral"
                                      , "METRIC_NAME"
                                      , drop = TRUE]

  # coral data
  df_corals <- BioMonTools::data_coral_bcg_metric_dev

  # Function
  df_metval <- BioMonTools::metric.values(df_corals
                                          , "coral"
                                          , boo.Shiny = TRUE)
  metnam_fun <- colnames(df_metval)[-(1:3)] # remove first 3 columns

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

})## Test ~ metric names, coral, Function, Names

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## metval, names, coral, xlScoring, xlNames ####
testthat::test_that("metric.values, names, coral, xlScoring, xlNames", {
  # Packages
  #library(readxl) # part of BioMonTools

  # Data
  fn_metnam_xlNames   <- file.path(system.file(package = "BioMonTools")
                                   , "extdata"
                                   , "MetricNames.xlsx")
  fn_metnam_xlScoring <- file.path(system.file(package = "BioMonTools")
                                   , "extdata"
                                   , "MetricScoring.xlsx")
  # Import
  df_metnam_xlNames   <- readxl::read_excel(fn_metnam_xlNames
                                            , sheet = "MetricMetadata"
                                            , skip = 4)
  df_metnam_xlScoring <- readxl::read_excel(fn_metnam_xlScoring
                                            , sheet = "metric.scoring")
  # Convert to data frames
  df_metnam_xlNames <- as.data.frame(df_metnam_xlNames)
  df_metnam_xlScoring <- as.data.frame(df_metnam_xlScoring)

  # Names
  metnam_xlNames <- df_metnam_xlNames[df_metnam_xlNames[, "Community"] == "coral"
                                      , "METRIC_NAME", drop = TRUE]
  metnam_xlScoring <- unique(df_metnam_xlScoring[df_metnam_xlScoring[
    , "Community"] == "coral", "METRIC_NAME", drop = TRUE])

  # Check
  metnam_xlScoring_match <- sum(metnam_xlScoring %in% metnam_xlNames)
  metnam_xlScoring_len <- length(metnam_xlScoring)

  # Show non-matches
  metnam_xlScoring[!(metnam_xlScoring %in% metnam_xlNames)]

  # test
  testthat::expect_equal(metnam_xlScoring_match, metnam_xlScoring_len)
})## Test - metric.values, names, Excel, Scoring

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## metval, names, coral, xlScoring, Function ####
testthat::test_that("metric.values, names, coral, xlScoring, Function", {
  # Packages
  #library(readxl) # part of BioMonTools

  # Data
  fn_metnam_xlScoring <- file.path(system.file(package = "BioMonTools")
                                   , "extdata"
                                   , "MetricScoring.xlsx")
  # Import
  df_metnam_xlScoring <- readxl::read_excel(fn_metnam_xlScoring
                                            , sheet = "metric.scoring")
  df_metnam_xlScoring <- as.data.frame(df_metnam_xlScoring)

  # Metric Names
  metnam_xlScoring <- unique(df_metnam_xlScoring[df_metnam_xlScoring[
    , "Community"] == "coral", "METRIC_NAME", drop = TRUE])

  # coral data
  df_corals <- BioMonTools::data_coral_bcg_metric_dev

  # Function
  df_metval <- BioMonTools::metric.values(df_corals
                                          , "coral"
                                          , boo.Shiny = TRUE)
  metnam_fun <- colnames(df_metval)[-(1:3)] # remove first 3 columns

  # Check
  metnam_len <- length(metnam_xlScoring)
  metnam_match <- sum(metnam_xlScoring %in% metnam_fun)

  # Show non-matches
  metnam_xlScoring[!(metnam_xlScoring %in% metnam_fun)]

  metnam_xlScoring[!(metnam_xlScoring %in% metnam_fun)]

  # test
  testthat::expect_equal(metnam_len, metnam_match)  # fails due to structure
})## Test ~ metric.values, names, coral, Function, Names


# Excel ----
## metval, xlNames, NA ####
testthat::test_that("metric.values, xlNames, description", {
  # Packages
  #library(readxl) # part of BioMonTools

  # Data
  fn_metnam_xlNames   <- file.path(system.file(package = "BioMonTools")
                                   , "extdata"
                                   , "MetricNames.xlsx")
  # Import
  df_metnam_xlNames   <- readxl::read_excel(fn_metnam_xlNames
                                            , sheet = "MetricMetadata"
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

  ### test, NA, COMMENTS ----
  testthat::expect_equal(num_NA_comm, 0)

})## Test ~ metval, xlNames, NA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## metval, xlNames, dups ####
testthat::test_that("metric.values, names, xlNames, duplicates", {
  # Packages
  #library(readxl) # part of BioMonTools

  # Data
  fn_metnam_xlNames   <- file.path(system.file(package = "BioMonTools")
                                   , "extdata"
                                   , "MetricNames.xlsx")
  # Import
  df_metnam_xlNames   <- readxl::read_excel(fn_metnam_xlNames
                                            , sheet = "MetricMetadata"
                                            , skip = 4)
  df_metnam_xlNames <- as.data.frame(df_metnam_xlNames)

  # dups
  df_metric_count <- df_metnam_xlNames |>
    dplyr::count(Community,
                 METRIC_NAME,
                 name = "metric_count") |>
    dplyr::filter(metric_count > 1)

  ### test, dups, bugs ----
  n_dups_bugs <- df_metric_count |>
    dplyr::filter(Community == "bugs") |>
    nrow()
  testthat::expect_equal(n_dups_bugs, 0)

  ### test, dups, fish ----
  n_dups_fish <- df_metric_count |>
    dplyr::filter(Community == "fish") |>
    nrow()
  testthat::expect_equal(n_dups_fish, 0)

  ### test, dups, algae ----
  n_dups_algae <- df_metric_count |>
    dplyr::filter(Community == "algae") |>
    nrow()
  testthat::expect_equal(n_dups_algae, 0)

  ### test, dups, coral ----
  n_dups_coral <- df_metric_count |>
    dplyr::filter(Community == "coral") |>
    nrow()
  testthat::expect_equal(n_dups_coral, 0)

})## Test ~ metval, xlNames, dups
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
