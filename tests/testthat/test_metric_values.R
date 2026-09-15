# Bugs----
## metval, bugs, no_erorr, no_messages ----
testthat::test_that("metric.values, bugs, no_erorr, no_messages", {
  # Packages
  #library(readxl) # part of BioMonTools

  # Benthic data
  df_benthos <- BioMonTools::data_benthos_PacNW

  # Function
  # df_metval <- metric.values(df_benthos,
  #                                         "bugs",
  #                                         boo.marine = TRUE,
  #                                         boo.Shiny = TRUE,
  #                                         verbose = TRUE)

  # test
  testthat::expect_no_error(BioMonTools::metric.values(
    df_benthos,
    "bugs",
    boo.marine = TRUE,
    boo.Shiny = TRUE,
    verbose = TRUE))

  testthat::expect_no_message(BioMonTools::metric.values(
    df_benthos,
    "bugs",
    boo.marine = TRUE,
    boo.Shiny = TRUE,
    verbose = FALSE))
  # join by, step 14/18, calc, metrics

})## Test ~ metval, bugs, no_erorr, no_messages

# Fish ----
## metval, fish, vno_erorr, no_messages ----
testthat::test_that("metric.values, fish, no_erorr, no_messages", {
  # Packages
  #library(readxl) # part of BioMonTools

  # fish data
  df_fish <- BioMonTools::data_fish_MBSS
  # Munge (v1.0.2.9015, 2024-04-29)
  df_fish$TOLVAL2 <- NA_integer_
  df_fish$BCG_ATTR2 <- NA_character_
  # 2025-12-26
  df_fish$AGECLASS <- c(NA, 1:10)
  # 20260915
  # Add Exclude = TRUE to avoid warning
  # checking names only not values
  # ok if data not correct
  df_fish[1:3, "EXCLUDE"] <- TRUE

  # # Function
  # df_metval <- metric.values(df_fish,
  #                                         "fish",
  #                                         boo.Shiny = TRUE,
  #                                         verbose = TRUE)

  # test
  testthat::expect_no_error(BioMonTools::metric.values(
    df_fish,
    "fish",
    boo.Shiny = TRUE,
    verbose = TRUE))

  testthat::expect_no_message(BioMonTools::metric.values(
    df_fish,
    "fish",
    boo.Shiny = TRUE,
    verbose = FALSE))

})## Test ~ metval, fish, no_erorr, no_messages

# Algae----
## metval, algae, no_erorr, no_messages ----
testthat::test_that("metric.values, algae, no_erorr, no_messages", {
  # Packages
  #library(readxl) # part of BioMonTools

  # Algae data
  df_diatoms <- BioMonTools::data_diatom_mmi_dev[1:5000, ]

  # 20250908
  # Add Exclude = TRUE to avoid warning
  # checking names only not values
  # ok if data not correct
  df_diatoms[1, "EXCLUDE"] <- TRUE

  # Function
  # df_metval <- metric.values(df_diatoms,
  #                                         "algae",
  #                                         boo.Shiny = TRUE,
  #                                         verbose = TRUE)
  # # 5.09 seconds, trim from 24797

  # test
  testthat::expect_no_error(BioMonTools::metric.values(
    df_diatoms,
    "algae",
    boo.Shiny = TRUE,
    verbose = TRUE))

  testthat::expect_no_message(BioMonTools::metric.values(
    df_diatoms,
    "algae",
    boo.Shiny = TRUE,
    verbose = FALSE))

})## Test ~ metval, algae, no_erorr, no_messages

## metval, algae, POLL_TOL, all NA----
testthat::test_that("metric.values, algae, POLL_TOL, all NA", {

  # Create Data
  df <- BioMonTools::data_diatom_mmi_dev[1:100, ]
  # Add Exclude = TRUE to avoid warning
  # checking names only not values
  # ok if data not correct
  df[1, "EXCLUDE"] <- TRUE
  df_bad <- df
  df_bad[, "POLL_TOL"] <- NA_character_
  df_missing <- df
  df_missing[, "POLL_TOL"] <- NULL

  # QC
  ## good, error, no
  testthat::expect_no_error(BioMonTools::metric.values(df
                                                       , "algae"
                                                       , boo.Shiny = TRUE))
  ## bad, error, yes
  testthat::expect_error(BioMonTools::metric.values(df_bad
                                                    , "algae"
                                                    , boo.Shiny = TRUE))
  ## missing col, warning, yes
  testthat::expect_warning(BioMonTools::metric.values(df_missing
                                                       , "algae"
                                                       , boo.Shiny = TRUE))

})## Test ~ metval, algae, POLL_TOL, all NA

# Coral ----
## metval, coral, no_erorr, no_messages ----
testthat::test_that("metric.values, coral, no_erorr, no_messages", {
  # Packages
  #library(readxl) # part of BioMonTools

  # coral data
  df_corals <- BioMonTools::data_coral_bcg_metric_dev

  # # Function
  # df_metval <- metric.values(df_corals,
  #                                         "coral",
  #                                         boo.Shiny = TRUE,
  #                                         verbose = TRUE)

  # test
  testthat::expect_no_error(BioMonTools::metric.values(
    df_corals,
    "coral",
    boo.Shiny = TRUE,
    verbose = TRUE))

  testthat::expect_no_message(BioMonTools::metric.values(
    df_corals,
    "coral",
    boo.Shiny = TRUE,
    verbose = FALSE))

})## Test ~ metval, coral, no_erorr, no_messages
