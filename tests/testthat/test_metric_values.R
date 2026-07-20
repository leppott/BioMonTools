# Algae----
## metval, algae, POLL_TOL, all NA####
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
