# Thresholds, Num Digits, Index ----
test_that("thresholds, num digits, index", {
  # Packages
  #library(readxl) # part of BioMonTools

  # Thresholds
  fn_thresh <- file.path(system.file(package = "BioMonTools")
                         , "extdata"
                         , "MetricScoring.xlsx")
  df_thresh_metric <- readxl::read_excel(fn_thresh, sheet = "metric.scoring")
  df_thresh_index <- readxl::read_excel(fn_thresh, sheet = "index.scoring")

  # Number of Characters (as character)
  index_thresh01 <- nchar(as.character(df_thresh_index$Thresh01))
  index_thresh02 <- nchar(as.character(df_thresh_index$Thresh02))
  index_thresh03 <- nchar(as.character(df_thresh_index$Thresh03))
  index_thresh04 <- nchar(as.character(df_thresh_index$Thresh04))
  index_thresh05 <- nchar(as.character(df_thresh_index$Thresh05))
  index_thresh06 <- nchar(as.character(df_thresh_index$Thresh06))
  index_thresh07 <- nchar(as.character(df_thresh_index$Thresh07))

  metric_thresh_lo <- nchar(as.character(df_thresh_metric$Thresh_Lo))
  metric_thresh_mid <- nchar(as.character(df_thresh_metric$Thresh_Mid))
  metric_thresh_hi <- nchar(as.character(df_thresh_metric$Thresh_Hi))

  # Number of "bad" entries
  # Max is 11 (MBSS)
  digmax <- 11
  # after that is most likely a floating point error that needs correction
  index_thresh01_nbad <- sum(index_thresh01 > digmax, na.rm = TRUE)
  index_thresh02_nbad <- sum(index_thresh02 > digmax, na.rm = TRUE)
  index_thresh03_nbad <- sum(index_thresh03 > digmax, na.rm = TRUE)
  index_thresh04_nbad <- sum(index_thresh04 > digmax, na.rm = TRUE)
  index_thresh05_nbad <- sum(index_thresh05 > digmax, na.rm = TRUE)
  index_thresh06_nbad <- sum(index_thresh06 > digmax, na.rm = TRUE)
  index_thresh07_nbad <- sum(index_thresh07 > digmax, na.rm = TRUE)

  metric_thresh_lo_nbad  <- sum(metric_thresh_lo > digmax, na.rm = TRUE)
  metric_thresh_mid_nbad <- sum(metric_thresh_mid > digmax, na.rm = TRUE)
  metric_thresh_hi_nbad  <- sum(metric_thresh_hi > digmax, na.rm = TRUE)

  # Find those rows in Excel with errors
  which(metric_thresh_lo  %in% metric_thresh_lo[metric_thresh_lo > digmax])
  which(metric_thresh_mid %in% metric_thresh_mid[metric_thresh_mid > digmax])
  which(metric_thresh_hi  %in% metric_thresh_hi[metric_thresh_hi > digmax])

  # test
  testthat::expect_true(index_thresh01_nbad == 0)
  testthat::expect_true(index_thresh02_nbad == 0)
  testthat::expect_true(index_thresh03_nbad == 0)
  testthat::expect_true(index_thresh04_nbad == 0)
  testthat::expect_true(index_thresh05_nbad == 0)
  testthat::expect_true(index_thresh06_nbad == 0)
  testthat::expect_true(index_thresh07_nbad == 0)

  testthat::expect_true(metric_thresh_lo_nbad == 0)
  testthat::expect_true(metric_thresh_mid_nbad == 0)
  testthat::expect_true(metric_thresh_hi_nbad == 0)

  # Easiest to View df and find visually
 # View(df_thresh_metric)

})## Test ~ thresholds, num digits ~ END

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
