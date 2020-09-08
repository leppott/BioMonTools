#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# metric.values, Names, bugs, Function, xlNames ####
test_that("metric.values, names, bugs, Function, xlNames", {
  # Packages
  #library(readxl) # part of BioMonTools

  # Data
  fn_metnam_xlNames   <- file.path(system.file(package="BioMonTools"), "extdata", "MetricNames.xlsx")
  # Import
  df_metnam_xlNames   <- readxl::read_excel(fn_metnam_xlNames, sheet="MetricMetadata", skip = 4)
  # Metric Names
  metnam_xlNames <- df_metnam_xlNames[df_metnam_xlNames[, "Community"]=="bugs", "METRIC_NAME", drop = TRUE]

  # Function
  df_metval <- suppressWarnings(suppressMessages(BioMonTools::metric.values(BioMonTools::data_benthos_PacNW, "bugs", boo.marine = TRUE, boo.Shiny = TRUE)))
  metnam_fun <- colnames(df_metval)[-c(1:3)] # remove first 3 columns

  # Check
  metnam_len <- length(metnam_fun)
  metnam_match <- sum(metnam_fun %in% metnam_xlNames)

  # Show non-matches
  metnam_fun[!(metnam_fun %in% metnam_xlNames)]

  # test
  testthat::expect_equal(metnam_len, metnam_match)

})## Test ~ metric names, bugs, Function, Names ~ END
# metric.values, Names, bugs, xlScoring, xlNames ####

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("metric.values, names, bugs, xlNames, xlScoring", {
  # Packages
  #library(readxl) # part of BioMonTools

  # Data
  fn_metnam_xlNames   <- file.path(system.file(package="BioMonTools"), "extdata", "MetricNames.xlsx")
  fn_metnam_xlScoring <- file.path(system.file(package="BioMonTools"), "extdata", "MetricScoring.xlsx")
  # Import
  df_metnam_xlNames   <- readxl::read_excel(fn_metnam_xlNames, sheet="MetricMetadata", skip = 4)
  df_metnam_xlScoring <- readxl::read_excel(fn_metnam_xlScoring, sheet="metric.scoring")

  # Names
  metnam_xlNames <- df_metnam_xlNames[df_metnam_xlNames[, "Community"]=="bugs", "METRIC_NAME", drop = TRUE]
  metnam_xlScoring <- unique(df_metnam_xlScoring[df_metnam_xlScoring[, "Community"]=="bugs", "METRIC_NAME", drop = TRUE])

  # Check
  metnam_xlScoring_match <- sum(metnam_xlScoring %in% metnam_xlNames)
  metnam_xlScoring_len <- length(metnam_xlScoring)

  # Show non-matches
  metnam_xlScoring[!(metnam_xlScoring %in% metnam_xlNames)]

  # test
  testthat::expect_equal(metnam_xlScoring_match, metnam_xlScoring_len)
})## Test - metric.values, names, Excel, Scoring ~ END

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# metric.values, Names, bugs, xlScoring, Function ####
test_that("metric.values, names, bugs, Function, Names", {
  # Packages
  #library(readxl) # part of BioMonTools

  # Data
  fn_metnam_xlScoring <- file.path(system.file(package="BioMonTools"), "extdata", "MetricScoring.xlsx")
  # Import
  df_metnam_xlScoring <- readxl::read_excel(fn_metnam_xlScoring, sheet="metric.scoring")
  # Metric Names

  metnam_xlScoring <- unique(df_metnam_xlScoring[df_metnam_xlScoring[, "Community"]=="bugs", "METRIC_NAME", drop = TRUE])

  # Function
  df_metval <- suppressWarnings(suppressMessages(BioMonTools::metric.values(BioMonTools::data_benthos_PacNW, "bugs", boo.marine = TRUE, boo.Shiny = TRUE)))
  metnam_fun <- colnames(df_metval)[-c(1:3)] # remove first 3 columns

  # Check
  metnam_len <- length(metnam_xlScoring)
  metnam_match <- sum(metnam_xlScoring %in% metnam_fun)

  # Show non-matches
  metnam_xlScoring[!(metnam_xlScoring %in% metnam_fun)]

  # test
  testthat::expect_equal(metnam_len, metnam_match)  # fails due to structure
})## Test ~ metric.values, names, bugs, Function, Names ~ END
