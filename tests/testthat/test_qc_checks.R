# qc_checks ####
test_that("qc_checks", {
  # # Calculate Metrics
  # df.samps.bugs <- readxl::read_excel(system.file("./extdata/Data_Benthos.xlsx"
  #                                         , package="BioMonTools")
  #                             , guess_max = 10^6)
  #
  # # Columns to keep
  # myCols <- c("Area_mi2", "SurfaceArea", "Density_m2", "Density_ft2")
  #
  # # Run Function
  # myDF <- df.samps.bugs
  # df.metric.values.bugs <- BioMonTools::metric.values(myDF, "bugs"
  #                                                     , fun.cols2keep=myCols)
  #
  # # Import Checks
  # df.checks <- readxl::read_excel(system.file("./extdata/MetricFlags.xlsx"
  #                                     , package="BioMonTools"), sheet="Flags")
  #
  # # Run Function
  # df.flags <- BioMonTools::qc.checks(df.metric.values.bugs, df.checks)
  #
  # # Summarize Results
  # a <- table(df.flags[,"CHECKNAME"], df.flags[,"FLAG"], useNA="ifany")
  # tib_calc <- tidyr::pivot_wider(as.data.frame(a)
  #                                , Var1
  #                                , names_from = Var2
  #                                , values_from = Freq)
  # names(tib_calc)[4] <- "NA"
  #
  #
  #
  #
  # # QC data
  # Var1 <- c("brackish organisms present", "catchment, Large"
  #           , "catchment, small", "individuals, dominant 02, Large"
  #           , "individuals, Large", "individuals, small", "Low density (ft2)"
  #           , "Low density (m2)", "Ramellogammarus", "surface area, small")
  # FAIL <- as.integer(c(7, 0, 158, 204, 0, 80, 0, 0, 8, 112))
  # PASS <- as.integer(c(671, 678, 520, 474, 678, 598, 0, 0, 670, 556))
  # `NA` <- as.integer(c(rep(0, 6), 678, 678, 0, 10))
  # tib_qc <- dplyr::as_tibble(data.frame(Var1, FAIL, PASS, `NA`
  #                                      , stringsAsFactors = TRUE))
  # names(tib_qc)[4] <- "NA"
  #
  #
  #
  # # test
  # testthat::expect_equal(tib_calc, tib_qc, tolerance = 0.01)
  #
  # # -- Warning (test_qc_checks.R:13:3): qc_checks ----------------------------------
  # #   `group_by_()` is deprecated as of dplyr 0.7.0.
  # # Please use `group_by()` instead.
  # # See vignette('programming') for more help
  # # This warning is displayed once every 8 hours.
  # # Call `lifecycle::last_warnings()` to see where this warning was generated.
  # # Backtrace:
  # #   1. BioMonTools::metric.values(myDF, "bugs", fun.cols2keep = myCols) test_qc_checks.R:13:2
  # # 5. dplyr::group_by_(., .dots = c("SAMPLEID", cols2keep))
  # # 6. dplyr:::lazy_deprec("group_by")

})## Test ~ qc_checks ~ END

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
