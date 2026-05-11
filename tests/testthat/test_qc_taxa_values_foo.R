# Test qc_taxa_values_*
# Erik.Leppo@tetratech.com
# 2026-04-05
#~~~~~~~~~~~~~~~~~~~~~~~~~~
# Test error conditions and expected outputs
#~~~~~~~~~~~~~~~~~~~~~~~~~~

# logical ----
testthat::test_that("qc_taxa_values, logical, errors", {
  # data
  data <- BioMonTools::data_benthos_MBSS

  # data, missing
  testthat::expect_error(qc_taxa_values_logical())

  # column, missing
  testthat::expect_error(qc_taxa_values_logical(data))

  # column, wrong case
  testthat::expect_error(qc_taxa_values_logical(data, "exclude"))

  # column, wrong class (numeric)
  testthat::expect_error(qc_taxa_values_logical(data, "TOLVAL"))

  # column, wrong class (character)
  testthat::expect_error(qc_taxa_values_logical(data, "FFG"))

  # no error
  testthat::expect_no_error(qc_taxa_values_logical(data, "EXCLUDE"))


})## logical ~ errors

# numeric ----

# character ----
