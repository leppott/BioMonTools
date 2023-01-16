# taxa_translate ####
test_that("taxa_translate", {


  # from example


  # Example 1, PacNW
  ## Input Parameters
  df_user <- data_benthos_PacNW
  fn_official <- file.path(system.file("extdata", package = "BioMonTools")
                           , "taxa_official"
                           , "TAXA_TRANSLATOR_ORWA_MASTER_20221219.csv")
  df_official <- read.csv(fn_official)
  fn_official_metadata <- file.path(system.file("extdata"
                                                , package = "BioMonTools")
                                    , "taxa_official"
                                    , "TAXA_TRANSLATOR_ORWA_MASTER_METADATA_20221117.csv")
  df_official_metadata <- read.csv(fn_official_metadata)
  taxaid_user <- "TaxaID"
  taxaid_official_match <- "Taxon_orig"
  taxaid_official_project <- "OTU_BCG_MariNW"
  taxaid_drop <- NULL
  col_drop <- c("Taxon_v2", "OTU_MTTI") # non desired ID cols in Official
  sum_n_taxa_boo <- TRUE
  sum_n_taxa_col <- "N_TAXA"
  sum_n_taxa_group_by <- c("INDEX_NAME", "INDEX_CLASS", "SampleID", "TaxaID")
  ## Run Function
  taxatrans <- taxa_translate(df_user
                              , df_official
                              , df_official_metadata
                              , taxaid_user
                              , taxaid_official_match
                              , taxaid_official_project
                              , taxaid_drop
                              , col_drop
                              , sum_n_taxa_boo
                              , sum_n_taxa_col
                              , sum_n_taxa_group_by)

  # QC stuff

  TaxaID <- c("Eukiefferiella coerulescens/claripennis groups"
              , "Telmatodrilinae")
  N_Taxa_Sum <- c(4L, 1L)
  N_Taxa_Count <- c(1L, 1L)

  df_qc_nonmatch <- data.frame(TaxaID, N_Taxa_Sum, N_Taxa_Count)

  expect_identical(taxatrans$nonmatch, df_qc_nonmatch)


  # Check sum
  sum_qc <- sum(df_user$N_TAXA) - sum(df_qc_nonmatch$N_Taxa_Sum)
  sum_calc <- sum(taxatrans$merge$N_TAXA)
  expect_equal(sum_calc, sum_qc)



})## Test ~ taxa_translate

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("taxa_translate_multistage", {
  # taxa with multiple stage should be combined if the column is not kept

  # create example data ----
  TAXAID <- c(rep("Agapetus", 3), rep("Zavrelimyia", 2))
  N_TAXA <- c(rep(33, 3), rep(50, 2))
  STAGE <- c("A","L","P","X","")
  df_user <- data.frame(TAXAID, N_TAXA, STAGE)
  df_user[, "INDEX_NAME"] <- "BCG_MariNW_Bugs500ct"
  df_user[, "INDEX_CLASS"] <- "HiGrad-HiElev"
  df_user[, "SAMPLEID"] <- "Test2023"
  df_user[, "STATIONID"] <- "Test"
  df_user[, "DATE"] <- "2023-01-16"

  # Example 2, multistage
  ## Input Parameters
  fn_official <- file.path(system.file("extdata", package = "BioMonTools")
                           , "taxa_official"
                           , "ORWA_TAXATRANSLATOR_20221219.csv")
  df_official <- read.csv(fn_official)
  fn_official_metadata <- file.path(system.file("extdata"
                                                , package = "BioMonTools")
                                    , "taxa_official"
                                    , "ORWA_ATTRIBUTES_20221212.csv")
  df_official_metadata <- read.csv(fn_official_metadata)
  taxaid_user <- "TAXAID"
  taxaid_official_match <- "Taxon_orig"
  taxaid_official_project <- "OTU_BCG_MariNW"
  taxaid_drop <- NULL
  col_drop <- c("Taxon_v2", "OTU_MTTI") # non desired ID cols in Official
  sum_n_taxa_boo <- TRUE
  sum_n_taxa_col <- "N_TAXA"
  sum_n_taxa_group_by <- c("INDEX_NAME", "INDEX_CLASS", "SAMPLEID", "TAXAID")
  ## Run Function
  taxatrans <- BioMonTools::taxa_translate(df_user
                                          , df_official
                                          , df_official_metadata
                                          , taxaid_user
                                          , taxaid_official_match
                                          , taxaid_official_project
                                          , taxaid_drop
                                          , col_drop
                                          , sum_n_taxa_boo
                                          , sum_n_taxa_col
                                          , sum_n_taxa_group_by)

  # test
  ntaxa_calc <- taxatrans$merge$N_TAXA
  ntaxa_qc <- c(99, 100)
  testthat::expect_identical(ntaxa_calc, ntaxa_qc)

})## Test ~ taxa_translate_multistage
