# Test for assign_indexclass

testthat::test_that("assign_indexclass, MBSS, normal", {

  # Data (example 1)
  data <- data.frame(SITEID = paste0("Site_", LETTERS[1:6])
                     , INDEX_NAME = "MBSS_2005_Bugs"
                     , ECO3 = c(63:67, 69))

  # doesn't work for single field with multiple choices
  name_indexclass <- "INDEX_CLASS"
  name_indexname <- names(data)[2]
  name_siteid <- names(data)[1]
  data_shape <- "WIDE"

  # Import criteria
  df_criteria <- readxl::read_excel(system.file("extdata/IndexClass.xlsx"
                                                , package = "BioMonTools")
                                    , sheet = "Index_Class")

  # Run Function
  df_results <- BioMonTools::assign_IndexClass(data,
                                               df_criteria,
                                               "INDEX_CLASS")

  # QC
  ic_qc <- c("COASTAL"
             , "EPIEDMONT"
             , "COASTAL"
             , "HIGHLAND"
             , "HIGHLAND"
             , "HIGHLAND")
  ic_calc <- df_results$INDEX_CLASS

  # Test
  testthat::expect_equal(ic_calc, ic_qc)

})## test 1

testthat::test_that("assign_indexclass, PacNW, normal", {

  # Data (example 2)
  data <- data.frame(SITEID = paste0("Site_", LETTERS[1:10])
                     , INDEX_NAME = "BCG_MariNW_Bugs500ct"
                     , GRADIENT = round(seq.int(0.5, 1.5, length.out = 10), 1)
                     , ELEVATION = round(seq.int(500, 800, length.out = 10), 1))

  name_indexclass <- "INDEX_CLASS"
  name_indexname <- names(data)[2]
  name_siteid <- names(data)[1]
  data_shape <- "WIDE"

  # Import criteria
  df_criteria <- readxl::read_excel(system.file("extdata/IndexClass.xlsx"
                                        , package = "BioMonTools")
                            , sheet = "Index_Class")

  # Run Function
  df_results <- BioMonTools::assign_IndexClass(data, df_criteria, name_indexclass)

  # QC
  ic_qc <- c("LoGrad-LoElev"
             , "LoGrad-LoElev"
             , "LoGrad-LoElev"
             , "LoGrad-LoElev"
             , "LoGrad-LoElev"
             , "HiGrad-LoElev"
             , "HiGrad-LoElev"
             , "HiGrad-LoElev"
             , "HiGrad-HiElev"
             , "HiGrad-HiElev")
  ic_calc <- df_results[, name_indexclass]

  # Test
  testthat::expect_equal(ic_calc, ic_qc)

})## test 2

testthat::test_that("assign_indexclass, PacNW, normal wIC", {

  # Data (example 2b) - include index class
  data <- data.frame(SITEID = paste0("Site_", LETTERS[1:10])
                     , INDEX_NAME = "BCG_MariNW_Bugs500ct"
                     , GRADIENT = round(seq.int(0.5, 1.5, length.out = 10), 1)
                     , ELEVATION = round(seq.int(500, 800, length.out = 10), 1)
                     , INDEX_CLASS = letters[1:10])
  name_indexclass <- names(data)[5]
  name_indexname <- names(data)[2]
  name_siteid <- names(data)[1]
  data_shape <- "WIDE"

  # Import criteria
  df_criteria <- readxl::read_excel(system.file("extdata/IndexClass.xlsx"
                                                , package = "BioMonTools")
                                    , sheet = "Index_Class")

  # Run Function
  df_results <- BioMonTools::assign_IndexClass(data
                                  , criteria = df_criteria
                                  , name_indexclass = name_indexclass)

  # QC
  ic_qc <- c("LoGrad-LoElev"
             , "LoGrad-LoElev"
             , "LoGrad-LoElev"
             , "LoGrad-LoElev"
             , "LoGrad-LoElev"
             , "HiGrad-LoElev"
             , "HiGrad-LoElev"
             , "HiGrad-LoElev"
             , "HiGrad-HiElev"
             , "HiGrad-HiElev")
  ic_calc <- df_results[, name_indexclass]

  # Test
  testthat::expect_equal(ic_calc, ic_qc)

})## test 2b

testthat::test_that("assign_indexclass, PacNW, NA", {

  # Data (example 3)
  badentry <- c(NA, "")
  data <- data.frame(stationcode = paste0("Site_", LETTERS[1:14])
                     , iname = "BCG_MariNW_Bugs500ct"
                     , GRADIENT = c(round(seq.int(0.5, 1.5, length.out = 10), 1)
                                    , badentry, 1, NA)
                     , ELEVATION = c(round(seq.int(500, 800, length.out = 10), 1)
                                     , badentry, NA, 700))

  name_indexclass <- "iclass"
  name_indexname <- names(data)[2]
  name_siteid <- names(data)[1]
  data_shape <- "WIDE"

  # Import criteria
  df_criteria <- readxl::read_excel(system.file("extdata/IndexClass.xlsx"
                                                , package = "BioMonTools")
                                    , sheet = "Index_Class")

  # Run Function
  df_results <- BioMonTools::assign_IndexClass(data
                                  , criteria = df_criteria
                                  , name_indexclass = name_indexclass
                                  , name_indexname = name_indexname
                                  , name_siteid = name_siteid)

  # QC
  ic_qc <- c("LoGrad-LoElev"
             , "LoGrad-LoElev"
             , "LoGrad-LoElev"
             , "LoGrad-LoElev"
             , "LoGrad-LoElev"
             , "HiGrad-LoElev"
             , "HiGrad-LoElev"
             , "HiGrad-LoElev"
             , "HiGrad-HiElev"
             , "HiGrad-HiElev"
             , rep(NA, 4))
  ic_calc <- df_results[, name_indexclass]

  # Test
  testthat::expect_equal(ic_calc, ic_qc)

})## test 3

testthat::test_that("assign_indexclass, PacNW, IC diff case", {

  # Data (example 4)
  data <- data.frame(INDEX_NAME = "BCG_MariNW_Bugs500ct"
                     , Index_Class = NA_character_
                     , StationID = c(374, 413, 2272, 2274, 2849, 2862, 2863)
                     , GRADIENT = c(2.1, 4.9, 0.9, 4.1, 0.8, 0.3, 0.4)
                     , ELEVATION = c(111.5, 113.7, 127.6, 104.3, 91.1, 47.4, 163.4)
  )

  name_indexclass <- names(data)[2]
  name_indexname <- names(data)[1]
  name_siteid <- names(data)[3]
  data_shape <- "WIDE"

  # Import criteria
  df_criteria <- readxl::read_excel(system.file("extdata/IndexClass.xlsx"
                                                , package = "BioMonTools")
                                    , sheet = "Index_Class")

  # Run Function
  df_results <- BioMonTools::assign_IndexClass(data
                                  , criteria = df_criteria
                                  , name_indexclass = name_indexclass
                                  , name_indexname = name_indexname
                                  , name_siteid = name_siteid)

  # QC
  ic_qc <- c("HiGrad-LoElev"
             , "HiGrad-LoElev"
             , "LoGrad-LoElev"
             , "HiGrad-LoElev"
             , "LoGrad-LoElev"
             , "LoGrad-LoElev"
             , "LoGrad-LoElev"
             )
  ic_calc <- df_results[, name_indexclass]

  # Test
  testthat::expect_equal(ic_calc, ic_qc)

})## test 4

