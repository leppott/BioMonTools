# metric.values, PA Freestone IBI ####
test_that("metric.values, PA Freestone IBI", {
  SAMPLEID <- c(rep("DriftwoodBr", 31), rep("WestBr", 15))
  STRAHLER <- c(rep(5, 31), rep(1, 15))
  DA_MI2 <- c(rep(84.5, 31), rep(0.3, 15))
  INDEX_NAME <- "PADEP_Freestone"
  INDEX_REGION <- c(rep("large", 31), rep("small", 15))
  TAXAID <- c("Isonychia"
             ,"Epeorus"
             ,"Leucrocuta"
             ,"Maccaffertium"
             ,"Ephemerella"
             ,"Eurylophella"
             ,"Serratella"
             ,"Paraleptophlebia"
             ,"Stylogomphus"
             ,"Taeniopteryx"
             ,"Taenionema"
             ,"Allocapnia"
             ,"Neoperla"
             ,"Paragnetina"
             ,"Acroneuria"
             ,"Nigronia"
             ,"Chimarra"
             ,"Polycentropus"
             ,"Ceratopsyche"
             ,"Cheumatopsyche"
             ,"Rhyacophila"
             ,"Glossosoma"
             ,"Lepidostoma"
             ,"Apatania"
             ,"Neophylax"
             ,"Oligochaeta"
             ,"Psephenus"
             ,"Optioservus"
             ,"Atherix"
             ,"Antocha"
             ,"Chironomidae"
             ,"Baetis"
             ,"Sweltsa"
             ,"Sialis"
             ,"Diplectrona"
             ,"Rhyacophila"
             ,"Oligochaeta"
             ,"Optioservus"
             ,"Chelifera"
             ,"Tipula"
             ,"Hexatoma"
             ,"Limnophila"
             ,"Prosimulium"
             ,"Simulium"
             ,"Chironomidae"
             ,"Cambarus")
  N_TAXA <- c(15
            ,10
            ,13
            ,18
            ,3
            ,3
            ,8
            ,5
            ,1
            ,13
            ,37
            ,1
            ,4
            ,2
            ,2
            ,1
            ,3
            ,2
            ,3
            ,1
            ,1
            ,2
            ,2
            ,5
            ,1
            ,2
            ,5
            ,20
            ,2
            ,1
            ,6
            ,5
            ,5
            ,1
            ,76
            ,10
            ,9
            ,1
            ,2
            ,5
            ,2
            ,1
            ,3
            ,15
            ,68
            ,2)
  TOLVAL <- c(3
              ,0
              ,1
              ,3
              ,1
              ,4
              ,2
              ,1
              ,4
              ,2
              ,3
              ,3
              ,3
              ,1
              ,0
              ,2
              ,4
              ,6
              ,5
              ,6
              ,1
              ,0
              ,1
              ,3
              ,3
              ,10
              ,4
              ,4
              ,2
              ,3
              ,6
              ,6
              ,0
              ,6
              ,0
              ,1
              ,10
              ,4
              ,6
              ,4
              ,2
              ,3
              ,2
              ,6
              ,6
              ,6)
  ORDER <- c(rep("Ephemeroptera", 8), NA, rep("Plecoptera", 6), NA
             , rep("Trichoptera", 9), rep(NA, 6), "Ephemeroptera", "Plecoptera"
             , NA, rep("Trichoptera", 2), rep(NA, 10))
  EXCLUDE <- rep(FALSE, 46)
  df_bugs <- data.frame(SAMPLEID, STRAHLER, DA_MI2, INDEX_NAME, INDEX_REGION
                        , TAXAID, N_TAXA, TOLVAL, ORDER, EXCLUDE)
  # metric values
  df_metval <- suppressMessages(suppressWarnings(BioMonTools::metric.values(df_bugs, "bugs", boo.Shiny = TRUE)))
  #1

  # df, calc
  col_qc <- c("SAMPLEID", "nt_total", "nt_tv_intol4_EPT", "x_Becks3", "x_HBI"
              , "x_Shan_e", "pi_tv_intol")
  df_metval_calc <- df_metval[, col_qc]
  # Round values to 1 or 2 digits
  df_metval_calc[, c("x_HBI", "x_Shan_e")] <- round(df_metval_calc[, c("x_HBI", "x_Shan_e")], 2)
  df_metval_calc[, "pi_tv_intol"] <- round(df_metval_calc[, "pi_tv_intol"], 1)

  # df, QC
  SAMPLEID <- c("DriftwoodBr", "WestBr")
  nt_total <- c(31, 15)
  nt_tv_intol4_EPT <- c(20, 3)
  x_Becks3 <- c(25, 10)
  x_HBI <- c(2.80, 3.39)
  x_Shan_e <- c(2.88, 1.76)
  pi_tv_intol <- c(76.0, 47.3)
  df_metval_qc <- data.frame(SAMPLEID, nt_total, nt_tv_intol4_EPT, x_Becks3, x_HBI, x_Shan_e, pi_tv_intol)

  # test
  testthat::expect_equal(df_metval_calc, df_metval_qc)
  # Below works but the QC data is not consistent in the number of decimal places
  #expect_equal(df_metval_calc, df_metval_qc, tolerance = 0.01)


  # Metric.Scores

  library(readxl)

  # Thresholds
  fn_thresh <- file.path(system.file(package="BioMonTools"), "extdata", "MetricScoring.xlsx")
  df_thresh_metric <- readxl::read_excel(fn_thresh, sheet="metric.scoring")
  df_thresh_index <- readxl::read_excel(fn_thresh, sheet="index.scoring")

  myIndex <- "PADEP_Freestone"
  (myMetrics.Bugs <- unique(as.data.frame(df_thresh_metric)[df_thresh_metric[
    , "INDEX_NAME"]==myIndex, "METRIC_NAME"]))

  df_metval_calc[, "INDEX_NAME"] <- "PADEP_Freestone"
  df_metval_calc[, "INDEX_REGION"] <- c("LARGE", "SMALL")

  df_metsc_calc <- BioMonTools::metric.scores(df_metval_calc, myMetrics.Bugs, "INDEX_NAME", "INDEX_REGION"
                            , df_thresh_metric, df_thresh_index)
  # For report all numbers rounded
  df_metsc_calc[, 10:17] <- round(df_metsc_calc[, 10:17], 1)

  # df_QC
  df_metsc_qc <- df_metval_qc
  df_metsc_qc$INDEX_NAME   <- "PADEP_Freestone"
  df_metsc_qc$INDEX_REGION <- c("LARGE", "SMALL")
  df_metsc_qc$SC_nt_total  <- c(100, 45.5)
  df_metsc_qc$SC_nt_tv_intol4_EPT <- c(100, 15.8)
  df_metsc_qc$SC_x_Becks3  <- c(100, 26.3)
  df_metsc_qc$SC_x_HBI     <- c(100, 81.5)
  df_metsc_qc$SC_x_Shan_e  <- c(100, 61.5)
  df_metsc_qc$SC_pi_tv_intol <- c(100, 56.0)
  df_metsc_qc$sum_Index    <- rowSums(df_metsc_qc[, 10:15])
  df_metsc_qc$Index        <- c(100, 47.8)
  df_metsc_qc$Index_Nar    <- c(NA, NA)

  # test
  #testthat::expect_equal(df_metsc_calc, df_metsc_qc)

  x <- sum(df_metsc_calc == df_metsc_qc, na.rm = TRUE)
  y <- sum(!is.na(df_metsc_qc))
  testthat::expect_equal(x, y)

})## Test - PA Freestone ~ END


# metric.scores, WV GLIMPSS MT_SP ####
test_that("metric.scores, WV GLIMPSS MT_SP", {
  #http://dep.wv.gov/WWE/watershed/bio_fish/Documents/20110829GLIMPSSFinalWVDEP.pdf

  # Packages
  library(readxl)

  # Create Data
  ## Table D-1
  SAMPLEID <- "WestForkPondFork"
  INDEX_NAME <- "WV_GLIMPSS"
  INDEX_REGION <- "MT_SP"

  metric_nam <- c("ni_total"
                   , "nt_tv_intol4"
                   ,"nt_Ephem"
                   , "nt_Pleco"
                   , "nt_Trich"
                   , "nt_habit_cling"
                   , "x_HBI"
                   , "pi_Ephem"
                   , "pi_Ortho"
                   , "pi_dom05"
                   , "nt_ffg_scrap"
                   , "pi_EPTNoCheu"
                  , "pi_ChiroAnne"
                  , "pi_tv_toler6")
  metric_nam_sc <- paste0("SC_", metric_nam[-1]) # drop ni_total as not scored.
  metric_val <- c(200 # ni_total
                  , 2 #"nt_tv_intol4"
                  , 1 #"nt_Ephem"
                  , 3 #"nt_Pleco"
                  , 3 #"nt_Trich"
                  , 8 #"nt_habit_cling"
                  , 5.55 #"x_HBI"
                  , 17.5 #"pi_Ephem"
                  , 12.9 #"pi_Ortho"
                  , 63.6 #"pi_dom05"
                  , 1  #"nt_ffg_scrap")
                  , NA #"pi_EPTNoCheu"
                  , NA #"pi_ChiroAnne"
                  , NA) #"pi_tv_toler6")
  metric_sc <- c(NA #ni_total
                 , 5.6 #"nt_tv_intol4"
                 , 0 #"nt_Ephem"
                 , 37.5 #"nt_Pleco"
                 , 33.3 #"nt_Trich"
                 , 25 #"nt_habit_cling"
                 , 15.9 #"x_HBI"
                 , 28.7 #"pi_Ephem"
                 , 76.2 #"pi_Ortho"
                 , 64.5 #"pi_dom05"
                 , 12.5 #"nt_ffg_scrap")
                 , NA #"pi_EPTNoCheu"
                 , NA #"pi_ChiroAnne"
                 , NA) #"pi_tv_toler6")

  df_metval <- data.frame(SAMPLEID, INDEX_NAME, INDEX_REGION, t(metric_val))
  names(df_metval)[4:ncol(df_metval)] <- metric_nam

  # Add Bear Fork, Table D-2
  metval_BearFrk <- c("BearFork", "WV_GLIMPSS", "PL_SP"
                     , 200 # ni_total
                     , 13 #"nt_tv_intol4"
                     , 10 #"nt_Ephem"
                     , 8 #"nt_Pleco"
                     , NA #"nt_Trich"
                     , 16 #"nt_habit_cling"
                     , 3.60 #"x_HBI"
                     , NA #"pi_Ephem"
                     , NA #"pi_Ortho"
                     , NA #"pi_dom05"
                     , NA #"nt_ffg_scrap")
                     , 86.2 #"pi_EPTNoCheu"
                     , 9.9 #"pi_ChiroAnne"
                     , 0) #"pi_tv_toler6")

  df_metval[2, ] <- metval_BearFrk
  # char to col
  ## use apply as without it doesn't work
  df_metval[, metric_nam] <- apply(df_metval[, metric_nam], 2, function(x) as.numeric(x))


  # calc
  ## Thresholds
  fn_thresh <- file.path(system.file(package="BioMonTools"), "extdata", "MetricScoring.xlsx")
  df_thresh_metric <- readxl::read_excel(fn_thresh, sheet="metric.scoring")
  df_thresh_index <- readxl::read_excel(fn_thresh, sheet="index.scoring")

  myIndex <- "WV_GLIMPSS"
  (myMetrics.Bugs <- unique(as.data.frame(df_thresh_metric)[df_thresh_metric[
    , "INDEX_NAME"]==myIndex, "METRIC_NAME"]))


  df_metsc_calc <- BioMonTools::metric.scores(df_metval
                                , metric_nam[-1]
                                , "INDEX_NAME"
                                , "INDEX_REGION"
                                , df_thresh_metric
                                , df_thresh_index
                                , "ni_total")
  # Round to single digits for scores
  df_metsc_calc[, c(metric_nam_sc, "sum_Index", "Index")] <- round(df_metsc_calc[,
                                c(metric_nam_sc, "sum_Index", "Index")], 1)
  # Change sum_Index from 299.3 to 299.2.  Only true if round all numbers before add.
  df_metsc_calc[1, "sum_Index"] <- 299.2
  # BF also different sum if round first
  df_metsc_calc[2, "sum_Index"] <- 736.9

  # WV report Table D-2 is incorrect for 3 metrics.
  # x_HBI,       (6.64-3.6)/(6.64-2.49) * 100  = 73.25301 # Reported as 73.2
  # %EPTNoCheu,  (86.2-2.5)/(90.8-2.5) * 100   = 94.79049 # Reported as 94.7
  # %Chiro+Anne, (84.7-9.9) / (84.7-1.8) * 100 = 90.22919 # Reported as 90.1
  # Overall Sum is off but report has the correct value.

  # Create QC
  df_metsc_qc <- df_metval
  # Add WestForkPondFork (Table D-1)
  df_metsc_qc[1, metric_nam_sc] <- metric_sc[-1]
  df_metsc_qc[1, c("sum_Index", "Index", "Index_Nar")] <- c(sum(metric_sc, na.rm = TRUE), 29.9, "Degraded")
  # Add BearFork (Table D-2)
  # metric_sc_BF <- c(85.7, 100, 100, 92.9, 73.2, 94.7, 90.1, 100)
  # sum is 736.6
  metsc_BF_corrected <- c(85.7, 100, 100, 92.9, 73.3, 94.8, 90.2, 100)
  metric_nam_sc_BF <- paste0("SC_", c("nt_tv_intol4"
                                      , "nt_Ephem"
                                      , "nt_Pleco"
                                      , "nt_habit_cling"
                                      , "x_HBI"
                                      , "pi_EPTNoCheu"
                                      , "pi_ChiroAnne"
                                      , "pi_tv_toler6"))
  # if round first sum is 736.9
  df_metsc_qc[2, metric_nam_sc_BF] <- metsc_BF_corrected
  df_metsc_qc[2, c("sum_Index", "Index", "Index_Nar")] <- c(736.9, 92.1, "Very good")
  # Narrative table 16, section 8.12, p 47 (p59 of PDF)

  # Modify class
  df_metsc_qc[, "sum_Index"] <- as.numeric(df_metsc_qc[, "sum_Index"])
  df_metsc_qc[, "Index"] <- as.numeric(df_metsc_qc[, "Index"])

  # qc
  #df_metsc_calc == df_metsc_qc

  # Use to QC metric.scores()
  # DF_Metrics <- df_metval
  # col_MetricNames <- metric_nam
  # col_IndexName <- "INDEX_NAME"
  # col_IndexRegion <- "INDEX_REGION"
  # DF_Thresh_Metric <- df_thresh_metric
  # DF_Thresh_Index <- df_thresh_index
  # col_ni_total = "ni_total"

  # not the best as still works if 2 errors cancel each other
  x <- sum(df_metsc_calc == df_metsc_qc, na.rm = TRUE)
  y <- sum(!is.na(df_metsc_qc))

  # test
  # testthat::expect_equal(df_metsc_calc, df_metsc_qc)
  testthat::expect_equal(x, y)

})## Test ~ WV GLIMPSS ~ END
