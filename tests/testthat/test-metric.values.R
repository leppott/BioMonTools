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
  df_metval <- suppressWarnings(metric.values(df_bugs, "bugs"))
  1

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
  df_thresh_metric <- read_excel(fn_thresh, sheet="metric.scoring")
  df_thresh_index <- read_excel(fn_thresh, sheet="index.scoring")

  myIndex <- "PADEP_Freestone"
  (myMetrics.Bugs <- unique(as.data.frame(df_thresh_metric)[df_thresh_metric[
    , "INDEX_NAME"]==myIndex, "METRIC_NAME"]))

  df_metval_calc[, "INDEX_NAME"] <- "PADEP_Freestone"
  df_metval_calc[, "INDEX_REGION"] <- c("LARGE", "SMALL")

  df_metsc <- metric.scores(df_metval_calc, myMetrics.Bugs, "INDEX_NAME", "INDEX_REGION"
                            , df_thresh_metric, df_thresh_index)

  ### NEEDS MORE ###
  # Visual check of scores = ok


})
