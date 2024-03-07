# Bugs----

## Sum Components ----
# Sum of parts is equal to the whole
testthat::test_that("metrics, bugs, sum of parts", {
  # data
  df <- metric.values(data_benthos_PacNW, "bugs")

  ### Phylo ----
  #### nt----
  val_obj <- df$nt_EPT
  val_exp <- df$nt_Ephem + df$nt_Pleco + df$nt_Trich
  testthat::expect_equal(val_obj, val_exp)
  #### pi ----
  val_obj <- df$pi_EPT
  val_exp <- df$pi_Ephem + df$pi_Pleco + df$pi_Trich
  testthat::expect_equal(val_obj, val_exp)
  #### pt ----


  # EPT, COET, ET, ECT, OET, POET
  # SphaerCorb
  # Spion2Poly
  # LucinTellin
  # JugaFlumi
  # EphemNoCae
  # DiptNonIns
  # AmpIsop
  # AmpeHaust
  # AmphIsop
  # PolyNoSpion
  # CruMol

  # CHIRONOM
  # Orth2Chi
  # Tanyp2Chi

  # All percent, 0 - 100

  # Dom, one less than (or equal to) the next

  # Habit, CN + CB

  # Thermal, some "add" ones

  ### BCG ----
  #### nt----
  val_obj <- df$nt_BCG_att1i234b
  val_exp <- df$nt_BCG_att1i23 + df$nt_BCG_att4b
  testthat::expect_equal(val_obj, val_exp)

  val_obj <- df$nt_BCG_att4w5
  val_exp <- df$nt_BCG_att4w + df$nt_BCG_att5
  testthat::expect_equal(val_obj, val_exp)

  #### pi----
  val_obj <- df$pi_BCG_att1i234b
  val_exp <- df$pi_BCG_att1i23 + df$pi_BCG_att4b
  testthat::expect_equal(val_obj, val_exp)

  val_obj <- df$pi_BCG_att4w5
  val_exp <- df$pi_BCG_att4w + df$pi_BCG_att5
  testthat::expect_equal(val_obj, val_exp)

  #### pt----
  val_obj <- df$pt_BCG_att1i234b
  val_exp <- df$pt_BCG_att1i23 + df$pt_BCG_att4b
  testthat::expect_equal(val_obj, val_exp)

  val_obj <- df$pt_BCG_att4w5
  val_exp <- df$pt_BCG_att4w + df$pt_BCG_att5
  testthat::expect_equal(val_obj, val_exp)

})## test ~ sum of parts


## met val_sc, PA Freestone IBI ----
testthat::test_that("metric values_scores, PA Freestone IBI", {
  ### _Metric.Values ----
  SAMPLEID <- c(rep("DriftwoodBr", 31), rep("WestBr", 15))
  STRAHLER <- c(rep(5, 31), rep(1, 15))
  DA_MI2 <- c(rep(84.5, 31), rep(0.3, 15))
  INDEX_NAME <- "PADEP_Freestone"
  INDEX_CLASS <- c(rep("large", 31), rep("small", 15))
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
  df_bugs <- data.frame(SAMPLEID, STRAHLER, DA_MI2, INDEX_NAME, INDEX_CLASS
                        , TAXAID, N_TAXA, TOLVAL, ORDER, EXCLUDE)

  # Add extra columns
  col_extra <- c("NONTARGET"
                 , "PHYLUM"
                 , "SUBPHYLUM"
                 , "CLASS"
                 , "SUBCLASS"
                 , "INFRAORDER"
                 , "FAMILY"
                 , "SUBFAMILY"
                 , "TRIBE"
                 , "GENUS"
                 , "FFG"
                 , "HABIT"
                 , "LIFE_CYCLE"
                 , "BCG_ATTR"
                 , "THERMAL_INDICATOR"
                 , "LONGLIVED"
                 , "NOTEWORTHY"
                 , "FFG2"
                 , "TOLVAL2"
                 , "HABITAT"
                 , "UFC"
                 , "ELEVATION_ATTR"
                 , "GRADIENT_ATTR"
                 , "WSAREA_ATTR"
                 , "HABSTRUCT"
                 , "BCG_ATTR2"
                 , "AIRBREATHER")
  df_bugs[, col_extra] <- NA
  df_bugs[, "NONTARGET"] <- FALSE

  # ADD extra row for EXCLUDE = TRUE
  df_bugs[nrow(df_bugs) + 1, ] <- NA
  df_bugs[nrow(df_bugs), "SAMPLEID"] <- "Test_Remove"
  df_bugs[nrow(df_bugs), "INDEX_NAME"] <- "PADEP_Freestone"
  df_bugs[nrow(df_bugs), "INDEX_CLASS"] <- "large"
  df_bugs[nrow(df_bugs), "TAXAID"] <- "Test_Remove"
  df_bugs[nrow(df_bugs), "N_TAXA"] <- 999
  df_bugs[nrow(df_bugs), "EXCLUDE"] <- TRUE

  # extra column for a required field
  df_bugs[, "UFC"] <- NA_integer_

  # metric values
  df_metval <- BioMonTools::metric.values(df_bugs, "bugs", boo.Shiny = TRUE)
  # get warnings and test fail if don't put in dummy data to fool test

  # REMOVE extra row for EXCLUDE = TRUE
  df_metval <- df_metval[df_metval[, "SAMPLEID"] != "Test_Remove", ]
  # redo row numbers for expect_equal()
  rownames(df_metval) <- seq_len(nrow(df_metval))

  # df, calc
  col_qc <- c("SAMPLEID", "nt_total", "nt_tv_intol4_EPT", "x_Becks3", "x_HBI"
              , "x_Shan_e", "pi_tv_intol")
  df_metval_calc <- df_metval[, col_qc]
  # Round values to 1 or 2 digits
  df_metval_calc[, c("x_HBI", "x_Shan_e")] <- round(df_metval_calc[
    , c("x_HBI", "x_Shan_e")], 2)
  df_metval_calc[, "pi_tv_intol"] <- round(df_metval_calc[, "pi_tv_intol"], 1)

  # df, QC
  SAMPLEID <- c("DriftwoodBr", "WestBr")
  nt_total <- c(31, 15)
  nt_tv_intol4_EPT <- c(20, 3)
  x_Becks3 <- c(25, 10)
  x_HBI <- c(2.80, 3.39)
  x_Shan_e <- c(2.88, 1.76)
  pi_tv_intol <- c(76.0, 47.3)
  df_metval_qc <- data.frame(SAMPLEID, nt_total, nt_tv_intol4_EPT, x_Becks3
                             , x_HBI, x_Shan_e, pi_tv_intol)

  # test
  testthat::expect_equal(df_metval_calc, df_metval_qc)
  # Below works but
  #           the QC data is not consistent in the number of decimal places
  #expect_equal(df_metval_calc, df_metval_qc, tolerance = 0.01)


  ### _Metric.Scores ----

  # Thresholds
  fn_thresh <- file.path(system.file(package="BioMonTools")
                         , "extdata"
                         , "MetricScoring.xlsx")
  df_thresh_metric <- readxl::read_excel(fn_thresh, sheet="metric.scoring")
  df_thresh_index <- readxl::read_excel(fn_thresh, sheet="index.scoring")

  myIndex <- "PADEP_Freestone"
  (myMetrics.Bugs <- unique(as.data.frame(df_thresh_metric)[df_thresh_metric[
    , "INDEX_NAME"]==myIndex, "METRIC_NAME"]))

  df_metval_calc[, "INDEX_NAME"] <- "PADEP_Freestone"
  df_metval_calc[, "INDEX_CLASS"] <- c("LARGE", "SMALL")

  df_metsc_calc <- BioMonTools::metric.scores(df_metval_calc
                                              , myMetrics.Bugs
                                              , "INDEX_NAME"
                                              , "INDEX_CLASS"
                                              , df_thresh_metric
                                              , df_thresh_index)
  # For report all numbers rounded
  df_metsc_calc[, 10:17] <- round(df_metsc_calc[, 10:17], 1)

  # df_QC
  df_metsc_qc <- df_metval_qc
  df_metsc_qc$INDEX_NAME   <- "PADEP_Freestone"
  df_metsc_qc$INDEX_CLASS <- c("LARGE", "SMALL")
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


## met sc, WV GLIMPSS MT_SP ----
testthat::test_that("metric scores, WV GLIMPSS MT_SP", {
#http://dep.wv.gov/WWE/watershed/bio_fish/Documents/20110829GLIMPSSFinalWVDEP.pdf

  # Packages
  #library(readxl)

  # Create Data
  ## Table D-1
  SAMPLEID <- "WestForkPondFork"
  INDEX_NAME <- "WV_GLIMPSS"
  INDEX_CLASS <- "MT_SP"

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

  df_metval <- data.frame(SAMPLEID, INDEX_NAME, INDEX_CLASS, t(metric_val))
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
  df_metval[, metric_nam] <- apply(df_metval[, metric_nam]
                                   , 2
                                   , function(x) as.numeric(x))


  # calc
  ## Thresholds
  fn_thresh <- file.path(system.file(package="BioMonTools")
                         , "extdata"
                         , "MetricScoring.xlsx")
  df_thresh_metric <- readxl::read_excel(fn_thresh, sheet="metric.scoring")
  df_thresh_index <- readxl::read_excel(fn_thresh, sheet="index.scoring")

  myIndex <- "WV_GLIMPSS"
  (myMetrics.Bugs <- unique(as.data.frame(df_thresh_metric)[df_thresh_metric[
    , "INDEX_NAME"]==myIndex, "METRIC_NAME"]))


  df_metsc_calc <- BioMonTools::metric.scores(df_metval
                                , metric_nam[-1]
                                , "INDEX_NAME"
                                , "INDEX_CLASS"
                                , df_thresh_metric
                                , df_thresh_index
                                , "ni_total")
  # Round to single digits for scores
  df_metsc_calc[, c(metric_nam_sc, "sum_Index", "Index")] <- round(
    df_metsc_calc[, c(metric_nam_sc, "sum_Index", "Index")], 1)
  # Change sum_Index from 299.3 to 299.2.
  #      Only true if round all numbers before add.
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
  df_metsc_qc[1, c("sum_Index", "Index", "Index_Nar")] <- c(sum(metric_sc
                                                                , na.rm = TRUE)
                                                            , 29.9, "Degraded")
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
  df_metsc_qc[2, c("sum_Index", "Index", "Index_Nar")] <- c(736.9
                                                            , 92.1
                                                            , "Very good")
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
  # col_IndexRegion <- "INDEX_CLASS"
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


## met val_sc, MA kick_lograd IBI ----
testthat::test_that("metric values_scores, MA kick/lowgrad IBI", {
  ### _Metric.Values ----
  SAMPLEID <- c(rep("1985006", 16), rep("2011036", 19), rep("1985024", 21))
  INDEX_NAME <- "MassDEP_2020_Bugs"
  INDEX_CLASS <- c(rep("KickIBI_CH_100ct", 16)
                    , rep("LowGradientIBI", 19)
                    , rep("KickIBI_WH_100ct", 21))
  TAXAID <- c("Cricotopus"
              ,"Cricotopus bicinctus"
              ,"Cricotopus bicinctus group"
              ,"Cricotopus tremulus group"
              ,"Cricotopus trifasciatus group"
              ,"Cricotopus/Orthocladius"
              ,"Helobdella modesta"
              ,"Nanocladius"
              ,"Orthocladiinae"
              ,"Physa"
              ,"Polypedilum"
              ,"Polypedilum fallax group"
              ,"Polypedilum trigonum"
              ,"Thienemannimyia"
              ,"Thienemannimyia group"
              ,"Triaenodes"
              ,"Acerpenna"
              ,"Alotanypus"
              ,"Anchytarsus bicolor"
              ,"Dicranota"
              ,"Diplectrona modesta group"
              ,"Heterotrissocladius marcidus group"
              ,"Larsia"
              ,"Leuctra"
              ,"Lumbriculidae"
              ,"Micropsectra"
              ,"Nigronia serricornis"
              ,"Parachaetocladius"
              ,"Parametriocnemus"
              ,"Polypedilum"
              ,"Rheotanytarsus exiguus group"
              ,"Simulium"
              ,"Thienemannimyia group"
              ,"Tribelos"
              ,"Tubificoid Naididae"
              ,"Acariformes"
              ,"Baetidae"
              ,"Baetidae"
              ,"Baetis"
              ,"Cricotopus"
              ,"Cricotopus bicinctus"
              ,"Cricotopus tremulus group"
              ,"Cricotopus/Orthocladius"
              ,"Drunella cornutella"
              ,"Ephemerella dorothea"
              ,"Ephemerellidae"
              ,"Eukiefferiella"
              ,"Eukiefferiella gracei group"
              ,"Hemerodromia"
              ,"Hydropsyche"
              ,"Orthocladiinae"
              ,"Orthocladius"
              ,"Parametriocnemus"
              ,"Polypedilum flavum"
              ,"Rheocricotopus"
              ,"Simulium")
  N_TAXA <- c(5
              ,11
              ,1
              ,35
              ,10
              ,16
              ,1
              ,1
              ,1
              ,1
              ,1
              ,8
              ,1
              ,1
              ,6
              ,1
              ,1
              ,1
              ,2
              ,1
              ,8
              ,3
              ,2
              ,3
              ,1
              ,7
              ,1
              ,1
              ,10
              ,1
              ,1
              ,59
              ,4
              ,1
              ,1
              ,1
              ,5
              ,3
              ,22
              ,1
              ,3
              ,7
              ,28
              ,2
              ,2
              ,5
              ,3
              ,1
              ,2
              ,4
              ,1
              ,3
              ,1
              ,4
              ,1
              ,1)
  TOLVAL <- c(7
              ,7
              ,7
              ,7
              ,7
              ,7
              ,8
              ,4
              ,5
              ,9
              ,6
              ,6
              ,6
              ,6
              ,6
              ,6
              ,5
              ,NA
              ,2
              ,5
              ,2
              ,5
              ,5
              ,2
              ,5
              ,5
              ,5
              ,2
              ,5
              ,5
              ,5
              ,5
              ,NA
              ,5
              ,8
              ,6
              ,5
              ,5
              ,6
              ,7
              ,7
              ,7
              ,7
              ,1
              ,1
              ,1
              ,6
              ,4
              ,6
              ,4
              ,5
              ,6
              ,5
              ,6
              ,6
              ,5)
  FFG <- c("SH"
           ,"CG"
           ,"CG"
           ,"SH"
           ,"SH"
           ,"CG"
           ,"PR"
           ,"CG"
           ,"CG"
           ,"CG"
           ,"SH"
           ,"SH"
           ,"SH"
           ,"PR"
           ,"PR"
           ,"SH"
           ,"CG"
           ,"PR"
           ,"SH"
           ,"PR"
           ,"CF"
           ,"CG"
           ,"PR"
           ,"SH"
           ,"CG"
           ,"CG"
           ,"PR"
           ,"CG"
           ,"CG"
           ,"SH"
           ,"CF"
           ,"CF"
           ,"PR"
           ,"CG"
           ,'CG'
           ,"PR"
           ,"CG"
           ,"CG"
           ,"CG"
           ,"SH"
           ,"CG"
           ,"SH"
           ,"CG"
           ,"SC"
           ,"CG"
           ,"CG"
           ,"CG"
           ,"CG"
           ,"PR"
           ,"CF"
           ,"CG"
           ,"CG"
           ,"CG"
           ,"SH"
           ,"CG"
           ,"CF")
  LIFE_CYCLE <- c(NA
                  ,NA
                  ,NA
                  ,NA
                  ,NA
                  ,NA
                  ,"multi"
                  ,NA
                  ,NA
                  ,NA
                  ,"multi"
                  ,"multi"
                  ,"multi"
                  ,NA
                  ,NA
                  ,"uni"
                  ,"multi"
                  ,NA
                  ,"semi"
                  ,"uni"
                  ,"uni"
                  ,NA
                  ,NA
                  ,"uni"
                  ,NA
                  ,"multi"
                  ,"semi"
                  ,NA
                  ,"multi"
                  ,"multi"
                  ,"multi"
                  ,"multi"
                  ,NA
                  ,NA
                  ,NA
                  ,NA
                  ,NA
                  ,NA
                  ,"multi"
                  ,NA
                  ,NA
                  ,NA
                  ,NA
                  ,"uni"
                  ,"uni"
                  ,NA
                  ,"multi"
                  ,"multi"
                  ,NA
                  ,"uni"
                  ,NA
                  ,NA
                  ,"multi"
                  ,"multi"
                  ,NA
                  ,"multi")
  EXCLUDE <- c(TRUE
               ,FALSE
               ,TRUE
               ,FALSE
               ,FALSE
               ,TRUE
               ,FALSE
               ,FALSE
               ,TRUE
               ,FALSE
               ,FALSE
               ,FALSE
               ,FALSE
               ,FALSE
               ,FALSE
               ,FALSE
               ,FALSE
               ,FALSE
               ,FALSE
               ,FALSE
               ,FALSE
               ,FALSE
               ,FALSE
               ,FALSE
               ,FALSE
               ,FALSE
               ,FALSE
               ,FALSE
               ,FALSE
               ,FALSE
               ,FALSE
               ,FALSE
               ,FALSE
               ,FALSE
               ,FALSE
               ,FALSE
               ,TRUE
               ,FALSE
               ,FALSE
               ,TRUE
               ,FALSE
               ,FALSE
               ,TRUE
               ,FALSE
               ,FALSE
               ,TRUE
               ,TRUE
               ,FALSE
               ,FALSE
               ,FALSE
               ,TRUE
               ,FALSE
               ,FALSE
               ,FALSE
               ,FALSE
               ,FALSE)
  NONTARGET <- rep(FALSE, 56)
  PHYLUM <- c("Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Annelida"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Mollusca"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Annelida"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Annelida"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda"
              ,"Arthropoda")
  CLASS <- c("Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Hirudinea"
             ,"Insecta"
             ,"Insecta"
             ,"Gastropoda"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Clitellata"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Clitellata"
             ,"Arachnida"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,'Insecta'
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta"
             ,"Insecta")
  ORDER <- c("Diptera"
             ,"Diptera"
             ,"Diptera"
             ,"Diptera"
             ,"Diptera"
             ,"Diptera"
             ,"Rhynchobdellida"
             ,"Diptera"
             ,"Diptera"
             ,"Basommatophora"
             ,"Diptera"
             ,"Diptera"
             ,"Diptera"
             ,"Diptera"
             ,"Diptera"
             ,"Trichoptera"
             ,"Ephemeroptera"
             ,"Diptera"
             ,"Coleoptera"
             ,"Diptera"
             ,"Trichoptera"
             ,"Diptera"
             ,"Diptera"
             ,"Plecoptera"
             ,"Lumbriculida"
             ,"Diptera"
             ,"Megaloptera"
             ,"Diptera"
             ,"Diptera"
             ,"Diptera"
             ,"Diptera"
             ,"Diptera"
             ,"Diptera"
             ,"Diptera"
             ,"Tubificida"
             ,NA
             ,"Ephemeroptera"
             ,"Ephemeroptera"
             ,"Ephemeroptera"
             ,"Diptera"
             ,"Diptera"
             ,"Diptera"
             ,"Diptera"
             ,"Ephemeroptera"
             ,"Ephemeroptera"
             ,"Ephemeroptera"
             ,"Diptera"
             ,"Diptera"
             ,"Diptera"
             ,"Trichoptera"
             ,"Diptera"
             ,"Diptera"
             ,"Diptera"
             ,"Diptera"
             ,"Diptera"
             ,"Diptera")

  FAMILY <- c("Chironomidae"
              ,'Chironomidae'
              ,"Chironomidae"
              ,"Chironomidae"
              ,"Chironomidae"
              ,"Chironomidae"
              ,"Glossiphoniidae"
              ,"Chironomidae"
              ,"Chironomidae"
              ,"Physidae"
              ,"Chironomidae"
              ,"Chironomidae"
              ,"Chironomidae"
              ,"Chironomidae"
              ,"Chironomidae"
              ,"Leptoceridae"
              ,"Baetidae"
              ,"Chironomidae"
              ,"Ptilodactylidae"
              ,"Tipulidae"
              ,"Hydropsychidae"
              ,"Chironomidae"
              ,"Chironomidae"
              ,"Leuctridae"
              ,"Lumbriculidae"
              ,"Chironomidae"
              ,"Corydalidae"
              ,"Chironomidae"
              ,"Chironomidae"
              ,"Chironomidae"
              ,"Chironomidae"
              ,"Simuliidae"
              ,"Chironomidae"
              ,"Chironomidae"
              ,"Naididae"
              ,NA
              ,"Baetidae"
              ,"Baetidae"
              ,"Baetidae"
              ,"Chironomidae"
              ,"Chironomidae"
              ,"Chironomidae"
              ,"Chironomidae"
              ,"Ephemerellidae"
              ,"Ephemerellidae"
              ,"Ephemerellidae"
              ,"Chironomidae"
              ,"Chironomidae"
              ,"Empididae"
              ,"Hydropsychidae"
              ,"Chironomidae"
              ,"Chironomidae"
              ,"Chironomidae"
              ,"Chironomidae"
              ,"Chironomidae"
              ,"Simuliidae")

  SUBFAMILY <- c("Orthocladiinae"
                 ,"Orthocladiinae"
                 ,"Orthocladiinae"
                 ,"Orthocladiinae"
                 ,"Orthocladiinae"
                 ,"Orthocladiinae"
                 ,NA
                 ,"Orthocladiinae"
                 ,"Orthocladiinae"
                 ,NA
                 ,"Chironominae"
                 ,"Chironominae"
                 ,"Chironominae"
                 ,"Tanypodinae"
                 ,"Tanypodinae"
                 ,NA
                 ,NA
                 ,"Tanypodinae"
                 ,NA
                 ,NA
                 ,NA
                 ,"Orthocladiinae"
                 ,"Tanypodinae"
                 ,NA
                 ,NA
                 ,"Chironominae"
                 ,NA
                 ,"Orthocladiinae"
                 ,"Orthocladiinae"
                 ,"Chironominae"
                 ,"Chironominae"
                 ,NA
                 ,"Tanypodinae"
                 ,"Chironominae"
                 ,NA
                 ,NA
                 ,NA
                 ,NA
                 ,NA
                 ,"Orthocladiinae"
                 ,"Orthocladiinae"
                 ,"Orthocladiinae"
                 ,"Orthocladiinae"
                 ,NA
                 ,NA
                 ,NA
                 ,"Orthocladiinae"
                 ,"Orthocladiinae"
                 ,NA
                 ,NA
                 ,"Orthocladiinae"
                 ,"Orthocladiinae"
                 ,"Orthocladiinae"
                 ,"Chironominae"
                 ,"Orthocladiinae"
                 ,NA)

  TRIBE <- c(NA
             ,NA
             ,NA
             ,NA
             ,NA
             ,NA
             ,NA
             ,NA
             ,NA
             ,NA
             ,"Chironomini"
             ,'Chironomini'
             ,"Chironomini"
             ,NA
             ,NA
             ,NA
             ,NA
             ,"Coelotanypodini"
             ,NA
             ,NA
             ,NA
             ,NA
             ,"Pentaneurini"
             ,NA
             ,NA
             ,"Tanytarsini"
             ,NA
             ,NA
             ,NA
             ,"Chironomini"
             ,"Tanytarsini"
             ,NA
             ,NA
             ,"Chironomini"
             ,NA
             ,NA
             ,NA
             ,NA
             ,NA
             ,NA
             ,NA
             ,NA
             ,NA
             ,NA
             ,NA
             ,NA
             ,NA
             ,NA
             ,NA
             ,NA
             ,NA
             ,NA
             ,NA
             ,"Chironomini"
             ,NA
             ,NA)

  GENUS <- c("Cricotopus"
             ,"Cricotopus"
             ,"Cricotopus"
             ,"Cricotopus"
             ,"Cricotopus"
             ,NA
             ,"Helobdella"
             ,"Nanocladius"
             ,NA
             ,"Physa"
             ,"Polypedilum"
             ,"Polypedilum"
             ,"Polypedilum"
             ,"Thienemannimyia"
             ,"Thienemannimyia"
             ,"Triaenodes"
             ,"Acerpenna"
             ,"Alotanypus"
             ,"Anchytarsus"
             ,"Dicranota"
             ,"Diplectrona"
             ,"Heterotrissocladius"
             ,"Larsia"
             ,"Leuctra"
             ,NA
             ,"Micropsectra"
             ,"Nigronia"
             ,"Parachaetocladius"
             ,"Parametriocnemus"
             ,"Polypedilum"
             ,"Rheotanytarsus"
             ,"Simulium"
             ,"Thienemannimyia"
             ,"Tribelos"
             ,NA
             ,NA
             ,NA
             ,NA
             ,"Baetis"
             ,"Cricotopus"
             ,"Cricotopus"
             ,"Cricotopus"
             ,NA
             ,"Drunella"
             ,"Ephemerella"
             ,NA
             ,"Eukiefferiella"
             ,"Eukiefferiella"
             ,"Hemerodromia"
             ,"Hydropsyche"
             ,NA
             ,"Orthocladius"
             ,"Parametriocnemus"
             ,"Polypedilum"
             ,"Rheocricotopus"
             ,"Simulium")

  df_bugs <- data.frame(INDEX_NAME
                        , INDEX_CLASS
                        , SAMPLEID
                        , TAXAID
                        , N_TAXA
                        , EXCLUDE
                        , NONTARGET
                        , PHYLUM
                        , CLASS
                        , ORDER
                        , FAMILY
                        , SUBFAMILY
                        , TRIBE
                        , GENUS
                        , TOLVAL
                        , FFG
                        , LIFE_CYCLE
                        , SUBPHYLUM = NA_character_
                        , SUBCLASS = NA_character_
                        , INFRAORDER = NA_character_
                        , HABIT = NA_character_
                        , BCG_ATTR = NA_integer_
                        , THERMAL_INDICATOR = NA_integer_
                        , LONGLIVED = NA_character_
                        , NOTEWORTHY = NA
                        , FFG2 = NA_character_
                        , TOLVAL2 = NA_integer_
                        , HABITAT = NA_character_
                        , UFC = NA_integer_
                        , ELEVATION_ATTR = NA_character_
                        , GRADIENT_ATTR = NA_character_
                        , WSAREA_ATTR = NA_character_
                        , HABSTRUCT = NA_character_
                        , BCG_ATTR2 = NA_character_
                        , AIRBREATHER = NA)
  # metric values
  df_metval <- BioMonTools::metric.values(df_bugs, "bugs", boo.Shiny = TRUE)
  #1

  # df, calc
  col_qc <- c("SAMPLEID"
              ,"nt_total"
              ,"pt_EPT"
              ,"pi_EphemNoCaeBae"
              ,"pi_ffg_filt"
              ,"pt_ffg_pred"
              ,"pt_tv_intol"
              ,"pi_Pleco"
              ,"pi_ffg_shred"
              ,"pi_tv_intol"
              ,"x_Becks"
              ,"pi_OET"
              ,"pt_NonIns"
              ,"pt_POET"
              ,"pt_tv_toler"
              ,"pt_volt_semi")

  df_metval_calc <- df_metval[, col_qc]
  # Round values to 1 or 2 digits
  df_metval_calc[, c("pt_EPT", "pi_EphemNoCaeBae", "pi_ffg_filt",	"pt_ffg_pred"
                     ,	"pt_tv_intol", "pi_Pleco","pi_ffg_shred",	"pi_tv_intol"
                     ,	"pi_OET",	"pt_NonIns",	"pt_POET",	"pt_tv_toler"
                     , "pt_volt_semi")] <- round(df_metval_calc[, c("pt_EPT"
                      , "pi_EphemNoCaeBae", "pi_ffg_filt",	"pt_ffg_pred"
                      ,	"pt_tv_intol", "pi_Pleco","pi_ffg_shred",	"pi_tv_intol"
                      ,	"pi_OET",	"pt_NonIns",	"pt_POET",	"pt_tv_toler"
                      , "pt_volt_semi")], 2)

  # df, QC
  SAMPLEID <- c("1985006"
                ,"1985024"
                ,"2011036")
  nt_total <- c(12
                ,15
                ,19)
  pt_EPT <- c(8.33
              ,33.33
              ,15.79)
  pi_EphemNoCaeBae <- c(0
                        ,9
                        ,0)
  pi_ffg_filt <- c(0
                   ,5
                   ,62.96)
  pt_ffg_pred <- c(25
                   ,13.33
                   ,26.32)
  pt_tv_intol <- c(0
                   ,13.33
                   ,21.05)
  pi_Pleco <- c(0
                ,0
                ,2.78)
  pi_ffg_shred <- c(61
                    ,12
                    ,5.56)
  pi_tv_intol <- c(0
                   ,9
                   ,12.96)
  x_Becks <- c(1
               ,6
               ,4)
  pi_OET <- c(1
              ,43
              ,8.33)
  pt_NonIns <- c(16.67
                 ,6.67
                 ,10.53)
  pt_POET <- c(8.33
               ,33.33
               ,15.79)
  pt_tv_toler <- c(41.67
                   ,13.33
                   ,5.26)
  pt_volt_semi <- c(0
                    ,0
                    ,10.53)


  df_metval_qc <- data.frame(SAMPLEID
                             ,nt_total
                             ,pt_EPT
                             ,pi_EphemNoCaeBae
                             ,pi_ffg_filt
                             ,pt_ffg_pred
                             ,pt_tv_intol
                             ,pi_Pleco
                             ,pi_ffg_shred
                             ,pi_tv_intol
                             ,x_Becks
                             ,pi_OET
                             ,pt_NonIns
                             ,pt_POET
                             ,pt_tv_toler
                             ,pt_volt_semi)

  # test
  testthat::expect_equal(df_metval_calc, df_metval_qc)
  # Below works but
  #               the QC data is not consistent in the number of decimal places
  #expect_equal(df_metval_calc, df_metval_qc, tolerance = 0.01)


  ### _Metric.Scores ----

  # Thresholds
  fn_thresh <- file.path(system.file(package="BioMonTools")
                         , "extdata"
                         , "MetricScoring.xlsx")
  df_thresh_metric <- readxl::read_excel(fn_thresh, sheet="metric.scoring")
  df_thresh_index <- readxl::read_excel(fn_thresh, sheet="index.scoring")

  myIndex <- "MassDEP_2020_Bugs"
  (myMetrics.Bugs <- unique(as.data.frame(df_thresh_metric)[df_thresh_metric[
    , "INDEX_NAME"]==myIndex, "METRIC_NAME"]))


  df_metval_calc[, "INDEX_NAME"] <- "MassDEP_2020_Bugs"
  df_metval_calc[, "INDEX_CLASS"] <- c("KickIBI_CH_100ct"
                                        , "KickIBI_WH_100ct"
                                        , "LowGradientIBI")

  df_metsc_calc <- BioMonTools::metric.scores(df_metval_calc
                                              , myMetrics.Bugs
                                              , "INDEX_NAME"
                                              , "INDEX_CLASS"
                                              , df_thresh_metric
                                              , df_thresh_index)
  # For report all numbers rounded
  df_metsc_calc[, 19:35] <- round(df_metsc_calc[, 19:35], 1)

  # df_QC
  df_metsc_qc <- df_metval_qc
  df_metsc_qc$INDEX_NAME   <- "MassDEP_2020_Bugs"
  df_metsc_qc$INDEX_CLASS <- c("KICKIBI_CH_100CT"
                                , "KICKIBI_WH_100CT"
                                , "LOWGRADIENTIBI")
  df_metsc_qc$SC_nt_total <- c(34.4
                               ,38.7
                               ,NA)
  df_metsc_qc$SC_pi_EphemNoCaeBae <- c(0
                                       ,NA
                                       ,NA)
  df_metsc_qc$SC_pi_ffg_filt <- c(100
                                  ,100
                                  ,NA)
  df_metsc_qc$SC_pi_ffg_shred <- c(NA
                                   ,52.2
                                   ,NA)
  df_metsc_qc$SC_pi_OET <- c(NA
                             ,NA
                             ,17)
  df_metsc_qc$SC_pi_Pleco <- c(NA
                               ,0
                               ,NA)
  df_metsc_qc$SC_pi_tv_intol <- c(NA
                                  ,17.5
                                  ,NA)
  df_metsc_qc$SC_pt_EPT <- c(15.3
                             ,NA
                             ,NA)
  df_metsc_qc$SC_pt_ffg_pred <- c(87.7
                                  ,NA
                                  ,82.2)
  df_metsc_qc$SC_pt_NonIns <- c(NA
                                ,NA
                                ,84.5)
  df_metsc_qc$SC_pt_POET <- c(NA
                              ,NA
                              ,39.5)
  df_metsc_qc$SC_pt_tv_intol <- c(0
                                  ,NA
                                  ,NA)
  df_metsc_qc$SC_pt_tv_toler <- c(NA
                                  ,NA
                                  ,93.2)
  df_metsc_qc$SC_pt_volt_semi <- c(NA
                                   ,NA
                                   ,87.8)
  df_metsc_qc$SC_x_Becks <- c(NA
                              ,16.3
                              ,NA)
  df_metsc_qc$sum_Index <- c(237.4
                             ,224.6
                             ,404.1)
  df_metsc_qc$Index <- c(39.6
                         ,37.4
                         ,67.3)
  df_metsc_qc$Index_Nar <- c("Moderately Degraded"
                             ,"Moderately Degraded"
                             ,"Satisfactory")

  # test
  #testthat::expect_equal(df_metsc_calc, df_metsc_qc)

  col_order <- names(df_metsc_calc)

  # `%>%` <- dplyr::`%>%`
  # df_metsc_qc<- df_metsc_qc %>%
  #   dplyr::select(dplyr::all_of(col_order))

  df_metsc_qc <- df_metsc_qc[, col_order]

  x <- sum(df_metsc_calc == df_metsc_qc, na.rm = TRUE)
  y <- sum(!is.na(df_metsc_qc))
  testthat::expect_equal(x, y)

})## Test - met val_sc, MA kick/lograd IBI ~ END


# Check individual metrics
testthat::test_that("metrics, bugs, nt_COETNoBraBaeHydTri", {
  # Example 1
  df_metric_values_bugs <- BioMonTools::metric.values(BioMonTools::data_benthos_PacNW, "bugs")
  # Filter for one metric
  df_val <- df_metric_values_bugs[order(df_metric_values_bugs$SAMPLEID)
                                  , c("SAMPLEID", "nt_COETNoBraBaeHydTri")]
  df_calc <- data.frame(SAMPLEID = sort(df_val$SAMPLEID)
                        , nt_COETNoBraBaeHydTri = c(13
                                                    , 5
                                                    , 21
                                                    , 17
                                                    , 22
                                                    , 4
                                                    , 20
                                                    , 10
                                                    , 20
                                                    , 17
                                                    , 3
                                                    , 11
                        ))
  # Test
  testthat::expect_equal(df_val[, 2], df_calc[, 2])


})## Test - met val, WY, nt_COETNoBraBaeHydTri


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fish ----
# MBSS or GA DNR

# Sum components
# some phylo
# BCG

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Algae----

# Sum components
# BC 1 + 2
# Salinity 1 + 2 and 3 + 4
# Trophic 12, 456, 56
# O 345
# PT 12



## met val_sc, IDEM Diatom IBIs ----
testthat::test_that("metric values_scores, MA kick/lowgrad IBI", {
  ### _Metric.Values ----

  data(data_diatom_mmi_dev) #added via data.R
  df_diatoms <- data_diatom_mmi_dev

  # metric values
  df_metval_calc <- BioMonTools::metric.values(fun.DF = df_diatoms
                                          , fun.Community = "algae"
                                          , boo.Shiny = TRUE)

  # df, calc
  data(data_diatom_mmi_qc)

  df_metval_qc <- data_diatom_mmi_qc

  # change integers to numeric
  # Round values to 1 or 2 digits
  #library(dplyr)

  `%>%` <- dplyr::`%>%`

  df_metval_calc <- df_metval_calc %>%
    dplyr::mutate_if(is.integer, as.numeric) %>%
    dplyr::mutate_if(is.numeric, round, digits =2)

  df_metval_qc <- df_metval_qc %>%
    dplyr::mutate_if(is.integer, as.numeric) %>%
    dplyr::mutate_if(is.numeric, round, digits =2)


  # test
  testthat::expect_equal(df_metval_calc, df_metval_qc)
  # Below works but
  #               the QC data is not consistent in the number of decimal places
  #expect_equal(df_metval_calc, df_metval_qc, tolerance = 0.01)


})## Test - met val_sc, IDEM Diatom IBIs ~ END


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## met val_sc, SCMB_IBI ----
# testthat::test_that("metric values_scores, SCMB IBI", {
#   ### _Metric.Values ----
#
#   data(data_metval_scmb_ibi) #added via data.R
#   df_metval <- data_metval_scmb_ibi
#
#   # metric values
#   # df_metval_calc <- BioMonTools::metric.values(fun.DF = df_diatoms
#   #                                              , fun.Community = "algae"
#   #                                              , boo.Shiny = TRUE)
#   #
#   # # df, calc
#   # data(data_diatom_mmi_qc)
#
#   # Thresholds
#   fn_thresh <- file.path(system.file(package="BioMonTools"), "extdata"
#                          , "MetricScoring.xlsx")
#   df_thresh_metric <- readxl::read_excel(fn_thresh, sheet="metric.scoring")
#   df_thresh_index <- readxl::read_excel(fn_thresh, sheet="index.scoring")
#   myIndex <- "SCMB_IBI"
#   myMetrics <- unique(as.data.frame(df_thresh_metric)[df_thresh_metric[
#     , "INDEX_NAME"]==myIndex, "METRIC_NAME"])
#
#   # Calculate
#   df_metsc_calc <- metric.scores(df_metval[, 1:7]
#                                  , myMetrics
#                                  , "INDEX_NAME"
#                                  , "INDEX_CLASS"
#                                  , df_thresh_metric
#                                  , df_thresh_index)
#
#   df_metsc_qc <- df_metval[, 8:11]
#   names(df_metsc_qc) <- myMetrics
#
#   # change integers to numeric
#   # Round values to 1 or 2 digits
#   #library(dplyr)
#
#   # `%>%` <- dplyr::`%>%`
#   #
#   # df_metval_calc <- df_metval_calc %>%
#   #   dplyr::mutate_if(is.integer, as.numeric) %>%
#   #   dplyr::mutate_if(is.numeric, round, digits =2)
#   #
#   # df_metval_qc <- df_metval_qc %>%
#   #   dplyr::mutate_if(is.integer, as.numeric) %>%
#   #   dplyr::mutate_if(is.numeric, round, digits =2)
#
#
#   # test
#   testthat::expect_equal(df_metval_calc[, 4:7], df_metval_qc[, 1:4])
#   # Below works but
#   #               the QC data is not consistent in the number of decimal places
#   #expect_equal(df_metval_calc, df_metval_qc, tolerance = 0.01)
#
#
# })## Test - met val_sc, SCMB_IBI

# Coral ####
  ### _Metric.Values ----
# BenB TEST ####
# wd <- getwd()
# input.dir <- "inst/extdata"
# library(readr)
# library(dplyr)
# myTestfile <- read_csv(file.path(wd, input.dir
#                                  , "FL_BCG_BioMonTools_Input_20240307.csv")
#                        , na = c("NA",""), trim_ws = TRUE, skip = 0
#                        , col_names = TRUE, guess_max = 100000)
# unique_samps <- unique(myTestfile$SampleID)
# unique_samps_250 <- unique_samps[1:250]
#
# myDF <- myTestfile %>%
#   filter(SampleID %in% unique_samps_250)
#
# # Convert to data.frame.  Code breaks if fun.DF is a tibble.
# myDF <- as.data.frame(myDF)
# # convert Field Names to UPPER CASE
# names(myDF) <- toupper(names(myDF))
#
# myDF$N_TAXA <- 27 # need to add ignore N_TAXA to metric_values
# myDF$INDEX_CLASS <- "CORAL_TEST"
# myDF$INDEX_NAME <- "CORAL_TEST"
#
#   # data(data_diatom_mmi_dev) #added via data.R
#   # df_diatoms <- data_diatom_mmi_dev
#
#   # metric values
#   df_metval_calc <- metric.values(fun.DF = myDF
#                                                , fun.Community = "CORAL"
#                                                , boo.Shiny = FALSE)
#
#   # df, calc
#   # data(data_diatom_mmi_qc)
#   #
#   # df_metval_qc <- data_diatom_mmi_qc
#
#   # test
#   testthat::expect_equal(df_metval_calc, df_metval_qc)
