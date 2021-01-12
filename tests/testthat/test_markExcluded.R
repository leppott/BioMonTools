# qc_checks ####
test_that("markExcluded", {

  # # add pipe
  # `%>%` <- dplyr::`%>%`
  #
  # # Data
  # df_samps_bugs <- readxl::read_excel(system.file("./extdata/Data_Benthos.xlsx"
  #                                         , package="BioMonTools")
  #                             , guess_max=10^6)
  #
  # # Variables
  # SampID     <- "SampleID"
  # TaxaID     <- "TaxaID"
  # TaxaCount  <- "N_Taxa"
  # Exclude    <- "Exclude_New"
  # TaxaLevels <- c("Kingdom"
  #                 , "Phylum"
  #                 , "SubPhylum"
  #                 , "Class"
  #                 , "SubClass"
  #                 , "Order"
  #                 , "SubOrder"
  #                 , "SuperFamily"
  #                 , "Family"
  #                 , "SubFamily"
  #                 , "Tribe"
  #                 , "Genus"
  #                 , "SubGenus"
  #                 , "Species"
  #                 , "Variety")
  # # Taxa that should be treated as equivalent
  # Exceptions <- data.frame("TaxaID"=c("Sphaeriidae"), "PhyloID"=c("Pisidiidae"))
  #
  # # Filter Data
  # # df_samptax <- filter(df_samps_bugs, !!as.name(SampID) == "08BEA3478__2013-08-21_0")
  # # df_tst_small <- markExcluded(df_samptax, SampID, TaxaID, TaxaCount
  # #                              , TaxaLevels, Exceptions, Exclude)
  #
  # # EXAMPLE 1
  # df_tst <- BioMonTools::markExcluded(df_samps_bugs
  #                                     , SampID="SampleID"
  #                                     , TaxaID="TaxaID"
  #                                     , TaxaCount = "N_Taxa"
  #                                     , Exclude="Exclude_New"
  #                                     , TaxaLevels=TaxaLevels
  #                                     , Exceptions=Exceptions)
  #
  # # Compare
  # df_compare <- dplyr::summarise(dplyr::group_by(df_tst, SampleID)
  #                                , Exclude_Import=sum(Exclude)
  #                                , Exclude_R=sum(Exclude_New))
  # df_compare$Diff <- df_compare$Exclude_Import - df_compare$Exclude_R
  # #
  # tbl_diff <- table(df_compare$Diff)
  # knitr::kable(tbl_diff)
  # # sort
  # df_compare <- df_compare %>% dplyr::arrange(desc(Diff))
  #
  # # Number with issues
  # sum(abs(df_compare$Diff))
  # # total samples
  # nrow(df_compare)
  #
  # # confusion matrix
  # tbl_results <- table(df_tst$Exclude, df_tst$Exclude_New, useNA = "ifany")
  # #
  # # Show differences
  # knitr::kable(tbl_results)
  # knitr::kable(df_compare[1:10, ])
  # knitr::kable(df_compare[672:678, ])
  # # samples with differences
  # samp_diff <- as.data.frame(df_compare[df_compare[,"Diff"]!=0, "SampleID"])
  # # results for only those with differences
  # df_tst_diff <- df_tst[df_tst[,"SampleID"] %in% samp_diff$SampleID, ]
  # # add diff field
  # df_tst_diff$Exclude_Diff <- df_tst_diff$Exclude - df_tst_diff$Exclude_New
  #
  # # Classification Performance Metrics
  # class_TP <- tbl_results[2,2] # True Positive
  # class_FN <- tbl_results[2,1] # False Negative
  # class_FP <- tbl_results[1,2] # False Positive
  # class_TN <- tbl_results[1,1] # True Negative
  # class_n <- sum(tbl_results)  # total
  # #
  # # sensitivity (recall); TP / (TP+FN); measure model to ID true positives
  # class_sens <- class_TP / (class_TP + class_FN)
  # # precision; TP / (TP+FP); accuracy of model positives
  # class_prec <- class_TP / (class_TP + class_FP)
  # # specifity; TN / (TN + FP); measure model to ID true negatives
  # class_spec <- class_TN  / (class_TN + class_FP)
  # # overall accuracy; (TP + TN) / all cases; accuracy of all classifications
  # class_acc <- (class_TP + class_TN) / class_n
  # # F1; 2 * (class_prec*class_sens) / (class_prec+class_sens)
  # ## balance of precision and recall
  # class_F1 <- 2 * (class_prec * class_sens) / (class_prec + class_sens)
  # #
  # results_names <- c("Sensitivity (Recall)", "Precision", "Specificity", "OVerall Accuracy", "F1")
  # results_values <- c(class_sens, class_prec, class_spec, class_acc, class_F1)
  # #
  # tbl_class <- data.frame(results_names, results_values)
  # names(tbl_class) <- c("Performance Metrics", "Percent")
  # tbl_class$Percent <- round(tbl_class$Percent *100, 2)
  # knitr::kable(tbl_class)

  # test
  # testthat::expect_equal(tib_calc, tib_qc)
})## Test ~ qc_checks ~ END

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
