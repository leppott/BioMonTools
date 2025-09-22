#' @title Mark "exclude" (non-distinct / non-unique / ambiguous) taxa
#'
#' @description Takes as an input data frame with Sample ID, Taxa ID, and
#' phlogenetic name fields and returns a similar dataframe with a column for
#' "exclude" taxa (TRUE or FALSE).
#'
#' Exclude taxa are refered to by multiple names; ambiguous, non-distinct, and
#' non-unique. The "exclude" name was chosen so as to be consistent with
#' "non-target" taxa. That is, taxa marked as "TRUE" are treated as
#' undesireables.  Exclude taxa are those that are present in a sample when
#' taxa of the same group are present in the same sample are identified finer
#' level.  That is, the parent is marked as exclude when child taxa are
#' present in the same sample.
#'
#' @details The exclude taxa are referenced in the metric values function.
#' These taxa are removed from the taxa richness metrics.  This is because these
#' are coarser level taxa when fine level taxa are present in the same sample.
#'
#' Exceptions is a 2 column data frame of synonyms or other exceptions.
#' Column 1 is the name used in the TaxaID column the input data frame
#' (df_samptax). Column 2 is the name used in the TaxaLevels columns of the
#' input data frame (df_samptax).  The phylogenetic columns (TaxaLevels) will be
#' modified from Column 2 of the Exceptions data frame to match Column 1 of the
#' Exceptions data frame.  This ensures that the algorithm for markExcluded
#' works properly. The changes will not be stored and the original names
#' provided in the input data frame (df_samptax) will be returned in the final
#' result.  The function example below includes a practical case.
#'
#' Taxa Levels are phylogenetic names that are to be checked.  They should be
#' listed in order from course (kingdom) to fine (species).  Names not appearing
#' in the data will be skipped.
#'
#' The spelling of names must be consistent (including case) for this function
#' to produce the intended output.
#
# Erik.Leppo@tetratech.com (EWL), 20181119
#
#' @param df_samptax Input data frame.
#' @param SampID Column name in df_samptax for sample identifier.
#' Default = "SAMPLEID".
#' @param TaxaID Column name in df_samptax for organism identifier.
#' Default = "TAXAID".
#' @param TaxaCount Column name in df_samptax for organism count.
#' Default = "N_TAXA".
#' @param Exclude Column name for Exclude Taxa results in returned data frame.
#' Default = "Exclude".
#' @param TaxaLevels Column names in df_samptax that for phylogenetic names to
#' be evaluated.
#' Need to be in order from coarse to fine (i.e., Phylum to Species).
#' @param Exceptions NA or two column data frame of synonyms or other
#' exceptions.  Default = NA
#' Column 1 is the name used in the TaxaID column of df_samptax.
#' Column 2 is the name used in the TaxaLevels columns of df_samptax.
#'
#' @return Returns a data frame of df_samptax with an additional column,
#' Exclude.
#'
#' @examples
#'
#' # Packages
#' library(readxl)
#' library(dplyr)
#' library(lazyeval)
#' library(knitr)
#'
#' # Data
#' df_samps_bugs <- read_excel(system.file("./extdata/Data_Benthos.xlsx"
#'                                         , package="BioMonTools")
#'                            , guess_max=10^6)
#'
#' # Variables
#' SampID     <- "SampleID"
#' TaxaID     <- "TaxaID"
#' TaxaCount  <- "N_Taxa"
#' Exclude    <- "Exclude_New"
#' TaxaLevels <- c("Kingdom"
#'                 , "Phylum"
#'                 , "SubPhylum"
#'                 , "Class"
#'                 , "SubClass"
#'                 , "Order"
#'                 , "SubOrder"
#'                 , "SuperFamily"
#'                 , "Family"
#'                 , "SubFamily"
#'                 , "Tribe"
#'                 , "Genus"
#'                 , "SubGenus"
#'                 , "Species"
#'                 , "Variety")
#' # Taxa that should be treated as equivalent
#' Exceptions <- data.frame("TaxaID"=c("Sphaeriidae")
#'                                     , "PhyloID"=c("Pisidiidae"))
#'
#' # EXAMPLE 1
#' df_tst <- markExcluded(df_samps_bugs
#'                        , SampID = "SampleID"
#'                        , TaxaID = "TaxaID"
#'                        , TaxaCount = "N_Taxa"
#'                        , Exclude = "Exclude_New"
#'                        , TaxaLevels = TaxaLevels
#'                        , Exceptions = Exceptions)
#'
#' # Compare
#' df_compare <- dplyr::summarise(dplyr::group_by(df_tst, SampleID)
#'                                , Exclude_Import = sum(Exclude)
#'                                , Exclude_R = sum(Exclude_New))
#' df_compare$Diff <- df_compare$Exclude_Import - df_compare$Exclude_R
#' #
#' tbl_diff <- table(df_compare$Diff)
#' kable(tbl_diff)
#' # sort
#' df_compare <- df_compare %>% arrange(desc(Diff))
#'
#' # Number with issues
#' sum(abs(df_compare$Diff))
#' # total samples
#' nrow(df_compare)
#'
#' # confusion matrix
#' tbl_results <- table(df_tst$Exclude, df_tst$Exclude_New, useNA = "ifany")
#' #
#' # Show differences
#' kable(tbl_results)
#' knitr::kable(df_compare[1:10, ])
#' knitr::kable(df_compare[672:678, ])
#' # samples with differences
#' samp_diff <- as.data.frame(df_compare[df_compare[,"Diff"] != 0, "SampleID"])
#' # results for only those with differences
#' df_tst_diff <- df_tst[df_tst[,"SampleID"] %in% samp_diff$SampleID, ]
#' # add diff field
#' df_tst_diff$Exclude_Diff <- df_tst_diff$Exclude - df_tst_diff$Exclude_New
#'
#' # Classification Performance Metrics
#' class_TP <- tbl_results[2,2] # True Positive
#' class_FN <- tbl_results[2,1] # False Negative
#' class_FP <- tbl_results[1,2] # False Positive
#' class_TN <- tbl_results[1,1] # True Negative
#' class_n <- sum(tbl_results)  # total
#' #
#' # sensitivity (recall); TP / (TP+FN); measure model to ID true positives
#' class_sens <- class_TP / (class_TP + class_FN)
#' # precision; TP / (TP+FP); accuracy of model positives
#' class_prec <- class_TP / (class_TP + class_FP)
#' # specifity; TN / (TN + FP); measure model to ID true negatives
#' class_spec <- class_TN  / (class_TN + class_FP)
#' # overall accuracy; (TP + TN) / all cases; accuracy of all classifications
#' class_acc <- (class_TP + class_TN) / class_n
#' # F1; 2 * (class_prec*class_sens) / (class_prec+class_sens)
#' ## balance of precision and recall
#' class_F1 <- 2 * (class_prec * class_sens) / (class_prec + class_sens)
#' #
#' results_names <- c("Sensitivity (Recall)"
#'                    , "Precision"
#'                    , "Specificity"
#'                    , "Overall Accuracy"
#'                    , "F1")
#' results_values <- c(class_sens
#'                    , class_prec
#'                    , class_spec
#'                    , class_acc
#'                    , class_F1)
#' #
#' tbl_class <- data.frame(results_names, results_values)
#' names(tbl_class) <- c("Performance Metrics", "Percent")
#' tbl_class$Percent <- round(tbl_class$Percent * 100, 2)
#' kable(tbl_class)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' # EXAMPLE 2
#' ## No Exceptions
#'
#' df_tst2 <- markExcluded(df_samps_bugs
#'                         , SampID = "SampleID"
#'                         , TaxaID = "TaxaID"
#'                         , TaxaCount = "N_Taxa"
#'                         , Exclude = "Exclude_New"
#'                         , TaxaLevels = TaxaLevels
#'                         , Exceptions = NA)
#'
#' # Compare
#' df_compare2 <- dplyr::summarise(dplyr::group_by(df_tst2, SampleID)
#'                                , Exclude_Import = sum(Exclude)
#'                                , Exclude_R=sum(Exclude_New))
#' df_compare2$Diff <- df_compare2$Exclude_Import - df_compare2$Exclude_R
#' #
#' tbl_diff2 <- table(df_compare2$Diff)
#' kable(tbl_diff2)
#' # sort
#' df_compare2 <- df_compare2 %>% arrange(desc(Diff))
#'
#' # Number with issues
#' sum(abs(df_compare2$Diff))
#' # total samples
#' nrow(df_compare2)
#'
#' # confusion matrix
#' tbl_results2 <- table(df_tst2$Exclude, df_tst2$Exclude_New, useNA = "ifany")
#' #
#' # Show differences
#' kable(tbl_results2)
#' knitr::kable(df_compare2[1:10, ])
#' knitr::kable(tail(df_compare2))
#' # samples with differences
#' (samp_diff2 <- as.data.frame(df_compare2[df_compare2[,"Diff"]!=0
#'                                                   , "SampleID"]))
#' # results for only those with differences
#' df_tst_diff2 <- filter(df_tst2, SampleID %in% samp_diff2$SampleID)
#' # add diff field
#' df_tst_diff2$Exclude_Diff <- df_tst_diff2$Exclude - df_tst_diff2$Exclude_New
#'
#' # Classification Performance Metrics
#' class_TP2 <- tbl_results2[2,2] # True Positive
#' class_FN2 <- tbl_results2[2,1] # False Negative
#' class_FP2 <- tbl_results2[1,2] # False Positive
#' class_TN2 <- tbl_results2[1,1] # True Negative
#' class_n2 <- sum(tbl_results2)  # total
#' #
#' # sensitivity (recall); TP / (TP+FN); measure model to ID true positives
#' class_sens2 <- class_TP2 / (class_TP2 + class_FN2)
#' # precision; TP / (TP+FP); accuracy of model positives
#' class_prec2 <- class_TP2 / (class_TP2 + class_FP2)
#' # specifity; TN / (TN + FP); measure model to ID true negatives
#' class_spec2 <- class_TN2 / (class_TN2 + class_FP2)
#' # overall accuracy; (TP + TN) / all cases; accuracy of all classifications
#' class_acc2 <- (class_TP2 + class_TN2) / class_n2
#' # F1; 2 * (class_prec*class_sens) / (class_prec+class_sens)
#' ## balance of precision and recall
#' class_F12 <- 2 * (class_prec2 * class_sens2) / (class_prec2 + class_sens2)
#' #
#' results_names2 <- c("Sensitivity (Recall)"
#'                     , "Precision"
#'                     , "Specificity"
#'                     , "Overall Accuracy"
#'                     , "F1")
#' results_values2 <- c(class_sens2
#'                     , class_prec2
#'                     , class_spec2
#'                     , class_acc2
#'                     , class_F12)
#' #
#' tbl_class2 <- data.frame(results_names2, results_values2)
#' names(tbl_class2) <- c("Performance Metrics", "Percent")
#' tbl_class2$Percent <- round(tbl_class2$Percent * 100, 2)
#' kable(tbl_class2)
#
#' @export
markExcluded <- function(df_samptax,
                         SampID = "SAMPLEID",
                         TaxaID = "TAXAID",
                         TaxaCount = "N_TAXA",
                         Exclude = "EXCLUDE",
                         TaxaLevels,
                         Exceptions = NA) {

  #
  boo_QC <- FALSE
  if(isTRUE(boo_QC)){

    # Data
    df_samps_bugs <- readxl::read_excel(system.file("extdata/Data_Benthos.xlsx"
                                            , package="BioMonTools")
                                , guess_max=10^6)

    # Recode to Function Variables
    df_samptax <- df_samps_bugs

    # Variables
    SampID     <- "SampleID"
    TaxaID     <- "TaxaID"
    TaxaCount  <- "N_Taxa"
    Exclude    <- "Exclude_New"
    TaxaLevels <- c("Kingdom"
                    , "Phylum"
                    , "SubPhylum"
                    , "Class"
                    , "SubClass"
                    , "Order"
                    , "SubOrder"
                    , "SuperFamily"
                    , "Family"
                    , "SubFamily"
                    , "Tribe"
                    , "Genus"
                    , "SubGenus"
                    , "Species"
                    , "Variety")
    Exceptions <- data.frame("TaxaID"="Sphaeriidae"
                             , "PhyloID"="Pisidiidae")

  }## IF ~ isTRUE(boo_QC) ~ END

  # global variable bindings ----
  count_tl <- NULL

  # retain original dataframe
  df_orig <- df_samptax

  # Upper Case
  names(df_samptax) <- toupper(names(df_samptax))
  SampID            <- toupper(SampID)
  TaxaID            <- toupper(TaxaID)
  TaxaCount         <- toupper(TaxaCount)
  TaxaLevels        <- toupper(TaxaLevels)

  # QC, check for taxa levels in df
  tl_check <- TaxaLevels %in% names(df_samptax)
  # stop if null
  tl_present <- TaxaLevels[tl_check]
  msg_stop <- "stop, <2 taxa levels"
  try((length(tl_present)<2), msg_stop)
  # warning
  tl_missing <- TaxaLevels[!tl_check]
  tl_missing_num <- length(tl_missing)
  msg_warn <- paste0("*WARNING*, "
                     , tl_missing_num
                     , " taxa levels missing from input data frame; \n "
                     , paste(tl_missing, collapse = ", ", sep = "")
                     , "\n")
  #warning(msg_warn, tl_missing_num>0) # (commented out)
  # cat(msg_warn)
  # utils::flush.console()
  if(tl_missing_num > 0) {
    message(msg_warn)
  }##IF ~ tl_missing_num

  # QC, remove tibble from data frame
  df_samptax <- as.data.frame(df_samptax)

  # QC, TaxaLevels (present) as character
  #df_samptax <- as.data.frame(df_samptax)
  #df_samptax[, tl_present] <- as.character(df_samptax[, tl_present])
  #sapply(df_samptax[, tl_present], FUN=as.character)
  df_samptax[,tl_present] <- as.character(unlist(df_samptax[, tl_present]))

  # QC, N_TAXA as numeric
  #df_samptax[, TaxaCount] <- as.numeric(df_samptax[, TaxaCount])

  # QC, count <1 ?

  # QC, check for Exclude field (ask to continue if already present)
  # needs session to be interactive
  if (sum(Exclude %in% names(df_samptax))==1 & interactive()==TRUE) {
    ##IF.prompt.START
    #ask to continue
    prompt_01 <- paste0("The user provided name for Exclude ('", Exclude
                        ,"') already exists in the user provided data.")
    prompt_02 <- "Do you wish to overwrite it with the results of this function (YES or NO)?"
    msg_prompt <- paste(prompt_01, prompt_02, sep="\n")

    #user_input <- readline(prompt=myPrompt)
    user_input <- NA
    user_input <- utils::menu(c("YES", "NO"), title=msg_prompt)
    # any answer other than "YES" will stop the function.
    if(user_input!=1){##IF.user.input.START
      stop(paste0("\n The user chose *not* to continue due to duplicate field name; "
                  , Exclude))
    }##IF.user_input.END
  }##IF.prompt.END


  # Add "FALSE" to Exclude Fields
  df_samptax[, Exclude] <- FALSE



 # i <- tl_present[6]  # QC

  # Loop through TaxaLevels
  for (i in tl_present) {
    #
    # if(boo_QC==TRUE){##IF.QC_val.START
    #   #i <- tl_present[6]
    #   i <- "Order"
    # }##IF.QC_val.END
    # #i <- tl_present[1]

    #
    i_num <- match(i, tl_present)
    i_len <- length(tl_present)
    msg_progress <- paste0("Working on item (", i_num, "/", i_len,"); ", i)
    print(msg_progress)
    utils::flush.console()

    # Exceptions
    # Rename "Exceptions" in TaxaLevels (present) columns
    if(sum(!is.na(Exceptions))!=0){##IF.is.na.START
      a <-  as.character(Exceptions[,2][1])
      for (a in Exceptions[,2]){##FOR.a.START
        a.num <- match(a, Exceptions[,2])
        df_samptax[, i] <- ifelse(df_samptax[,i]==a
                                  , as.character(Exceptions[,1][a.num])
                                  , df_samptax[,i])
      }##FOR.a.END
    }##IF.is.na.END


    # QC, bad characters
    ## comment out.  Can't say for sure some characters are bad.
    # ## Check for space and parens
    # badchar_val <- c(" ", "\\(", "\\)")
    # badchar_nam <- c("space", "open parenthesis", "close parenthesis")
    # j <- badchar_val[1] # QC
    # for (j in badchar_val){##IF.j.START
    #   j_num <- match(j, badchar_val)
    #   #
    #   badchar_sum <- sum(grepl(j, df_samptax[,i], perl=FALSE))
    #   if(badchar_sum!=0){##IF.badchar_sum.START
    #     msg_stop_badchar <- paste0("Execution halted due to ", badchar_sum
    #                                , " records in the '", i
    #                       , "' field that contain the restricted character '"
    #                                , badchar_nam[j_num], "'.")
    #     stop(msg_stop_badchar)
    #   }##IF.badchar_sum.START
    #   #
    # }##IF.j.END

    # QC, stringdist
    # library(stringdist)
    # create matrix of names and number of character differences between others
    #a <- sort(unique(df_samptax[,i]))
    # stringdistmatrix(a, a, useNames = "strings")

    # Count TaxaLevel by SampleID
    #
    # original Non Standard Evaluation but have to know column names
    # i_count_orig <- dplyr::summarise(dplyr::group_by(df_samptax
    #                                                 , SampleID
    #                                                 , Phylum)
    #               # individuals #
    #               , count_tl=dplyr::n_distinct(TaxaID, na.rm=TRUE)
    #               )
    # (i_count_orig)
    #~~~~~~~
    # Use variable names and "_" versions
    #~~~~~~~
    # Use
    # eval(substitute(SampID))
    # as.name(TaxaCount) == eval(substitute(TaxaCount))
    # interp(SampID)
    # https://stackoverflow.com/questions/27949212/using-dplyr-n-distinct-in-function-with-quoted-variable?rq=1
    # i_count <- dplyr::summarise_(dplyr::group_by_(df_samptax
    #                                              , SampID
    #                                              , i)
    #            # individuals #
    #            , count_tl = lazyeval::interp(~dplyr::n_distinct(var, na.rm=TRUE)
    #                                          , var = as.name(TaxaID))
    # )

    # 2022-11-17, get rid of deprecation warning for summarise_ and group_by_
    # https://stackoverflow.com/questions/47081564/replacing-group-by-with-group-by-when-the-argument-is-a-string-in-dplyr
    # use !!as.name(foo)
    # https://dplyr.tidyverse.org/reference/se-deprecated.html
    # rework without interp

    i_count <- dplyr::summarise(dplyr::group_by(df_samptax
                                                , !!as.name(SampID)
                                                , !!as.name(i))
                              , count_tl = dplyr::n_distinct(!!as.name(TaxaID))
                              , .groups = "drop_last")



    #https://www.r-bloggers.com/dplyr-do-some-tips-for-using-and-programming/

    # could pipe directly

    # Filter for count >1
    i_count_dups <- dplyr::filter(i_count, count_tl > 1)
    i_count_dups[, "count_tl"] <- TRUE


    # i_not_unique <- dplyr::summarise_se(seplyr::group_by_se(df_samptax
    #     , SampID, i)
    #               # individuals #
    #               , count_tl=dplyr::n_distinct(var, na.rm=TRUE)
    #                                           , var=as.name(TaxaID))
    #                ) %>% filter(count_tl > 1)

    # add count_tl with merge
    # (keep all columns)
    if(i_num==1){##IF.i_num.START
      df_Exclude <- df_samptax
      # # QC, TaxaLevels (present) as character
      # df_Exclude[,tl_present] <- as.character(unlist(df_Exclude[,tl_present]))
      # ## gets undone some how
    }##IF.i_num.START
    # (merge where TaxaID==i)
    # df_Exclude <- merge(df_Exclude, i_count_dups, all.x=TRUE, sort=FALSE
    #                     , by.x=c(SampID, TaxaID)
    #                     , by.y=c(SampID, i)
    #                     )

    # use left_join to keep order of columns and rows
    # https://stackoverflow.com/questions/28781750/using-left-join-from-dplyr-with-merge-variables-specified
    # df_Exclude <- dplyr::left_join(df_Exclude, i_count_dups
    #                                 , by=c("SampleID"="SampleID"
    # , "TaxaID"="Order"))


    # left_join(df_Exclude, i_count_dups, by=c("SampleID"="SampleID"
    # , "TaxaID"= "Phylum"))
    #
    # i_NSE <- interp(~y, y=as.name(i))


    # df_Exclude_orig <- dplyr::left_join(df_Exclude, i_count_dups
    #                                 , by=c("SampleID"="SampleID"
    #                                        , "TaxaID"="Phylum")
    #                                )
    # dim(df_Exclude_orig)
    # http://www.win-vector.com/blog/2016/12/parametric-variable-names-and-dplyr/
    # the above is replyr

    by_x_1 <- SampID
    by_y_1 <- SampID
    by_x_2 <- TaxaID
    by_y_2 <- i

    #left_join(df_Exclude, i_count_dups, by=c("SampleID"="SampleID"
    # , "TaxaID"= as_character(i)))


    #2015, not sure if work around, Hadley comment that setNames should work
    #https://stackoverflow.com/questions/28125816/r-standard-evaluation-for-join-dplyr

    #by_var <- c(by_x_1=by_y_1, by_x_2=i)

    # dplyr::inner_join with variable by
    # https://stackoverflow.com/questions/28399065/dplyr-join-on-by-a-b-where-a-and-b-are-variables-containing-strings/31612991
    # Experiment so by_var is ok
    by_var <- c(stats::setNames(nm = by_x_1, by_y_1), stats::setNames(nm = by_x_2
                                                                    , by_y_2))

    df_Exclude <- dplyr::left_join(df_Exclude, i_count_dups, by = by_var)
    #dim(df_Exclude)

    # Update Exclude (only for TRUE)
    df_Exclude[is.na(df_Exclude[,"count_tl"]), "count_tl"] <- FALSE
    df_Exclude[df_Exclude[,"count_tl"] == TRUE, Exclude] <- TRUE
    # Remove "count_tl"
    col_drop <- match("count_tl", names(df_Exclude))
    df_Exclude <- df_Exclude[, -col_drop]
    #
  }##for.i.END
  #
  # Return results
  #df_result <- df_Exclude
  df_result <- df_orig
  df_result[, Exclude] <- df_Exclude[, Exclude]
  return(df_result)
  #
}##FUNCTION.markExcluded.END
