#' @title Taxa Translate
#'
#' @description Convert user taxa names to those in an official project based
#' name list.
#'
#' @details Merges user file with official file.  The official file has
#' phylogeny, autecology, and other project specific fields.
#'
#' The inputs for the function uses existing data frames (or tibbles).
#'
#' Any fields that match between the user file and the official file the
#' official data column name have the 'official' version retained.
#'
#' The 'col_drop' parameter can be used to remove unwanted columns; e.g.,
#' the other taxa id fields in the 'official' data file.
#'
#' By default, taxa are not collapsed to the official taxaid.  That is, if
#' multiple taxa in a sample have the same name the rows will not be combined.
#' If collapsing is desired set the parameter `sum_n_taxa_boo` to TRUE.
#' Will also need to provide `sum_n_taxa_col` and `sum_n_taxa_group_by`.
#'
#' Slightly different than `qc_taxa` since no options in `taxa_translate` for
#' using one field over another and is more generic.
#'
#' The parameter `taxaid_drop` is used to drop records that matched to a new
#' name that should not be included in the results.  Examples include "999" or
#' "DNI" (Do Not Include).  Default is NULL so no action is taken.  "NA"s are
#' always removed.
#'
#' Optional parameter `trim_ws` is used to invoke the function `trimws` to
#' remove from the taxa matching field any leading and trailing white space.
#' Default is FALSE (no action).  All horizontal and vertical white space
#' characters are removed.  See ?trimws for additional information.
#' Additionally, non-breaking spaces (nbsp) inside the text string will be
#' replaced with a normal space.  This cuts down on the number of permutations
#' need to be added to the translation table.
#'
#'
#' The taxa list and metadata file names will be added to the results as two
#' new columns.
#'
#' Another output is the unique taxa with old and new names.
#'
#' @param df_user User taxa data
#' @param df_official Official project taxa data (master taxa list).
#' @param df_official_metadata Metadata for offiical project taxa data.
#' Included
#' Default is NULL
#' @param taxaid_user Taxonomic identifier in user data.  Default is "TAXAID".
#' @param taxaid_official_match Taxonomic identifier in official data user to
#' match with user data.  This is not the project taxanomic identifier.
#' @param taxaid_official_project Taxonomic identifier in official data that is
#' specific to a project, e.g., after operational taxonomic unit (OTU) applied.
#' @param taxaid_drop Official taxonomic identifier that signals a record
#' should be dropped; e.g., DNI (Do Not Include) or -999.  Default = NULL
#' @param col_drop Columns to remove in output.  Default = NULL
#' @param sum_n_taxa_boo Boolean value for if the results should be summarized
#' Default = FALSE
#' @param sum_n_taxa_col Column name for number of individuals for user data
#' when summarizing.  This column will be summed.
#' Default = NULL (suggestion = N_TAXA)
#' @param sum_n_taxa_group_by Column names for user data to use for grouping the
#' data when summarizing the user data.  Suggestions are SAMPID and TAXA_ID.
#' Default = NULL
#' @param trim_ws Should the taxa have leading and trailing white space removed.
#' Non-braking spaces (e.g., from ITIS) also removed (including inside text).
#' Default = FALSE
#'
#' @return A list with four elements.  The first (merge) is the user data frame
#' with additional columns from the official data appended to it.  Names from
#' the user data that overlap with the official data have the suffix '_User'.
#' The second element (nonmatch) of the list is a vector of the non-matching
#' taxa from the user data.  The third element (metadata) includes the
#' metadata for the official data (if provided).  The fourth element (unique) is
#' a data frame of the unique taxa names old and new.
#'
#' @examples
#' # Example 1, PacNW
#' ## Input Parameters
#' df_user <- data_benthos_PacNW
#' fn_official <- file.path(system.file("extdata", package = "BioMonTools")
#'                          , "taxa_official"
#'                          , "ORWA_TAXATRANSLATOR_20221219.csv")
#' df_official <- read.csv(fn_official)
#' fn_official_metadata <- file.path(system.file("extdata"
#'                                               , package = "BioMonTools")
#'                                   , "taxa_official"
#'                       , "ORWA_ATTRIBUTES_METADATA_20221117.csv")
#' df_official_metadata <- read.csv(fn_official_metadata)
#' taxaid_user <- "TaxaID"
#' taxaid_official_match <- "Taxon_orig"
#' taxaid_official_project <- "OTU_MTTI"
#' taxaid_drop <- "DNI"
#' col_drop <- c("Taxon_v2", "OTU_BCG_MariNW") # non desired ID cols in Official
#' sum_n_taxa_boo <- TRUE
#' sum_n_taxa_col <- "N_TAXA"
#' sum_n_taxa_group_by <- c("INDEX_NAME", "INDEX_CLASS", "SampleID", "TaxaID")
#' ## Run Function
#' taxatrans <- taxa_translate(df_user
#'                                , df_official
#'                                , df_official_metadata
#'                                , taxaid_user
#'                                , taxaid_official_match
#'                                , taxaid_official_project
#'                                , taxaid_drop
#'                                , col_drop
#'                                , sum_n_taxa_boo
#'                                , sum_n_taxa_col
#'                                , sum_n_taxa_group_by)
#' ## View Results
#' # View(taxatrans$merge)
#' taxatrans$nonmatch
#' # View(taxatrans$official_metadata)
#'
#' #~~~~~
#' # Example 2, Multiple Stages
#' # Create data
#' TAXAID <- c(rep("Agapetus", 3), rep("Zavrelimyia", 2))
#'
#' N_TAXA <- c(rep(33, 3), rep(50, 2))
#' STAGE <- c("A","L","P","X","")
#' df_user <- data.frame(TAXAID, N_TAXA, STAGE)
#' df_user[, "INDEX_NAME"] <- "BCG_MariNW_Bugs500ct"
#' df_user[, "INDEX_CLASS"] <- "HiGrad-HiElev"
#' df_user[, "SAMPLEID"] <- "Test2023"
#' df_user[, "STATIONID"] <- "Test"
#' df_user[, "DATE"] <- "2023-01-16"
#' ## Input Parameters
#' fn_official <- file.path(system.file("extdata", package = "BioMonTools")
#'                          , "taxa_official"
#'                          , "ORWA_TAXATRANSLATOR_20221219.csv")
#' df_official <- read.csv(fn_official)
#' fn_official_metadata <- file.path(system.file("extdata"
#'                                               , package = "BioMonTools")
#'                                   , "taxa_official"
#'                                   , "ORWA_ATTRIBUTES_20221212.csv")
#' df_official_metadata <- read.csv(fn_official_metadata)
#' taxaid_user <- "TAXAID"
#' taxaid_official_match <- "Taxon_orig"
#' taxaid_official_project <- "OTU_BCG_MariNW"
#' taxaid_drop <- NULL
#' col_drop <- c("Taxon_v2", "OTU_MTTI") # non desired ID cols in Official
#' sum_n_taxa_boo <- TRUE
#' sum_n_taxa_col <- "N_TAXA"
#' sum_n_taxa_group_by <- c("INDEX_NAME", "INDEX_CLASS", "SAMPLEID", "TAXAID")
#' ## Run Function
#' taxatrans <- BioMonTools::taxa_translate(df_user
#'                                          , df_official
#'                                          , df_official_metadata
#'                                          , taxaid_user
#'                                       , taxaid_official_match
#'                                       , taxaid_official_project
#'                                       , taxaid_drop
#'                                       , col_drop
#'                                       , sum_n_taxa_boo
#'                                       , sum_n_taxa_col
#'                                       , sum_n_taxa_group_by)
#' ## View Results (before and after)
#' df_user
#' taxatrans$merge
#
#'@export
taxa_translate <- function(df_user = NULL
                           , df_official = NULL
                           , df_official_metadata = NULL
                           , taxaid_user = "TAXAID"
                           , taxaid_official_match = NULL
                           , taxaid_official_project = NULL
                           , taxaid_drop = NULL
                           , col_drop = NULL
                           , sum_n_taxa_boo = FALSE
                           , sum_n_taxa_col = NULL
                           , sum_n_taxa_group_by = NULL
                           , trim_ws = FALSE) {

  # DEBUG ----
  boo_DEBUG_tt <- FALSE
  if (boo_DEBUG_tt == TRUE) {
    # Example 1, PacNW ----
    ## Input Parameters
    df_user <- data_benthos_PacNW
    fn_official <- file.path(system.file("extdata", package = "BioMonTools")
                             , "taxa_official"
                             , "ORWA_TAXATRANSLATOR_20221219.csv")
    df_official <- read.csv(fn_official)
    fn_official_metadata <- file.path(system.file("extdata"
                                                  , package = "BioMonTools")
                                      , "taxa_official"
                                      , "ORWA_ATTRIBUTES_METADATA_20221117.csv")
    df_official_metadata <- read.csv(fn_official_metadata)
    taxaid_user <- "TaxaID"
    taxaid_official_match <- "Taxon_orig"
    taxaid_official_project <- "OTU_MTTI"
    taxaid_drop <- "DNI"
    col_drop <- c("Taxon_v2", "OTU_BCG_MariNW") # non desired ID cols in Official
    sum_n_taxa_boo <- TRUE
    sum_n_taxa_col <- "N_TAXA"
    sum_n_taxa_group_by <- c("INDEX_NAME", "INDEX_CLASS", "SampleID", "TaxaID")
    clean <- TRUE
    match_caps <- TRUE

        ## OLD ----
    # # pick files
    # fn_pick <- "_pick_files.csv"
    # path_pick <- file.path("inst", "extdata", "taxa_official", fn_pick)
    # df_pick <- read.csv(path_pick)
    # # df_user
    # fn_user <- "_Input_HiGradHiElev_noExclude_20220108_small.csv"
    # path_user <- file.path("inst", "extdata", fn_user)
    # df_user <- read.csv(path_user)
    # # df_official
    # official_projects <- df_pick[, "project"]
    # official_files <- df_pick[, "filename"]
    # taxaid_projects <- df_pick[, "taxaid"]
    # sel_project <- official_projects[1] #"Pacific Northwest"  # USER INPUT
    # fn_official <- official_files[match(sel_project, official_projects)]
    # path_official <- file.path("inst", "extdata", "taxa_official", fn_official)
    # df_official <- read.csv(path_official, na.strings = "")
    # # taxaid_user
    # taxaid_user <- "TaxaID" # <- pre-defined but user could select
    # # taxaid_official
    # taxaid_official_match <- taxaid_projects[match(sel_project, official_projects)]
    # # taxaid_project
    # calc_type <- unlist(strsplit(df_pick[df_pick[, "project"] == sel_project
    #                                      , "calc_type"], ","))
    # calc_type_taxaid <- unlist(strsplit(df_pick[df_pick[, "project"] ==
    #                                     sel_project, "calc_type_taxaid"], ","))
    # sel_calc_type <- calc_type[1] # "BCG" # USER INPUT
    # taxaid_official_project <- calc_type_taxaid[match(sel_calc_type, calc_type)]
    # # col_drop_project <- unique(calc_type_taxaid[!calc_type_taxaid %in%
    # #                                                  taxaid_official_project])
    # # col_drop_project <- unlist(strsplit(df_pick[df_pick$project == sel_project
    # #                                    , "col_drop"]
    # #                                    , ","))
    #
    # # metadata
    # fn_meta <- df_pick[match(sel_project, official_projects), "metadata_file"]
    # path_meta <- file.path("inst", "extdata", "taxa_official", fn_meta)
    # df_official_metadata <- read.csv(path_meta)
    #
    # # QC, add bad row to user input for testing
    # df_user[nrow(df_user) + 1, taxaid_user] <- "_Test"
    # # add bad column to drop
    # df_user[, "Test_Col"] <- NA_character_
    #
    # col_drop <- "Test_Col"
    #
    # # summary
    # sum_n_taxa_boo <- TRUE
    # sum_n_taxa_col <- "N_TAXA"
    # sum_n_taxa_group_by <- c("INDEX_NAME"
    #                          , "INDEX_CLASS"
    #                          , "SampleID"
    #                          , taxaid_official_project)

  }##IF ~ boo_DEBUG_tt

  # QC ----
  ## QC, df type----
  ## ensure df_* are data frames, tibbles cause issues
  df_user <- data.frame(df_user)
  df_official <- data.frame(df_official)

  ## QC, df----

  if (is.null(df_user)) {
    msg <- "'df_user' not provided.  Unable to process."
    stop(msg)
  }## IF ~ is.null(df_user)

  if (is.null(df_official)) {
    msg <- "'df_official' not provided.  Unable to process."
    stop(msg)
  }## IF ~ is.null(df_official)

  ## QC, taxaid ----

  if (is.null(taxaid_user)) {
    msg <- "'taxaid_user' not provided.  Unable to process."
    stop(msg)
  }## IF ~ is.null(taxaid_user)

  if (is.null(taxaid_official_match)) {
    msg <- "'taxaid_official_match' not provided.  Unable to process."
    stop(msg)
  }## IF ~ is.null(taxaid_official)

  if (is.null(taxaid_official_project)) {
    msg <- "'taxaid_official_project' not provided.  Unable to process."
    stop(msg)
  }## IF ~ is.null(taxaid_official_project)

  ## QC, taxaid match df ----

  boo_taxaid_user <- taxaid_user %in% names(df_user)
  if (boo_taxaid_user == FALSE) {
    msg <- "'taxaid_user' not found in 'df_user'.  Unable to process."
    stop(msg)
  }## IF ~ boo_taxaid_user == FALSE

  boo_taxaid_official_match <- taxaid_official_match %in% names(df_official)
  if (boo_taxaid_official_match == FALSE) {
    msg <- "'taxaid_official_match' not found in 'df_official'.  Unable to process."
    stop(msg)
  }## IF ~ taxaid_official_match == FALSE

  boo_taxaid_official_project <- taxaid_official_project %in% names(df_official)
  if (boo_taxaid_official_project == FALSE) {
    msg <- "'taxaid_official_project' not found in 'df_official'.  Unable to process."
    stop(msg)
  }## IF ~ taxaid_official_match == FALSE



  # Munge1, trim_ws ----
  # 20240430, v1.0.2.9017, partial
  # 20240528, v1.0.2.9025 and 9026
  if (trim_ws) {
    # Munge, replace internal nbsp
    df_user[, taxaid_user] <- gsub("\u00a0", " ", df_user[, taxaid_user])
    # Munge, trim leading and trailing spaces (all)
    df_user[, taxaid_user] <- trimws(df_user[, taxaid_user]
                                     , whitespace = "[\\h\\v]")
  }## IF ~ clean

  # MERGE----
  df_merge <- merge(df_official, df_user
                    , by.x = taxaid_official_match
                    , by.y = taxaid_user
                    , all.y = TRUE
                    , suffixes = c("", "_USER")
                    , sort = FALSE)


  if (boo_DEBUG_tt == TRUE) {
    testthat::expect_equal(nrow(df_user), nrow(df_merge))
  } ## IF ~ boo_DEBUG_tt

  # user taxa id will be gone after the merge


  # Munge2 ----

  ## new Col, match merge main ID to df_official----
  df_merge[, "Match_Official"] <- df_merge[, taxaid_official_match] %in%
    df_official[, taxaid_official_match]
  # new Col, TaxaID modified
  df_merge[, "Changed"] <- df_merge[, taxaid_official_match]  ==
    df_merge[, taxaid_official_project]

  ## Element 4, unique taxa translate ----
  # run here to get "raw" version with all rows
  if (sum_n_taxa_boo == TRUE) {
    df_taxatrans_unique <- dplyr::summarise(
      dplyr::group_by(df_merge
                      , !!as.name(taxaid_official_match)
                      , !!as.name(taxaid_official_project)
                      , Match_Official)
      , N_Taxa_Sum = sum(!!as.name(sum_n_taxa_col), na.rm = TRUE)
      # , N_Taxa_Count = dplyr::n_distinct(!!as.name(taxaid_official_match)
      #                                    , na.rm = TRUE)
      , .groups = "drop_last")
    df_taxatrans_unique <- data.frame(df_taxatrans_unique)
  } else {
     df_taxatrans_unique <- unique(df_merge[, c(taxaid_official_match
                                            , taxaid_official_project
                                            , "Match_Official"
                                            )])

  }## IF ~ sum_n_taxa_boo

  # rename column
  names(df_taxatrans_unique)[1] <- taxaid_user
  # sort
  df_taxatrans_unique <- df_taxatrans_unique[order(df_taxatrans_unique[
    , taxaid_user]), ]

  # add "modified" column
  df_taxatrans_unique[, "Modified"] <-
    df_taxatrans_unique[, taxaid_user] !=
    df_taxatrans_unique[, taxaid_official_project]
  # move to new position
  df_taxatrans_unique <- dplyr::relocate(df_taxatrans_unique
                                         , Modified
                                         , .after = Match_Official)

  ## Drop the "matching" column----
  col_drop_idmatch <- names(df_merge)[!names(df_merge) %in% taxaid_official_match]
  df_merge <- df_merge[, col_drop_idmatch]

  ## Drop "_USER" columns----
  col_user <- grepl("_USER$", names(df_merge))
  df_merge <- df_merge[, names(df_merge)[!col_user]]

  ## Rename taxaid_official_project to taxaid_user----
  names(df_merge)[names(df_merge) %in% taxaid_official_project] <- taxaid_user

  # ## Drop "other" project taxaid columns
  # col_keep <- !names(df_merge) %in% col_drop_project
  # df_merge <- df_merge[, col_keep]

  ## Drop Col----
  if (is.null(col_drop) == FALSE) {
    df_merge <- df_merge[
      , names(df_merge)[!names(df_merge) %in% col_drop]]
  }## IF ~ is.null(col_drop)

  ## Resort columns----
  # Make taxaid first (taxaid_user - was taxaid_official_project)
  col_reorder <- c(taxaid_user
                   , "Match_Official"
                   , names(df_merge)[!names(df_merge)
                                     %in% c(taxaid_user
                                            , "Match_Official")])
  df_merge <- df_merge[, col_reorder]

  ## Drop TaxaID ----
  ### NA
  row_taxaid_NA <- is.na(df_merge[, taxaid_user])
  df_merge <- df_merge[!row_taxaid_NA, ]
  ### taxaid_drop
  if (!is.null(taxaid_drop)) {
    row_taxaid_drop <- !df_merge[, taxaid_user] %in% taxaid_drop
    df_merge <- df_merge[row_taxaid_drop, ]
  }## IF ~ is.null(taxaid_drop)


  # Summary ----
  if (sum_n_taxa_boo == TRUE) {
    # Recalc
    df_summ <- dplyr::summarise(
                    dplyr::group_by(df_merge
                            , dplyr::across(dplyr::all_of(sum_n_taxa_group_by)))
                    , col2rename = sum(!!as.name(sum_n_taxa_col)
                                       , na.rm = TRUE)
                    , .groups = "drop_last")
    names(df_summ)[names(df_summ) %in% "col2rename"] <- sum_n_taxa_col
    ## Merge2----
    ## Re-merge official taxa info
    df_summ_merge <- merge(df_summ
                           , df_official
                           , by.x = taxaid_user
                           , by.y = taxaid_official_match
                           , all.x = TRUE
                           , sort = FALSE)
    ## if matched official as a column
    df_summ_merge[, "Match_Official"] <- df_summ_merge[, taxaid_official_project] %in%
      df_official[, taxaid_official_match]
    # QC----
    if (boo_DEBUG_tt == TRUE) {
      # nrows
      testthat::expect_equal(nrow(df_summ_merge)
                             , nrow(df_summ))
      # ni_total
      testthat::expect_equal(sum(df_summ[, sum_n_taxa_col], na.rm = TRUE)
                             , sum(df_merge[, sum_n_taxa_col], na.rm = TRUE))
      # ni_total
      testthat::expect_equal(sum(df_summ_merge[, sum_n_taxa_col], na.rm = TRUE)
                             , sum(df_summ[, sum_n_taxa_col], na.rm = TRUE))
    } ## IF ~ boo_DEBUG_tt
    df_merge <- df_summ_merge
  }## IF ~ boo_combine


  # QC, NA or DNI taxa names----

  # NonMatch Info ----
  taxa_user <- sort(unique(df_user[, taxaid_user]))
  taxa_user_n <- length(taxa_user)
  #df_nonmatch <- df_merge[df_merge[, "Match_Official"] == FALSE, ]

  taxa_nonmatch <- taxa_user[!taxa_user %in% df_official[, taxaid_official_match]]
  taxa_nonmatch_n <- length(taxa_nonmatch)
  #
  df_user_nonmatch <- df_user[df_user[, taxaid_user] %in% taxa_nonmatch, ]
  #
  df_nonmatch <- dplyr::summarise(
    dplyr::group_by(df_user_nonmatch, !!as.name(taxaid_user))
    , N_Taxa_Sum = sum(!!as.name(sum_n_taxa_col), na.rm = TRUE)
    , N_Taxa_Count = dplyr::n_distinct(!!as.name(taxaid_user), na.rm = TRUE)
    , .groups = "drop_last")
  df_nonmatch <- data.frame(df_nonmatch)

  # Console Output ----

  ## Console, matches----
  msg <- paste0("User taxa match, "
                , taxa_user_n - taxa_nonmatch_n
                , " / "
                , taxa_user_n)
  message(msg)

  ## Console, non-matches----
  if (taxa_nonmatch_n > 0) {
    str_tax <- ifelse(taxa_nonmatch_n == 1, "taxon", "taxa")
    msg_1 <- paste0("The following user "
                    , str_tax
                    , " ("
                    , taxa_nonmatch_n
                    , "/"
                    , taxa_user_n
                    , ") did not match the official taxa list:\n")
    msg_2 <- paste0(taxa_nonmatch, collapse = "\n")
    message(paste0(msg_1, msg_2))
  }## IF ~ non-matches message


  # RESULTS ----
  ls_results <- list("merge" = df_merge
                     , "nonmatch" = df_nonmatch
                     , "official_metadata" = df_official_metadata
                     , "taxatrans_unique" = df_taxatrans_unique)


  if (boo_DEBUG_tt == TRUE) {
    str(ls_results)
  } ## IF ~ boo_DEBUG_tt

  return(ls_results)

}## FUNCTION ~ end





