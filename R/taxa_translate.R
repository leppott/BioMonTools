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
#' official data column name is retained.  The data fields from the user file
#' have the suffix "_USER".
#'
#' By default, taxa are not collapsed to the official taxaid.  That is, if
#' multiple taxa in a sample have the same name the rows will not be combined.
#' If collapsing is desired set the parameter `sum_n_taxa_boo` to TRUE.
#' Will also need to provide `sum_n_taxa_col` and `sum_n_taxa_group_by`.
#'
#' Slightly different than `qc_taxa` since no options in `taxa_translate` for
#' using one field over another and is more generic.
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
#' @param col_drop Columns to remove in output.  Default = NULL
#' @param sum_n_taxa_boo Boolean value for if the results should be summarized
#' Default = FALSE
#' @param sum_n_taxa_col Column name for number of individuals for user data
#' when summarizing.  This column will be summed.
#' Default = NULL (suggestion = N_TAXA)
#' @param sum_n_taxa_group_by Column names for user data to use for grouping the
#' data when summarizing the user data.  Suggestions are SAMPID and TAXA_ID.
#' Default = NULL
#'
#' @return A list with three elements.  The first (merge) is the user data frame
#' with additional columns from the official data appended to it.  Names from
#' the user data that overlap with the official data have the suffix '_User'.
#' The second element (nonmatch) of the list is a vector of the non-matching
#' taxa from the user data.  The third element (metadata) includes the
#' metadata for the official data (if provided).
#'
#' @examples
#' # none at this time
#'
#'@export
taxa_translate <- function(df_user = NULL
                           , df_official = NULL
                           , df_official_metadata = NULL
                           , taxaid_user = "TAXAID"
                           , taxaid_official_match = NULL
                           , taxaid_official_project = NULL
                           , col_drop = NULL
                           , sum_n_taxa_boo = FALSE
                           , sum_n_taxa_col = NULL
                           , sum_n_taxa_group_by = NULL) {

  # DEBUG ----
  boo_DEBUG_tt <- FALSE
  if(boo_DEBUG_tt == TRUE){
    # pick files
    fn_pick <- "_pick_files.csv"
    path_pick <- file.path("inst", "extdata", "taxa_official", fn_pick)
    df_pick <- read.csv(path_pick)
    # df_user
    fn_user <- "_Input_HiGradHiElev_noExclude_20220108_small.csv"
    path_user <- file.path("inst", "extdata", fn_user)
    df_user <- read.csv(path_user)
    # df_official
    official_projects <- df_pick[, "project"]
    official_files <- df_pick[, "filename"]
    taxaid_projects <- df_pick[, "taxaid"]
    sel_project <- official_projects[1] #"Pacific Northwest"  # USER INPUT
    fn_official <- official_files[match(sel_project, official_projects)]
    path_official <- file.path("inst", "extdata", "taxa_official", fn_official)
    df_official <- read.csv(path_official, na.strings = "")
    # taxaid_user
    taxaid_user <- "TaxaID" # <- pre-defined but user could select
    # taxaid_official
    taxaid_official_match <- taxaid_projects[match(sel_project, official_projects)]
    # taxaid_project
    calc_type <- unlist(strsplit(df_pick[df_pick[, "project"] == sel_project
                                         , "calc_type"], ","))
    calc_type_taxaid <- unlist(strsplit(df_pick[df_pick[, "project"] ==
                                        sel_project, "calc_type_taxaid"], ","))
    sel_calc_type <- calc_type[1] # "BCG" # USER INPUT
    taxaid_official_project <- calc_type_taxaid[match(sel_calc_type, calc_type)]
    col_drop_project <- unique(calc_type_taxaid[!calc_type_taxaid %in%
                                                     taxaid_official_project])

    # metadata
    fn_meta <- df_pick[match(sel_project, official_projects), "metadata_file"]
    path_meta <- file.path("inst", "extdata", "taxa_official", fn_meta)
    df_official_metadata <- read.csv(path_meta)

    # QC, add bad row to user input for testing
    df_user[nrow(df_user) + 1, taxaid_user] <- "_Test"
    # add bad column to drop
    df_user[, "Test_Col"] <- NA_character_

    col_drop <- "Test_Col"

    # summary
    sum_n_taxa_boo <- TRUE
    sum_n_taxa_col <- "N_TAXA"
    sum_n_taxa_group_by <- c("INDEX_NAME", "INDEX_CLASS", "SampleID", "TaxaID")

  }##IF ~ boo_DEBUG_tt

  # QC ----
  ## QC, df type----
  ## ensure df_* are data frames, tibbles cause issues
  df_user <- data.frame(df_user)
  df_official <- data.frame(df_official)

  ## QC, df----

  if(is.null(df_user)) {
    msg <- "'df_user' not provided.  Unable to process."
    stop(msg)
  }## IF ~ is.null(df_user)

  if(is.null(df_official)) {
    msg <- "'df_official' not provided.  Unable to process."
    stop(msg)
  }## IF ~ is.null(df_official)

  ## QC, taxaid ----

  if(is.null(taxaid_user)) {
    msg <- "'taxaid_user' not provided.  Unable to process."
    stop(msg)
  }## IF ~ is.null(taxaid_user)

  if(is.null(taxaid_official_match)) {
    msg <- "'taxaid_official_match' not provided.  Unable to process."
    stop(msg)
  }## IF ~ is.null(taxaid_official)

  if(is.null(taxaid_official_project)) {
    msg <- "'taxaid_official_project' not provided.  Unable to process."
    stop(msg)
  }## IF ~ is.null(taxaid_official_project)

  ## QC, taxaid match df ----

  boo_taxaid_user <- taxaid_user %in% names(df_user)
  if(boo_taxaid_user == FALSE) {
    msg <- "'taxaid_user' not found in 'df_user'.  Unable to process."
    stop(msg)
  }## IF ~ boo_taxaid_user == FALSE

  boo_taxaid_official_match <- taxaid_official_match %in% names(df_official)
  if(boo_taxaid_official_match == FALSE) {
    msg <- "'taxaid_official_match' not found in 'df_official'.  Unable to process."
    stop(msg)
  }## IF ~ taxaid_official_match == FALSE

  boo_taxaid_official_project <- taxaid_official_project %in% names(df_official)
  if(boo_taxaid_official_project == FALSE) {
    msg <- "'taxaid_official_project' not found in 'df_official'.  Unable to process."
    stop(msg)
  }## IF ~ taxaid_official_match == FALSE



  # Merge ----
  df_merge <- merge(df_user, df_official
                    , by.x = taxaid_user
                    , by.y = taxaid_official_match
                    , all.x = TRUE
                    , suffixes = c("_USER", "")
                    , sort = FALSE)

  if(boo_DEBUG_tt == TRUE){
    testthat::expect_equal(nrow(df_user), nrow(df_merge))
  } ## IF ~ boo_DEBUG_tt


  # Munge ----
  ## if matched official as a column
  df_merge[, "Match_Official"] <- df_merge[, taxaid_user] %in%
    df_official[, taxaid_official_match]

  ## remove "other" project taxaid columns
  col_keep <- !names(df_merge) %in% col_drop #col_drop_project
  df_merge <- df_merge[, col_keep]

  ## Drop Col
  if(is.null(col_drop) == FALSE) {
    df_merge <- df_merge[
      , names(df_merge)[!names(df_merge) %in% col_drop]]
  }## IF ~ is.null(col_drop)


  # NonMatch Info ----
  taxa_user <- sort(unique(df_user[, taxaid_user]))
  taxa_user_n <- length(taxa_user)
  df_nonmatch <- df_merge[df_merge[, "Match_Official"] == FALSE, ]
  taxa_nonmatch <- sort(unique(df_nonmatch[, taxaid_user]))
  taxa_nonmatch_n <- length(taxa_nonmatch)


  # Summary ----
  if(sum_n_taxa_boo == TRUE) {
    df_summ <- dplyr::summarise(
                    dplyr::group_by(df_merge
                            , dplyr::across(dplyr::all_of(sum_n_taxa_group_by)))
                    , col2rename = sum(!!as.name(sum_n_taxa_col)
                                       , na.rm = TRUE)
                    , .groups = "drop_last")
    names(df_summ)[names(df_summ) %in% "col2rename"] <- sum_n_taxa_col
    # QC, ni_total
    if(boo_DEBUG_tt == TRUE){
      testthat::expect_equal(sum(df_summ[, sum_n_taxa_col], na.rm = TRUE)
                             , sum(df_merge[, sum_n_taxa_col], na.rm = TRUE))
    } ## IF ~ boo_DEBUG_tt
    df_merge <- df_summ
  }## IF ~ boo_combine


  # Console Output ----
  ## Console, matches----
  msg <- paste0("User taxa match, "
                , taxa_user_n - taxa_nonmatch_n
                , " / "
                , taxa_user_n)
  message(msg)
  ## Console, non-matches----
  if(taxa_nonmatch_n > 0) {
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
                     , "nonmatch" = taxa_nonmatch
                     , "official_metadata" = df_official_metadata)


  if(boo_DEBUG_tt == TRUE){
    str(ls_results)
  } ## IF ~ boo_DEBUG_tt

  return(ls_results)

}## FUNCTION ~ end





