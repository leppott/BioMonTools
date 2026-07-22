#' @title QC Taxa Phylo
#'
#' @description Performs basic quality control on a phylogenetic list.
#'
#' @details Returns a list of with elements corresponding the various checks on
#' a phylogenetic (standard or master) taxa list.
#'
#' The phylogenetic list is multiple columns that the user will provide in rank
#' order from coarse to fine along with a FinalID column.
#'
#' The checks are listed below and only report the entries that fail.
#'
#' Some checks will detect those with potential issues but others that are
#' valid.
#'
#' * **unique_parent** Each taxonomic rank (child) has a unique parent (coarser
#' rank).  Parents include all coarser ranks (as defined by user).
#'
#' * **phylo_unique_rank** Each name is in only one phylogenetic rank column
#'
#' * **phylo_as_finalid** Each phylogenetic name is also in FinalID
#'
#' * **finalid_as_phylo** Each final id is a phylogenetic name
#'
#' others checks?
#'
#' case (all lower, all upper)
#'
#' spaces
#' ?
#' non A-Z (any case), e.g., slash, dash, underscore, parentheses, etc.
#'
#' If ignore_case is TRUE then all columns (finalid and phylo_names) will be
#' converted to upper case before checks are performed.
#'
#'
#' @param data A data frame
#' @param finalid Column name for FinalID.  Default = "FinalID"
#' @param phylo_names Vector of phylogenetic names in order from coarse to fine.
#' Default = c("Phylum", "Subphylum", "Class", "Subclass", "Order", "Suborder",
#' "Family", "Subfamily", "Tribe", "Genus")
#' @param ignore_case Should case be ignored for checks.
#' Default = FALSE.
#'
#' @return A list with elements for each qc check.
#'
#' @examples
#' qc_phylo_PacNW <- qc_taxa_phylo(TaxaMaster_Ben_BCG_PacNW,
#'                                 "TaxaID",
#'                                  phylo_names = c("Phylum",
#'                                                  "SubPhylum",
#'                                                  "Class",
#'                                                  "SubClass",
#'                                                  "Order",
#'                                                  "SuperFamily",
#'                                                  "Family",
#'                                                  "Tribe",
#'                                                  "Genus",
#'                                                  "SubGenus",
#'                                                  "Species"))
#' qc_phylo_PacNW$issues
#' qc_phylo_PacNW$unique_parent
#' qc_phylo_PacNW$phylo_unique_rank
#' qc_phylo_PacNW$phylo_as_finalid
#' qc_phylo_PacNW$finalid_as_phylo
#'
#' @export
qc_taxa_phylo <- function(data,
                          finalid = "FinalID",
                          phylo_names = c("Phylum",
                                          "Subphylum",
                                          "Class",
                                          "Subclass",
                                          "Order",
                                          "Suborder",
                                          "Family",
                                          "Subfamily",
                                          "Tribe",
                                          "Genus"),
                          ignore_case = FALSE) {

  # global variable bindings ----
  child_name <- child_rank <- parent_rank <- parent_n <- phylo_name <-
    phylo_level <- n_phylo_name <- match_finalid <- num_phylo_col <-
    match_phylo <- NULL

  # QC
  boo_debug <- FALSE
  if (boo_debug) {
    data = BioMonTools::TaxaMaster_Ben_BCG_PacNW
    finalid = "TaxaID"
    phylo_names = c("Phylum",
                    "SubPhylum",
                    "Class",
                    "SubClass",
                    "Order",
                    "SuperFamily",
                    "Family",
                    "Tribe",
                    "Genus",
                    "SubGenus",
                    "Species")
  }## boo_debug

  # Suppress empty tibbles
  # @title Drop empty data frames from output
  # @description
  # Returns NULL instead of printing a zero-row tibble.
  # @param x A data frame or tibble.
  # @return The data frame, or NULL if it has zero rows.
  drop_if_empty <- function(x) {
    if (nrow(x) == 0) NULL else x
  }## FUNCTION ~ drop_if_empty

  # QC ----
  # check for any phylo_names not in data
  stopifnot(all(phylo_names %in% names(data)))

  # 00. Data Prep----
  cols_phylo <- phylo_names

  # 01. unique_parent ----
  # run as a list since number of "phylo_name"s unknown

  # need extra loop if do all combinations

  # get all combinations of parent and child
  len_cols_phylo <- length(cols_phylo)
  df_pairs <- data.frame(
    col_num_child = rep(2:len_cols_phylo, times = 1:(len_cols_phylo-1)),
    col_num_parent  = unlist(lapply(2:len_cols_phylo,
                               function(i) (i - 1):1)))
  df_pairs$phylo_child  <- cols_phylo[df_pairs$col_num_child]
  df_pairs$phylo_parent <- cols_phylo[df_pairs$col_num_parent]
  df_pairs$check_name <- paste(df_pairs$phylo_child,
                               df_pairs$phylo_parent,
                               sep = "_")

  # create results list
  i_list <- vector("list", nrow(df_pairs))
  names(i_list) <- df_pairs$check_name

  for (i in seq_len(nrow(df_pairs))) {
    # message(paste0(i, "; ", df_pairs[i, "check_name"]))

    i_list[[i]] <- data |>
      # rename child column
      dplyr::rename(child_name = dplyr::all_of(df_pairs[i, "phylo_child"])) |>
      # add ranks
      dplyr::mutate(child_rank = df_pairs[i, "phylo_child"]) |>
      dplyr::mutate(parent_rank = df_pairs[i, "phylo_parent"]) |>
      # calc
      dplyr::group_by(child_name, child_rank, parent_rank) |>
      dplyr::filter(!is.na(child_name)) |>
      dplyr::summarize(parent_n = dplyr::n_distinct(.data[[df_pairs[i, "phylo_parent"]]]),
                       parent_names = paste(sort(unique(.data[[df_pairs[i, "phylo_parent"]]])),
                                              collapse = ", "),
                       .groups = "drop")
  }## FOR ~ i

  df_unique_parent <- i_list |>
    # combine element of list
    # source is target_rank
    dplyr::bind_rows() |>
    # filter
    dplyr::filter(parent_n > 1) |>
    # filter if blank
    drop_if_empty()


  # 03. phylo_as_finalid ----
  df_phylo_as_finalid <- data |>
    # cols to keep
    dplyr::select(dplyr::all_of(finalid), dplyr::all_of(cols_phylo)) |>
    # pivot
    tidyr::pivot_longer(cols = dplyr::all_of(cols_phylo),
                        names_to = "phylo_level",
                        values_to = "phylo_name") |>
    # non blank
    dplyr::filter(!is.na(phylo_name)) |>
    # add count by phylo_level
    dplyr::add_count(phylo_name, name = "n_phylo_name") |>
    # unique
    dplyr::distinct(.data[[finalid]],
                    phylo_name,
                    phylo_level,
                    n_phylo_name) |>
    # match final id
    dplyr::mutate(match_finalid = phylo_name %in% data[, finalid]) |>
    # cols 2 keep
    dplyr::select(phylo_name, phylo_level, n_phylo_name, match_finalid) |>
    # unique
    dplyr::distinct(phylo_name, phylo_level, n_phylo_name, match_finalid) |>
    # find issues, false and not blank
    dplyr::filter(match_finalid == FALSE,
                  phylo_name != "") |>
    # add factor for sorting
    dplyr::mutate(phylo_level = factor(phylo_level,
                                       levels = cols_phylo)) |>
    # sort
    dplyr::arrange(phylo_level, phylo_name)

  # 02. phylo_unique_rank ----
  # duplicate phylo names
  ## appearing in more than one phylo column
  # df_phylo_unique_rank <- df_match_phylo_final |>
  df_phylo_unique_rank <- df_phylo_as_finalid |>
    # add count of duplicate names
    dplyr::add_count(phylo_name, name = "num_phylo_col") |>
    # filter for dups
    dplyr::filter(num_phylo_col > 1) |>
    # sort
    dplyr::arrange(phylo_name, phylo_level) |>
    # filter if blank
    drop_if_empty()

  # 04. finalid_as_phylo ----
  df_finalid_as_phylo <- data |>
    dplyr::mutate(match_phylo = rowSums(dplyr::across(dplyr::all_of(cols_phylo),
                                                      ~ .x == .data[[finalid]]),
                                        na.rm = TRUE) > 0) |>
    dplyr::select(dplyr::all_of(finalid), match_phylo) |>
    dplyr::filter(match_phylo == FALSE)


  # 05. Result ----
  # combine
  results <- list(
    "issues"   = NULL,
    "unique_parent"     = df_unique_parent,
    "phylo_unique_rank" = df_phylo_unique_rank,
    "phylo_as_finalid"  = df_phylo_as_finalid,
    "finalid_as_phylo"  = df_finalid_as_phylo)
  # report names of elements with length > 0 to "issues"
  results$issues <- names(results)[vapply(results, length, integer(1)) > 0]

  # Return Result
  return(results)

}## FUNCTION ~ END

