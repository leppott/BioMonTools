#' @title QC Taxa List Proofreading
#'
#' @description Performs basic proofreading of names in a taxa list.
#'
#' @details Returns possible differences in a data frame with three columns (qc check,
#' name, potential match(es)).  Not all hits are errors but are potential issues
#' that may need to be addressed.
#'
#' The distance check Computes pairwise string distances between names and
#' returns name pairs that are likely duplicates.
#'
#' Uses Jaro-Winkler (jw) distance by default which performs well for names.
#' Other options are Levenshtein (lv), good for typos, and osa, like Levenshtein
#' but slightly faster.
#'
#' Good thresholds are jw 0.1 to 0.2, lv and osa <= 2
#'
#' The checks include:
#'
#' * **spaces**, leading or trailing, including html white space, or doulble
#' space, or more than 3
#'
#' * **case**, differences
#'
#' * **sp** variants; (with/without .) sp and spp, inside next to slash
#'
#' * **stage** variants; adult, A, pupa, pupae, P, immature, I, imm, juv,
#' juvenile, larva, larvae, L, zoea, myses, mysops?, megalops, megadrile
#'
#' * **probably**, variants; "?", " prob ", " prob. ", " probably "
#' * add parentheses
#'
#' * **cf**, variants start, or in string, cf, c.f., cf., c.f
#'
#' * backslash_dash_underscore
#'
#' * terrestrial (terr.), megadrile
#'
#' complex cmplx
#'
#' all caps
#'
#' and, &
#'
#' star
#'
#' head
#'
#' possibly, poss, poss.
#'
#'
#' unknown unk undetermined undet(.), indet, indetermined
#'
#'  large small with space or parentheses
#'
#'  backslash_dash
#'
#' * **slash, direction** direction; including dash
#'
#' * **slash, taxa** x/y vs. y/x
#'
#' * **grp** variants; grp, gr, group, (with/without .) and without
#' and dash and genus group, gp,  dash or space before
#'
#' * **unid** variants; unid, unidentified, unid diff, uid, (with/without .)
#'
#' diff without unid
#'
#' * **prob** variants; prob, prob., probably, including "?" (anywhere in text)
#'
#' * **sensu**
#'
#' * **parenthetical** text; sensu, prob, inc spec, (with/without .)
#'
#' * **near** variants; nr n
#'
#' aff.  , f
#' flag
#'
#' quotes
#'
#' slash order; c/o vs o/c
#'
#' with, without, w/, w/o, w/ o, w /, w / o
#'
#' frag and fragment
#'
#' Tubificid
#'
#' * **colon** e.g., Family: Genus
#'
#' * **patterns** tera$ in Order, idae$ in Family, inae$ Subfamily, and
#' ini$ in Tribe.  Look for those patterns not in the expected columns.
#' would need the entire taxa table.  Right now only looking at a single vector.
#'
#' immature, imm, w/ and w/o hair chaetae, hair+pectinate, bifid
#' setae, chaetae
#'
#' Common authors not in parentheses, e.g., Epler
#'
#'
#' text mining algorithms (word similarity)
#' Other checks caught:
#'
#' some not included:
#'
#' * f. = forma = valid
#'
#' @param names A character vector containing taxa name data.
#' @param method String distance method (passed to stringdist).
#' Default = "jw"
#' @param max_distance Numeric threshold for similarity.  Default = 0.13
#'
#' @return A data frame with col_tolval values, occurrence (n), and if valid
#' (TRUE/FALSE).
#'
#' @examples
#' # Example Issues
#' proof_issues <- qc_taxa_names_proof(data_taxa_names_issues$FinalID)
#' proof_issues$issues
#' lapply(proof_issues, nrow)
#'
#'
#' # Example Master Taxa Lists
#' proof_MBSS <- qc_taxa_names_proof(data_benthos_MBSS$TAXAID, "jw", 0.13)
#' proof_MBSS$issues
#' head(proof_MBSS$distance)
#'
#' proof_PacNW_taxaid <- qc_taxa_names_proof(data_benthos_PacNW$TaxaID)
#' proof_PacNW_taxaid$issues
#' head(proof_PacNW_taxaid$distance)
#'
#' proof_PacNW_master_taxaid <- qc_taxa_names_proof(TaxaMaster_Ben_BCG_PacNW$TaxaID)
#' proof_PacNW_master_taxaid$issues
#' proof_PacNW_master_taxaid$stage
#' head(proof_PacNW_taxaid$distance)
#'
#' @export
qc_taxa_names_proof <- function(names,
                                method = "jw",
                                max_distance = 0.13) {

  boo_debug <- FALSE
  if (boo_debug) {
    names = BioMonTools::data_benthos_MBSS$TAXAID
    # names = BioMonTools::data_benthos_PacNW$TaxaID
    # names = BioMonTools::TaxaMaster_Ben_BCG_PacNW$TaxaID
    method = "jw"
    max_distance = 0.13
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


  # unique----
  # avoid duplicates in output
  names <- unique(names)

  # 01. distance ----
  df_distance <- data.frame(name = names) |>
    # generate all pairwise combinations
    # join of id < id enforces unique pairs
    dplyr::mutate(id = dplyr::row_number()) |>
    dplyr::inner_join(
      y = data.frame(name = names) |>
        dplyr::mutate(id = dplyr::row_number()),
      by = dplyr::join_by(id < id),
      suffix = c("_x", "_y")) |>
    # calc distance
    dplyr::mutate(
      distance = stringdist::stringdist(
        name_x,
        name_y,
        method = method)) |>
    # filter
    dplyr::filter(distance <= max_distance) |>
    # sort
    dplyr::arrange(distance) |>
    # filter if blank
    drop_if_empty()

  # 02. space ----
  # beginning or ending white space
  # including non-breaking ws (html $nbsp), unicode \u00A0
  pat_space <- "^(?:\\s|\\u00A0)+|(?:\\s|\\u00A0)+$"

  df_space <- names[grepl(pat_space,
                          names,
                          ignore.case = TRUE,
                          perl = FALSE)]


  # 03. case ----
  df_case <- data.frame(name_orig = names) |>
    dplyr::mutate(name_lower = tolower(name_orig)) |>
    dplyr::group_by(name_lower) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::summarise(count = dplyr::n(),
                     original_names = paste(unique(name_orig), collapse = ", "),
                     .groups = "drop")  |>
    # filter if blank
    drop_if_empty()

  # 04. sp ----
  # " sp ", " sp. ", " spp ", and " spp. "
  # pat_sp <- "(?<=\\s)sp{1,2}\\.?(?=\\s)"
  # # sp and spp
  # # dot and not dot
  #
  # df_sp <- names[grepl(pat_sp,
  #                      names,
  #                      ignore.case = TRUE)]


  # 05. stage ----
  # regex pattern at end after a space
  # negative look ahead for " sp. A" and " sp A"
  pat_stage <- "(?<! sp)(?<! sp\\.) A$| (adult|P|pupa|pupae|I|imm|immature|L|larvae)$"

  df_stage <- names[grepl(pat_stage,
                          names,
                          ignore.case = TRUE,
                          perl = TRUE)]

  # 06. probably----
  # "?", " prob ", " prob. ", " probably "
  # and parens
  # pat_prob <- "\\?|\\sprob\\.?(?=\\s)|\\sprobably\\s"
#  pat_prob <- "\?|(?:^|[ ()])prob\.?(?=$|[ ()])|(?:^|[ ()])probably(?:$|[ ()])"
  # [ ()] = space or a parenthesis
  # (?=[ ()]) = next char is space or parenthesis

#   df_prob <- names[grepl(pat_prob,
#                        names,
#                        ignore.case = TRUE,
#                        perl = TRUE)]

  # 07. unk ----
  # " unk ", " Unknown ", " Undetermined ", " undet ", " undet. "
  # (undet.)
  # Unknown)
  # (unk
  pat_unk <- "(?<=[\\s()])(unk|unknown|undetermined|undet)\\.?(?=[\\s()])"

  df_unk <- names[grepl(pat_unk,
                        names,
                        ignore.case = TRUE,
                        perl = TRUE)]

  # 08. cf ----
  # " cf " and " cf. "
  pat_cf <- "\\scf\\.?\\s"
  # \\s ensures spaces around words
  # `\.?’ makes the period optional

  df_cf <- names[grepl(pat_cf,
                       names,
                       ignore.case = TRUE)]

  # 09. backslash_dash_underscore ----
  # "\", "-", or "_", one or more
  pat_bsdus <- "[\\\\-_]"
  # need 2 slashes to get 1
  # need to escape slash with 2 slashes (\\)
  # find any inside [foo]

  df_bsdus <- names[grepl(pat_bsdus,
                       names,
                       ignore.case = TRUE)]


  # Combine ----
  results <- list(
    "issues"   = NULL,
    "distance" = df_distance,
    "space"    = df_space,
    "case"     = df_case,
    # "sp"       = df_sp,
    "stage"    = df_stage,
    # "probably" = df_prob,
    "unk"      = df_unk,
    "cf"       = df_cf,
    "backslash_dash_underscore" = df_bsdus)
  # report names of elements with length > 0 to "issues"
  results$issues <- names(results)[vapply(results, length, integer(1)) > 0]

  # Result----
  return(results)

}## FUNCTION ~ END

