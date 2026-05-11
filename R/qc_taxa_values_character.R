#' QC Autecological Character Values
#'
#' Performs basic QC of a character column against a list of accepted values.
#'
#' Returns a data frame the values from the input with counts (column = n) from
#' the column and whether the values appeared in valid values (column =
#' valid). Values in the accepted values not appearing in the input are appended
#' to the bottom of the returned data frame.  These values are marked as n = NA
#' and valid = TRUE.  If NA is a valid value it must be included in valid_vals
#' or in the output NA will be labeled as valid = FALSE.
#'
#' The default accepted values are the abbreviations are those used as
#' metric.values().
#'
#' For Function Feeding Group (FFG); CF, CG, MH, OM, PA, PI, PR, SC, SH, and XY.  User using FC
#' and GC over CF and CG can modify the accepted values.  Both versions are
#' accepted in metric.values().
#'
#' For Habit; BU, CB, CN, SK, SP, and SW.  Valid separated with "," are
#' first split apart and spaces removed.
#'
#' life cycle
#'
#' bcg_attr
#'
#' habitat, habitat structure, elevation, gradient, thermal
#'
#' @param data A data frame containing autecological taxa data.
#' @param col_char The column containing the character values to be checked.
#' @param valid_vals Accepted values.
#' @param separator If values should be separated and checked include a
#' delimiter.  Default = NULL
#'
#' @return A data frame with col_char values, occurrence (n), and if valid (TRUE/
#' FALSE).  Any missing valid_vals are appended.
#'
#' @examples
#' Values, FFG, Abr
#' qc_taxa_values_char(data_benthos_PacNW,
#'                     "FFG",
#'                     valid_vals = c("CF",
#'                                    "CG",
#'                                    "MH",
#'                                    "OM",
#'                                    "PA",
#'                                    "PH",
#'                                    "PI",
#'                                    "PR",
#'                                    "SC",
#'                                    "SH",
#'                                    "XY",
#'                                    NA))
#'
#' # Values, FFG, full names
#' qc_taxa_values_char(data_benthos_MBSS,
#'                     "FFG",
#'                     valid_vals = c("Collector",
#'                                    "Filterer",
#'                                    "Predator",
#'                                    "Scraper",
#'                                    "Shredder"))
#'
#' # Values, Habit, no separator
#' qc_taxa_values_char(data_benthos_MBSS,
#'                     "Habit",
#'                     valid_vals = c("bu", "cb", "cn", "dv", "sk", "sp", "sw"))
#'
#' # Values, Habit, no separator
#' qc_taxa_values_char(data_benthos_MBSS,
#'                     "Habit",
#'                     valid_vals = c("bu", "cb", "cn", "dv", "sk", "sp", "sw"),
#'                     separator = ",")
#'
#'
#'
#' @export
qc_taxa_values_char <- function(data,
                                col_char = NULL,
                                valid_vals = NULL,
                                separator = NULL) {

  # QC----
  # Check for values
  # stopifnot(!is.null(data))
  stopifnot(!is.null(col_char))
  stopifnot(!is.null(valid_vals))
  # Check for col_char in data
  if (!rlang::as_string(col_char) %in% names(data)) {
    stop("Column '",
         rlang::as_string(col_char),
         "' is missing from input data.", call. = FALSE)
  }# IF ~ col_char

  # Convert valid_vals to data frame
  df_valid_vals <- as.data.frame(valid_vals)
  names(df_valid_vals) <- col_char

  # occurrence----
  ## separator ----
  # parse column first (remove whitespace)
  # track original columns?
  if (is.null(separator)) {

  }## IF ~ separator

  # as is, no separator
  df_result <- data |>
    # occurrence
    dplyr::count(.data[[col_char]], name = "n") |>
    # force valid value rows to exist
    # *ERROR*20260511*comment out
    # tidyr::complete(
    #   .data[[col_char]] = c(TRUE, FALSE, NA),
    #   fill = list(n = 0)) |>
    # valid
    dplyr::mutate(valid = .data[[col_char]] %in% valid_vals) #|>
    # ## values
    # dplyr::full_join(y = df_valid_vals,
    #                  by = dplyr::join_by({{col_char}})) |>
    # ## convert NA to TRUE
    # dplyr::mutate(valid = dplyr::case_when(is.na(valid) ~ TRUE,
    #                                        .default = valid))

  # look at logical, rename then rename back

  # Result----
  return(df_result)

}## FUNCTION ~ END

