#' QC Autecological Character Values
#'
#' Performs basic QC of a character column against a list of accepted values.
#'
#' Returns a data frame of the values from the input with counts (column = n)
#' from the column and whether the values appeared in valid values (column =
#' valid). Values in the accepted values not appearing in the input are appended
#' to the bottom of the returned data frame.  These values are marked as n = NA
#' and valid = TRUE.  If NA is a valid value it must be included in valid_vals
#' or in the output NA will be labeled as valid = FALSE.
#'
#' The default accepted values for the abbreviations are those used in the
#' function metric.values().  See below for examples.
#'
#' Function Feeding Group (FFG)
#' CF, CG, MH, OM, PA, PI, PR, SC, SH, XY
#' User using FC and GC over CF and CG can modify the accepted values.  Both
#' versions are accepted in metric.values().
#'
#' Habit
#' BU, CB, CN, SK, SP, SW
#' Values separated with "," are first split apart and spaces removed before
#' checking.  Not necessary to supply all possible combinations as each part is
#' checked against the valid values.
#'
#' Life Cycle (Voltinism)
#' MULTI, SEMI, UNI
#'
#' FFG2
#' DD, PRE
#'
#' Thermal_Indicator
#' STENOC, COLD, COOL, WARM, STENOW, EURYTHERMAL, COWA
#'
#' HabStruct
#' CS, NF, RM, SG
#'
#' Habitat
#' BRAC, DEPO, GENE, HEAD, LENT, LOTI, RHEA, RIVE, SPEC, TERR, UNKN
#'
#' Elevation
#' LOW, HIGH
#'
#' Gradient
#' LOW, MOD, HIGH
#'
#' WSArea
#' SMALL, MEDIUM, LARGE, XLARGE
#'
#' BCG_ATTR
#' 1, 2, 3, 4, 5, 6, 1I, 4_BETTER, 4_MIDDLE, 4_WORSE, 5.5, 6I, 6M, 6T,
#'
#'
#' @param data A data frame containing autecological taxa data.
#' @param col_vals The column containing the character values to be checked.
#' @param valid_vals Accepted values.
#' @param separator If values should be separated and checked include a
#' delimiter.  Default = NULL
#'
#' @return A data frame with col_vals values, occurrence (n), and if valid (TRUE/
#' FALSE).  Any missing valid_vals are appended.
#'
#' @examples
#' # Values, FFG, Abr
#' qc_taxa_values_character(data_benthos_PacNW,
#'                          "FFG",
#'                          valid_vals = c("CF",
#'                                         "CG",
#'                                         "MH",
#'                                         "OM",
#'                                         "PA",
#'                                         "PH",
#'                                         "PI",
#'                                         "PR",
#'                                         "SC",
#'                                         "SH",
#'                                         "XY",
#'                                         NA))
#'
#' # Values, FFG, full names
#' qc_taxa_values_character(data_benthos_MBSS,
#'                          "FFG",
#'                          valid_vals = c("Collector",
#'                                         "Filterer",
#'                                         "Predator",
#'                                         "Scraper",
#'                                         "Shredder"))
#'
#' # Values, Habit, no separator
#' qc_taxa_values_character(data_benthos_MBSS,
#'                          "Habit",
#'                          valid_vals = c("bu", "cb", "cn", "dv", "sk", "sp", "sw"))
#'
#' # Values, Habit, no separator
#' qc_taxa_values_character(data_benthos_MBSS,
#'                          "Habit",
#'                          valid_vals = c("bu", "cb", "cn", "dv", "sk", "sp", "sw"),
#'                          separator = ",")
#'
#'
#'
#' @export
qc_taxa_values_character <- function(data,
                                     col_vals = NULL,
                                     valid_vals = NULL,
                                     separator = NULL) {

  # QC----
  # Check for values
  # stopifnot(!is.null(data))
  stopifnot(!is.null(col_vals))
  stopifnot(!is.null(valid_vals))
  # Check for col_vals in data
  if (!rlang::as_string(col_vals) %in% names(data)) {
    stop("Column '",
         rlang::as_string(col_vals),
         "' is missing from input data.", call. = FALSE)
  }# IF ~ col_vals

  # Convert valid_vals to data frame
  df_valid_vals <- as.data.frame(valid_vals)
  names(df_valid_vals) <- col_vals

  # occurrence----
  ## separator ----
  # parse column first (remove whitespace)
  # track original columns?
  if (is.null(separator)) {

  }## IF ~ separator

  # as is, no separator
  df_result <- data |>
    # occurrence
    dplyr::count(.data[[col_vals]], name = "n") |>
    # force valid value rows to exist
    # *ERROR*20260511*comment out
    # tidyr::complete(
    #   .data[[col_vals]] = c(TRUE, FALSE, NA),
    #   fill = list(n = 0)) |>
    # valid
    dplyr::mutate(valid = .data[[col_vals]] %in% valid_vals) #|>
    # ## values
    # dplyr::full_join(y = df_valid_vals,
    #                  by = dplyr::join_by({{col_vals}})) |>
    # ## convert NA to TRUE
    # dplyr::mutate(valid = dplyr::case_when(is.na(valid) ~ TRUE,
    #                                        .default = valid))

  # look at logical, rename then rename back

  # Result----
  return(df_result)

}## FUNCTION ~ END

