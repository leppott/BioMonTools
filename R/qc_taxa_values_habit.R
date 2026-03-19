#' QC Habitat Values
#'
#' Performs basic QC of the Habit column against a list of accepted values.
#'
#' Returns a data frame the values from the input with counts (column = n) from
#' the Habit column and whether the value appeared in valid values (column =
#' valid). Values in the accepted values not appearing in the input are appended
#' to the bottom of the returned data frame.  These values are marked as n = NA
#' and valid = TRUE.
#'
#' The default accepted values are the abbreviations are those used as
#' metric.values(); BU, CB, CN, SK, SP, and SW.  Valid separated with "," are
#' first split apart and spaces removed.
#'
#' @param df_data A data frame containing taxa data.
#' @param col_habit The column containing Habit values.  Default = "Habit"
#' @param valid_vals Accepted values.
#' Default = c(BU, CB, CN, SK ,SP, SW.)
#'
#' @return A data frame with col_habit values, occurrence (n), and if valid
#' (TRUE/FALSE).  Additional values from valid_vals are appended.
#'
#' @examples
#' # Values, Default
#' qc_taxa_values_habit(data_benthos_MBSS)
#'
#' # Values, User
#' qc_taxa_values_habit(data_benthos_MBSS,
#'                      "Habit",
#'                      valid_vals = c("bu", "cb", "cn", "dv", "sk", "sp", "sw"))
#'
#' @export
qc_taxa_values_habit <- function(df_data,
                                 col_habit = "Habit",
                                 valid_vals = c("BU",
                                                "CB",
                                                "CN",
                                                "SK",
                                                "SP",
                                                "SW")) {

  # QC
  if (!rlang::as_string(col_habit) %in% names(df_data)) {
    stop("Column '",
         rlang::as_string(col_habit),
         "' is missing from input data.", call. = FALSE)
  }# IF ~ col_habit

  # Convert valid_vals to data frame
  df_valid_vals <- as.data.frame(valid_vals)
  names(df_valid_vals) <- col_habit

  # occurrence
  df_match <- df_data |>
    # get all values, split on comma with optional surrounding spaces
    tidyr::separate_rows(.data[[col_habit]], sep = "\\s*,\\s*") |>
    # remove spaces
    dplyr::mutate({{col_habit}} := trimws(.data[[col_habit]])) |>
    # occurrence
    dplyr::count(.data[[col_habit]], name = "n") |>
    # valid
    ## T/F
    dplyr::mutate(valid = .data[[col_habit]] %in% valid_vals) |>
    ## values
    dplyr::full_join(y = df_valid_vals,
                     by = dplyr::join_by({{col_habit}})) |>
    ## convert NA to TRUE
    dplyr::mutate(valid = dplyr::case_when(is.na(valid) ~ TRUE,
                                           .default = valid))

  # Result
  return(df_match)

}## FUNCTION ~ END

