#' QC Habitat Values
#'
#' Performs basic QC of the Tolerance Value column.
#'
#' Returns a data frame the values from the input with counts (column = n) from
#' the TolVal column and whether the value appeared in valid values (column =
#' valid).
#'
#' The default accepted values are 0 - 10.
#'
#' @param df_data A data frame containing taxa data.
#' @param col_tolval The column containing Tolerance Values.  Default = "TolVal"
#' @param valid_min Valid values range minimum.  Default = 0.
#' @param valid_max Valid values range maximum.  Default = 10.
#'
#' @return A data frame with col_tolval values, occurrence (n), and if valid
#' (TRUE/FALSE).
#'
#' @examples
#' qc_taxa_values_tolval(data_benthos_MBSS, "TOLVAL")
#'
#' @export
qc_taxa_values_tolval <- function(df_data,
                                  col_tolval = "TolVal",
                                  valid_min = 0,
                                  valid_max = 10) {

  # QC----
  ## col_tolval in df_data
  if (!rlang::as_string(col_tolval) %in% names(df_data)) {
    stop("Column '",
         rlang::as_string(col_tolval),
         "' is missing from input data.", call. = FALSE)
  }# IF ~ col_tolval exists

  ## col_tolval is numeric
  if (!is.numeric(df_data[[rlang::as_string(col_tolval)]])) {
    stop("Column '",
         rlang::as_string(col_tolval),
         "' must be numeric.", call. = FALSE)
  }## IF ~ col_tolval is numeric

  ## valid_min is numeric
  if (!is.numeric(valid_min)) {
    stop("'valid_min' must be numeric.", call. = FALSE)
  }## IF ~ valid_min is numeric

  ## valid_min is numeric
  if (!is.numeric(valid_min)) {
    stop("'valid_min' must be numeric.", call. = FALSE)
  }## IF ~ valid_min is numeric

  # occurrence----
  df_match <- df_data |>
    # occurrence
    dplyr::count(.data[[col_tolval]], name = "n") |>
    # valid
    ## T/F
    dplyr::mutate(valid = dplyr::case_when(
      .data[[col_tolval]] >= valid_min &
        .data[[col_tolval]] <= valid_max ~ TRUE,
      .default = FALSE))

  # Result----
  return(df_match)

}## FUNCTION ~ END

