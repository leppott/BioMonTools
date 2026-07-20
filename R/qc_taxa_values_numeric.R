#' QC Autecological Numeric Values
#'
#' Performs basic QC of a numeric column showing all values.
#'
#' Returns a data frame of the values from the input with counts (column = n) from
#' the specified column.  User provided valid_min and valid_max are applied to
#' each set of values and evaluated as valid TRUE or FALSE.
#'
#' The BioMonTools accepted values for TolVal are 0 - 10.
#'
#' The BioMonTools accepted values for UFC are 1 - 6.
#'
#' @param data A data frame containing autecological taxa data.
#' @param col_vals The column containing numeric values to be evaluated.
#' Default = NA.
#' @param valid_min Valid values range minimum (inclusive).  Default = NA.
#' @param valid_max Valid values range maximum (inclusive).  Default = NA.
#'
#' @return A data frame with col_vals values, occurrence (n), and valid
#' (TRUE/FALSE) within range of valid_min and valid_max.
#'
#' @examples
#' # TolVal
#' qc_taxa_values_numeric(data_benthos_MBSS, "TOLVAL", 0, 10)
#'
#' # TolVal2
#' qc_taxa_values_numeric(data_benthos_MBSS, "TOLVAL2", 0, 10)
#'
#' # UFC
#' qc_taxa_values_numeric(data_benthos_MBSS, "UFC", 1, 6)
#'
#' @export
qc_taxa_values_numeric <- function(data,
                                   col_vals = NULL,
                                   valid_min = NULL,
                                   valid_max = NULL) {

  # QC----
  # col_vals, missing
  if (is.null(col_vals)) {
    stop("'col_vals' is missing.", call. = FALSE)
  }## IF ~ col_vals is missing

  ## col_vals in data
  if (!rlang::as_string(col_vals) %in% names(data)) {
    stop("Column '",
         rlang::as_string(col_vals),
         "' is missing from input data.", call. = FALSE)
  }# IF ~ col_vals exists

  ## col_vals is numeric
  if (!is.numeric(data[[rlang::as_string(col_vals)]])) {
    stop("Column '",
         rlang::as_string(col_vals),
         "' must be numeric", call. = FALSE)
  }## IF ~ col_vals is numeric

  ## valid_min, missing
  if (!is.null(valid_min)) {
    stop("'valid_min' is missing.", call. = FALSE)
  }## IF ~ valid_min is missing

  ## valid_max, missing
  if (!is.null(valid_min)) {
    stop("'valid_max' is missing.", call. = FALSE)
  }## IF ~ valid_max is missing

  ## valid_min is numeric
  if (!is.numeric(valid_min)) {
    stop("'valid_min' must be numeric.", call. = FALSE)
  }## IF ~ valid_min is numeric

  ## valid_min is numeric
  if (!is.numeric(valid_min)) {
    stop("'valid_min' must be numeric.", call. = FALSE)
  }## IF ~ valid_min is numeric

  # occurrence----
  df_result <- data |>
    # occurrence
    dplyr::count(.data[[col_vals]], name = "n") |>
    # valid
    ## T/F
    dplyr::mutate(valid = dplyr::case_when(
      .data[[col_vals]] >= valid_min &
        .data[[col_vals]] <= valid_max ~ TRUE,
      .default = FALSE))

  # Result----
  return(df_result)

}## FUNCTION ~ END

