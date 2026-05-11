#' QC Autecological Numeric Values
#'
#' Performs basic QC of a numeric column showing all values.
#'
#' Returns a data frame the values from the input with counts (column = n) from
#' the column.  Given valid_min and valid_max are applied to each values and
#' evaluated as valid TRUE or FALSE.
#'
#' The accepted values for TolVal are 0 - 10.
#'
#' The BioMonTools accepted values for UFC are 1 - 6.
#'
#' @param data A data frame containing autecological taxa data.
#' @param col_vals The column containing Tolerance Values.  Default = NA.
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
                                  col_numeric = NULL,
                                  valid_min = NULL,
                                  valid_max = NULL) {

  # QC----
  # col_numeric, missing
  if (is.null(col_numeric)) {
    stop("'col_numeric' is missing.", call. = FALSE)
  }## IF ~ col_numeric is missing

  ## col_numeric in data
  if (!rlang::as_string(col_numeric) %in% names(data)) {
    stop("Column '",
         rlang::as_string(col_numeric),
         "' is missing from input data.", call. = FALSE)
  }# IF ~ col_numeric exists

  ## col_numeric is numeric
  if (!is.numeric(data[[rlang::as_string(col_numeric)]])) {
    stop("Column '",
         rlang::as_string(col_numeric),
         "' must be numeric", call. = FALSE)
  }## IF ~ col_numeric is numeric

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
    dplyr::count(.data[[col_numeric]], name = "n") |>
    # valid
    ## T/F
    dplyr::mutate(valid = dplyr::case_when(
      .data[[col_numeric]] >= valid_min &
        .data[[col_numeric]] <= valid_max ~ TRUE,
      .default = FALSE))

  # Result----
  return(df_result)

}## FUNCTION ~ END

