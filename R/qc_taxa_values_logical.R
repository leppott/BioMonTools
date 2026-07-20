#' QC Autecological Logical Values
#'
#' Performs basic QC of a logical column showing occurrence.
#'
#' Returns a data frame of the values from the input with counts (column = n) by
#' column.
#'
#' @param data A data frame containing autecological taxa data.
#' @param col_vals The column containing the logical value.
#'
#' @return A data frame with col_vals values, occurrence (n), and valid
#' (TRUE/FALSE).  Missing values (TRUE, FALSE, or NA) are appended.
#'
#' @examples
#' # Exclude
#' qc_taxa_values_logical(data_benthos_MBSS, "EXCLUDE")
#'
#' # NonTarget
#' qc_taxa_values_logical(data_benthos_MBSS, "NONTARGET")
#'
#' @export
qc_taxa_values_logical <- function(data,
                                   col_vals = NULL) {

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

  ## col_vals is logical
  if (!is.logical(data[[rlang::as_string(col_vals)]])) {
    stop("Column '",
         rlang::as_string(col_vals),
         "' must be logical", call. = FALSE)
  }## IF ~ col_vals is logical


  # occurrence----
  df_result <- data |>
    # all_of and = not working together in complete
    # rename before and after
    dplyr::rename(value = !!col_vals) |>
    # occurrence
    dplyr::count(value, name = "n") |>
    # force valid value rows to exist
    tidyr::complete(
      value = c(TRUE, FALSE, NA),
      fill = list(n = 0)) |>
    # rename back
    dplyr::rename(!!col_vals := value) |>
    # valid
    dplyr::mutate(valid = dplyr::case_when(
      is.na(.data[[col_vals]]) |
        .data[[col_vals]] %in% c(TRUE, FALSE)~ TRUE,
      .default = FALSE))

  # Result----
  return(df_result)

}## FUNCTION ~ END

