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
                                   col_logical = NULL) {

  # QC----
  # col_logical, missing
  if (is.null(col_logical)) {
    stop("'col_logical' is missing.", call. = FALSE)
  }## IF ~ col_logical is missing

  ## col_logical in data
  if (!rlang::as_string(col_logical) %in% names(data)) {
    stop("Column '",
         rlang::as_string(col_logical),
         "' is missing from input data.", call. = FALSE)
  }# IF ~ col_logical exists

  ## col_logical is logical
  if (!is.logical(data[[rlang::as_string(col_logical)]])) {
    stop("Column '",
         rlang::as_string(col_logical),
         "' must be logical", call. = FALSE)
  }## IF ~ col_logical is logical


  # occurrence----
  df_result <- data |>
    # all_of and = not working together in complete
    # rename before and after
    dplyr::rename(value = !!col_logical) |>
    # occurrence
    dplyr::count(value, name = "n") |>
    # force valid value rows to exist
    tidyr::complete(
      value = c(TRUE, FALSE, NA),
      fill = list(n = 0)) |>
    # rename back
    dplyr::rename(!!col_logical := value) |>
    # valid
    dplyr::mutate(valid = dplyr::case_when(
      is.na(.data[[col_logical]]) |
        .data[[col_logical]] %in% c(TRUE, FALSE)~ TRUE,
      .default = FALSE))

  # Result----
  return(df_result)

}## FUNCTION ~ END

