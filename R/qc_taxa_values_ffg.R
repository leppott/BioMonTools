#' QC Functional Feeding Group (FFG) Values
#'
#' Performs basic QC of the FFG column against a list of accepted values.
#'
#' Returns a data frame the values from the input with counts (column = n) from
#' the FFG column and whether the value appeared in valid values (column =
#' valid). Values in the accepted values not appearing in the input are appended
#' to the bottom of the returned data frame.  These values are marked as n = NA
#' and valid = TRUE.
#'
#' The default accepted values are the abbreviations are those used as
#' metric.values(); CF, CG, MH, OM, PA, PI, PR, SC, SH, and XY.  User using FC
#' and GC over CF and CG can modify the accepted values.  Both versions are
#' accepted in metric.values().
#'
#' @param df_data A data frame containing taxa data.
#' @param col_ffg The column containing FFG values.  Default = "FFG"
#' @param valid_vals Accepted values.
#' Default = c(CF, CG, MH, OM ,PA, PH, PI, PR, SC, SH, XY)
#'
#' @return A data frame with col_ffg values, occurrence (n), and if valid (TRUE/
#' FALSE).  Additional values from valid_vals are appended.
#'
#' @examples
#' # Values, Default
#' qc_taxa_values_ffg(data_benthos_PacNW)
#'
#' # Values, User (full names)
#' qc_taxa_values_ffg(data_benthos_MBSS,
#'                    "FFG",
#'                    valid_vals = c("Collector",
#'                                   "Filterer",
#'                                   "Predator",
#'                                   "Scraper",
#'                                   "Shredder"))
#'
#' @export
qc_taxa_values_ffg <- function(df_data,
                               col_ffg = "FFG",
                               valid_vals = c("CF",
                                              "CG",
                                              "MH",
                                              "OM",
                                              "PA",
                                              "PH",
                                              "PI",
                                              "PR",
                                              "SC",
                                              "SH",
                                              "XY")) {

  # QC
  if (!rlang::as_string(col_ffg) %in% names(df_data)) {
    stop("Column '",
         rlang::as_string(col_ffg),
         "' is missing from input data.", call. = FALSE)
  }# IF ~ col_ffg

  # Convert valid_vals to data frame
  df_valid_vals <- as.data.frame(valid_vals)
  names(df_valid_vals) <- col_ffg

  # occurrence
  df_match <- df_data |>
    # occurrence
    dplyr::count(.data[[col_ffg]], name = "n") |>
    # valid
    ## T/F
    dplyr::mutate(valid = .data[[col_ffg]] %in% valid_vals) |>
    ## values
    dplyr::full_join(y = df_valid_vals,
                     by = dplyr::join_by({{col_ffg}})) |>
    ## convert NA to TRUE
    dplyr::mutate(valid = dplyr::case_when(is.na(valid) ~ TRUE,
                                           .default = valid))

  # Result
  return(df_match)

}## FUNCTION ~ END

