#' @title run Shiny Example
#'
#' @description Launches Shiny app for BioMonTools package.
#'
#' @details The Shiny app based on the R package BioMonTools is included in the
#' R package. This function launches that app.
#'
#' The Shiny app is online at:
#' https://tetratech-wtr-wne.shinyapps.io/BioMonTools/
#'
#' @examples
#' \dontrun{
#' # Run Function
#' runShiny()
#' }
#
#' @export
runShiny <- function(){##FUNCTION.START
  #
  appDir <- system.file("shiny-examples", "BioMonTools", package = "BioMonTools")
  #
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `BioMonTools`."
         , call. = FALSE)
  }
  #
  shiny::runApp(appDir, display.mode = "normal")
  #
}##FUNCTION.END
