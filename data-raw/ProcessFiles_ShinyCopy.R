# BioMonTools
# Process Files - Copy to Shiny
# Erik.Leppo@tetratech.com
# 2022-01-28
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Copy some files from package to Shiny folders


# Copy Metrics File to Shiny www for download
fn_metrics <- "MetricNames.xlsx"
pn_metrics <- file.path(system.file(package="BioMonTools")
                        , "extdata"
                        , fn_metrics)

dn_shiny <- file.path(".", "inst", "shiny-examples", "BioMonTools", "www")
pn_shiny <- file.path(dn_shiny, fn_metrics)

file.copy(pn_metrics, pn_shiny, overwrite = TRUE)
