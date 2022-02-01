# Create HTML files for use with Shiny app
# Erik.Leppo@tetratech.com
# 2022-01-11
#~~~~~~~~~~~~~~


# Packages
# libary(rmarkdown)

# Files
path_rmd <- "data-raw/RMD"
myFiles <- list.files(path = path_rmd
                      , pattern = "^ShinyHTML_.+\\.Rmd$"
                      , full.names = TRUE)
#myFiles <- myFiles[!myFiles %in% "data-raw/RMD/ShinyHTML_About.Rmd"]

# Loop over files


# Render as HTML
path_shiny_www <- file.path("inst", "shiny-examples", "BioMonTools", "www")


for(i in myFiles) {
  # file name w/o extension
  #i_fn <- tools::file_path_sans_ext(basename(i))
  # save to HTML
  rmarkdown::render(input = i
                    , output_dir = path_shiny_www
                #, output_format = rmarkdown::html_fragment(df_print = "kable")
                )
}## FOR ~ i

shell.exec(normalizePath(path_shiny_www))

#______________________________
# Run manually.
# otherwise tables don't get lines.
shell.exec(normalizePath(path_rmd))


# Move
# myFiles <- list.files(path = path_rmd
#                      , pattern = "^ShinyHTML.+\\.html$"
#                      , full.names = TRUE)
# Only About
myFile <- list.files(path = path_rmd
                     , pattern = "^ShinyHTML_About\\.html$"
                     , full.names = TRUE)

file.copy(myFile
          , file.path(path_shiny_www, basename(myFile))
          , overwrite = TRUE)
