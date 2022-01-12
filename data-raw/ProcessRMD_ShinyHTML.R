# Create HTML files for use with Shiny app
# Erik.Leppo@tetratech.com
# 2022-01-11
#~~~~~~~~~~~~~~


# Packages
# libary(rmarkdown)

# Files
myFiles <- list.files(path = "data-raw/RMD"
                      , pattern = "^ShinyHTML"
                      , full.names = TRUE)

# Loop over files


# Render as HTML
path_shiny_www <- file.path(".", "inst", "shiny-examples", "BioMonTools", "www")


for(i in myFiles) {
  # file name w/o extension
  #i_fn <- tools::file_path_sans_ext(basename(i))
  # save to HTML
  rmarkdown::render(input = i
                    , output_dir = path_shiny_www)
}## FOR ~ i

shell.exec(normalizePath(path_shiny_www))
