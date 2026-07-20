# Prepare data for example for qc_taxa_names_match()
#
# Erik.Leppo@tetratech.com
# 2026-07-20
# Data from Jen, algae with non-ASCII
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep ----
wd <- getwd() # assume is package directory
# Packages
#library(devtools)

# 1. Get data and process#####
# 1.1. Import Data
myFile <- "_TaxaTranslator_20260717_nonASCII.xlsx"
df <- readxl::read_excel(file.path(wd, "data-raw", "data", myFile))

# 1.2. Process Data
names(df)[names(df) %in% "Taxon orig"] <- "Taxon"
data_algae_names_user <- df[, "Taxon"]
data_algae_names_official <- df

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package----
#
usethis::use_data(data_algae_names_user, overwrite = TRUE)
usethis::use_data(data_algae_names_official, overwrite = TRUE)
# change from devtools:: to usethis::

# document
# promptData(data_algae_names_user)
# promptData(data_algae_names_official)
