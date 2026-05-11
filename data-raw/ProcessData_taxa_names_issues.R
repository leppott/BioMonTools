# Prepare data for examples for qc_taxa_names_proof
# From multiple datasets
# Examples of different issues to be caught by function
#
# Erik.Leppo@tetratech.com
# 20260415
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Packages ----
library(readxl)
library(usethis)

# Data ----
dn_data <- file.path("data-raw", "Data")
fn_data <- "BMT_Examples_Taxa_Issues.xlsx"
sh_data <- "example_name_issues"
skip_data <- 7
df_data <- readxl::read_excel(file.path(dn_data, fn_data),
                              sheet = sh_data,
                              skip = skip_data)

# Save ----
data_taxa_names_issues <- df_data
usethis::use_data(data_taxa_names_issues, overwrite = TRUE)

