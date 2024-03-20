# Prepare data for metric.values.coral() QC

# Ben.Block@tetratech.com
# 2024-03-18
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory

# 1. Get data and process#####
# 1.1. Import Data
fn <- "data_coral_bcg_metric_qc.tsv"
df <- read.delim(file.path(wd, "data-raw", "data", fn))

# 1.2. Process Data
View(df)
# QC check
dim(df)
# structure
str(df)

# Change integer type to numeric type
#library(dplyr)
`%>%` <- dplyr::`%>%`

df <- df %>%
  dplyr::mutate_if(is.integer, as.numeric)

# structure
str(df)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
data_coral_bcg_metric_qc <- df
usethis::use_data(data_coral_bcg_metric_qc, overwrite = TRUE)

