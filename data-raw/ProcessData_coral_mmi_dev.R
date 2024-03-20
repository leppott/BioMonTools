# Prepare data for example for metric.values.coral()

# Ben.Block@tetratech.com
# 2024-03-18
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory

# 1. Get data and process#####
# 1.1. Import Data
fn <- "data_coral_bcg_metric_dev.tsv"
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

# change date from character to date type
df$SampDate <-  as.Date(df$SampDate, format =  "%Y-%m-%d")

# change BCG attr to character
df$BCG_ATTR <- as.character(df$BCG_ATTR)

# structure
str(df)

# Change Names
df$INDEX_NAME <- "CORAL_TEST"
df$INDEX_CLASS <- "CORAL_TEST"

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
data_coral_bcg_metric_dev <- df
usethis::use_data(data_coral_bcg_metric_dev, overwrite = TRUE)

