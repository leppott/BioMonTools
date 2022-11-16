# Prepare data for example for MapTaxaObs()
#
# Erik.Leppo@tetratech.com
# 20180917
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory
#library(devtools)

# df.1, date, time, and datetime
# df.2 only datetime (different format)
# df.3 subset (one month) of df.2

# 1. Get data and process#####
# 1.1. Import Data
fn <- "data_ELMR_MA_ALL.tsv"
df <- read.delim(file.path(wd, "data-raw", "data", fn))


# 1.2. Process Data
View(df)
# QC check
dim(df)
# structure
str(df)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
data_Taxa_MA <- df
devtools::use_data(data_Taxa_MA, overwrite = TRUE)

