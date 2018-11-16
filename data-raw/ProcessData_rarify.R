# Prepare data for example for rarify()
#
# Erik.Leppo@tetratech.com
# 20170926
# Replace MS example with USGS 500 to 300 count data for PacNW BCG
# Use only a few samples for smaller file size
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory
#library(devtools)

# df.1, date, time, and datetime
# df.2 only datetime (different format)
# df.3 subset (one month) of df.2

# 1. Get data and process#####
# 1.1. Import Data
#myFile <- "data_bio2rarify_500.tsv"
myFile <- "data_bio2rarify_3col_600.tsv"
df <- read.delim(file.path(wd,"data-raw",myFile))

# 1.2. Process Data
View(df)
# QC check
dim(df)
# structure
str(df)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
data_bio2rarify <- df
devtools::use_data(data_bio2rarify,overwrite = TRUE)

