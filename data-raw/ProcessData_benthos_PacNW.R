# Prepare data for example for metric.values()
#
# Erik.Leppo@tetratech.com
# 2020-04-28
# Benthic macroinvertebrate data to use with new Habitat and BCG_attNA metrics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory
# Packages
#library(devtools)

# df.1, date, time, and datetime
# df.2 only datetime (different format)
# df.3 subset (one month) of df.2

# 1. Get data and process#####
# 1.1. Import Data
#myFile <- "data_bio2rarify_500.tsv"
myFile <- "data_benthos_PacNW.tsv"
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
data_benthos_PacNW <- df
usethis::use_data(data_benthos_PacNW, overwrite = TRUE) # change from devtools:: to usethis::

