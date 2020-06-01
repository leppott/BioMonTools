# Prepare data for example for metric.stats()

# Erik.Leppo@tetratech.com
# 2020-05-29
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory
#library(devtools)

# df.1, date, time, and datetime
# df.2 only datetime (different format)
# df.3 subset (one month) of df.2

# 1. Get data and process#####
# 1.1. Import Data
fn <- "data_mmi_dev.tsv"
df <- read.delim(file.path(wd, "data-raw", fn))

# Add columns for metric.values
col_add <- c("INDEX_NAME", "SUBPHYLUM", "CLASS", "SUBCLASS", "INFRAORDER"
             , "LIFE_CYCLE", "BCG_ATTR", "THERMAL_INDICATOR", "LONGLIVED"
             , "NOTEWORTHY", "FFG2", "TOLVAL2", "HABITAT")
df[, col_add] <- NA
df[, "INDEX_NAME"] <- "TEST"

#Change class
for (i in col_add) {
  class(df[, i]) <- "character"
}##FOR ~ i ~ END

class(df[, "TOLVAL2"]) <- "numeric"

# 1.2. Process Data
View(df)
# QC check
dim(df)
# structure
str(df)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
data_mmi_dev <- df
usethis::use_data(data_mmi_dev, overwrite = TRUE)

