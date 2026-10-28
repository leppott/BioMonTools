# Prepare data for example for metric.values()
#
# Erik.Leppo@tetratech.com
# 2026-10-22
# Lake Macrophyte data from project from Ben B
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep ----
wd <- getwd() # assume is package directory
# Packages
#library(devtools)

# 1. Get data and process#####
# 1.1. Import Data
myFile <- "macrophytes_samps.csv"
df <- read.csv(file.path(wd, "data-raw", "data", myFile))

# 1.2. Process Data

# add missing required columns
df$INDEX_NAME <- "MACROPHYTES"
df$INDEX_CLASS <- "US_NE"

# Add columns so don't get any warnings
# col2add_char <- c(NA)
col2add_logic <- c("NONTARGET", "EXCLUDE")
# df[, col2add_char] <- NA_character_
df[, col2add_logic] <- NA

df$NONTARGET <- FALSE
df$EXCLUDE <- FALSE

# Remove extra columns
df$match.traits <- NULL

# View
# View(df)
# # QC check
# dim(df)
# structure
str(df)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package----
#
data_macrophytes <- df
usethis::use_data(data_macrophytes, overwrite = TRUE)
# change from devtools:: to usethis::

# documentation
promptData(data_macrophytes)

