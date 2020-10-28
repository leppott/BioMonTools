# Prepare data for example for metric.values()
#
# Erik.Leppo@tetratech.com
# 2020-10-28
# Benthic macroinvertebrate data from MBSStools::taxa_bugs_genus
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory
# Packages
#library(devtools)

# 1. Get data and process#####
# 1.1. Import Data
myFile <- "data_taxa_bugs_genus_MBSS.tab"
df <- read.delim(file.path(wd,"data-raw",myFile))

# Replace "." with "_"
df$Index.Name <- gsub("\\.", "_", df$Index.Name)

# 1.2. Process Data
View(df)
# QC check
dim(df)
# structure
str(df)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
data_benthos_MBSS <- df
usethis::use_data(data_benthos_MBSS, overwrite = TRUE) # change from devtools:: to usethis::

