# Prepare data for example for metric.values()
#
# Erik.Leppo@tetratech.com
# 2020-10-28
# Benthic macroinvertebrate data from MBSStools::taxa_bugs_genus
# 2022-11-15 (EWL) update for INDEX_CLASS and missing columns, other changes
#   date is obfuscated to date of admission to the Union for MD
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep ----
wd <- getwd() # assume is package directory
# Packages
#library(devtools)

# 1. Get data and process#####
# 1.1. Import Data
myFile <- "data_taxa_bugs_genus_MBSS.tab"
df <- read.delim(file.path(wd, "data-raw", "data", myFile))

# 1.2. Process Data

# Replace "." with "_"
df$Index.Name <- gsub("\\.", "_", df$Index.Name)

# Add columns so don't get any warnings
col2add_char <- c("SUBPHYLUM", "SUBCLASS", "INFRAORDER", "SUBFAMILY"
                  , "LIFE_CYCLE", "BCG_ATTR"
                  , "THERMAL_INDICATOR", "LONGLIVED", "NOTEWORTHY", "FFG2"
                  , "HABITAT", "ELEVATION_ATTR", "GRADIENT_ATTR", "WSAREA_ATTR"
                  , "HABSTRUCT", "BCG_ATTR2")
col2add_logic <- c("NONTARGET", "LONGLIVED", "NOTEWORTHY", "AIRBREATHER")
df[, col2add_char] <- NA_character_
df[, col2add_logic] <- NA

# Change Names
names(df)[names(df) %in% "Index.Name"] <- "INDEX_NAME"
names(df)[names(df) %in% "strata_r"] <- "INDEX_CLASS"
names(df)[names(df) %in% "SITE"] <- "SAMPLEID"
names(df)[names(df) %in% "TAXON"] <- "TAXAID"
names(df)[names(df) %in% "FinalTolVal07"] <- "TOLVAL"
names(df)[names(df) %in% "FinalTolVal08"] <- "TOLVAL2" # FamTV?

df$NONTARGET <- FALSE
df[df$EXCLUDE == "Y", "EXCLUDE"] <- TRUE
df[df$EXCLUDE == "", "EXCLUDE"] <- FALSE
df$EXCLUDE <- as.logical(df$EXCLUDE)


# View
# View(df)
# # QC check
# dim(df)
# structure
str(df)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package----
#
data_benthos_MBSS <- df
usethis::use_data(data_benthos_MBSS, overwrite = TRUE)
# change from devtools:: to usethis::
