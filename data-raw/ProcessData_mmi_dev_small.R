# Prepare data for example for metric.stats()

# Erik.Leppo@tetratech.com
# 2020-05-29
# 2025-09-11, make the dataset smaller
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
df <- read.delim(file.path(wd, "data-raw", "data", fn))

# Add columns for metric.values
col_add <- c("INDEX_NAME", "SUBPHYLUM", "CLASS", "SUBCLASS", "INFRAORDER"
             , "LIFE_CYCLE", "BCG_ATTR", "THERMAL_INDICATOR", "LONGLIVED"
             , "NOTEWORTHY", "FFG2", "HABITAT")
df[, col_add] <- NA_character_
df[, "INDEX_NAME"] <- "TEST"
df[, "TOLVAL2"] <- NA_real_

# #Change class
# for (i in col_add) {
#   class(df[, i]) <- "character"
# }##FOR ~ i ~ END
#
# class(df[, "TOLVAL2"]) <- "numeric"

# 1.2. Process Data
#View(df)
# QC check
#dim(df)
# structure
str(df)

# Make smaller
df |>
  dplyr::filter(Class == "CentralHills") |>
  dplyr::group_by(Unique_ID, Ref_v1, CalVal_Class4) |>
  dplyr::summarize(n = dplyr::n())

# 10 of each Ref (n=3) per CalVal (n=2) = 60
# 272, cut to 45, get the first of each

samp_small <- df |>
  dplyr::filter(Class == "CentralHills") |>
  dplyr::mutate(Grp = paste(Class,
                             CalVal_Class4,
                             Ref_v1,
                             sep = "|")) |>
  dplyr::select(c(Grp, Unique_ID)) |>
  unique() |>
  dplyr::group_by(Grp) |>
  dplyr::slice_head(n = 10)

df_small <- df |>
  dplyr::filter(Unique_ID %in% samp_small$Unique_ID)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
data_mmi_dev_small <- df_small
usethis::use_data(data_mmi_dev_small, overwrite = TRUE)

# document
promptData(data_mmi_dev_small)
