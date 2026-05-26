# Prepare data for example for metric.values.algae()

# Ben.Block@tetratech.com
# 2021-06-23
# 2022-11-15 (EWL) update for INDEX_CLASS
# 2026-05-26, Add Mertens columns for new metrics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory

# 1. Get data and process#####
# 1.1. Import Data
fn <- "data_IDEM_diatom_mmi_dev.tab"
df <- read.delim(file.path(wd, "data-raw", "data", fn))

# 1.2. Process Data
View(df)
# QC check
dim(df)
# structure
str(df)

# Change integer type to numeric type
#library(dplyr)

df <- df |>
  dplyr::mutate_if(is.integer, as.numeric)

# change date from character to date type
df$COLLDATE <-  as.Date(df$COLLDATE, format =  "%m/%d/%Y")

# structure
str(df)

# Add Phylum
## For test, 2021-11-02, Erik.Leppo@tetratech.com
df$PHYLUM <- NA_character_

# Change Names
names(df)[names(df) %in% "INDEX_REGION"] <- "INDEX_CLASS"

# Mertens columns, 20260526
df <- df |>
  # create temp columns
  dplyr::mutate(temp_1_5 = POLL_TOL) |>
  dplyr::mutate(temp_1_5_x = dplyr::case_when(is.na(temp_1_5) ~ "X",
                                            TRUE ~ as.character(temp_1_5))) |>
  dplyr::mutate(temp_1_4 = dplyr::case_when(temp_1_5 == 5 ~ NA,
                                            TRUE ~ temp_1_5)) |>
  dplyr::mutate(temp_1_7 = dplyr::case_when(TOLVAL <= 7 ~ TOLVAL,
                                            TRUE ~ NA)) |>
  dplyr::mutate(temp_1_6 = dplyr::case_when(TOLVAL <= 6 ~ TOLVAL,
                                            TRUE ~ NA)) |>
  dplyr::mutate(temp_1_6_x = dplyr::case_when(is.na(temp_1_6) ~ "X",
                                              TRUE ~ as.character(temp_1_6))) |>
  # new cols
  dplyr::mutate(PH_MERTENS = temp_1_5_x,
                SALINITY_MERTENS = temp_1_7,
                N_MERTENS = temp_1_4,
                O_MERTENS = temp_1_5,
                SAP_MERTENS = temp_1_5,
                TROPHIC_MERTENS = temp_1_6_x,
                MOISTURE_MERTENS = temp_1_5) |>
  # drop temp cols
  dplyr::select(-c(temp_1_5, temp_1_5_x, temp_1_4, temp_1_7, temp_1_6, temp_1_6_x))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
data_diatom_mmi_dev <- df
usethis::use_data(data_diatom_mmi_dev, overwrite = TRUE)

