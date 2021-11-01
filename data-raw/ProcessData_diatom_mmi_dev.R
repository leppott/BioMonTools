# Prepare data for example for metric.values.algae()

# Ben.Block@tetratech.com
# 2021-06-23
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory

# 1. Get data and process#####
# 1.1. Import Data
fn <- "data_IDEM_diatom_mmi_dev.tab"
df <- read.delim(file.path(wd, "data-raw", fn))

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
df$COLLDATE <-  as.Date(df$COLLDATE, format =  "%m/%d/%Y")

# structure
str(df)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
data_diatom_mmi_dev <- df
usethis::use_data(data_diatom_mmi_dev, overwrite = TRUE)

