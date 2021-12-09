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

df$SubClass <- NA_character_
df$Elevation_Attr <- NA_character_
df$Gradient_Attr <- NA_character_
df$WSArea_Attr <- NA_character_

# 20211209, change thermal
table(df$Thermal_Indicator)
df$Thermal_Indicator[df$Thermal_Indicator == "cold"] <- "CoreC"
df$Thermal_Indicator[df$Thermal_Indicator == "cold_cool"] <- "Cold"
df$Thermal_Indicator[df$Thermal_Indicator == "cool_warm"] <- "Cool"
df$Thermal_Indicator[df$Thermal_Indicator == "warm"] <- "Warm"
table(df$Thermal_Indicator)

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

