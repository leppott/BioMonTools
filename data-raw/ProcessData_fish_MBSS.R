# Prepare data for example for MMIcalc.R;
#
# Sample Taxa - Fish
#
# Erik.Leppo@tetratech.com
# 20170601
# 20170907, modify file for just MBSS
# 20211210, copied from MBSStools package
# 2022-11-15 (EWL) update for INDEX_CLASS
# 2023-12-11, EXCLUDE to logical not character
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep ####
# library(devtools)
wd <- getwd()
# assume wd is package root

# 1. Get data and Process #####
# 1.1. Import Data
myFile <- "data_taxa_fish_MBSS.tab"
data.import <- read.delim(file.path(".", "data-raw", "data", myFile))
# 1.2. Process Data
#View(data.import)
names(data.import)
dim(data.import)

# # 20190501, Convert EXCLUDE from Y/NULL to TRUE/FALSE
# str(data.import)
# table(data.import$EXCLUDE, useNA = "ifany")
# data.import[,"EXCLUDE"] <- as.character(data.import[,"EXCLUDE"] )
# data.import[data.import[,"EXCLUDE"]=="Y", ] <- "TRUE"
# data.import[data.import[,"EXCLUDE"]=="", ] <- "FALSE"
# data.import[is.na(data.import[,"EXCLUDE"]), ] <- "FALSE"
# table(data.import$EXCLUDE, useNA = "ifany")
# data.import[,"EXCLUDE"] <- as.logical(data.import[,"EXCLUDE"] )
# table(data.import$EXCLUDE, useNA = "ifany")
# str(data.import)
data.import$EXCLUDE <- FALSE
data.import$BCG_ATTR <- NA_character_


# 20211210, Munge Columns
## col, rename
names(data.import)[names(data.import) == "SITE"] <- "SAMPLEID"
names(data.import)[names(data.import) == "Index.Name"] <- "INDEX_NAME"
names(data.import)[names(data.import) == "FIBISTRATA"] <- "INDEX_CLASS"
names(data.import)[names(data.import) == "SPECIES"] <- "TAXAID"
names(data.import)[names(data.import) == "TOTAL"] <- "N_TAXA"
names(data.import)[names(data.import) == "LEN_SAMP"] <- "SAMP_LENGTH_M"
names(data.import)[names(data.import) == "AVWID"] <- "SAMP_WIDTH_M"
names(data.import)[names(data.import) == "NATIVE_MBSS"] <- "NATIVE"
names(data.import)[names(data.import) == "TOTBIOM"] <- "SAMP_BIOMASS"
names(data.import)[names(data.import) == "TROPHIC_MBSS"] <- "TROPHIC"
names(data.import)[names(data.import) == "PTOLR"] <- "TOLER"
# col, add
data.import$INDEX_NAME <- "MBSS_2005_Fish"
data.import$DA_MI2 <- round(data.import$ACREAGE / 640, 2)
data.import$N_ANOMALIES <- 0
col.req.char <- c("FAMILY", "GENUS", "BCG_ATTR", "THERMAL_INDICATOR"
                  , "ELEVATION_ATTR", "GRADIENT_ATTR", "WSAREA_ATTR"
                  , "REPRODUCTION", "HABITAT", "CONNECTIVITY", "SCC")
data.import[, col.req.char] <- NA_character_
col.req.logical <- c("HYBRID", "CONNECTIVITY", "SCC")
data.import[, col.req.logical] <- NA

# col, remove
col_remove <- c("PIRHALLA", "ACREAGE")
data.import <- data.import[, !(names(data.import) %in% col_remove)]



# QC column names from metric.values()
col.req <- c("SAMPLEID", "TAXAID", "N_TAXA", "N_ANOMALIES", "SAMP_BIOMASS"
             , "INDEX_NAME", "INDEX_CLASS"
             , "DA_MI2", "SAMP_WIDTH_M", "SAMP_LENGTH_M"
             , "TYPE", "TOLER", "NATIVE", "TROPHIC", "SILT"
             , "FAMILY", "GENUS", "HYBRID", "THERMAL_INDICATOR"
             , "ELEVATION_ATTR", "GRADIENT_ATTR", "WSAREA_ATTR"
             , "REPRODUCTION", "HABITAT", "CONNECTIVITY", "SCC"
)

# Missing columns
## both should be character(0)
col.req[!col.req %in% names(data.import)]
names(data.import)[!names(data.import) %in% col.req]

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for Use in Package ####
#
data_fish_MBSS <- data.import
usethis::use_data(data_fish_MBSS, overwrite = TRUE)


