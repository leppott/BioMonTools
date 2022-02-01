library(readxl)
library(knitr)
library(dplyr)
library(lazyeval)
library(BioMonTools)

wd <-'C:/Users/Jen.Stamp/Documents/R_code/BioMonTools_4.3.C/MarkExclude'
setwd(wd)
df_data <- read_excel("~/R_code/BioMonTools_4.3.C/MarkExclude/Input1_MarkExclude_20210817.xlsx")

SampID     <- "SampleID"
TaxaID     <- "TaxaID"
TaxaCount  <- "N_Taxa"
Exclude    <- "Exclude"
TaxaLevels <- c("Phylum"
                , "Class"
                , "Order"
                , "Family"
                , "SubFamily"
                , "Tribe"
                , "Genus")

Exceptions <- NA

df_example <- markExcluded(df_data, SampID="SampleID", TaxaID="TaxaID", TaxaCount= "N_Taxa"
                           , Exclude="Exclude", TaxaLevels=TaxaLevels, Exceptions=NA)

write.csv(df_example, "Output1_wExclude_20210817.csv")
