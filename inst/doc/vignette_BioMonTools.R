## ----rmd_setup, include = FALSE------------------------------------------
library(knitr)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----Pkg_Install, eval=FALSE---------------------------------------------
#  # Installing the BioMonTools library (with the vignette) from GitHub
#  library(devtools)
#  install_github("leppott/BioMonTools", force=TRUE, build_vignettes=TRUE)

## ----Pkg_Help, eval=FALSE------------------------------------------------
#  help(package="BioMonTools")

## ----MetricValues_Keep2----------------------------------------------------------------------------
# Packages
library(readxl)
library(knitr)
library(BioMonTools)

# Load Data
df.data <- read_excel(system.file("./extdata/Data_Benthos.xlsx"
                                       , package="BioMonTools"))
# Columns to keep
myCols <- c("Area_mi2", "SurfaceArea", "Density_m2", "Density_ft2")

# Run Function
df.metval <- metric.values(df.data, "bugs", fun.cols2keep=myCols)
# Metrics of Interest
## thermal indicator (_ti_)
#names(df.metval)[grepl("_ti_", names(df.metval))]
col.met2keep <- c("ni_total", "nt_total", "nt_ti_c", "nt_ti_cc", "nt_ti_cw"
                , "nt_ti_w", "pi_ti_c", "pi_ti_cc", "pi_ti_cw", "pi_ti_w"
                , "pt_ti_c", "pt_ti_cc", "pt_ti_cw", "pt_ti_w")
col.ID <- c("SAMPLEID", toupper(myCols), "INDEX_NAME", "SITE_TYPE")
# Ouput
df.metval.ci <- df.metval[, c(col.ID, col.met2keep)]
# RMD table
kable(head(df.metval.ci), caption = "Metric Calculation, select metrics")

## ----Excl01----------------------------------------------------------------------------------------
# Packages
library(readxl)
library(dplyr)
library(lazyeval)
library(knitr)

# Data
df_samps_bugs <- read_excel(system.file("./extdata/Data_Benthos.xlsx"
                                        , package="BioMonTools"), guess_max=10^6)

# Variables
SampID     <- "SampleID"
TaxaID     <- "TaxaID"
TaxaCount  <- "N_Taxa"
Exclude    <- "Exclude_New"
TaxaLevels <- c("Kingdom"
                , "Phylum"
                , "SubPhylum"
                , "Class"
                , "SubClass"
                , "Order"
                , "SubOrder"
                , "Family"
                , "SubFamily"
                , "Tribe"
                , "Genus"
                , "SubGenus"
                , "Species"
                , "Variety")
# Taxa that should be treated as equivalent
Exceptions <- data.frame("TaxaID"=c("Sphaeriidae"), "PhyloID"=c("Pisidiidae"))

# Filter Data
# df_samptax <- filter(df_samps_bugs, !!as.name(SampID) == "08BEA3478__2013-08-21_0")
# df_tst_small <- markExcluded(df_samptax, SampID, TaxaID, TaxaCount, TaxaLevels, Exceptions, Exclude)

# EXAMPLE 1
df_tst <- markExcluded(df_samps_bugs, SampID="SampleID", TaxaID="TaxaID", TaxaCount = "N_Taxa"
                       , Exclude="Exclude_New", TaxaLevels=TaxaLevels, Exceptions=Exceptions)

# Compare
df_compare <- dplyr::summarise(dplyr::group_by(df_tst, SampleID)
                               , Exclude_Import=sum(Exclude)
                               , Exclude_R=sum(Exclude_New))
df_compare$Diff <- df_compare$Exclude_Import - df_compare$Exclude_R
#
tbl_diff <- table(df_compare$Diff)
#kable(tbl_diff)
# sort
df_compare <- df_compare %>% arrange(desc(Diff))

# Number with issues
#sum(abs(df_compare$Diff))
# total samples
#nrow(df_compare)

# confusion matrix
tbl_results <- table(df_tst$Exclude, df_tst$Exclude_New, useNA = "ifany")
#
# Show differences
kable(tbl_results, caption="Confusion Matrix")
# samples with differences
samp_diff <- as.data.frame(df_compare[df_compare[,"Diff"]!=0, "SampleID"])
# results for only those with differences
df_tst_diff <- df_tst[df_tst[,"SampleID"] %in% samp_diff$SampleID, ]
# add diff field
df_tst_diff$Exclude_Diff <- df_tst_diff$Exclude - df_tst_diff$Exclude_New

# Classification Performance Metrics
class_TP <- tbl_results[2,2] # True Positive
class_FN <- tbl_results[2,1] # False Negative
class_FP <- tbl_results[1,2] # False Positive
class_TN <- tbl_results[1,1] # True Negative
class_n <- sum(tbl_results)  # total
#
# sensitivity (recall); TP / (TP+FN); measure model to ID true positives
class_sens <- class_TP / (class_TP + class_FN)
# precision; TP / (TP+FP); accuracy of model positives
class_prec <- class_TP / (class_TP + class_FP)
# specifity; TN / (TN + FP); measure model to ID true negatives
class_spec <- class_TN  / (class_TN + class_FP)
# overall accuracy; (TP + TN) / all cases; accuracy of all classifications
class_acc <- (class_TP + class_TN) / class_n
# F1; 2 * (class_prec*class_sens) / (class_prec+class_sens)
## balance of precision and recall
class_F1 <- 2 * (class_prec * class_sens) / (class_prec + class_sens)
#
results_names <- c("Sensitivity (Recall)", "Precision", "Specificity", "OVerall Accuracy", "F1")
results_values <- c(class_sens, class_prec, class_spec, class_acc, class_F1)
#
tbl_class <- data.frame(results_names, results_values)
names(tbl_class) <- c("Performance Metrics", "Percent")
tbl_class$Percent <- round(tbl_class$Percent *100, 2)
kable(tbl_class, caption="Classification Performance Metrics")


## ----Excl02----------------------------------------------------------------------------------------
# Packages
library(readxl)
library(dplyr)
library(lazyeval)
library(knitr)

# Data
df_samps_bugs <- read_excel(system.file("./extdata/Data_Benthos.xlsx"
                                        , package="BioMonTools"), guess_max=10^6)

# Variables
SampID     <- "SampleID"
TaxaID     <- "TaxaID"
TaxaCount  <- "N_Taxa"
Exclude    <- "Exclude_New"
TaxaLevels <- c("Kingdom"
                , "Phylum"
                , "SubPhylum"
                , "Class"
                , "SubClass"
                , "Order"
                , "SubOrder"
                , "Family"
                , "SubFamily"
                , "Tribe"
                , "Genus"
                , "SubGenus"
                , "Species"
                , "Variety")
# Taxa that should be treated as equivalent
Exceptions <- NA

# EXAMPLE 2
## No Exceptions

df_tst2 <- markExcluded(df_samps_bugs, SampID="SampleID", TaxaID="TaxaID", TaxaCount = "N_Taxa"
                        , Exclude="Exclude_New", TaxaLevels=TaxaLevels, Exceptions=NA)

# Compare
df_compare2 <- dplyr::summarise(dplyr::group_by(df_tst2, SampleID)
                               , Exclude_Import=sum(Exclude)
                               , Exclude_R=sum(Exclude_New))
df_compare2$Diff <- df_compare2$Exclude_Import - df_compare2$Exclude_R
#
tbl_diff2 <- table(df_compare2$Diff)
#kable(tbl_diff2)
# sort
df_compare2 <- df_compare2 %>% arrange(desc(Diff))

# Number with issues
#sum(abs(df_compare2$Diff))
# total samples
#nrow(df_compare2)

# confusion matrix
tbl_results2 <- table(df_tst2$Exclude, df_tst2$Exclude_New, useNA = "ifany")
#
# Show differences
kable(tbl_results2, caption="Confusion Matrix")
knitr::kable(df_compare2[1:10, ])
knitr::kable(tail(df_compare2))
# samples with differences
(samp_diff2 <- as.data.frame(df_compare2[df_compare2[,"Diff"]!=0, "SampleID"]))
# results for only those with differences
df_tst_diff2 <- filter(df_tst2, SampleID %in% samp_diff2$SampleID)
# add diff field
df_tst_diff2$Exclude_Diff <- df_tst_diff2$Exclude - df_tst_diff2$Exclude_New

# Classification Performance Metrics
class_TP2 <- tbl_results2[2,2] # True Positive
class_FN2 <- tbl_results2[2,1] # False Negative
class_FP2 <- tbl_results2[1,2] # False Positive
class_TN2 <- tbl_results2[1,1] # True Negative
class_n2 <- sum(tbl_results2)  # total
#
# sensitivity (recall); TP / (TP+FN); measure model to ID true positives
class_sens2 <- class_TP2 / (class_TP2 + class_FN2)
# precision; TP / (TP+FP); accuracy of model positives
class_prec2 <- class_TP2 / (class_TP2 + class_FP2)
# specifity; TN / (TN + FP); measure model to ID true negatives
class_spec2 <- class_TN2 / (class_TN2 + class_FP2)
# overall accuracy; (TP + TN) / all cases; accuracy of all classifications
class_acc2 <- (class_TP2 + class_TN2) / class_n2
# F1; 2 * (class_prec*class_sens) / (class_prec+class_sens)
## balance of precision and recall
class_F12 <- 2 * (class_prec2 * class_sens2) / (class_prec2 + class_sens2)
#
results_names2 <- c("Sensitivity (Recall)", "Precision", "Specificity", "OVerall Accuracy", "F1")
results_values2 <- c(class_sens2, class_prec2, class_spec2, class_acc2, class_F12)
#
tbl_class2 <- data.frame(results_names2, results_values2)
names(tbl_class2) <- c("Performance Metrics", "Percent")
tbl_class2$Percent <- round(tbl_class2$Percent *100, 2)
kable(tbl_class2, caption="Classification Performance Metrics")

## ----rarify----------------------------------------------------------------------------------------
# Subsample to 500 organisms (from over 500 organisms) for 12 samples.

# load bio data
df_biodata <- data_bio2rarify
#dim(df_biodata)
#kable(head(df_biodata))

# subsample
mySize <- 500
Seed_OR <- 18590214
Seed_WA <- 18891111
Seed_US <- 17760704
bugs_mysize <- rarify(inbug=df_biodata, sample.ID="SampleID"
                     ,abund="N_Taxa",subsiz=mySize, mySeed=Seed_US)

# view results
#dim(bugs_mysize)
#kable(head(bugs_mysize))

# Compare pre- and post- subsample counts
df_compare <- merge(df_biodata, bugs_mysize, by=c("SampleID", "TaxaID")
                    , suffixes = c("_Orig","_500"))
df_compare <- df_compare[,c("SampleID", "TaxaID", "N_Taxa_Orig", "N_Taxa_500")]
kable(head(df_compare), caption = "Comparison, by Sample")

# compare totals
tbl_totals <- aggregate(cbind(N_Taxa_Orig, N_Taxa_500) ~ SampleID, df_compare, sum)
kable(head(tbl_totals), caption = "Comparison, sample totals")

## Not run: 
# save the data
#write.table(bugs_mysize, paste("bugs",mySize,"txt",sep="."),sep="\t")
## End(Not run)

