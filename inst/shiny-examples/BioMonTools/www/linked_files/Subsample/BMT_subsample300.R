library(BioMonTools)
library(knitr)

# set directory, upload data frame
wd <- file.path("C:/Users/Jen.Stamp/Documents/R_code/BioMonTools_4.3.C/Subsample")
myFile <- "Input_Example1.csv"
df_biodata <- read.csv(file.path(wd,myFile))

# set number of organisms and seed (if you want reproducible results; otherwise each run is random and will be different)
mySize <- 300
Seed_US <- 17760704
bugs_mysize <- rarify(inbug=df_biodata, sample.ID="SampleID"
                      ,abund="N_Taxa",subsiz=mySize, mySeed=Seed_US)

# Save output
myFile.Out <- paste0("Output1_Rarify_",mySize,".csv")
write.csv(bugs_mysize, file=myFile.Out, row.names=FALSE)

#read in the rarified csv
myFile <- "Output1_Rarify_300.csv"
bugs_mysize <- read.csv(file.path(wd,myFile))

# create pre- and post- subsample comparison data frame
df_compare <- merge(df_biodata, bugs_mysize, by=c("SampleID", "TaxaID")
                    , suffixes = c("_Orig","_300"))
# compare totals
tbl_totals <- aggregate(cbind(N_Taxa_Orig, N_Taxa_300) ~ SampleID, df_compare, sum)
kable(head(tbl_totals), caption = "Comparison, sample totals")

# Compare pre- and post- subsample taxa counts
df_compare <- df_compare[,c("SampleID", "TaxaID", "N_Taxa_Orig", "N_Taxa_300")]
kable(head(df_compare), caption = "Comparison, by Sample")

# Save output
write.csv(df_compare, paste("Compare",mySize,"csv",sep="."))

