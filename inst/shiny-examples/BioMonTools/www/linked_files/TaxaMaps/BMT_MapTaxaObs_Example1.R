library(readxl)
library(BioMonTools)

#set working directory
wd <-'C:/Users/Jen.Stamp/Documents/R_code/BioMonTools_4.3.C/TaxaDistribMaps'
setwd(wd)

#this script creates maps in the order in which the taxa appear in your input file.
#I recommend sorting by taxonomy (your preference - e.g., phylum, order, family, genus) before running the script

#df_obs <- read_excel("~/BioMonTools/Maps_Plecop_genus.xlsx")
data_example <- read_excel(file.path(wd, 'MapInput_Example1.xlsx'))

df_obs <- data_example
SampID <- "SampleID"
TaxaID <- "TaxaID"
TaxaCount <- "N_Taxa"
Lat <- "Latitude"
Long <- "Longitude"
output_dir <- getwd()
output_prefix <- "maps.taxa."
output_type <- "pdf"

myDB <- "state"
myRegion <- c("iowa", "nebraska", "kansas", "missouri", "oklahoma", "minnesota")

# Iowa lat/long
x_IA <- c(-(96+38/60), -(90+8/60))
y_IA <- c((40+23/60), (43+30/60))

# Nebraska lat/long
x_NE <- c(-(104+3/60), -(95+19/60))
y_NE <- c((40), (43))

# Kansas lat/long
x_KS <- c(-(102+3/60), -(94+35/60))
y_KS <- c((37), (40))

# Missouri lat/long
x_MO <- c(-(95+46/60), -(89+6/60))
y_MO <- c((36), (40+37/60))

# Oklahoma lat/long
x_OK <- c(-(103), -(94+26/60))
y_OK <- c((33+37/60), (37))

# Minnesota lat/long
x_MN <- c(-(89+29/60), -(97+14/60))
y_MN <- c((43+30/60), (46))

myXlim <- c(min(x_IA, x_NE, x_KS, x_MO, x_OK, x_MN), max(x_IA, x_NE, x_KS, x_MO, x_OK, x_MN))
myYlim <- c(min(y_IA, y_NE, y_KS, y_MO, y_OK, y_MN), max(y_IA, y_NE, y_KS, y_MO, y_OK, y_MN))

df_obs <- as.data.frame(df_obs)
# Run function with extra arguments for map

MapTaxaObs(df_obs, SampID, TaxaID, TaxaCount, Lat, Long
           , database=myDB, regions=myRegion, xlim=myXlim, ylim=myYlim
           , map_grp = "Source")



