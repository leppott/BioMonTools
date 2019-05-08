## ----rmd_setup, include = FALSE------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----data, echo=FALSE----------------------------------------------------
# Packages
library(BioMonTools)
library(knitr)
# Data
df_obs <- data_Taxa_MA
# Show
kable(head(df_obs))

## ----Ex_gg, fig.width = 6------------------------------------------------
# Packages
library(BioMonTools)
library(ggplot2)
library(knitr)

#
df_obs <- data_Taxa_MA

TaxaID <- "TaxaName"
TaxaCount <- "Count"
Lat <- "Latitude"
Long <- "Longitude"




myTaxa <- "ALEWIFE, ADULTS, 0.5-25 ppt"

df_map <- subset(df_obs, df_obs[,TaxaID]==myTaxa)

myDB <- "state"
myRegion <- "massachusetts"

Lat <- "Latitude"
Long <- "Longitude"




# Base Map
m1 <- ggplot(data=subset(map_data(myDB), region %in% c(myRegion))) + 
          geom_polygon(aes(x=long, y=lat, group=group), fill="light gray", color="black") + 
          coord_fixed(1.3) + theme_void()
# Add points (all)
m1 <- m1 + geom_point(data=df_obs, aes(df_obs[,Long], df_obs[,Lat]), fill=NA, color="gray")
# Add points (Taxa)
m1 <- m1 + geom_point(data=df_map, aes(df_map[,Long], df_map[,Lat]), color="blue")
# Map Title (center)
m1 <- m1 + labs(title=myTaxa) + theme(plot.title = element_text(hjust = 0.5))
# Caption (left justified)
m1 <- m1 + labs(caption="caption1 \n caption2") + theme(plot.caption = element_text(hjust=0))

# Show Results
kable(head(df_obs))
m1

## ----Ex_PDF, eval=FALSE--------------------------------------------------
#  df_obs <- data_Taxa_MA
#  SampID <- "estuary"
#  TaxaID <- "TaxaName"
#  TaxaCount <- "Count"
#  Lat <- "Latitude"
#  Long <- "Longitude"
#  output_dir <- getwd()
#  output_prefix <- "maps.taxa."
#  output_type <- "pdf"
#  
#  myDB <- "state"
#  myRegion <- "massachusetts"
#  myXlim     <- c(-(73+(30/60)), -(69+(56/60)))
#  myYlim     <- c((41+(14/60)),(42+(53/60)))
#  
#  # Run function with extra arguments for map
#  MapTaxaObs(df_obs, SampID, TaxaID, TaxaCount, Lat, Long
#             , output_dir, output_prefix, output_type
#             , database="state", regions="massachusetts", xlim=myXlim, ylim=myYlim)
#  

