#' Taxa Observation Maps
#'
#' Map taxonomic observations from a data frame.  Input a dataframe with SampID, TaxaID, 
#' TaxaCount, Latitude, and Longitude.
#' Other arguments are format (jpg vs. pdf), file name prefix, and output directory.  
#' Files are saved with the prefix "map.taxa." by default.
#' 
#' The user will pass arguments for maps::map function that is used for the map.
#' For example, 'database' and 'regions'.  Without these arguments no map will be created.
#' 
#' The map will have all points and colored points for each taxon.  
#' In addition the map will include the number of samples by taxon.
#'
#' The example data is fish but can be used for benthic macroinvertebrates as well.
#'
#' The R package maps is required for this function.
#'
#' @param df_obs Observation data frame
#' @param SampID df_obs column name, Unique Sample identifier
#' @param TaxaID df_obs column name, Unique Taxa identifier
#' @param TaxaCount df_obs column name, Number of individuals for TaxonID and SampID
#' @param Lat df_obs column name, Latitude
#' @param Long df_obs column name, Longitude
#' @param output_dir Directory to save output.  Default is working directory.
#' @param output_prefix Prefix to TaxaID for each file.  Default = "map.taxa."
#' @param output_type File format for output; jpg or pdf.
# @param map_db maps::map function database; world, usa, state, county
# @param map_regions maps::map function regions.  Names pertinent to map_db.
# @param map_xlim maps::map function xlim;
# @param map_ylim maps::map function ylim;
#'
#' @return Taxa maps to user defined directory as jpg or pdf.
#' 
#' @examples
#' df_obs <- data_Taxa_MA
#' SampID <- "estuary"
#' TaxaID <- "TaxaName"
#' TaxaCount <- "Count"
#' Lat <- "Latitude"
#' Long <- "Longitude"
#' output_dir <- getwd()
#' output_prefix <- "maps.taxa."
#' output_type <- "pdf"
#' 
# map arguments
#' myDB <- "state"
#' myRegion <- "massachusetts"
#' myXlim     <- c(-(73+(30/60)), -(69+(56/60)))
#' myYlim     <- c((41+(14/60)),(42+(53/60)))
#' 
#' # Run function with extra arguments for map
#'\dontrun{
#' MapTaxaObs(df_obs, SampID, TaxaID, TaxaCount, Lat, Long
#'            , database="state", regions="massachusetts", xlim=myXlim, ylim=myYlim)
#'}
#'
#' #Example #2
#' # (doesn't work right now)
#' \dontrun{
#' df_obs <- read.delim("Fish_MD.txt")
#' SampID <- "SITEYR"
#' TaxaID <- "FishTaxa"
#' TaxaCount <- "Count"
#' Lat <- "Latitude83"
#' Long <- "Longitude83"
#' output_dir <- getwd()
#' output_prefix <- "maps.taxa."
#' output_type <- "pdf"
#' #' 
#' # map arguments
#' myDB <- "state"
#' myRegion <- "maryland"
#' 
#' df_obs[,TaxaCount] <- 1
#' 
#' MapTaxaObs(df_obs, SampID, TaxaID, TaxaCount, Lat, Long
#'            , database=myDB, regions=myRegion)
#' }

#
#' @export
MapTaxaObs <- function(df_obs, SampID, TaxaID, TaxaCount, Lat, Long
                  , output_dir, output_prefix="maps.taxa", output_type="pdf"
                  , ...)
{##FUNCTION.MapTaxaObs.START
  
  # Munge
  Samps.ALL         <- unique(df_obs[,SampID])
  NumSamps.ALL     <- length(Samps.ALL)

  # reclass "Count" (ensure is numeric)
  df_obs[,TaxaCount] <- as.numeric(df_obs[,TaxaCount])
  Count.ALL        <- sum(df_obs[,TaxaCount],na.rm=TRUE)
  
  # Calc Density
  #df_obs[,"Pct"] <- 
  
  TaxaNames <- unique(df_obs[,TaxaID])
  
  # Define Counters for the Loop
  data2process <- TaxaNames
  myCounterStart <- 0 #set at number of columns not used
  myCounterStop <- length(data2process)
  myCounter <- myCounterStart
  print(paste("Total files to process = ",myCounterStop-myCounterStart,sep=""))
  utils::flush.console()
  
  #Define PDF
  if (output_type=="pdf") {##IF.output_type.START
    pdf(file=paste(output_prefix, "pdf", sep="."))##PDF.START
  }##IF.output_type.END
  
  
  while (myCounter < myCounterStop)
  { #LOOP.START
    # 1.0. Increase the Counter
    myCounter <- myCounter+1
    #
    # define Target Taxa for this iteration of the loop
    myTargetMapCat <- data2process[myCounter]
    # Update User
    print(paste("Map ",myCounter," of ",myCounterStop,"; ", myTargetMapCat, sep=""))
    flush.console()
    
    # # subset
    data.TargetMapCat <- subset(df_obs, df_obs[,TaxaID]==myTargetMapCat)
    #
    # should be % occ in sample but use max as a surrogate (easier to see the dots)
    myDenom <- max(data.TargetMapCat[, TaxaCount])
    
    # jpg
    if (output_type=="jpg") {##IF.output_type.START
      jpeg(filename = paste(output_prefix, myTargetMapCat, "jpg",sep="."), width=1024, height=768, quality=100, pointsize=20)
    }##IF.output_type.END
    
    #~~~~~~~~~~~~~~~~~
    # Generate Base Map #####
    #~~~~~~~~~~~~~~~~~
    # use method 1 or 2
    #
    # Map.1. Default R outlines
    #map('state',region=State)#,) border=0.1)
    maps::map(...)
  #  map(database=myDB, regions=myRegion)
    
    # Map Points
    #~~~~~~~~~~~~~~~~~
    # point size
    # PctCount (use this to have size between maps be relative)
#    myCEX.PctSample <- data.TargetMapCat[,myPctCount]
    # Pct of max Count for the target organism (ensures one point will be cex=1)
#    myCEX.PctTarget <- data.TargetMapCat[,TaxaCount]/myDenom
    #
    myCEX <- 1 #myCEX.PctTarget
    #
    # map all sites (as gray, open circle)
    points(df_obs[,Long], df_obs[,Lat], col="lightgray", pch=1, cex=1)
    #
    points(data.TargetMapCat[,Long], data.TargetMapCat[,Lat], col="blue", pch=19, cex=myCEX)
    # main
    #mtext(paste(data2process[myCounter]," = ",myTargetMapCat,sep=""))
    mtext(myTargetMapCat)
    # 20130923, Target samples and count (lower left, side=1,outer=T,adj=0)
    # Pct Count, top line (line=-2)
    (Count.Target <- sum(data.TargetMapCat[,TaxaCount]))
    (Pct.Count <- round(Count.Target/Count.ALL,2))
  #  mtext(paste("Pct of Total Count (n=",Count.Target,")= ",Pct.Count,sep=""), side=1,outer=TRUE,adj=0,line=-2,cex=0.75)
    # Pct Samps, bottom line (line=-1)
    Samps.Target <- unique(data.TargetMapCat[,SampID])
    (NumSamps.Target <- length(Samps.Target))
    (Pct.Samps <- round(NumSamps.Target/NumSamps.ALL,2))
    mtext(paste("Pct of Total Samples (n=",NumSamps.ALL,") = ",Pct.Samps,sep=""),side=1,outer=TRUE,adj=0,line=-1,cex=0.75)
    # number of samples
    mtext(paste("n=",NumSamps.Target,sep=""),side=1)
    #
    
    
    if (output_type=="jpg") {##IF.output_type.START
      dev.off() ##JPEG.END
    }##IF.output_type.END
      
    #
    # 1.6. Display progress to user (needs flush.console or only writes at end)
    # print(paste("Finished file ",myCounter-myCounterStart," of ",myCounterStop-myCounterStart,", ",data2process[myCounter],".",sep=""))
    # flush.console()  	
    #
    # 1.7. Some clean up
    #rm(data.TargetMapCat)
    #par(def.par)

    
  } #LOOP.END
  #
  # Close Device
  if (output_type=="pdf") {##IF.output_type.START
    dev.off() ##PDF.END
  }##IF.output_type.END
  #
  print(paste("Processing of ",myCounter-myCounterStart," of ",myCounterStop-myCounterStart," files complete.",sep=""))
  utils::flush.console()
  #data2process[myCounter] #use for troubleshooting if get error
      

  #
}##FUNCTION.MapTaxaObs.END



