#' @title Rarify (subsample) biological sample to fixed count
#'
#' @description Takes as an input a 3 column data frame (SampleID, TaxonID
#' , Count) and returns a similar dataframe with revised Counts.
#'
#' The other inputs are subsample size (target number of organisms in each
#' sample) and seed. The seed is given so the results can be reproduced from the
#'  same input file.  If no seed is given a random seed is used.
#'
#' @details rarify function:
#'  R function to rarify (subsample) a macroinvertebrate sample down to a fixed
#'  count; by John Van Sickle, USEPA. email: VanSickle.John@epa.gov ;
#'  Version 1.0, 06/10/05;
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Erik.Leppo@tetratech.com (EWL)
# 20170912
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @param inbug Input data frame.  Needs 3 columns (SampleID, taxonomicID
#' , Count).
#' @param sample.ID Column name in inbug for sample identifier.
#' @param abund Column name in inbug for organism count.
#' @param subsiz Target subsample size for each sample.
#' @param mySeed Seed for random number generator.  If provided the results with
#'  the same inbug file will produce the same results. Defaut = NA (random seed
#'  will be used.)
#' @return Returns a data frame with the same three columns but the abund field
#' has been modified so the total count for each sample is no longer above the
#' target (subsiz).
#' @examples
#' # Subsample to 500 organisms (from over 500 organisms) for 12 samples.
#'
#' # load bio data
#' df_biodata <- data_bio2rarify
#' dim(df_biodata)
#'\dontrun{
#' View(df_biodata)
#'}
#'
#' # subsample
#' mySize <- 500
#' Seed_OR <- 18590214
#' Seed_WA <- 18891111
#' Seed_US <- 17760704
#' bugs_mysize <- rarify(inbug=df_biodata, sample.ID="SampleID"
#'                      ,abund="N_Taxa",subsiz=mySize, mySeed=Seed_US)
#'
#' # view results
#' dim(bugs_mysize)
#'\dontrun{
#' View(bugs_mysize)
#'}
#'
#' # Compare pre- and post- subsample counts
#' df_compare <- merge(df_biodata, bugs_mysize, by=c("SampleID", "TaxaID")
#'                     , suffixes = c("_Orig","_500"))
#' df_compare <- df_compare[, c("SampleID"
#'                              , "TaxaID"
#'                              , "N_Taxa_Orig"
#'                              , "N_Taxa_500")]
#'
#'\dontrun{
#' View(df_compare)
#' }
#'
#' # compare totals
#' tbl_totals <- aggregate(cbind(N_Taxa_Orig, N_Taxa_500) ~ SampleID
#'                                                      , df_compare, sum)
#'
#'\dontrun{
#' View(tbl_totals)
#'
#' # save the data
#' write.table(bugs_mysize, paste("bugs",mySize,"txt",sep="."),sep="\t")
#' }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
rarify<-function(inbug, sample.ID, abund, subsiz, mySeed=NA) {
  ##FUNCTION.rarify.START
  start.time <- proc.time()
  outbug<-inbug
  sampid<-unique(inbug[, sample.ID])
  nsamp<-length(sampid)
  #parameters are set up
  #zero out all abundances in output data set
  outbug[,abund]<-0
  #loop over samples, rarify each one in turn

  for(i in 1:nsamp) {
    #extract current sample
    isamp<-sampid[i]
    utils::flush.console()
    #print(as.character(isamp))
    onesamp<-inbug[inbug[,sample.ID] == isamp, ]
    #add sequence numbers as a new column
    onesamp<-data.frame(onesamp,row.id=seq(1,dim(onesamp)[[1]]))
    #expand the sample into a vector of individuals
    samp.expand<-rep(x=onesamp$row.id,times=onesamp[,abund])
    nbug<-length(samp.expand) #number of bugs in sample
    #vector of uniform random numbers
    if(!is.na(mySeed)) set.seed(mySeed)  #use seed if provided.
    ranvec <- stats::runif(n=nbug)
    #sort the expanded sample randomly
    samp.ex2<-samp.expand[order(ranvec)]
    #keep only the first piece of ranvec, of the desired fixed count size
    #if there are fewer bugs than the fixed count size, keep them all
    if(nbug>subsiz){subsamp<-samp.ex2[1:subsiz]} else{subsamp<-samp.ex2}
    #tabulate bugs in subsample
    subcnt<-table(subsamp)
    #define new subsample frame and fill it with new reduced counts
    newsamp<-onesamp
    newsamp[,abund]<-0
    newsamp[match(newsamp$row.id,names(subcnt),nomatch=0)>0,abund] <- as.vector(
      subcnt)
    outbug[outbug[,sample.ID]==isamp,abund]<-newsamp[,abund]
  } #end of sample loop

  elaps<-proc.time()-start.time
  cat(c("Rarify of samples complete. \n Number of samples = ",nsamp,"\n"))
  cat(c(" Execution time (sec) = ", elaps[1]))
  utils::flush.console()
  return(outbug) #return subsampled data set as function value
} #end of function ##FUNCTION.rarify.END
