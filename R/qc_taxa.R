#' Quality Control Check on User Data Against Master Taxa List
#'
#' This function has been deprecated (March 2026).
#'
#' The new function is qc_taxa_match_official.
#'
#' This function exists only as a wrapper to avoid breaking older code.
#'
#' @param DF_User User taxa data.
#' @param DF_Official Official master taxa list.  Can be a local file or
#' from a URL.
#' Default is NULL.  A NULL value will use the official online files.
#' @param fun.Community Community name for which to compare the master taxa list
#'  (bugs or fish).
#' @param useOfficialTaxaInfo Select how to handle new/different taxa.
#' See 'Details' for more information.
#' Valid values are "only_Official", "only_user", "add_new".
#' Default = "only_Official".
#'
#' @return input data frame with master taxa information added to it.
#'
#' @examples
#' # Example 1, Master Taxa List, Bugs
#' url_mt_bugs <- "https://github.com/leppott/MBSStools_SupportFiles/raw/master/Data/CHAR_Bugs.csv"
#' df_mt_bugs  <- read.csv(url_mt_bugs)
#'
#' # User data
#' DF_User <- data_benthos_MBSS
#' DF_Official <- NULL   # NULL df_mt_bugs
#' fun.Community <- "bugs"
#' useOfficialTaxaInfo <- "only_Official"
#' # modify taxa id column
#' DF_User[, "TAXON"] <- DF_User[, "TAXAID"]
#'
#' df_qc_taxa_bugs <- qc_taxa(DF_User,
#'                            DF_Official,
#'                            fun.Community,
#'                            useOfficialTaxaInfo)
#'
#' # QC input/output
#' dim(DF_User)
#' dim(df_qc_taxa_bugs)
#' names(DF_User)
#' names(df_qc_taxa_bugs)
#
#' @export
qc_taxa <- function(DF_User,
                    DF_Official = NULL,
                    fun.Community = NULL,
                    useOfficialTaxaInfo = "only_Official") {
  #
  .Deprecated("qc_taxa")
  qc_taxa(DF_User,
          DF_Official,
          fun.Community,
          useOfficialTaxaInfo)
  #
}##FUNCTION ~ qc_taxa ~ END
