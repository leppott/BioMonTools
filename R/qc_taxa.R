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
#' @export
qc_taxa <- function(DF_User,
                    DF_Official = NULL,
                    fun.Community = NULL,
                    useOfficialTaxaInfo = "only_Official") {
  #
  .Deprecated("qc_taxa_match_official")

  qc_taxa_match_official(DF_User,
                         DF_Official,
                         fun.Community,
                         useOfficialTaxaInfo)
  #
}##FUNCTION ~ qc_taxa ~ END
