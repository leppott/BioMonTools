#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "data_bio2rarify" ####
#' @title rarify example data
#'
#' @description A dataset with example benthic macroinvertebrate data (600 count)
#' to be used with the rarify function.  Includes 12 samples.
#'
#' @format A data frame with 223 rows and 28 variables:
#' \describe{
#'    \item{SampleID}{Sample ID}
#'    \item{TaxaID}{unique taxonomic identifier}
#'    \item{N_Taxa}{number of individuals in sample}
#' }
#' @source example data
"data_bio2rarify"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "data_fish_MA" ####
#' @title Estuary taxa data
#'
#' @description A dataset with example fish taxa data and locations for mapping.
#'
#' @format A data frame with 2,675 observations on the following 15 variables.
#' \describe{
#'  \item{\code{estuary}}{a factor with levels \code{BOSTON HARBOR} \code{BUZZARDS BAY} \code{CAPE COD BAY} \code{MASSACHUSETTS BAY} \code{WAQUOIT BAY}}
#'  \item{\code{CommonName}}{a factor with levels \code{ALEWIFE} \code{AMERICAN EEL} \code{AMERICAN LOBSTER} \code{AMERICAN PLAICE} \code{AMERICAN SAND LANCE} \code{AMERICAN SHAD} \code{ATLANTIC COD} \code{ATLANTIC CROAKER} \code{ATLANTIC HERRING} \code{ATLANTIC MACKEREL} \code{ATLANTIC MENHADEN} \code{ATLANTIC ROCK CRAB} \code{ATLANTIC SALMON} \code{ATLANTIC STINGRAY} \code{ATLANTIC STURGEON} \code{ATLANTIC TOMCOD} \code{BAY ANCHOVY} \code{BAY SCALLOP} \code{BLACK DRUM} \code{BLACK SEA BASS} \code{BLUE CRAB} \code{BLUE MUSSEL} \code{BLUEBACK HERRING} \code{BLUEFISH} \code{BROWN SHRIMP} \code{BUTTERFISH} \code{CHANNEL CATFISH} \code{COWNOSE RAY} \code{CUNNER} \code{DAGGERBLADE GRASS SHRIMP} \code{EASTERN OYSTER} \code{FOURSPINE STICKLEBACK} \code{GOBIES} \code{GREEN CRAB} \code{GREEN SEA URCHIN} \code{GRUBBY} \code{HADDOCK} \code{HOGCHOKER} \code{JONAH CRAB} \code{KILLIFISHES} \code{LONGHORN SCULPIN} \code{MULLETS} \code{MUMMICHOG} \code{NINESPINE STICKLEBACK} \code{NORTHERN KINGFISH} \code{NORTHERN PIPEFISH} \code{NORTHERN SEAROBIN} \code{NORTHERN SHRIMP} \code{OCEAN POUT} \code{OYSTER TOADFISH} \code{PINFISH} \code{POLLOCK} \code{QUAHOG} \code{RAINBOW SMELT} \code{RED DRUM} \code{RED HAKE} \code{ROCK GUNNEL} \code{SCUP} \code{SEA SCALLOP} \code{SEVENSPINE BAY SHRIMP} \code{SHEEPSHEAD MINNOW} \code{SHORTHORN SCULPIN} \code{SHORTNOSE STURGEON} \code{SILVER HAKE} \code{SILVERSIDES} \code{SKATES} \code{SMOOTH FLOUNDER} \code{SOFTSHELL CLAM} \code{SPINY DOGFISH} \code{SPOT} \code{SPOTTED SEATROUT} \code{STRIPED BASS} \code{SUMMER FLOUNDER} \code{TAUTOG} \code{THREESPINE STICKLEBACK} \code{WEAKFISH} \code{WHITE HAKE} \code{WHITE PERCH} \code{WINDOWPANE FLOUNDER} \code{WINTER FLOUNDER} \code{YELLOW PERCH} \code{YELLOWTAIL FLOUNDER}}
#'  \item{\code{LifeStage}}{a factor with levels \code{ADULTS} \code{EGGS} \code{JUVENILES} \code{LARVAE} \code{MATING} \code{PARTURITION} \code{SPAWNING}}
#'  \item{\code{SalZone}}{a factor with levels \code{>25 ppt} \code{0.5-25 ppt}}
#'  \item{\code{Winter}}{a numeric vector}
#'  \item{\code{Spring}}{a numeric vector}
#'  \item{\code{Summer}}{a numeric vector}
#'  \item{\code{Fall}}{a numeric vector}
#'  \item{\code{All}}{a numeric vector}
#'  \item{\code{TaxaName}}{Taxa Names for mapping}
#'  \item{\code{State}}{a factor with levels \code{MA}}
#'  \item{\code{Latitude}}{a numeric vector}
#'  \item{\code{Longitude}}{a numeric vector}
#'  \item{\code{Count}}{a numeric vector}
#'  \item{\code{PctDensity}}{a numeric vector}
#'}
#' @source example data
"data_Taxa_MA"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "data_benthos_PacNW" ####
#' @title Benthic macroinvertebrate taxa data; Pacific Northwest
#'
#' @description A dataset with example taxa data and attributes for calculating metric values.
#'
#' @format A data frame with 598 observations on the following 29 variables.
#' \describe{
#' \item{\code{INDEX_NAME}}{a character vector}
#' \item{\code{Index_Region}}{a character vector}
#' \item{\code{SITE_TYPE}}{a character vector}
#' \item{\code{SampleID}}{a character vector}
#' \item{\code{TaxaID}}{a character vector}
#' \item{\code{N_TAXA}}{a numeric vector}
#' \item{\code{Exclude}}{a logical vector}
#' \item{\code{NonTarget}}{a logical vector}
#' \item{\code{Phylum}}{a character vector}
#' \item{\code{Class}}{a character vector}
#' \item{\code{Order}}{a character vector}
#' \item{\code{Family}}{a character vector}
#' \item{\code{Subfamily}}{a character vector}
#' \item{\code{Tribe}}{a character vector}
#' \item{\code{Genus}}{a character vector}
#' \item{\code{BCG_Attr}}{a numeric vector}
#' \item{\code{Thermal_Indicator}}{a character vector}
#' \item{\code{FFG}}{a character vector}
#' \item{\code{Clinger}}{a character vector}
#' \item{\code{LongLived}}{a logical vector}
#' \item{\code{Noteworthy}}{a logical vector}
#' \item{\code{Habitat}}{a character vector}
#' \item{\code{SubPhylum}}{a logical vector}
#' \item{\code{InfraOrder}}{a logical vector}
#' \item{\code{Habit}}{a logical vector}
#' \item{\code{Life_Cycle}}{a logical vector}
#' \item{\code{TolVal}}{a logical vector}
#' \item{\code{FFG2}}{a logical vector}
#' \item{\code{TolVal2}}{a logical vector}
#'}
#' @source example data
"data_benthos_PacNW"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "data_mmi_dev" ####
#' @title Metric data for metric stats for mmi development
#'
#' @description A data set with example benthic macroinvertebrate data.
#' Calculate metrics then statistics.
#'
#' @format A data frame with 10,574 observations on the following 10 variables.
#'
#'  \describe{
#'    \item{\code{Class}}{a character vector}
#'    \item{\code{Ref_v1}}{a character vector}
#'    \item{\code{CalVal_Class4}}{a character vector}
#'    \item{\code{Unique_ID}}{a character vector}
#'    \item{\code{BenSampID}}{a character vector}
#'    \item{\code{CollDate}}{a character vector}
#'    \item{\code{CollMeth}}{a character vector}
#'    \item{\code{TaxaID}}{a character vector}
#'    \item{\code{Individuals}}{a numeric vector}
#'    \item{\code{Exclude}}{a logical vector}
#'  }
#' @source example data
"data_mmi_dev"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "data_benthos_MBSS" ####
#' @title Benthic macroinvertebrate taxa data; MBSS
#'
#' @description A data set with example benthic macroinvertebrate data.
#' Calculate metrics then statistics.  Data from MBSS.
#'
#' @format A data frame with 50,664 observations on the following 20 variables.
#'
#'  \describe{
#'   \item{\code{Index.Name}}{a character vector}
#'   \item{\code{SITE}}{a character vector}
#'   \item{\code{DATE}}{a character vector}
#'   \item{\code{TAXON}}{a character vector}
#'   \item{\code{N_TAXA}}{a numeric vector, count}
#'   \item{\code{N_GRIDS}}{a numeric vector, number of grids in subsample (max = 30)}
#'   \item{\code{EXCLUDE}}{a character vector, whether taxon should be excluded from taxa richness metrics}
#'   \item{\code{strata_r}}{a character vector, index region}
#'   \item{\code{Phylum}}{a character vector}
#'   \item{\code{Class}}{a character vector}
#'   \item{\code{Order}}{a character vector}
#'   \item{\code{Family}}{a character vector}
#'   \item{\code{Genus}}{a character vector}
#'   \item{\code{Other_Taxa}}{a character vector}
#'   \item{\code{Tribe}}{a character vector}
#'   \item{\code{FFG}}{a character vector}
#'   \item{\code{FAM_TV}}{a numeric vector}
#'   \item{\code{Habit}}{a character vector}
#'   \item{\code{FinalTolVal07}}{a numeric vector}
#'   \item{\code{FinalTolVal08}}{a numeric vector}
#'  }
#' @source example data from MBSS
"data_benthos_MBSS"
