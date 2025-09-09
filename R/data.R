#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "data_bio2rarify" ####
#' @title rarify example data
#'
#' @description A dataset with example benthic macroinvertebrate data
#' (600 count) to be used with the rarify function.  Includes 12 samples.
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
# "data_fish_MBSS" ####
#' @title Fish data, MBSS
#'
#' @description A dataset with example fish taxa data for metric calculation.
#' @format{ A data frame with 1694 observations on the following 30 variables.
#' \describe{
#'   \item{\code{SAMPLEID}}{a character vector}
#'   \item{\code{TAXAID}}{a character vector}
#'   \item{\code{N_TAXA}}{a numeric vector}
#'   \item{\code{TYPE}}{a character vector}
#'   \item{\code{TOLER}}{a character vector}
#'   \item{\code{NATIVE}}{a character vector}
#'   \item{\code{TROPHIC}}{a character vector}
#'   \item{\code{SILT}}{a character vector}
#'   \item{\code{INDEX_CLASS}}{a character vector}
#'   \item{\code{SAMP_LENGTH_M}}{a numeric vector}
#'   \item{\code{SAMP_WIDTH_M}}{a numeric vector}
#'   \item{\code{SAMP_BIOMASS}}{a numeric vector}
#'   \item{\code{INDEX_NAME}}{a character vector}
#'   \item{\code{EXCLUDE}}{a logical vector}
#'   \item{\code{BCG_ATTR}}{a character vector}#'
#'   \item{\code{DA_MI2}}{a numeric vector}
#'   \item{\code{N_ANOMALIES}}{a numeric vector}
#'   \item{\code{FAMILY}}{a character vector}
#'   \item{\code{GENUS}}{a character vector}
#'   \item{\code{THERMAL_INDICATOR}}{a character vector}
#'   \item{\code{ELEVATION_ATTR}}{a character vector}
#'   \item{\code{GRADIENT_ATTR}}{a character vector}
#'   \item{\code{WSAREA_ATTR}}{a character vector}
#'   \item{\code{REPRODUCTION}}{a character vector}
#'   \item{\code{HABITAT}}{a character vector}
#'   \item{\code{CONNECTIVITY}}{a logical vector}
#'   \item{\code{SCC}}{a logical vector}
#'   \item{\code{HYBRID}}{a logical vector}
#'   \item{\code{BCGATTR2}}{a character vector}
#'   \item{\code{TOLVAL2}}{a numeric vector}
#' }
#' }
#' @source example data
"data_fish_MBSS"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "data_fish_MA" ####
#' @title Estuary taxa data
#'
#' @description A dataset with example fish taxa data and locations for mapping.
#'
#' @format A data frame with 2,675 observations on the following 15 variables.
#' \describe{
#'  \item{\code{estuary}}{a factor with levels \code{BOSTON HARBOR}
#'  \code{BUZZARDS BAY} \code{CAPE COD BAY} \code{MASSACHUSETTS BAY}
#'  \code{WAQUOIT BAY}}
#'  \item{\code{CommonName}}{a factor with levels \code{ALEWIFE}
#'  \code{AMERICAN EEL} \code{AMERICAN LOBSTER} \code{AMERICAN PLAICE}
#'  \code{AMERICAN SAND LANCE} \code{AMERICAN SHAD} \code{ATLANTIC COD}
#'  \code{ATLANTIC CROAKER} \code{ATLANTIC HERRING} \code{ATLANTIC MACKEREL}
#'  \code{ATLANTIC MENHADEN} \code{ATLANTIC ROCK CRAB} \code{ATLANTIC SALMON}
#'  \code{ATLANTIC STINGRAY} \code{ATLANTIC STURGEON} \code{ATLANTIC TOMCOD}
#'  \code{BAY ANCHOVY} \code{BAY SCALLOP} \code{BLACK DRUM}
#'  \code{BLACK SEA BASS} \code{BLUE CRAB} \code{BLUE MUSSEL}
#'  \code{BLUEBACK HERRING} \code{BLUEFISH} \code{BROWN SHRIMP}
#'  \code{BUTTERFISH} \code{CHANNEL CATFISH} \code{COWNOSE RAY} \code{CUNNER}
#'  \code{DAGGERBLADE GRASS SHRIMP} \code{EASTERN OYSTER}
#'  \code{FOURSPINE STICKLEBACK} \code{GOBIES} \code{GREEN CRAB}
#'  \code{GREEN SEA URCHIN} \code{GRUBBY} \code{HADDOCK} \code{HOGCHOKER}
#'  \code{JONAH CRAB} \code{KILLIFISHES} \code{LONGHORN SCULPIN} \code{MULLETS}
#'  \code{MUMMICHOG} \code{NINESPINE STICKLEBACK} \code{NORTHERN KINGFISH}
#'  \code{NORTHERN PIPEFISH} \code{NORTHERN SEAROBIN} \code{NORTHERN SHRIMP}
#'  \code{OCEAN POUT} \code{OYSTER TOADFISH} \code{PINFISH} \code{POLLOCK}
#'  \code{QUAHOG} \code{RAINBOW SMELT} \code{RED DRUM} \code{RED HAKE}
#'  \code{ROCK GUNNEL} \code{SCUP} \code{SEA SCALLOP}
#'  \code{SEVENSPINE BAY SHRIMP} \code{SHEEPSHEAD MINNOW}
#'  \code{SHORTHORN SCULPIN} \code{SHORTNOSE STURGEON} \code{SILVER HAKE}
#'  \code{SILVERSIDES} \code{SKATES} \code{SMOOTH FLOUNDER}
#'  \code{SOFTSHELL CLAM} \code{SPINY DOGFISH} \code{SPOT}
#'  \code{SPOTTED SEATROUT} \code{STRIPED BASS} \code{SUMMER FLOUNDER}
#'  \code{TAUTOG} \code{THREESPINE STICKLEBACK} \code{WEAKFISH}
#'  \code{WHITE HAKE} \code{WHITE PERCH} \code{WINDOWPANE FLOUNDER}
#'  \code{WINTER FLOUNDER} \code{YELLOW PERCH} \code{YELLOWTAIL FLOUNDER}}
#'  \item{\code{LifeStage}}{a factor with levels \code{ADULTS} \code{EGGS}
#'  \code{JUVENILES} \code{LARVAE} \code{MATING} \code{PARTURITION}
#'  \code{SPAWNING}}
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
#' @description A dataset with example (demonstration only) taxa data and
#' attributes for calculating metric values.
#'
#' This dataset is an example only.  DO NOT USE for any analyses.
#'
#' @format A data frame with 598 observations on the following 38 variables.
#' \describe{
#' \item{\code{INDEX_NAME}}{a character vector}
#' \item{\code{INDEX_CLASS}}{a character vector}
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
#' \item{\code{SubPhylum}}{a character vector}
#' \item{\code{InfraOrder}}{a character vector}
#' \item{\code{Habit}}{a logical vector}
#' \item{\code{Life_Cycle}}{a logical vector}
#' \item{\code{TolVal}}{a logical vector}
#' \item{\code{FFG2}}{a logical vector}
#' \item{\code{TolVal2}}{a logical vector}
#' \item{\code{UFC}}{a character vector}
#' \item{\code{UFC_Comment}}{a numeric vector}
#' \item{\code{SubClass}}{a character vector}
#' \item{\code{Elevation_Attr}}{a character vector}
#' \item{\code{Gradient_Attr}}{a character vector}
#' \item{\code{WSArea_Attr}}{a character vector}
#' \item{\code{HabStruct}}{a character vector}
#' \item{\code{BCG_Attr2}}{a character vector}
#' \item{\code{AirBreather}}{a logical vector}
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
#' @format A data frame with 10,574 observations on the following 34 variables.
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
#'    \item{\code{NonTarget}}{a character vector}
#'    \item{\code{Phylum}}{a character vector}
#'    \item{\code{Benthic_MasterTaxa.Class}}{a character vector}
#'    \item{\code{Order}}{a character vector}
#'    \item{\code{Family}}{a character vector}
#'    \item{\code{Subfamily}}{a character vector}
#'    \item{\code{Tribe}}{a character vector}
#'    \item{\code{Genus}}{a character vector}
#'    \item{\code{TolVal}}{a character vector}
#'    \item{\code{FFG}}{a character vector}
#'    \item{\code{Habit}}{a character vector}
#'    \item{\code{INDEX_NAME}}{a character vector}
#'    \item{\code{SUBPHYLUM}}{a character vector}
#'    \item{\code{CLASS}}{a character vector}
#'    \item{\code{SUBCLASS}}{a character vector}
#'    \item{\code{INFRAORDER}}{a character vector}
#'    \item{\code{LIFE_CYCLE}}{a character vector}
#'    \item{\code{BCG_ATTR}}{a character vector}
#'    \item{\code{THERMAL_INDICATOR}}{a character vector}
#'    \item{\code{LONGLIVED}}{a character vector}
#'    \item{\code{NOTEWORTHY}}{a character vector}
#'    \item{\code{FFG2}}{a character vector}
#'    \item{\code{TOLVAL2}}{a character vector}
#'    \item{\code{HABITAT}}{a numeric vector}
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
#' @format A data frame with 5,666 observations on the following 40 variables.
#'
#'  \describe{
#'   \item{\code{INDEX_NAME}}{a character vector}
#'   \item{\code{SAMPLEID}}{a character vector}
#'   \item{\code{DATE}}{a character vector}
#'   \item{\code{TAXAID}}{a character vector}
#'   \item{\code{N_TAXA}}{a numeric vector, count}
#'   \item{\code{N_GRIDS}}{a numeric vector, number of grids in subsample
#'   (max = 30)}
#'   \item{\code{EXCLUDE}}{a character vector, whether taxon should be excluded
#'   from taxa richness metrics}
#'   \item{\code{INDEX_CLASS}}{a character vector, index region}
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
#'   \item{\code{TOLVAL}}{a numeric vector}
#'   \item{\code{TOLVAL2}}{a numeric vector}
#'   \item{\code{UFC}}{a numeric vector}
#'   \item{\code{UFC_Comment}}{a character vector}
#'   \item{\code{SUBPHYLUM}}{a character vector}
#'   \item{\code{SUBCLASS}}{a character vector}
#'   \item{\code{INFRAORDER}}{a character vector}
#'   \item{\code{SUBFAMILY}}{a character vector}
#'   \item{\code{LIFE_CYCLE}}{a character vector}
#'   \item{\code{BCG_ATTR}}{a character vector}
#'   \item{\code{THERMAL_INDICATOR}}{a character vector}
#'   \item{\code{LONGLIVED}}{a character vector}
#'   \item{\code{NOTEWORTHY}}{a character vector}
#'   \item{\code{FFG2}}{a character vector}
#'   \item{\code{HABITAT}}{a character vector}
#'   \item{\code{ELEVATION_ATTR}}{a character vector}
#'   \item{\code{GRADIENT_ATTR}}{a character vector}
#'   \item{\code{WSAREA_ATTR }}{a character vector}
#'   \item{\code{HABSTRUCT}}{a character vector}
#'   \item{\code{BCG_ATTR2}}{a character vector}
#'   \item{\code{NONTARGET}}{a logical vector}
#'   \item{\code{AIRBREATHER}}{a logical vector}
#'  }
#' @source example data from MBSS
"data_benthos_MBSS"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "data_diatom_mmi_dev" ####
#' @title Diatom taxa data; Indiana DEM
#' @description A data set with example diatom data.
#' Calculate metrics.  Data from IDEM.
#'
#' @format A data frame with 24797 observations on the following 38 variables.
#'
#'   \describe{
#'   \item{\code{INDEX_NAME}}{a character vector}
#'   \item{\code{INDEX_CLASS}}{a character vector}
#'   \item{\code{STATIONID}}{a character vector}
#'   \item{\code{COLLDATE}}{a Date}
#'   \item{\code{SAMPLEID}}{a character vector}
#'   \item{\code{TAXAID}}{a character vector}
#'   \item{\code{EXCLUDE}}{a logical vector}
#'   \item{\code{NONTARGET}}{a logical vector}
#'   \item{\code{N_TAXA}}{a numeric vector}
#'   \item{\code{ORDER}}{a character vector}
#'   \item{\code{FAMILY}}{a character vector}
#'   \item{\code{GENUS}}{a character vector}
#'   \item{\code{BC_USGS}}{a character vector}
#'   \item{\code{TROPHIC_USGS}}{a character vector}
#'   \item{\code{SAP_USGS}}{a character vector}
#'   \item{\code{PT_USGS}}{a character vector}
#'   \item{\code{O_USGS}}{a character vector}
#'   \item{\code{SALINITY_USGS}}{a character vector}
#'   \item{\code{BAHLS_USGS}}{a character vector}
#'   \item{\code{P_USGS}}{a character vector}
#'   \item{\code{N_USGS}}{a character vector}
#'   \item{\code{HABITAT_USGS}}{a character vector}
#'   \item{\code{N_FIXER_USGS}}{a character vector}
#'   \item{\code{MOTILITY_USGS}}{a character vector}
#'   \item{\code{SIZE_USGS}}{a character vector}
#'   \item{\code{HABIT_USGS}}{a character vector}
#'   \item{\code{MOTILE2_USGS}}{a character vector}
#'   \item{\code{TOLVAL}}{a numeric vector}
#'   \item{\code{DIATOM_ISA}}{a character vector}
#'   \item{\code{DIAT_CL}}{a numeric vector}
#'   \item{\code{POLL_TOL}}{a numeric vector}
#'   \item{\code{BEN_SES}}{a numeric vector}
#'   \item{\code{DIATAS_TP}}{a numeric vector}
#'   \item{\code{DIATAS_TN}}{a numeric vector}
#'   \item{\code{DIAT_COND}}{a numeric vector}
#'   \item{\code{DIAT_CA}}{a numeric vector}
#'   \item{\code{MOTILITY}}{a numeric vector}
#'   \item{\code{NF}}{a numeric vector}#'
#'   \item{\code{PHYLUM}}{a character vector}
#' }
#' @source example data from IDEM
"data_diatom_mmi_dev"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "data_diatom_mmi_qc ####
#' @title Diatom metric value data; Indiana DEM
#' @description A data set with diatom metric value data.
#' Used to compare to metric value calculations.  Data from IDEM.
#'
#' @format A data frame with 497 observations on the following 250 variables.
#'
#'   \describe{
#'     \item{\code{SAMPLEID}}{a character vector}
#'     \item{\code{INDEX_NAME}}{a character vector}
#'     \item{\code{INDEX_CLASS}}{a character vector}
#'     \item{\code{ni_total}}{a numeric vector}
#'     \item{\code{li_total}}{a numeric vector}
#'     \item{\code{nt_total}}{a numeric vector}
#'     \item{\code{nt_Achnan_Navic}}{a numeric vector}
#'     \item{\code{nt_LOW_N}}{a numeric vector}
#'     \item{\code{nt_HIGH_N}}{a numeric vector}
#'     \item{\code{nt_LOW_P}}{a numeric vector}
#'     \item{\code{nt_HIGH_P}}{a numeric vector}
#'     \item{\code{nt_BC_1}}{a numeric vector}
#'     \item{\code{nt_BC_2}}{a numeric vector}
#'     \item{\code{nt_BC_3}}{a numeric vector}
#'     \item{\code{nt_BC_4}}{a numeric vector}
#'     \item{\code{nt_BC_5}}{a numeric vector}
#'     \item{\code{nt_BC_12}}{a numeric vector}
#'     \item{\code{nt_BC_45}}{a numeric vector}
#'     \item{\code{nt_PT_1}}{a numeric vector}
#'     \item{\code{nt_PT_2}}{a numeric vector}
#'     \item{\code{nt_PT_3}}{a numeric vector}
#'     \item{\code{nt_PT_4}}{a numeric vector}
#'     \item{\code{nt_PT_5}}{a numeric vector}
#'     \item{\code{nt_PT_12}}{a numeric vector}
#'     \item{\code{nt_SALINITY_1}}{a numeric vector}
#'     \item{\code{nt_SALINITY_2}}{a numeric vector}
#'     \item{\code{nt_SALINITY_3}}{a numeric vector}
#'     \item{\code{nt_SALINITY_4}}{a numeric vector}
#'     \item{\code{nt_SALINITY_12}}{a numeric vector}
#'     \item{\code{nt_SALINITY_34}}{a numeric vector}
#'     \item{\code{nt_O_1}}{a numeric vector}
#'     \item{\code{nt_O_2}}{a numeric vector}
#'     \item{\code{nt_O_3}}{a numeric vector}
#'     \item{\code{nt_O_4}}{a numeric vector}
#'     \item{\code{nt_O_5}}{a numeric vector}
#'     \item{\code{nt_O_345}}{a numeric vector}
#'     \item{\code{nt_SESTONIC_HABIT}}{a numeric vector}
#'     \item{\code{nt_BENTHIC_HABIT}}{a numeric vector}
#'     \item{\code{nt_BAHLS_1}}{a numeric vector}
#'     \item{\code{nt_BAHLS_2}}{a numeric vector}
#'     \item{\code{nt_BAHLS_3}}{a numeric vector}
#'     \item{\code{nt_TROPHIC_1}}{a numeric vector}
#'     \item{\code{nt_TROPHIC_2}}{a numeric vector}
#'     \item{\code{nt_TROPHIC_3}}{a numeric vector}
#'     \item{\code{nt_TROPHIC_4}}{a numeric vector}
#'     \item{\code{nt_TROPHIC_5}}{a numeric vector}
#'     \item{\code{nt_TROPHIC_6}}{a numeric vector}
#'     \item{\code{nt_TROPHIC_7}}{a numeric vector}
#'     \item{\code{nt_TROPHIC_456}}{a numeric vector}
#'     \item{\code{nt_SAP_1}}{a numeric vector}
#'     \item{\code{nt_SAP_2}}{a numeric vector}
#'     \item{\code{nt_SAP_3}}{a numeric vector}
#'     \item{\code{nt_SAP_4}}{a numeric vector}
#'     \item{\code{nt_SAP_5}}{a numeric vector}
#'     \item{\code{nt_NON_N_FIXER}}{a numeric vector}
#'     \item{\code{nt_N_FIXER}}{a numeric vector}
#'     \item{\code{nt_HIGHLY_MOTILE}}{a numeric vector}
#'     \item{\code{nt_MODERATELY_MOTILE}}{a numeric vector}
#'     \item{\code{nt_NON_MOTILE}}{a numeric vector}
#'     \item{\code{nt_SLIGHTLY_MOTILE}}{a numeric vector}
#'     \item{\code{nt_WEAKLY_MOTILE}}{a numeric vector}
#'     \item{\code{nt_BIG}}{a numeric vector}
#'     \item{\code{nt_SMALL}}{a numeric vector}
#'     \item{\code{nt_MEDIUM}}{a numeric vector}
#'     \item{\code{nt_VERY_BIG}}{a numeric vector}
#'     \item{\code{nt_VERY_SMALL}}{a numeric vector}
#'     \item{\code{nt_ADNATE}}{a numeric vector}
#'     \item{\code{nt_STALKED}}{a numeric vector}
#'     \item{\code{nt_HIGHLY_MOTILE.1}}{a numeric vector}
#'     \item{\code{nt_ARAPHID}}{a numeric vector}
#'     \item{\code{nt_DIAT_CL_1}}{a numeric vector}
#'     \item{\code{nt_DIAT_CL_2}}{a numeric vector}
#'     \item{\code{nt_BEN_SES_1}}{a numeric vector}
#'     \item{\code{nt_BEN_SES_2}}{a numeric vector}
#'     \item{\code{nt_DIAT_CA_1}}{a numeric vector}
#'     \item{\code{nt_DIAT_CA_2}}{a numeric vector}
#'     \item{\code{nt_DIAT_COND_1}}{a numeric vector}
#'     \item{\code{nt_DIAT_COND_2}}{a numeric vector}
#'     \item{\code{nt_DIATAS_TN_1}}{a numeric vector}
#'     \item{\code{nt_DIATAS_TN_2}}{a numeric vector}
#'     \item{\code{nt_DIATAS_TP_1}}{a numeric vector}
#'     \item{\code{nt_DIATAS_TP_2}}{a numeric vector}
#'     \item{\code{nt_MOTILITY_1}}{a numeric vector}
#'     \item{\code{nt_MOTILITY_2}}{a numeric vector}
#'     \item{\code{nt_NF_1}}{a numeric vector}
#'     \item{\code{nt_NF_2}}{a numeric vector}
#'     \item{\code{pi_Achnan_Navic}}{a numeric vector}
#'     \item{\code{pi_HIGH_N}}{a numeric vector}
#'     \item{\code{pi_LOW_N}}{a numeric vector}
#'     \item{\code{pi_HIGH_P}}{a numeric vector}
#'     \item{\code{pi_LOW_P}}{a numeric vector}
#'     \item{\code{pi_BC_1}}{a numeric vector}
#'     \item{\code{pi_BC_2}}{a numeric vector}
#'     \item{\code{pi_BC_3}}{a numeric vector}
#'     \item{\code{pi_BC_4}}{a numeric vector}
#'     \item{\code{pi_BC_5}}{a numeric vector}
#'     \item{\code{pi_PT_1}}{a numeric vector}
#'     \item{\code{pi_PT_2}}{a numeric vector}
#'     \item{\code{pi_PT_3}}{a numeric vector}
#'     \item{\code{pi_PT_4}}{a numeric vector}
#'     \item{\code{pi_PT_5}}{a numeric vector}
#'     \item{\code{pi_PT_45}}{a numeric vector}
#'     \item{\code{pi_SALINITY_1}}{a numeric vector}
#'     \item{\code{pi_SALINITY_2}}{a numeric vector}
#'     \item{\code{pi_SALINITY_3}}{a numeric vector}
#'     \item{\code{pi_SALINITY_4}}{a numeric vector}
#'     \item{\code{pi_O_1}}{a numeric vector}
#'     \item{\code{pi_O_2}}{a numeric vector}
#'     \item{\code{pi_O_3}}{a numeric vector}
#'     \item{\code{pi_O_4}}{a numeric vector}
#'     \item{\code{pi_O_5}}{a numeric vector}
#'     \item{\code{pi_SESTONIC_HABIT}}{a numeric vector}
#'     \item{\code{pi_BENTHIC_HABIT}}{a numeric vector}
#'     \item{\code{pi_BAHLS_1}}{a numeric vector}
#'     \item{\code{pi_BAHLS_2}}{a numeric vector}
#'     \item{\code{pi_BAHLS_3}}{a numeric vector}
#'     \item{\code{pi_TROPHIC_1}}{a numeric vector}
#'     \item{\code{pi_TROPHIC_2}}{a numeric vector}
#'     \item{\code{pi_TROPHIC_3}}{a numeric vector}
#'     \item{\code{pi_TROPHIC_4}}{a numeric vector}
#'     \item{\code{pi_TROPHIC_5}}{a numeric vector}
#'     \item{\code{pi_TROPHIC_6}}{a numeric vector}
#'     \item{\code{pi_TROPHIC_7}}{a numeric vector}
#'     \item{\code{pi_SAP_1}}{a numeric vector}
#'     \item{\code{pi_SAP_2}}{a numeric vector}
#'     \item{\code{pi_SAP_3}}{a numeric vector}
#'     \item{\code{pi_SAP_4}}{a numeric vector}
#'     \item{\code{pi_SAP_5}}{a numeric vector}
#'     \item{\code{pi_NON_N_FIXER}}{a numeric vector}
#'     \item{\code{pi_N_FIXER}}{a numeric vector}
#'     \item{\code{pi_HIGHLY_MOTILE}}{a numeric vector}
#'     \item{\code{pi_MODERATELY_MOTILE}}{a numeric vector}
#'     \item{\code{pi_NON_MOTILE}}{a numeric vector}
#'     \item{\code{pi_SLIGHTLY_MOTILE}}{a numeric vector}
#'     \item{\code{pi_WEAKLY_MOTILE}}{a numeric vector}
#'     \item{\code{pi_BIG}}{a numeric vector}
#'     \item{\code{pi_SMALL}}{a numeric vector}
#'     \item{\code{pi_MEDIUM}}{a numeric vector}
#'     \item{\code{pi_VERY_BIG}}{a numeric vector}
#'     \item{\code{pi_VERY_SMALL}}{a numeric vector}
#'     \item{\code{pi_ADNATE}}{a numeric vector}
#'     \item{\code{pi_STALKED}}{a numeric vector}
#'     \item{\code{pi_HIGHLY_MOTILE.1}}{a numeric vector}
#'     \item{\code{pi_ARAPHID}}{a numeric vector}
#'     \item{\code{pi_DIAT_CL_1}}{a numeric vector}
#'     \item{\code{pi_DIAT_CL_1_ASSR}}{a numeric vector}
#'     \item{\code{pi_DIAT_CL_2}}{a numeric vector}
#'     \item{\code{pi_BEN_SES_1}}{a numeric vector}
#'     \item{\code{pi_BEN_SES_2}}{a numeric vector}
#'     \item{\code{pi_DIAT_CA_1}}{a numeric vector}
#'     \item{\code{pi_DIAT_CA_2}}{a numeric vector}
#'     \item{\code{pi_DIAT_COND_1}}{a numeric vector}
#'     \item{\code{pi_DIAT_COND_2}}{a numeric vector}
#'     \item{\code{pi_DIATAS_TN_1}}{a numeric vector}
#'     \item{\code{pi_DIATAS_TN_2}}{a numeric vector}
#'     \item{\code{pi_DIATAS_TP_1}}{a numeric vector}
#'     \item{\code{pi_DIATAS_TP_2}}{a numeric vector}
#'     \item{\code{pi_MOTILITY_1}}{a numeric vector}
#'     \item{\code{pi_MOTILITY_2}}{a numeric vector}
#'     \item{\code{pi_NF_1}}{a numeric vector}
#'     \item{\code{pi_NF_2}}{a numeric vector}
#'     \item{\code{pt_Achnan_Navic}}{a numeric vector}
#'     \item{\code{pt_HIGH_N}}{a numeric vector}
#'     \item{\code{pt_LOW_N}}{a numeric vector}
#'     \item{\code{pt_HIGH_P}}{a numeric vector}
#'     \item{\code{pt_LOW_P}}{a numeric vector}
#'     \item{\code{pt_BC_1}}{a numeric vector}
#'     \item{\code{pt_BC_2}}{a numeric vector}
#'     \item{\code{pt_BC_3}}{a numeric vector}
#'     \item{\code{pt_BC_4}}{a numeric vector}
#'     \item{\code{pt_BC_5}}{a numeric vector}
#'     \item{\code{pt_BC_12}}{a numeric vector}
#'     \item{\code{pt_BC_45}}{a numeric vector}
#'     \item{\code{pt_PT_1}}{a numeric vector}
#'     \item{\code{pt_PT_2}}{a numeric vector}
#'     \item{\code{pt_PT_3}}{a numeric vector}
#'     \item{\code{pt_PT_4}}{a numeric vector}
#'     \item{\code{pt_PT_5}}{a numeric vector}
#'     \item{\code{pt_PT_12}}{a numeric vector}
#'     \item{\code{pt_SALINITY_1}}{a numeric vector}
#'     \item{\code{pt_SALINITY_2}}{a numeric vector}
#'     \item{\code{pt_SALINITY_3}}{a numeric vector}
#'     \item{\code{pt_SALINITY_4}}{a numeric vector}
#'     \item{\code{pt_SALINITY_34}}{a numeric vector}
#'     \item{\code{pt_O_1}}{a numeric vector}
#'     \item{\code{pt_O_2}}{a numeric vector}
#'     \item{\code{pt_O_3}}{a numeric vector}
#'     \item{\code{pt_O_4}}{a numeric vector}
#'     \item{\code{pt_O_5}}{a numeric vector}
#'     \item{\code{pt_O_345}}{a numeric vector}
#'     \item{\code{pt_SESTONIC_HABIT}}{a numeric vector}
#'     \item{\code{pt_BENTHIC_HABIT}}{a numeric vector}
#'     \item{\code{pt_BAHLS_1}}{a numeric vector}
#'     \item{\code{pt_BAHLS_2}}{a numeric vector}
#'     \item{\code{pt_BAHLS_3}}{a numeric vector}
#'     \item{\code{pt_TROPHIC_1}}{a numeric vector}
#'     \item{\code{pt_TROPHIC_2}}{a numeric vector}
#'     \item{\code{pt_TROPHIC_3}}{a numeric vector}
#'     \item{\code{pt_TROPHIC_4}}{a numeric vector}
#'     \item{\code{pt_TROPHIC_5}}{a numeric vector}
#'     \item{\code{pt_TROPHIC_6}}{a numeric vector}
#'     \item{\code{pt_TROPHIC_7}}{a numeric vector}
#'     \item{\code{pt_TROPHIC_456}}{a numeric vector}
#'     \item{\code{pt_SAP_1}}{a numeric vector}
#'     \item{\code{pt_SAP_2}}{a numeric vector}
#'     \item{\code{pt_SAP_3}}{a numeric vector}
#'     \item{\code{pt_SAP_4}}{a numeric vector}
#'     \item{\code{pt_SAP_5}}{a numeric vector}
#'     \item{\code{pt_NON_N_FIXER}}{a numeric vector}
#'     \item{\code{pt_N_FIXER}}{a numeric vector}
#'     \item{\code{pt_HIGHLY_MOTILE}}{a numeric vector}
#'     \item{\code{pt_MODERATELY_MOTILE}}{a numeric vector}
#'     \item{\code{pt_NON_MOTILE}}{a numeric vector}
#'     \item{\code{pt_SLIGHTLY_MOTILE}}{a numeric vector}
#'     \item{\code{pt_WEAKLY_MOTILE}}{a numeric vector}
#'     \item{\code{pt_BIG}}{a numeric vector}
#'     \item{\code{pt_SMALL}}{a numeric vector}
#'     \item{\code{pt_MEDIUM}}{a numeric vector}
#'     \item{\code{pt_VERY_BIG}}{a numeric vector}
#'     \item{\code{pt_VERY_SMALL}}{a numeric vector}
#'     \item{\code{pt_ADNATE}}{a numeric vector}
#'     \item{\code{pt_STALKED}}{a numeric vector}
#'     \item{\code{pt_HIGHLY_MOTILE.1}}{a numeric vector}
#'     \item{\code{pt_ARAPHID}}{a numeric vector}
#'     \item{\code{pt_DIAT_CL_1}}{a numeric vector}
#'     \item{\code{pt_DIAT_CL_2}}{a numeric vector}
#'     \item{\code{pt_BEN_SES_1}}{a numeric vector}
#'     \item{\code{pt_BEN_SES_2}}{a numeric vector}
#'     \item{\code{pt_DIAT_CA_1}}{a numeric vector}
#'     \item{\code{pt_DIAT_CA_2}}{a numeric vector}
#'     \item{\code{pt_DIAT_COND_1}}{a numeric vector}
#'     \item{\code{pt_DIAT_COND_2}}{a numeric vector}
#'     \item{\code{pt_DIATAS_TN_1}}{a numeric vector}
#'     \item{\code{pt_DIATAS_TN_2}}{a numeric vector}
#'     \item{\code{pt_DIATAS_TP_1}}{a numeric vector}
#'     \item{\code{pt_DIATAS_TP_2}}{a numeric vector}
#'     \item{\code{pt_MOTILITY_1}}{a numeric vector}
#'     \item{\code{pt_MOTILITY_2}}{a numeric vector}
#'     \item{\code{pt_NF_1}}{a numeric vector}
#'     \item{\code{pt_NF_2}}{a numeric vector}
#'     \item{\code{nt_Sens_810}}{a numeric vector}
#'     \item{\code{nt_RefIndicators}}{a numeric vector}
#'     \item{\code{nt_Tol_13}}{a numeric vector}
#'     \item{\code{pi_Sens_810}}{a numeric vector}
#'     \item{\code{pi_RefIndicators}}{a numeric vector}
#'     \item{\code{pi_Tol_13}}{a numeric vector}
#'     \item{\code{pt_Sens_810}}{a numeric vector}
#'     \item{\code{pt_RefIndicators}}{a numeric vector}
#'     \item{\code{pt_Tol_13}}{a numeric vector}
#'     \item{\code{wa_POLL_TOL}}{a numeric vector}
#'   }
#' @source example metric value data from IDEM
"data_diatom_mmi_qc"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "data_coral_bcg_metric_dev" ####
#' @title Coral taxa data; Florida BCG.
#' @description A data set with example coral data.
#' Calculate metrics.  Data from Florida BCG providers.
#'
#' @format A data frame with 2138 observations on the following 25 variables.
#'
#'   \describe{
#'   \item{\code{DataSource}}{a character vector}
#'   \item{\code{SampleID}}{a character vector}
#'   \item{\code{TotTranLngth_m}}{a numeric vector}
#'   \item{\code{SampDate}}{a Date}
#'   \item{\code{TAXAID}}{a character vector}
#'   \item{\code{CommonName}}{a character vector}
#'   \item{\code{Juvenile}}{a logical vector}
#'   \item{\code{DiamMax_cm}}{a numeric vector}
#'   \item{\code{DiamPerp_cm}}{a numeric vector}
#'   \item{\code{Height_cm}}{a numeric vector}
#'   \item{\code{TotMort_pct}}{a numeric vector}
#'   \item{\code{BCG_ATTR}}{a character vector}
#'   \item{\code{Weedy}}{a character vector}
#'   \item{\code{LRBC}}{a logical vector}
#'   \item{\code{MorphConvFact}}{a numeric vector}
#'   \item{\code{Phylum}}{a character vector}
#'   \item{\code{Class}}{a character vector}
#'   \item{\code{SubClass}}{a character vector}
#'   \item{\code{Order}}{a character vector}
#'   \item{\code{Family}}{a character vector}
#'   \item{\code{Genus}}{a character vector}
#'   \item{\code{SubGenus}}{a character vector}
#'   \item{\code{Species}}{a character vector}
#'   \item{\code{INDEX_NAME}}{a character vector}
#'   \item{\code{INDEX_CLASS}}{a character vector}
#' }
#' @source example coral data from Florida BCG
"data_coral_bcg_metric_dev"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "data_coral_bcg_metric_qc" ####
#' @title Coral metric value data; Florida BCG.
#' @description A data set with coral metric value data.
#' Used to compare to metric value calculations.  Data from Florida BCG providers.
#'
#' @format A data frame with 100 observations on the following 19 variables.
#'
#'   \describe{
#'   \item{\code{SAMPLEID}}{a character vector}
#'   \item{\code{INDEX_NAME}}{a character vector}
#'   \item{\code{INDEX_CLASS}}{a character vector}
#'   \item{\code{transect_area_m2}}{a numeric vector}
#'   \item{\code{ncol_total}}{a numeric vector}
#'   \item{\code{lcol_total}}{a numeric vector}
#'   \item{\code{nt_total}}{a numeric vector}
#'   \item{\code{ncol_Acropora}}{a numeric vector}
#'   \item{\code{ncol_AcroOrbi_m2}}{a numeric vector}
#'   \item{\code{pcol_Acropora}}{a numeric vector}
#'   \item{\code{nt_BCG_att123}}{a numeric vector}
#'   \item{\code{nt_BCG_att1234}}{a numeric vector}
#'   \item{\code{nt_BCG_att5}}{a numeric vector}
#'   \item{\code{pt_BCG_att5}}{a numeric vector}
#'   \item{\code{LCSA3D_samp_m2}}{a numeric vector}
#'   \item{\code{LCSA3D_BCG_att1234_m2}}{a numeric vector}
#'   \item{\code{LCSA3D_LRBC_m2}}{a numeric vector}
#'   \item{\code{ncol_SmallWeedy}}{a numeric vector}
#'   \item{\code{pcol_SmallWeedy}}{a numeric vector}
#' }
#' @source example coral metric results from Florida BCG
"data_coral_bcg_metric_qc"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "TaxaMaster_Ben_BCG_PacNW" ----
#' @title TaxaMaster_Ben_BCG_PacNW
#' @description Example data
#'
#' @format A data frame with 684 observations on the following 20 variables.
#'
#'   \describe{
#'     \item{\code{TaxaID}}{a character vector}
#'     \item{\code{Phylum}}{a character vector}
#'     \item{\code{SubPhylum}}{a character vector}
#'     \item{\code{Class}}{a character vector}
#'     \item{\code{SubClass}}{a character vector}
#'     \item{\code{Order}}{a character vector}
#'     \item{\code{SuperFamily}}{a character vector}
#'     \item{\code{Family}}{a character vector}
#'     \item{\code{Tribe}}{a character vector}
#'     \item{\code{Genus}}{a character vector}
#'     \item{\code{SubGenus}}{a character vector}
#'     \item{\code{Species}}{a character vector}
#'     \item{\code{BCG_Attr}}{a character vector}
#'     \item{\code{NonTarget}}{a logical vector}
#'     \item{\code{Thermal_Indicator}}{a character vector}
#'     \item{\code{Long_Lived}}{a character vector}
#'     \item{\code{FFG}}{a character vector}
#'     \item{\code{Habit}}{a character vector}
#'     \item{\code{Life_Cycle}}{a character vector}
#'     \item{\code{TolVal}}{a numeric vector}
#'   }
#' @source example master taxa from BCG Pacific Northwest
"TaxaMaster_Ben_BCG_PacNW"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "data_metval_scmb_ibi" ----
#' @title data_metval_scmb_ibi
#' @description Example data metrics
#'
#'  @format A data frame with 20 observations on the following 13 variables.
#'
#'   \describe{
#'     \item{\code{INDEX_NAME}}{a character vector}
#'     \item{\code{INDEX_REGION}}{a character vector}
#'     \item{\code{SampID}}{a character vector}
#'     \item{\code{nt_total}}{a numeric vector}
#'     \item{\code{nt_Mol}}{a numeric vector}
#'     \item{\code{ni_Noto}}{a numeric vector}
#'     \item{\code{pi_intol}}{a numeric vector}
#'     \item{\code{qc_nt_total}}{a numeric vector}
#'     \item{\code{qc_nt_Mol}}{a numeric vector}
#'     \item{\code{qc_ni_Noto}}{a numeric vector}
#'     \item{\code{qc_pi_intol}}{a numeric vector}
#'     \item{\code{qc_sum}}{a numeric vector}
#'     \item{\code{qc_nar}}{a character vector}
#'   }
#' @source example data
"data_metval_scmb_ibi"
