NEWS
================
<Erik.Leppo@tetratech.com>

<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->

    #> Last Update: 2023-10-20 14:03:55.65295

# Version History

## Changes in version 1.0.0.9038 (2023-10-20)

- refactor: Modify output of `taxa_translate` function, Issue \#94
  - taxatrans\$taxatrans_unique

## Changes in version 1.0.0.9037 (2023-10-20)

- feat: Add new metric, ni_totalNoDeca, Issue \#94
- style: Add spaces to code for readability

## Changes in version 1.0.0.9036 (2023-09-25)

- fix: Update Cheumatopsyche metrics in MetricNames.xlsx, Issue \#93
  - pi_Cheu
  - pi_EPTNoCheu

## Changes in version 1.0.0.9035 (2023-09-25)

- fix: Update code for Cheumatopsyche metrics, metric_values.R, Issue
  \#93
  - pi_Cheu
  - pi_EPTNoCheu

## Changes in version 1.0.0.9034 (2023-08-11)

- fix: BioMonTools vignette change reference to metric names
  - corecold changed to stenocold

## Changes in version 1.0.0.9033 (2023-08-11)

- fix: Fill in missing Vignette Builder for vignette_NewIndex

## Changes in version 1.0.0.9032 (2023-08-10)

- fix: Add line for “changed” taxa in `taxa_translate`

## Changes in version 1.0.0.9031 (2023-07-28)

- fix: Update domX calculations to combine taxa before calculate in
  metric_values
  - bugs and fish

## Changes in version 1.0.0.9030 (2023-07-28)

- refactor: Update metrics for MN BCG, Issue \#92
  - In myDF remove NA for specialized dom01 calculations

## Changes in version 1.0.0.9029 (2023-07-28)

- refactor: Update metrics for MN BCG, Issue \#92

## Changes in version 1.0.0.9028 (2023-07-26)

- refactor: Add percent Cheumatopsyche metric

## Changes in version 1.0.0.9027 (2023-07-12)

- refactor: Additional metrics for MN BCG, Issue \#92

## Changes in version 1.0.0.9026 (2023-07-12)

- refactor: Add bug metrics for MN BCG, Issue \#92
- style: Update spacing in test file metric_names

## Changes in version 1.0.0.9025 (2023-05-26)

- refactor: Update MetricFlags.xlsx for MTTI index

## Changes in version 1.0.0.9024 (2023-05-26)

- refactor: Update metric.values MTTI subset metrics, Issue \#90

## Changes in version 1.0.0.9023 (2023-05-26)

- refactor: Update MetricFlags.xlsx for MTTI index

## Changes in version 1.0.0.9022 (2023-05-25)

- feature: New parameter in `metric.values` to run only subset, Issue
  \#90
- style: Style changes in metric_values.R

## Changes in version 1.0.0.9021 (2023-02-02)

- docs: Add vignette for adding a new index, Issue \#87
  - incomplete
- refactor: Add CO MMI (bugs)
  - Draft 2006, verify with report and EDAS database

## Changes in version 1.0.0.9020 (2023-02-02)

- fix: Update `metric.values` thermal NA metrics
  - Add blank (““) to NA as a condition
- test: Additional `assign_indexclass` tests

## Changes in version 1.0.0.9019 (2023-01-23)

- refactor: Update `assign_indexclass` to account or NA and ““, Issue
  \#85
- fix: Update `assign_indexclass` to account non standard column names,
  Isue \#85
- tests: Add tests for `assign_indexclass`, Issue \#85
- refactor: Update `assign_indexclass` to not fail if index_class in
  input data, Issue \#85

## Changes in version 1.0.0.9018 (2023-01-20)

- refactor: Update MetricNames.xlsx groupings
  - Added “Experimental”

## Changes in version 1.0.0.9017 (2023-01-20)

- feature: Add unique taxa crosswalk to output of `taxa_translate`

## Changes in version 1.0.0.9016 (2023-01-16)

- test: Add test for multiple stages for `taxa_translate`

## Changes in version 1.0.0.9015 (2023-01-09)

- fix: Turn off QC testing in function `assign_indexclass`
  - Was using test data instead of user input

## Changes in version 1.0.0.9014 (2022-12-23)

- docs: Update `taxa_translate` example files

## Changes in version 1.0.0.9013 (2022-12-20)

- refactor: Update `taxa_translate` to drop a particular ID after
  translation , Issue \#81
  - Added input paramter
  - Updated example
  - Updated test

## Changes in version 1.0.0.9012 (2022-12-20)

- data: Update extdata for `taxa_translate` example, Issue \#81
  - Revised PacNW_ORWA data file (2022-12-19)
- tests: Revise `taxa_translate` test for revised data

## Changes in version 1.0.0.9011 (2022-12-19)

- tests: Create tests for `taxa_translate`, Issue \#81

## Changes in version 1.0.0.9010 (2022-12-16)

- fix: Update `taxa_translaste`
  - Update nonmatch to data frame with count and sum of occurrence,
    Issue \#81
  - Refine matching
  - Include summed taxa properly
  - Drop non-matching taxa (NA and DNI)
  - Update example

## Changes in version 1.0.0.9009 (2022-12-13)

- fix: Update `taxa_translate` for when sum taxa, Issue \#81
- update: Revise taxa_translate files in extdata

## Changes in version 1.0.0.9008 (2022-12-13)

- refactor: Tweak debug code in `taxa_translate`, Issue \#81
- fix: Update `taxa_translate` for col_drop_project
  - Should be debug only

## Changes in version 1.0.0.9007 (2022-12-13)

- refactor: Change internal workings of `taxa_translate`, Issue \#81
  - If summarize retain the official taxa columns
  - Reorder columns in the merged output

## Changes in version 1.0.0.9006 (2022-12-12)

- refactor: Change internal parameter boo_DEBUG to boo_DEBUG_tt in
  `taxa_translate`, Issue \#81

## Changes in version 1.0.0.9005 (2022-12-12)

- refactor: Make `taxa_translate` as an export object, Issue \#81

## Changes in version 1.0.0.9004 (2022-12-02)

- fix: Updated `taxa_translate` function to include metadata as
  parameter, Issue \#81

## Changes in version 1.0.0.9003 (2022-12-01)

- feature: Add `taxa_translate` function, Issue \#81
  - More general than qc_taxa

## Changes in version 1.0.0.9002 (2022-11-17)

- fix: Update `markExclude` to avoid dplyr deprecation warnings, Issue
  \#51

## Changes in version 1.0.0.9001 (2022-11-16)

- breaking change: Site_Type and Index_Region to Index_Class
- refactor: Update rarify function to save example to tempdir

## Changes in version 0.5.0.9127 (2022-11-15)

- feat: New function `assign_IndexClass`, Issue \#82
  - Add IndexClass.xlsx to ext folder

## Changes in version 0.5.0.9126 (2022-11-11)

- refactor: Additional benthic metrics
  - metric_values.R
  - MetricNames.xlsx
- refactor: Additional column for benthic metrics; AirBreather
  - modify metric_values for required column
  - Add to data_benthos_PacNW

## Changes in version 0.5.0.9125 (2022-11-10)

- fix: Update and add some metrics
  - metric_values.R
  - MetricNames.xlsx

## Changes in version 0.5.0.9124 (2022-11-08)

- fix: Tweak secondary metric name in MetricScoring.xlsx
  - GA_DNR_Fish_2020

## Changes in version 0.5.0.9123 (2022-11-03)

- refactor: Modify default term for markExcl from Excluded to Exclude
  - Ensure is consistent in function, example, vignette
- refactor: markExcl warning don’t trigger for 0 levels missing, Issue
  \#80
- refcator: markExcl add default of NA to Exceptions parameter, Issue
  \#80

## Changes in version 0.5.0.9122 (2022-09-29)

- docs: Typo in MetricNames.xlsx (Isoptera instead of Isopoda)
- fix: Change logic in metric.values for insertion of missing column
  names
  - Was not working in server environment (interacive == FALSE)
- docs: Update error message in metric.values for complex columns, Issue
  \#71
  - Add code needed to error message (base R and tidyverse)
- refactor: Change logic for Shiny on server when encounter complex,
  Issue \#71
  - BCG_ATTR column only, interactive FALSE or boo.Shiny TRUE

## Changes in version 0.5.0.9121 (2022-09-27)

- feat: Additional BCG metrics, Issue \#79
- style: Surround operators with space
  - metric_values
- test: Add sum of parts tests for new BCG metrics

## Changes in version 0.5.0.9120 (2022-09-22)

- fix: Update bug thermal metrics, Issue \#78

## Changes in version 0.5.0.9119 (2022-06-09)

- feature: Additional algae metrics, Issue \#75
- style: Add outlining to algae metrics
- style: Add spaces before and after operators in algae calculations
- refactor: Add missing values in MetricNames.xlsx based on tests
- refactor: Add missing algae metric pt_BC_12_adj as NA

## Changes in Version 0.5.0.9118 (2022-06-03)

- fix: Hemiptera metrics (nt and pt) not working, Issue \#74
  - Fix spelling

## Changes in Version 0.5.0.9117 (2022-05-31)

- refactor: Additional metrics, Issue \#74
- fix: Change metric HabitatStruct from RW to RM, Issue \#73

## Changes in Version 0.5.0.9116 (2022-05-18)

- refactor: Additional bug and fish metrics, Issue \#73
- refactor: Add BCG_ATTR as upper case for fish metrics
  - Update fish BCG metrics to use upper case letters for BCG metrics
- style: Add spacing and outlining to metric_values.R
- refactor: Updates to MetricNames.xlsx to pass tests

## Changes in Version 0.5.0.9115 (2022-04-08)

- refactor: Change variable classes in metric.scores()
  - Avoid errors in import via read_excel

## Changes in Version 0.5.0.9114 (2022-03-30)

- refactor: Rename one misspelled bug thermal metrics, Issue \#72
  - ‘pt_ti_cool_warm_stenowarm’ should be ‘pt_ti_warm_stenowarm’

## Changes in Version 0.5.0.9113 (2022-03-28)

- refactor: Add and modify bug thermal metrics, Issue \#72
  - metric_values()
  - extdata.xlsx
- style: Add extra spaces to some lines in metric_values()

## Changes in Version 0.5.0.9112 (2022-03-22)

- refactor: Add metrics and scoring for So Cal Marine Bays
  - extdata.xlsx
  - extdata.xlsx
- tests: Add tests for So Cal Marine Bays
  - Incomplete

## Changes in Version 0.5.0.9111 (2022-03-04)

- fix: Restore extdata.xlsx
  - Deleted in v0.5.0.9109, last mod was v0.5.0.9100

## Changes in Version 0.5.0.9110 (2022-03-02)

- docs: Update required fields text in metric.values()
- refactor: Copy (as comments) required fields code from bugs to other
  communities in metric.values()

## Changes in Version 0.5.0.9109 (2022-02-22)

- refactor: Additional output messages to screen in metric.values()

## Changes in Version 0.5.0.9108 (2022-02-22)

- tests: Update metric_stats test for new metrics
- fix: Update metric.values() for 1i and 1m metrics, use upper case
  - Recent change to have all columns to upper case make 1i into 1I

## Changes in Version 0.5.0.9107 (2022-02-21)

- fix: metric.values() remove some fields from conversion to upper case,
  Issue \#69
  - SAMPLEID, INDEX_NAME, INDEX_REGION
  - fixes issue with vignettes flag example, Issue \#70
- fix: Update metvalgrpxl() NOTES, Issue \#69
  - Input File Name to Input Data Frame
  - Convert text to formula for Description of Work
    - Moved from column 1 (text) to column 2 (formulas) in v0.5.0.9101
- fix: Shiny, BCG_Attr importing as complex, Issue \#71
  - Specify colClasses as character

## Changes in Version 0.5.0.9106 (2022-02-21)

- refactor: Tweak error checking for bugs in metric.values()
- fix: metric.values() columns to upper case for calculation
  - Error introduced in v0.5.0.9093 (2022-02-03)
- refactor: Change reference to some files in BioMonTools vignette
  - flag section
  - rebuild without vignettes

## Changes in Version 0.5.0.9105 (2022-02-18)

- refactor: Tweak error checking for bugs in metric.values()

## Changes in Version 0.5.0.9104 (2022-02-18)

- refactor: Tweak error checking for bugs in metric.values()

## Changes in Version 0.5.0.9103 (2022-02-18)

- refactor: Tweak error checking for bugs in metric.values()

## Changes in Version 0.5.0.9102 (2022-02-18)

- refactor: Update shiny app to use metric.values(…, verbose = TRUE)
- refactor: Update number of steps in verbose for metric.values()
- refactor: Update fill missing columns metric.values()
  - NA to NA_character\_

## Changes in Version 0.5.0.9101 (2022-02-18)

- refactor: Refine message output of metric.values() for use in Shiny
- refactor: Add verbose paramters to metric.values()
- refactor: Tweak Notes worksheet for metric Excel output

## Changes in Version 0.5.0.9100 (2022-02-11)

- refactor: Add new metric to MetricNames.xlsx

## Changes in Version 0.5.0.9099 (2022-02-11)

- refactor: Shiny add ‘tab’ as acceptable text file upload format.
- refactor: Add new bug metric to metric.values()
  - pi_SphaerCorb

## Changes in Version 0.5.0.9098 (2022-02-07)

- refactor: Modify debugging messages to metric.values() for Shiny log

## Changes in Version 0.5.0.9097 (2022-02-07)

- refactor: Add debugging messages to metric.values()

## Changes in Version 0.5.0.9096 (2022-02-07)

- fix: Update metric.values() for input data missing Index_Name and
  Index_Region

## Changes in Version 0.5.0.9095 (2022-02-04)

- fix: Update metric.values() with more error checking for missing
  columns
  - At start for minimal required fields before community specific
    sections
  - After metric values calculated in bug section for NA to 0

## Changes in Version 0.5.0.9094 (2022-02-03)

- refactor: Update metvalgrpxl() for blank cells hiding text to left

## Changes in Version 0.5.0.9093 (2022-02-03)

- fix: Update to changes in metric.values()

## Changes in Version 0.5.0.9092 (2022-02-03)

- fix: Remove redundant code in metric.values()
  - Causing failures on Shiny.io with missing required columns

## Changes in Version 0.5.0.9091 (2022-02-03)

- refactor: Update metvalgrpxl() to use input parameter for groupings

## Changes in Version 0.5.0.9090 (2022-02-02)

- refactor: Small edits to Shiny app
  - Text edits to help pages
  - Update links for TaxaMaps
- refactor: Update metvalgrpxl() to use formulas in output

## Changes in Version 0.5.0.9089 (2022-02-01)

- refactor: Edits to help pages in Shiny app
  - Update links to download files

## Changes in Version 0.5.0.9088 (2022-02-01)

- refactor: Shiny app updates
  - Calculate Metrics
  - Update help/notes for each function
    - RMD in data-raw
  - Add download files to www folder
- refactor: Define global variables in functions
  - metric.values
- refactor: Define imported function
  - MapTaxaObs
  - markExcluded
  - metric.stats2
- feature: Update MetricNames.xlsx with Sort_Group
- feature: Add new function, metvalgrpxl(), Issue \#68

## Changes in Version 0.5.0.9087 (2022-01-12)

- fix: Correct spelling of file name in global.R
  - ShinyApps.io is Linux and is case-sensitive

## Changes in Version 0.5.0.9086 (2022-01-12)

- refactor: Reorganize -raw folder with subfolders
  - Keep only the scripts to be run at the root
  - Will have to update scripts at later date to use the new structure
- feature: Added HTML creation scripts for text in shiny app to -raw  
- feature: Updates to Shiny app to include help text for each function
- feature: Add runShiny() to launch shiny app from console
- docs: Removed XLConnect from DESCRIPTION Suggests package list
  - Not used
- docs: Added writexl package to DESCRIPTIOn Suggest for upcoming change
  , Issue \# 68

## Changes in Version 0.5.0.9085 (2022-01-10)

- docs: Update GitHub default branch from “master” to “main”

## Changes in Version 0.5.0.9083 (2022-01-10)

- refactor: Added global variable bindings to fish metrics
  metric.values()
  - Only BCG_Attr
- fix: metric.values() example 4 (fish) view statement incorrect

## Changes in Version 0.5.0.9082 (2022-01-03)

- feature: Add new fish metrics to metric.values()
  - pi_BCG_att23_SCC
  - nt_BCG_att23_SCC
  - pt_BCG_att23_SCC

## Changes in Version 0.5.0.9081 (2021-12-22)

- feature: Add new fish metrics to metric.values()
  - pi_Lepomis
  - pt_Lepomis
  - nt_BCG_attNA
  - pi_BCG_attNA
  - pt_BCG_attNA

## Changes in Version 0.5.0.9080 (2021-12-22)

- refactor: Modify shiny mark excluded default new column name
  - EXCLUDED to Exclude to match required column for metric calc
- refactor: Add package version number to shiny About page
- docs: Add DT package to DESCRIPTION

## Changes in Version 0.5.0.9079 (2021-12-22)

- feature: Update Shiny app, all functions working, Issue \#67

## Changes in Version 0.5.0.9078 (2021-12-21)

- feature: Add Shiny app, basic outline only, Issue \#67

## Changes in Version 0.5.0.9077 (2021-12-16)

- refactor: Allow for mixed case in bug metric phylo metrics, Issue \#65

## Changes in Version 0.5.0.9076 (2021-12-15)

- fix: Update metric.values(), was returning 0 records
  - length_m was commented out and caused error in met.val

## Changes in Version 0.5.0.9075 (2021-12-13)

- tests: Update tests to ensure MetricNames.xlsx is complete
- refactor: Update metric.valus() with metrics used in
  MetricScoring.xlsx

## Changes in Version 0.5.0.9074 (2021-12-10)

- docs: Create documentation for data_fish_MBSS dataset
- docs: Update MetricNames.xlsx and MetricScoring.xlsx for any new
  metrics, Issue \#64

## Changes in Version 0.5.0.9073 (2021-12-10)

- feature: New fish metrics, Issue \#64
- feature: Add fish example data
- refactor: Add .groups = “drop_last” to each subfunction in
  metric_values(), Issue \#20
- refactor: Fish metric names
  - nt_natcent to nt_natCent_sunfish
    - GADNR_Fish_2020
    - Includes sunfish and had to add a Centrarchidae only metric
    - Fixed in metric.values() and MetricScoring.xlsx
  - lepomis to Lepomis

## Changes in Version 0.5.0.9072 (2021-12-09)

- refactor: Update thermal metric default values, Core-Cold to CoreC
  - metric.values()
  - data_benthos_PacNW

## Changes in Version 0.5.0.9071 (2021-12-09)

- fix: metric.values() thermal indicator metric not correct
  - nt_ti_corecold_cold
  - pt_ti_corecold_cold

## Changes in Version 0.5.0.9070 (2021-12-09)

- fix: metric.values() thermal indicator NA metric not correct
  - Using wrong column (Habitat) to find NA

## Changes in Version 0.5.0.9069 (2021-11-19)

- fix: metric.values() not working for new metrics, Issue \#62
  - Elevation, Gradient, and WSArea

## Changes in Version 0.5.0.9068 (2021-11-18)

- refactor: Update MetricScoring.xlsx for IEPA, Issue \#63
- tests: Update test_threshold_numdigits to show rows with issues

## Changes in Version 0.5.0.9067 (2021-11-18)

- feature: Add more benthic metrics, Issue \#62
  - Adds new required fields
    - GRADIENT
    - ELEVATION
    - WSAREA
  - Update data with new columns
    - data_benthos_PacNW
    - Data_Benthos.xlsx
  - Update data.R with changes to data_benthos_PacNW
- refactor: Update Vignettes for changes in metrics
- refactor: Update tests for changes in metrics
- break: Rename thermal benthic metrics, Issue \#62
- tests: Update metric test names

## Changes in Version 0.5.0.9066 (2021-11-02)

- tests: Update tests for benthos for UFC column
- refactor: Update diatom mmi dev data for tests, add Phylum
  - ProcessData
  - data description
- refactor: Update MetricNames to match MetricScores
  - Failed test after addition of algal metrics
  - Add metrics “pi_BCG_att5extra” and “x_UFC”  
  - Add notes
- tests: Update metric name tests
- tests: Update metric stats test

## Changes in Version 0.5.0.9065 (2021-11-01)

- refactor: Update MetricScoring.xlsx from dev_periphyton merge

## Changes in Version 0.5.0.9064 (2021-11-01)

- fix: Remove unneeded concatenation, markExcluded, Issue \#61

## Changes in Version 0.5.0.9063 (2021-11-01)

- fix: Update NEWS after merging of pull request
  - Add items below from branch NEWS
    - Some updates missing filled in from Branch info
- dev_metric_periphyton, Version 0.5.0.9066 (2021-06-23)
  - refactor: added diatom example dataset to package
    - Added diatom metric value dataset for internal QC
- dev_metric_periphyton, Version 0.5.0.9065 (2021-06-23)
  - refactor: Added example taxa data and example metric value data to
    package
  - refactor: Added testthat script (test_metric_calc.R) to test metric
    calculation.
- dev_metric_periphyton, Version 0.5.0.9064 (2021-06-23)
  - refactor: Updated MetricNames.xlsx according to Ben Jessup’s metric
    descriptions for Indiana
- dev_metric_periphyton, Version 0.5.0.9063 (2021-06-22)
  - refactor: metric.values
    - Added 52 new diatom metrics (added from EDAS)
  - refactor: MetricNames.xlsx
    - Update for new diatom metrics, but missing descriptions
  - refactor: Added scoring formulae for IDEM Diatom IBIs
- dev_metric_periphyton, Version 0.5.0.9062 (2021-06-22)
  - refactor: Added \*\_RefIndicators metrics
- dev_metric_periphyton, Version 0.5.0.9061 (2021-06-15)
  - Fix: grepl issues in algae metrics
- dev_metric_periphyton, Version 0.5.0.9060 (2021-06-11)
  - refactor: metric.values
    - Added over 150 diatom metrics
    - Almost all were USGS diatom metrics
  - refactor: MetricNames.xlsx
    - Update for new diatom metrics
- dev_metric_periphyton, Version 0.5.0.9059 (2021-06-10)
  - refactor: metric.values added periphyton/diatom metric functionality
    - Added metrics for Indiana diatoms IBIs
  - refactor: MetricNames.xlsx
    - Update for new diatom metrics

## Changes in Version 0.5.0.9062 (2021-09-23)

- feature: Add qc_taxa function from MBSStools package
  - Update example
- feature: Add Shiny app folders and files
- docs: Add shiny related packages to DESCRIPTION Suggests
- refactor: Started adding NJ fish IBI 2005 to MetricScoring.xlsx

## Changes in Version 0.5.0.9061 (2021-09-09)

- feature: Add taxonomic uncertainty frequency class metric for bugs,
  Issue \#57
- refactor: Update example data with UFC field, Issue \#57
  - extdata\_Benthos.xlsx
  - data_benthos_PacNW
  - Update data.R

## Changes in Version 0.5.0.9060 (2021-07-30)

- feature: Add pi_BCG_att5extra to metric.values() for bugs
  - 5 and 5.5

## Changes in Version 0.5.0.9059 (2021-07-30)

- feature: Add more fish metrics to metric.values()
- refactor: For fish community for metric.values() convert to upper case

## Changes in Version 0.5.0.9058 (2021-05-21)

- refactor: Move legend to the right margin, MapTaxaObs, Issue \#52
- docs: README, update install code to use remotes instead of devtools

## Changes in Version 0.5.0.9057 (2021-05-19)

- refactor: Update MapTaxaObs
  - subset function not working as intended
- refactor: MapTaxaObs, percent 0-100, Issue \#28
- refactor: MapTaxaObs, add output directory path, Issue \#27
- refactor: MapTaxaObs, add grouping variable to color points, Issue
  \#52
  - Add colors and legend
  - Add new parameters to function
  - Update example

## Changes in Version 0.5.0.9056 (2021-05-18)

- refactor: Convert messages to warnings in markExcluded
  - cat() and flush.console() to message()

## Changes in Version 0.5.0.9055 (2021-05-14)

- refactor: Update scoring to allow for scaling from metric to index
  scores
  - MetricScoring.xlsx, add ScoreScaling column
  - metric_scores.R, add AVERAGESCALE_100 as new index scoring option

## Changes in Version 0.5.0.9054 (2021-05-13)

- refactor: Incorporate pull request \#50
  - metric.stats2, fix group_by with change in dplyr

## Changes in Version 0.5.0.9053 (2021-05-10)

- refactor: Metric.Scoring.xlsx, updates for FFXCOVA_2018 metrics
  - Coastal Plain, x_HBI, Cont_0100 to Cont_0010
  - Floating point errors, n = 7
- refactor: Add new metric, pi_EPTNoHydro to MetricNames.xlsx

## Changes in Version 0.5.0.9052 (2021-05-10)

- refactor: Metric.Scoring.xlsx, update to remove NA from index scoring
  - Both numeric and narrative.  
  - Caused an issue with scoring FFXCOVA_2018

## Changes in Version 0.5.0.9051 (2021-05-10)

- refactor: metric.values, update new metric, pi_EPTNoHydro

## Changes in Version 0.5.0.9050 (2021-05-10)

- docs: Add Ben Block as contributor.
- refactor: metric.values, add new metric, pi_EPTNoHydro
- refactor: Metric.Scoring.xlsx, updates for FFXCOVA_2018

## Changes in Version 0.5.0.9049 (2021-04-15)

- refactor: metric.values, fix nt_ECT
  - Had nt_ECP

## Changes in Version 0.5.0.9048 (2021-04-14)

- refactor: metric.values add new metric if IL EPA lakes, Issue \#3
  - nt, pi, pt
    - Ephemeroptera, Coleoptera, Trichoptera
  - Add global variable bindings
- refactor: MetricNames.xlsx
  - Update for new metrics
- refactor: Update MetricScoring.xlsx
  - MassIBI Low Gradient
    - Update narrative thresholds
  - IL EPA Lakes
    - Add new indices
- docs: markExcluded example missing a comment line
  - Causing examples to fail

## Changes in Version 0.5.0.9047 (2021-04-08)

- refactor: metric.values, add 3 new FFG metrics, Issue \#3
  - nt, pi, pt
    - omnivore (OM)
    - parasite (PA)
- refactor: metric.values, Add global variable binding
  - FFG metrics new here and from v0.5.0.9444
- refactor: MetricNames.xlsx
  - Update for new metrics
- refactor: Modify ffg_filt metrics to use CF or FC
  - metric.values
    - nt, pi, pt
  - MetricNames.xlsx
- refactor: Modify ffg_col metrics to use CG or GC
  - metric.values
    - nt, pi, pt
  - MetricNames.xlsx

## Changes in Version 0.5.0.9046 (2021-04-06)

- feature: Add logo from RBP 2nd edition
- docs: Update README with logo

## Changes in Version 0.5.0.9045 (2021-04-01)

- style: Fix items, goodpractice::gp()
  - Replace ‘=’ with ‘\<-’
    - metric_stats.R
    - metric_stats2.R
  - Trim lines to 80 characters
    - test_qc_checks.R
    - test_metric_names.R
    - test_metric_calc.R
    - test_markExcluded.R
    - rarify.R
    - qc_checks.R
    - metric_values.R (partial)
    - metric_stats2.R
    - metric_stats.R
    - metric_scores.R (partial)
    - markExcluded.R
- style: Fix spelling, devtools::spell_check()
  - ReadMe
  - NEWS
  - metric_stats.R

## Changes in Version 0.5.0.9044 (2021-03-31)

- refactor: metric.values, add 3 new FFG metrics, Issue \#3
  - nt, pi, pt
    - macrophyte herbivore (MAH)
    - xylophage (XYL)
    - piercer-herbivore (PIH)
- refactor: MetricNames.xlsx
  - Update for new metrics
- refactor: MetricScores.xlsx
  - GADNR_fish_2005, ensure consistent index_region between metric and
    index scoring
  - Test was failing. Passes now

## Changes in Version 0.5.0.9043 (2021-03-22)

- refactor: metric.values, add pi_DiptNonIns metric
- style: metric.values, start to trim lines to 80 characters

## Changes in Version 0.5.0.9042 (2021-03-14)

- refactor: metric.values, modify GA fish metric pi_natinsctcypr, Issue
  \#48
  - Was not checking for Native

## Changes in Version 0.5.0.9041 (2021-02-26)

- refactor: metric.values, modify fish metrics for TROPHIC to account
  for multiple values

## Changes in Version 0.5.0.9040 (2021-02-26)

- refactor: MetricScoring.xlsx
  - GA DNR Fish, standardize 2005 index regions to all CAPS the same as
    2020.
  - Breaking change for scripts based on 2005 fish index

## Changes in Version 0.5.0.9039 (2021-02-23)

- docs: Move updates from NEWS to README
- refactor: Update MetricScoring.xlsx for MA

## Changes in Version 0.5.0.9038 (2021-02-22)

- docs: Update README install section, Issue \#11

## Changes in Version 0.5.0.9037 (2021-01-19)

- tests: Activate tests
  - rarify

## Changes in Version 0.5.0.9036 (2021-01-13)

- test: Add blank testing file for any function without a test
  - MapTaxaObs
- refactor: Update MapTaxaObs example
  - Direct to tempdir() instead of getwd()
  - Comment out 2nd non-working example
- style: Update MapTaxaObs for readability

## Changes in Version 0.5.0.9035 (2021-01-12)

- fix: Replace instances of deprecated functions
  - Change dplyr::group_by\_ to dplyr::group_by
    - metric.stats2
- test: Add tests
  - metric.stats2
- test: Comment out failing tests so package builds
  - Tests ok in console but not passing after build
  - markExcluded
  - qc_checks
  - metric.stats2
  - rarify

## Changes in Version 0.5.0.9034 (2021-01-12)

- fix: Replace instances of deprecated functions
  - Change dplyr::group_by\_ to dplyr::group_by
    - metric.values
- test: Add tests
  - qc_checks
  - markExcluded
  - metric.stats

## Changes in Version 0.5.0.9033 (2021-01-06)

- test: Add metric.values and metrics.scores for MA kick/logradient IBI
- docs: Update data.R for missing columns
  - data_benthos_MBSS
  - data_mmi_dev
- refactor: document internal functions of metric.values()
- fix: Remove non-ASCII character from data_mmi_dev

## Changes in Version 0.5.0.9032 (2021-01-03)

- refactor: Add global variable binding
  - metric.stats
  - metric.scores
- test: qc.check test not working, comment out
- docs: Document undocumented argument
  - metric.stats2
- fix: Rebuild data_benthos_MBSS with missing columns so no warnings in
  example
- chore: Add to .Rbuildignore
  - NEWS.rmd
  - README.rmd
  - data-raw folder

## Changes in Version 0.5.0.9031 (2021-01-03)

- docs: DESCRIPTION, move packages from Imports to Suggests
  - knitr
  - rmarkdown
- docs: DESCRIPTION, set license
- refactor: Add foo:: to missing functions
  - metric.stats
  - metric.stats2
- refactor: Add global variable binding
  - markExcluded
  - metric.scores
  - metric.stats
  - metric.stats2
  - metric.values
  - qc.checks

## Changes in Version 0.5.0.9030 (2021-01-03)

- doc: Delete docso will rebuild.
- style: Remove undesireable function, replace library(foo) with
  foo::bar()
  - vignette_MapTaxaObs
- style: Trim to 80 character lines
  - vignette_MapTaxaObs

## Changes in Version 0.5.0.9029 (2021-01-02)

test: Add tests + rarify + qc_checks (gives warning but passes)

## Changes in Version 0.5.0.9028 (2021-01-02)

- refactor: Remove unneeded concatenations
  - metric_stats
  - vignette_BioMonTools
- refactor: Remove undesireable function, replace library(foo) with
  foo::bar()
  - vignette_BioMonTools
- style: Trim to 80 character lines
  - vignette_BioMonTools
  - NEWS

## Changes in Version 0.5.0.9027 (2020-12-29)

- refactor: Add foo:: to missing functions
  - ProcessData_TaxaMater_Ben_BCG_PacNW.R
  - test_metric_calc.R
- refactor: Trim lines to 80 characters

## Changes in Version 0.5.0.9026 (2020-12-27)

- refactor: Add foo:: to missing functions

## Changes in Version 0.5.0.9025 (2020-12-25)

- ci: Add code coverage GitHub Action

## Changes in Version 0.5.0.9024 (2020-12-25)

- refactor: change 1:foo() to seq_len(foo())
  - test_metric_calc
  - metric_stats
  - qc_checks

## Changes in Version 0.5.0.9023 (2020-12-25)

- style: rarify, remove trailing ;

## Changes in Version 0.5.0.9022 (2020-12-25)

- docs: Change to pkgdown from manual (docs in main branch) to GitHub
  Action (gh-pages branch)

## Changes in Version 0.5.0.9021 (2020-12-25)

- docs: ReadMe, add badges for codefactor and codecov

## Changes in Version 0.5.0.9020 (2020-12-25)

- docs: ReadMe, knit with updates from previous commit

## Changes in Version 0.5.0.9019 (2020-12-25)

- ci: Change from TravisCI to GitHub Actions
- docs: ReadMe, Add CI badge
- docs: ReadMe, add lifecycle badge

## Changes in Version 0.5.0.9018 (2020-12-07)

- MetricScoring.xlsx
  - Fix two index threshold values with floating point errors;
    GBI_MS_2013
- Update Test
  - test_thresholds_numdigits
  - Test for any values with more than 11 digits.

## Changes in Version 0.5.0.9017 (2020-12-07)

- MetricScoring.xlsx
  - Modify MassDEP metrics and index scoring.
- Update Test
  - test_metric_names
  - Update test for AVERAGE_10 and AVERAGE_20 to AVERAGE_100.

## Changes in Version 0.5.0.9016 (2020-11-30)

- Change index scoring regimes.
  - Replace AVERAGE_10 and AVERAGE_20 with AVERAGE_100
    - Use number of metrics for the scaling to 100.
    - metric_scores.R
    - MetricScoring.xlsx, FFXCOVA indices.

## Changes in Version 0.5.0.9015 (2020-11-24)

- Check
  - Shorten lines longer than 100 characters.
    - markExcluded.R
    - metric_scores.R
  - Add stats to DESCRIPTION, IMPORTS

## Changes in Version 0.5.0.9014 (2020-11-24)

- metric_scores.R
  - Add CONT_0010 scoring regime.
  - Modify CONT_0100 to match CONT_0010 with score_max variable.
  - Add index scoring regimes.
    - AVERAGE_10
    - AVERAGE_20
- MetricScoring.xlsx
  - FFXCOVA_2018, INDEX_REGION
    - Triassic to Triassic Basin

## Changes in Version 0.5.0.9013 (2020-11-24)

- MetricScoring.xlsx, Issue \#3
  - Add Fairfax County, VA
- MetricNames.xlsx
  - Add 2 new metrics for Fairfax Co.
  - Rename pi_TricNoHydro to pi_TrichNoHydro
- metric_values.R
  - Add x_HBI2
  - Add pi_habit_cling_PlecoNoCling
  - Rename pi_TricNoHydro to pi_TrichNoHydro
- Update Test for number metric names for scoring index.

## Changes in Version 0.5.0.9012 (2020-10-28)

- MetricScoring.xlsx, Issue \#3
  - Update MassDEP and SNEP index scoring
  - Replace “.” with “\_“; MBSS_2005_Bugs, MSW_1999_Bugs, MBSS_2005_Fish
    , BCG_PacNW_L1
  - Remove NMSCI_2006 as had index scoring but not metric scroing.
- Update tests.
  - Rename “test” files.
  - Update tests to pass.
- Rename R function files with “\_” rather than “.”.
- metric_scores.R
  - Update examples from “.” to “\_“.
  - Change MBSStools::taxa_bugs_genus to internal data
    taxa_benthos_MBSS.
- Add new data taxa_benthos_MBSS from MBSStools::taxa_bugs_genus, Issue
  \#32
  - Add ProcessData_benthos_MBSS.R to data-raw.
  - Add to data.R
- MetricFlags.xlsx
  - MBSS.2005.Bugs to MBSS_2005_Bugs

## Changes in Version 0.5.0.9011 (2020-10-26)

- MetricScoring.xlsx
  - Add MA indices, Issue \#3.
- test-metric_calc.R
  - Update for MA indices.

## Changes in Version 0.5.0.9010 (2020-10-06)

- MetricScoring.xlsx
  - Update GADNR_Fish_2020, Issue \#3.

## Changes in Version 0.5.0.9009 (2020-10-06)

- MetricScoring.xlsx
  - Update GADNR_Fish_2020, Issue \#3.

## Changes in Version 0.5.0.9008 (2020-10-05)

- MetricScoring.xlsx
  - Update GADNR_Fish_2020, Issue \#3.

## Changes in Version 0.5.0.9007 (2020-09-04)

- MetricScoring.xlsx
  - Update MassDEP_2019_Bugs, Issue \#3.

## Changes in Version 0.5.0.9006 (2020-09-04)

- MetricScoring.xlsx
  - Update MassDEP_2019_Bugs, Issue \#3.

## Changes in Version 0.5.0.9005 (2020-08-11)

- Tweaks to tests.

## Changes in Version 0.5.0.9004 (2020-08-11)

- Update NEWS for pull request.
- Update notes in MetricScoring.xlsx

## Changes in Version 0.5.0.9003 (2020-08-11)

- Merged Pull Request \#44 (0.5.0.9001 and 9002)

## Changes in Version 0.5.0.9002 (2020-08-10)

- MetricScoring.xlsx
  - Updated MI narratives.

## Changes in Version 0.5.0.9001 (2020-08-03)

- MetricScoring.xlsx
  - Changed MI narratives and fixed NumMetrics error for MI - East.

## Changes in Version 0.5.0 (2020-07-24)

- Update to new version number.
- Rebuild package down website.
- Create new release on GitHub.

## Changes in Version 0.4.0.9025 (2020-07-24)

- Remove extra file (metric.values.tsv) from root folder of package.

## Changes in Version 0.4.0.9024 (2020-07-24)

- Updates for MI index; Issue \#3
  - metric.values.R
    - Add pi_IsopGastHiru
    - Add pi_EPTNoBaeHydro
    - Add pi_tv_toler6
    - Modified intol4 metrics from \<= to \<.
    - Modified toler6 metrics from \>= to \>.
  - MetricNames.xlsx
    - Add missing metrics.
    - Community name bmi to bugs.
    - Modified text on intol4 and toler6 metrics.
  - MetricScoring.xlsx
    - Community names to bugs and fish on both tabs.
- Add test to for metric names, Community = bugs.
  - metric.values and MetricNames.xlsx.
  - MetricScoring.xlsx and MetricNames.xlsx
  - MetricScroing.xlsx and metric.values()
  - Resolved issues.
- Update dataset “data_benthos_PacNW”.
  - Index_Name had a number and was different on each line (Excel
    autofill error).
  - Last updated 0.4.0.9008 (2020-04-28).
- Update tests for metric.values calc.
- metric.values.R
  - Modify NonTarget check for is.na() in cases where the column is
    missing; Issue \#39

## Changes in Version 0.4.0.9022 (2020-07-23)

- metric.values; Issue \#3, Issue \#41
  - Fix pi_EPT and pi_EPTNoCheu
    - swamped when created in v0.4.0.9019 (2020-07-06).

## Changes in Version 0.4.0.9021 (2020-07-06)

- metric.values, Skip column QC check in Shiny; Issue \#40

## Changes in Version 0.4.0.9020 (2020-07-06)

- metric.values, NonTarget still counted; Issue \#39
  - Inadvertently removed in v0.3.3.9017 (2020-02-24)

## Changes in Version 0.4.0.9019 (2020-07-06)

- WV GLIMPSS, Issue \#3
  - Narrative categories are: Very good, Good, Degraded, Severely
    degraded
    - Pond et al. 2012
    - MetricScoring.xlsx

## Changes in Version 0.4.0.9018 (2020-07-06)

- WV GLIMPSS, Issue \#3
  - MetricScoring.xlsx
  - MetricNames.xlsx
  - metric.values.R
  - testthat
- Update pkgdown website.

## Changes in Version 0.4.0.9017 (2020-06-25)

- metric.values.R
  - Correct nt_intol4_EPT to nt_tv_intol4_EPT
  - Add x_Becks3
- MetricScoring.xlsx
  - Add PADEP Freestone IBI (metrics and scoring).
- MetricNames.xlsx
  - Add new metrics.
- Use testthat
  - Set up tests for PA Freestone IBI (values and scores).
  - Passes both but test incomplete.

## Changes in Version 0.4.0.9016 (2020-06-19)

- Referenced wrong issue in last update.

## Changes in Version 0.4.0.9015 (2020-06-19)

- metric.values.R, Issue \#3
  - Add new metrics for PA and WV BIBIs.
    - nt_tv_intol4
    - nt_tv_intol4_EPT
    - pi_Ortho
    - pi_Chiro_Anne
  - Need to document.

## Changes in Version 0.4.0.9014 (2020-06-18)

- DESCRIPTION
  - Add package tidyr to support metric.stats2(); Issue \# 37

## Changes in Version 0.4.0.9013 (2020-06-03)

- metric.stats2.R, Issue \#36
  - Add DE and z-scores based on metric.stats and metric.values.

## Changes in Version 0.4.0.9012 (2020-06-01)

- Update pkgdown website documentation.

## Changes in Version 0.4.0.9011 (2020-06-01)

- metric.stats.R, Issue \#36
  - Add new function to calculate statistics for developing multi-metric
    index
- data_mmi_dev
  - Example data for metric.stats()
- data.R
  - Update for new data.

## Changes in Version 0.4.0.9010 (2020-05-11)

- metric.values.R, Issue \#35
  - Modify Oligochaete metrics to use Class or Subclass.
  - Add Subclass as a required field.

## Changes in Version 0.4.0.9009 (2020-05-07)

- metric.values.R
  - More new metrics.
    - nt, pt, pi - ET (Trichoptera, and Ephemeroptera)
    - nt, pt, pi - EOT (Odonata, Trichoptera, and Ephemeroptera)
    - nt, pt, pi - COTE (Coleoptera, Odonata, Trichoptera, and
      Ephemeroptera)
    - % Individs Amphipoda + Isopoda

## Changes in Version 0.4.0.9008 (2020-04-28)

- Start using R v4.0.0.
- metric.values.R
  - Additional metrics, Issue \#3.
    - nt_BCG_attNA
    - pi_BCG_attNA
    - pt_BCG_attNA
    - Habitat metrics, 8 values, nt, pi, pt.
- MetricNames.xlsx
  - Add new metrics.
- New data set to utilize new metrics.
  - data_benthos_PacNW

## Changes in Version 0.4.0.9007 (2020-04-27)

- metric.values.R
  - Additional metrics, Issue \#3.
    - nt_BCG_att1
    - pi_BCG_att1
    - pt_BCG_att1

## Changes in Version 0.4.0.9006 (2020-04-24)

- metric.values.R
  - QC on line 371 did not declare package. Issue \#33.
  - Define pipe in subfunctions.
- metric.scores.R
  - Modify example. Issue \#34.
    - Only one occurence of Index_Name and Index_Region in the
      data.frame.
    - Purge tibble error by converting to drop = TRUE.

## Changes in Version 0.4.0.9005 (2020-02-28)

- Tweak for SelMet.
  - metric.scores.R

## Changes in Version 0.4.0.9005 (2020-02-28)

- Change DepMet to SelMet.
  - Better terminology as selecting a metric to use.
  - metric.scores.R
  - MetricScoring.xlsx

## Changes in Version 0.4.0.9003 (2020-02-27)

- Add ability to score index when have zero individuals.
  - metric.scores.R
  - MetricScoring.xlsx

## Changes in Version 0.4.0.9002 (2020-02-27)

- metric.score.R
  - Account for “no organisms collected”.
    - TAXAID needs to be “NONE”.
    - N_TAXA needs to be zero.
    - Both bugs and fish will exclude all other N_TAXA = 0 but keep if
      TAXAID == “NONE”.
    - nt_total metric has condition for N_TAXA \> 0.
    - All other metrics will calculate but receive values of 0, NA, or
      NaN as appropriate.

## Changes in Version 0.4.0.9001 (2020-02-27)

- Tweak for GA Fish IBI, Issue \#3
  - Modify ni_natnonhybrid and ni_natnonhybridnonlepomis to also exclude
    mosquitofish.
    - MetricScoring.xlsx
    - metric.score.R
  - Add nt_nativenonhybrid
    - MetricScoring.xlsx
    - metric.score.R
- metric.score.R
  - Update ni_natnonhybridnonlepomis.

## Changes in Version 0.4.0 (2020-02-24)

- Changes to fish metric significant enough for a minor version update.
  - Some previous calculations may not work. Function made more generic.

## Changes in Version 0.3.3.9017 (2020-02-24)

- MetricScoring.xlsx, Issue \#3
  - Tweak scoring for GA Fish IBI
  - Add metric value calculation for GA Fish IBI metrics.
  - New Scoring Regimes and associated columns.
- Remove MBSStools from Suggests in DESCRIPTION.
- metric.values
  - Modifications for GA Fish IBI.
    - All columns to upper case.
  - Default values to NA rather than zero.
  - Use toupper rater than tolower.
- metric.scores
  - Modifications for GA Fish IBI.
    - Additional Scoring Regimes.
    - Additional fish metrics.
  - All columns to upper case using toupper.
  - Default values to NA rather than zero.
- vignette_BioMonTools.Rmd
  - Added library(dplyr) to one of the examples.

## Changes in Version 0.3.3.9016 (2020-01-31)

- MetricScoring.xlsx, Issue \#3
  - Tweak values for GA Fish IBI.
    - ACF, 6b, inflection point is 2 not 1.

## Changes in Version 0.3.3.9015 (2020-01-08)

- metric.values.R
  - Minor edits.
- MetricScoring.xlsx, Issue \#3
  - Tweak values for GA Fish IBI.
    - Convert some values to text.

## Changes in Version 0.3.3.9014 (2020-01-07)

- metric.values.R
  - Tweak index NormDist_135.
- MetricScoring.xlsx, Issue \#3
  - Tweak values for GA Fish IBI.
    - Convert some values to text.

## Changes in Version 0.3.3.9013 (2020-01-07)

- metric.values.R
  - Tweak index sum to account for NA.
- MetricScoring.xlsx, Issue \#3
  - Tweak values for GA Fish IBI.

## Changes in Version 0.3.3.9012 (2020-01-02)

- metric.values.R
  - Error in excluding marine metrics when no metric list given (i.e.,
    want all metrics).
    - Misplaced “)”. Issue \# 31.

## Changes in Version 0.3.3.9011 (2019-12-18)

- MetricScoring.xlsx, Issue \#3, Issue \#30
  - Added more columns for new scoring regimes for GA Fish IBI.
- MetricNames.xlsx
  - Added new fish metric names.
- metric.scores.R
  - Added more scoring regimes (GA Fish IBI).

## Changes in Version 0.3.3.9010 (2019-10-17)

- MetricScoring.xlsx, Issue \#3
  - Numeric to text.

## Changes in Version 0.3.3.9009 (2019-10-17)

- MetricScoring.xlsx, Issue \#3
  - Updates for GBI_MS_2013

## Changes in Version 0.3.3.9008 (2019-10-16)

- MetricScoring.xlsx, Issue \#3
  - Updates for GBI_MS_2013
  - Numeric to text.

## Changes in Version 0.3.3.9007 (2019-10-16)

- MetricScoring.xlsx, Issue \#3
  - Updates for GBI_MS_2013
  - Numeric to text.

## Changes in Version 0.3.3.9006 (2019-10-15)

- MetricScoring.xlsx, Issue \#3
  - Updates for GBI_MS_2013

## Changes in Version 0.3.3.9005 (2019-10-15)

- MetricScoring.xlsx, Issue \#3
  - Updates for GBI_MS_2013

## Changes in Version 0.3.3.9004 (2019-10-15)

- Additional scoring and index. Issue \#3
  - Gulf Biotic Index, 2011
  - Mississippi Gulf Biotic Index, 2013

## Changes in Version 0.3.3.9003 (2019-09-15)

- metric.values, Issue \#3
  - Additional estuary/marine metrics
    - Gulf Biotic Index, 2011
    - Mississippi Gulf Biotic Index, 2013
  - Additional input parameter, boo.Marine
  - Data_Benthos.xlsx
    - Add “InfraOrder” column.
    - Require in metric.values

## Changes in Version 0.3.3.9002 (2019-07-03)

- metric.values, Issue \#3
  - Add placeholders for marine/estuarine metrics
    - Not yet in MetricScoring.xlsx or MetricNames.xlsx

## Changes in Version 0.3.3.9001 (2019-07-02)

- metric.values
  - Update Beck’s Biotic Index cutoff signs.
    - \< 1.5 and \>= 1.5 to \<= 1.5 and \> 1.5.
    - Only affects tolerance values of exactly 1.5.

## Changes in Version 0.3.3 (2019-07-02)

- New release version.

## Changes in Version 0.3.2.9001 (2019-07-02)

- metric.scoring.xlsx
  - Update “scoring formula” column for 2 MA WestHighlands metrics.
  - Actual thresholds not affected.

## Changes in Version 0.3.2 (2019-06-28)

- New release version.

## Changes in Version 0.3.1.9001 (2019-06-27)

- MetricScoring.xlsx, issue \#22
  - New metric misspelled.
- Add capability for 6 narrative categories.
  - MetricScoring.xlsx
  - metric.scores.R
    - Minor edit for number of columns.

## Changes in Version 0.3.1 (2019-06-27)

- New release version.

## Changes in Version 0.3.0.9024 (2019-06-27)

- MetricScoring.xlsx, issue \#22
  - Some numbers to character with ’ prefix.
  - These numbers not importing correctly from Excel.
    - Floating point errors in rounding.

## Changes in Version 0.3.0.9023 (2019-06-27)

- MetricNames.xlsx
  - Add fish metric names, Issue \#21
  - Demonstration only. Not final format in function.
- MetricScoring.xlsx
  - Update date on Notes worksheet.
- metric.values.R
  - Update demonstration fish metrics, Issue \#21.

## Changes in Version 0.3.0.9022 (2019-06-27)

- Check
  - DESCRIPTION
    - rmarkdown listed under imports and suggests. Remove from suggests.
    - Add to suggests; ggplot2 and reshape2.
    - Add MBSStools to suggests. May import the data later and remove.
  - Example, move “View” to dontrun.
    - metric.scores.R
    - metric.values.R
    - qc.checks.R
    - rarify.R
  - Fix line widths greater than 100 characters:
    - markExcluded.R
    - metric.scores.R
  - Properly declare functions from other packages:
    - “grDevices”, “dev.off”, “jpeg”, “pdf”
    - “graphics”, “mtext”, “points”
    - “stats”, “median”, “runif”
    - “utils”, “flush.console”
  - MapTaxaObs.R
    - Document the “…” argument.

## Changes in Version 0.3.0.9021 (2019-06-27)

- Updated metric names for MA index, Issue \#3
  - MetricScoring.xlsx
- Added “to do” and “references” to metricscoring.xlsx, Issue \#3

## Changes in Version 0.3.0.9020 (2019-05-24)

- Updated metric names for MBISQ index, Issue \#3
  - MetricScoring.xlsx

## Changes in Version 0.3.0.9019 (2019-05-24)

- Added metrics, Issue \#3
  - metric.values
    - x_NCBI, x_D_Mg, x_D_G
    - Additional QC for TolVal2 as numeric (same as TolVal).
  - MetricNames.xlsx

## Changes in Version 0.3.0.9018 (2019-05-17)

- Fixes for R v3.6.0, Issue \#18
  - DESCRIPTION
    - Remove StagedInstall: no
  - README
    - Add directions for fix for devtools.
- README
  - Update badges.

## Changes in Version 0.3.0.9017 (2019-05-08)

- Metric names, fix those added in previous update, Issue \#3
  - Files
    - metric.values.R
    - MetricNames.xlsx
    - MetricScoring.xlsx
- metric.scores, Issue \#19
  - Continuous 0-100 scoring only reporting 1 value per column.
  - Fixed so reports all values.
    - Was taking median of column not per row.

## Changes in Version 0.3.0.9016 (2019-05-07)

- Temp fix for staged install with R v3.6.0, Issue \#18
  - DESCRIPTION, StagedInstall: no
- Add additional metrics (GA DNR). Issue \#3.
  - Files
    - metric.values.R
    - MetricNames.xlsx
    - MetricScoring.xlsx
  - Metrics
    - pi_Hydro2EPT
    - pi_Hydro2Trich
    - pi_Ortho2Chi
    - pi_Tanyp2Chi
    - pi_ChCr2Chi

## Changes in Version 0.3.0.9015 (2019-04-11)

- inst/extdata/MetricFlags.xlsx
  - Update ni_total for Hi to match Lo (both 450), Issue \#16

## Changes in Version 0.3.0.9014 (2019-04-02)

- metric.values, Issue \#15
  - Fix metric pi_EphemNoCaeBae
    - Was calculating the same as pi_Ephem.
  - Checked all other “No” metrics.
    - All ok.

## Changes in Version 0.3.0.9013 (2019-01-25)

- metric.values
  - Fix Trombidiformes metric.
    - Mispelled search term so wasn’t working.
  - Added Megaloptera metrics (nt, pi, and pt).

## Changes in Version 0.3.0.9012 (2019-01-17)

- metric.values
  - Remove metric sorting. Rely on fun.MetricNames to request and sort
    metrics.
  - Modify TolVal character check for NA.
  - Add example keeping only some metrics.
- extdata/MetricScoring.xlsx
  - Modify percent metric thresholds (from 0-1 to 0-100).
- extdata/MetricFlags.xlsx
  - Modify percent metric thresholds (from 0-1 to 0-100).

## Changes in Version 0.3.0.9011 (2019-01-17)

- docs/ExcludedTaxaDecisionCrit.pdf
  - Recreate. Wasn’t opening.

## Changes in Version 0.3.0.9010 (2019-01-17)

- extdata/MetricNames.xlsx
  - move “0-100” from notes to description.

## Changes in Version 0.3.0.9009 (2019-01-17)

- metric.values
  - Mistyped “FAMILY” as “Family” when added pi_Baet.

## Changes in Version 0.3.0.9008 (2019-01-17)

- docs
  - Added ExcludedTaxaDecisionCrit.docx

## Changes in Version 0.3.0.9007 (2019-01-17)

- Remove metric names PDF
- metric.scores
  - Add pi_Baet
  - Percent metrics to 0-100 from 0-1.
  - Add pi_Baet to extdata/MetricNames.xlsx
  - input TolVal “NA” (character) to NA.

## Changes in Version 0.3.0.9006 (2019-01-17)

- metric.scores
  - Additional metrics.
  - Metric name sort variable. Issue \#13
  - Organize metrics ni, nt, pi, pt for each section.
  - Update extdata/MetricNames.xlsx
- MapTaxaObs
  - Set up for ability to sort taxa but not active.

## Changes in Version 0.3.0.9005 (2019-01-14)

- DESCRIPTION
  - maps package missing from Imports.
    - Used in MapTaxaObs function.

## Changes in Version 0.3.0.9004 (2019-01-14)

- README
  - Update install example to force vignettes.
- metric.scores
  - pt_habit metrics (n=5) used ni_total instead of nt_total in
    denominator.

## Changes in Version 0.3.0.9003 (2019-01-14)

- MapTaxaObs
  - Account for tibbles as input.
    - Convert to a data frame inside the function. Otherwise fails.
  - Subset function not working in every case. Update line 128.
  - Add map inputs database and regions and funcion inputs.
- extdata/Data_Benthos.xlsx
  - Update with TolVal2 for example in Vignette to avoid warning.
- Vignette
  - SITE_TYPE to INDEX_REGION
  - qc.checks
    - Update example chunk to match example in function; package BCGcalc
      to BioMonTools.

## Changes in Version 0.3.0.9002 (2019-01-14)

- Vignette
  - kable used without library(knitr) in rarify chunk.

## Changes in Version 0.3.0.9001 (2018-12-21)

- Update QC flags input.
- Add metric names to Excel file.
- MetricName to METRIC_NAME in input files.

## Changes in Version 0.3 (2018-12-20)

- New release version.

## Changes in Version 0.2.0.9007 (2018-12-20)

- Metric scoring function, Issue \#12.

## Changes in Version 0.2.0.9006 (2018-12-03)

- README.rmd, Issue \#11.
  - install_github command comes out as smart quotes if copy and paste
    from GitHub website.
  - Looks ok in RStudio but fixed to test if that is the issue.

## Changes in Version 0.2.0.9005 (2018-11-26)

- Added MapTaxaObs.

## Changes in Version 0.2.0.9004 (2018-11-26)

- Percent metrics as 0-1. Issue \#9.

## Changes in Version 0.2.0.9003 (2018-11-26)

- Add qc.checks function from BCGcalc package. Issue \#9
  - qc.checks.R
  - Vignette
  - extdata.xlsx
- Update read_excel with guess_max=10^6 for bio data file.

## Changes in Version 0.2.0.9002 (2018-11-20)

- Update SuperFamily Tipuloidea.
  - Update data and examples in function and vignette

## Changes in Version 0.2.0.9001 (2018-11-20)

- Add Travis CI.

## Changes in Version 0.2.0 (2018-11-20)

- Initial release version.

## Changes in Version 0.1.0.9004 (2018-11-20)

- Add SuperFamily
  - function
  - vignette
  - example data file
- Add metric names files to extdata and doc

## Changes in Version 0.1.0.9003 (2018-11-20)

- markExcluded
  - Add function with examples
- extdata\_Benthos.xlsx
  - Updated bad Excluded entries.
    - FALSE to TRUE, n=6
    - TRUE to FALSE, n=1
- DESCRIPTION
  - Update Imports and Suggests
- NEWS
  - Update Planned and Future Updates
- README
  - Tweak language in Usage section.
  - Update icons.
  - Add Travis CI.
- Vignette

## Changes in Version 0.1.0.9002 (2018-11-16)

- extdata\_Benthos.xlsx
  - OneDrive messed up version included in package.

## Changes in Version 0.1.0.9001 (2018-11-16)

- Added basic package structure.
  - Folders (data, data-raw, inst/doc, inst/extdata, man, R, vignettes)
  - NEWS, README, LICENSE
- Update DESCRIPTION, NEWS, and README
- Add functions and data
  - rarify
  - metric.values

## Changes in Version 0.1.0 (2018-11-16)

- Initial commit on GitHub.
