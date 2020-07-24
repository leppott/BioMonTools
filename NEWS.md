NEWS
================
<Erik.Leppo@tetratech.com>

<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->

    #> Last Update: 2020-07-24 16:15:07

# Planned Updates

  - None at this time.

# Future Possible Updates

  - Periphyton metrics.
  - TaxaList Check
  - Map taxon observations

# Version History

## Changes in Version 0.4.0.9025 (2020-07-24)

  - Remove extra file (metric.values.tsv) from root folder of package.

## Changes in Version 0.4.0.9024 (2020-07-24)

  - Updates for MI index; Issue \#3
      - metric.values.R
          - Add pi\_IsopGastHiru
          - Add pi\_EPTNoBaeHydro
          - Add pi\_tv\_toler6
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
  - Update dataset “data\_benthos\_PacNW”.
      - Index\_Name had a number and was different on each line (Excel
        autofill error).
      - Last updated 0.4.0.9008 (2020-04-28).
  - Update tests for metric.values calc.
  - metric.values.R
      - Modify NonTarget check for is.na() in cases where the column is
        missing; Issue \#39

## Changes in Version 0.4.0.9022 (2020-07-23)

  - metric.values; Issue \#3, Issue \#41
      - Fix pi\_EPT and pi\_EPTNoCheu
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
      - Correct nt\_intol4\_EPT to nt\_tv\_intol4\_EPT
      - Add x\_Becks3
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
          - nt\_tv\_intol4
          - nt\_tv\_intol4\_EPT
          - pi\_Ortho
          - pi\_Chiro\_Anne
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
      - Add new function to calculate statistics for developing
        multi-metric index
  - data\_mmi\_dev
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
          - nt\_BCG\_attNA
          - pi\_BCG\_attNA
          - pt\_BCG\_attNA
          - Habitat metrics, 8 values, nt, pi, pt.
  - MetricNames.xlsx
      - Add new metrics.
  - New data set to utitilize new metrics.
      - data\_benthos\_PacNW

## Changes in Version 0.4.0.9007 (2020-04-27)

  - metric.values.R
      - Additional metrics, Issue \#3.
          - nt\_BCG\_att1
          - pi\_BCG\_att1
          - pt\_BCG\_att1

## Changes in Version 0.4.0.9006 (2020-04-24)

  - metric.values.R
      - QC on line 371 did not declare package. Issue \#33.
      - Define pipe in subfunctions.
  - metric.scores.R
      - Modify example. Issue \#34.
          - Only one occurence of Index\_Name and Index\_Region in the
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
          - N\_TAXA needs to be zero.
          - Both bugs and fish will exclude all other N\_TAXA = 0 but
            keep if TAXAID == “NONE”.
          - nt\_total metric has condition for N\_TAXA \> 0.
          - All other metrics will calculate but receive values of 0,
            NA, or NaN as appropriate.

## Changes in Version 0.4.0.9001 (2020-02-27)

  - Tweak for GA Fish IBI, Issue \#3
      - Modify ni\_natnonhybrid and ni\_natnonhybridnonlepomis to also
        exclude mosquitofish.
          - MetricScoring.xlsx
          - metric.score.R
      - Add nt\_nativenonhybrid
          - MetricScoring.xlsx
          - metric.score.R
  - metric.score.R
      - Update ni\_natnonhybridnonlepomis.

## Changes in Version 0.4.0 (2020-02-24)

  - Changes to fish metric significant enough for a minor version
    update.
      - Some previous calculations may not work. Function made more
        generic.

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
  - vignette\_BioMonTools.Rmd
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
      - Tweak index NormDist\_135.
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
      - Error in excluding marine metrics when no metric list given
        (i.e., want all metrics).
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
      - Updates for GBI\_MS\_2013

## Changes in Version 0.3.3.9008 (2019-10-16)

  - MetricScoring.xlsx, Issue \#3
      - Updates for GBI\_MS\_2013
      - Numeric to text.

## Changes in Version 0.3.3.9007 (2019-10-16)

  - MetricScoring.xlsx, Issue \#3
      - Updates for GBI\_MS\_2013
      - Numeric to text.

## Changes in Version 0.3.3.9006 (2019-10-15)

  - MetricScoring.xlsx, Issue \#3
      - Updates for GBI\_MS\_2013

## Changes in Version 0.3.3.9005 (2019-10-15)

  - MetricScoring.xlsx, Issue \#3
      - Updates for GBI\_MS\_2013

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
      - Data\_Benthos.xlsx
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
          - rmarkdown listed under imports and suggests. Remove from
            suggests.
          - Add to suggests; ggplot2 and reshape2.
          - Add MBSStools to suggests. May import the data later and
            remove.
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
          - x\_NCBI, x\_D\_Mg, x\_D\_G
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
          - pi\_Hydro2EPT
          - pi\_Hydro2Trich
          - pi\_Ortho2Chi
          - pi\_Tanyp2Chi
          - pi\_ChCr2Chi

## Changes in Version 0.3.0.9015 (2019-04-11)

  - inst/extdata/MetricFlags.xlsx
      - Update ni\_total for Hi to match Lo (both 450), Issue \#16

## Changes in Version 0.3.0.9014 (2019-04-02)

  - metric.values, Issue \#15
      - Fix metric pi\_EphemNoCaeBae
          - Was calculating the same as pi\_Ephem.
      - Checked all other “No” metrics.
          - All ok.

## Changes in Version 0.3.0.9013 (2019-01-25)

  - metric.values
      - Fix Trombidiformes metric.
          - Mispelled search term so wasn’t working.
      - Added Megaloptera metrics (nt, pi, and pt).

## Changes in Version 0.3.0.9012 (2019-01-17)

  - metric.values
      - Remove metric sorting. Rely on fun.MetricNames to request and
        sort metrics.
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
      - Mistyped “FAMILY” as “Family” when added pi\_Baet.

## Changes in Version 0.3.0.9008 (2019-01-17)

  - docs
      - Added ExcludedTaxaDecisionCrit.docx

## Changes in Version 0.3.0.9007 (2019-01-17)

  - Remove metric names PDF
  - metric.scores
      - Add pi\_Baet
      - Percent metrics to 0-100 from 0-1.
      - Add pi\_Baet to extdata/MetricNames.xlsx
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
      - pt\_habit metrics (n=5) used ni\_total instead of nt\_total in
        denominator.

## Changes in Version 0.3.0.9003 (2019-01-14)

  - MapTaxaObs
      - Account for tibbles as input.
          - Convert to a data frame inside the function. Otherwise
            fails.
      - Subset function not working in every case. Update line 128.
      - Add map inputs database and regions and funcion inputs.
  - extdata/Data\_Benthos.xlsx
      - Update with TolVal2 for example in Vignette to avoid warning.
  - Vignette
      - SITE\_TYPE to INDEX\_REGION
      - qc.checks
          - Update example chunk to match example in function; package
            BCGcalc to BioMonTools.

## Changes in Version 0.3.0.9002 (2019-01-14)

  - Vignette
      - kable used without library(knitr) in rarify chunk.

## Changes in Version 0.3.0.9001 (2018-12-21)

  - Update QC flags input.
  - Add metric names to Excel file.
  - MetricName to METRIC\_NAME in input files.

## Changes in Version 0.3 (2018-12-20)

  - New release version.

## Changes in Version 0.2.0.9007 (2018-12-20)

  - Metric scoring function, Issue \#12.

## Changes in Version 0.2.0.9006 (2018-12-03)

  - README.rmd, Issue \#11.
      - install\_github command comes out as smart quotes if copy and
        paste from GitHub website.
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
  - Update read\_excel with guess\_max=10^6 for bio data file.

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
      - Folders (data, data-raw, inst/doc, inst/extdata, man, R,
        vignettes)
      - NEWS, README, LICENSE
  - Update DESCRIPTION, NEWS, and README
  - Add functions and data
      - rarify
      - metric.values

## Changes in Version 0.1.0 (2018-11-16)

  - Initial commit on GitHub.