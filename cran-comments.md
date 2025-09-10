Erik.Leppo@tetratech.com
2025-09-10

# Test environments
* local Win 11, R 4.5.1
* win-builder (release)
* win-builder (devel)
* GitHub Actions (Win release)
* GitHub Actions (Ubuntu release)
* GitHub Actions (Ubuntu devel)
* GitHub Actions (Ubuntu old)
* MacOS (release)

# R CMD check results

0 errors ✔ | 0 warnings ✔ | 1 notes ✔
R CMD check succeeded

## Error
zero

## Warnings
zero

## NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Erik W. Leppo <Erik.Leppo@tetratech.com>'

New submission

Possibly misspelled words in DESCRIPTION:
  Bioassessment (3:36)
  Biomonitoring (3:18)
  benthic (17:66)
  bioassessment (16:5)
  biomonitoring (15:58)
  macroinvertebrates (18:5)
  multimetric (17:22)
  periphyton (18:31)
  
All spelled correctly.

# Downstream dependencies

devtools::revdep("BioMonTools")
character(0)

tools::dependsOnPkgs("BioMonTools")
"BCGcalc"

There are currently no downstream CRAN dependencies for this package.
