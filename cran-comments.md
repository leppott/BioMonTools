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

0 errors ✔ | 0 warnings ✔ | 0 notes ✔
R CMD check succeeded

## Error
zero

## Warnings
zero

## NOTE
On WinBuilder get the following Note:
* checking top-level files ... NOTE
Non-standard file/directory found at top level:
  'cran-comments.md'

# Downstream dependencies

devtools::revdep("BioMonTools")
character(0)

tools::dependsOnPkgs("BioMonTools")
"BCGcalc"

There are currently no downstream CRAN dependencies for this package.

This is a new release
