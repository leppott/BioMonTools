README-BioMonTools
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

    #> Last Update: 2021-02-22 10:57:48

# Purpose

Functions to aid the data analysis of bioassessment and biomonitoring
data. Suite of functions and tools for metric calculation and scoring
for mult-metric indices and related data manipulation.

# Badges

[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/leppott/BioMonTools/graphs/commit-activity)
[![](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![GitHub
license](https://img.shields.io/github/license/leppott/BioMonTools.svg)](https://github.com/leppott/BioMonTools/blob/master/LICENSE)

[![CodeFactor](https://www.codefactor.io/repository/github/leppott/BioMonTools/badge)](https://www.codefactor.io/repository/github/leppott/BioMonTools)
[![codecov](https://codecov.io/gh/leppott/BioMonTools/branch/master/graph/badge.svg)](https://codecov.io/gh/leppott/BioMonTools)
[![R-CMD-check](https://github.com/leppott/BioMonTools/workflows/R-CMD-check/badge.svg)](https://github.com/leppott/BioMonTools/actions)

[![GitHub
issues](https://img.shields.io/github/issues/leppott/BioMonTools.svg)](https://GitHub.com/leppott/BioMonTools/issues/)

[![GitHub
release](https://img.shields.io/github/release/leppott/BioMonTools.svg)](https://GitHub.com/leppott/BioMonTools/releases/)
[![Github all
releases](https://img.shields.io/github/downloads/leppott/BioMonTools/total.svg)](https://GitHub.com/leppott/BioMonTools/releases/)

# Installation

To install the current version use the code below to install from
GitHub. The use of “force = TRUE” ensures the package is installed even
if already present. If the package `devtools` is missing the code below
will install it.

``` r
if(!require(devtools)){install.packages("devtools")}  #install if needed
install_github("leppott/BioMonTools", force=TRUE)
```

Vignettes are not installed by default. If you want the additional
documentat (recommended) then use this version of the code.

``` r
if(!require(devtools)){install.packages("devtools")}  #install if needed
install_github("leppott/BioMonTools", force=TRUE, build_vignettes=TRUE)
```

# Status

In development.

# Usage

By those using involved with bioassessment and bionmonitoring and the
need for data manipulation.

A common set of tools will standardize outputs across entities. No
longer need to use the same database or spreadsheet. Only need to format
data for use with these tools.

# Documentation

Vignette and install guide udpates are planned for the future.

# Issues

<https://github.com/leppott/BioMonTools/issues>
