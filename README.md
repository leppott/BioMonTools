README-BioMonTools
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

    #> Last Update: 2026-01-06 13:42:46.059798

# BioMonTools <img src="man/figures/logo.png" align="right" height="139" />

Functions to aid the data analysis of bioassessment and biomonitoring
data. Suite of functions and tools for metric calculation and scoring
for mult-metric indices and related data manipulation.

<!-- badges: start -->

# Badges

[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/leppott/BioMonTools/graphs/commit-activity)
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Life
Cycle](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![GitHub
license](https://img.shields.io/github/license/leppott/BioMonTools.svg)](https://github.com/leppott/BioMonTools/blob/master/LICENSE)

[![CodeFactor](https://www.codefactor.io/repository/github/leppott/BioMonTools/badge)](https://www.codefactor.io/repository/github/leppott/BioMonTools)
[![codecov](https://codecov.io/gh/leppott/BioMonTools/branch/master/graph/badge.svg)](https://app.codecov.io/gh/leppott/BioMonTools)
[![R-CMD-check](https://github.com/leppott/BioMonTools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/leppott/BioMonTools/actions/workflows/R-CMD-check.yaml)

[![CRAN
release](https://www.r-pkg.org/badges/version/badger?color=orange)](https://cran.r-project.org/package=badger)
[![CRAN
downloads](http://cranlogs.r-pkg.org/badges/grand-total/BioMonTools?color=blue)](https://cran.r-project.org/package=BioMonTools)

[![GitHub
issues](https://img.shields.io/github/issues/leppott/BioMonTools.svg)](https://GitHub.com/leppott/BioMonTools/issues/)
<!-- badges: end -->

# Installation

## CRAN

To install from CRAN use the code below.

``` r
install.packages("BioMonTools")
```

## GitHub

To install the most current version on GitHub use the code below. The
use of “force = TRUE” ensures the package is installed even if already
present. If the package `remotes` is missing the code below will install
it.

``` r
if(!require(remotes)){install.packages("remotes")}  #install if needed
install_github("leppott/BioMonTools", force=TRUE)
```

Vignettes are not installed by default. If you want the additional
documentation (recommended) then use this version of the code.

``` r
if(!require(remotes)){install.packages("remotes")}  #install if needed
install_github("leppott/BioMonTools", force=TRUE, build_vignettes=TRUE)
```

If having issues with install (e.g., ‘cannot open URL’) it could be a
latency issue with GitHub.

Use the code below before retrying the above install commands.

``` r
options(timeout=400)
```

# Life Cycle Status

Stable and mature.

# Usage

By those using involved with bioassessment and biomonitoring and the
need for data manipulation.

A common set of tools will standardize outputs across entities. No
longer need to use the same database or spreadsheet. Only need to format
data for use with these tools.

# Documentation

Vignette and install guide updates are planned for the future.

Leppo, E.W., J. Stamp, and B. Block. 2025. BioMonTools: Tools for
Biomonitoring and Bioassessment. R package version 1.2.4.9004.
<https://github.com/leppott/BioMonTools>

# Issues

<https://github.com/leppott/BioMonTools/issues>

# Planned Updates

Add as issues for tracking purposes. Will add metrics and indices as
needed.

Updates to CRAN as have time.
