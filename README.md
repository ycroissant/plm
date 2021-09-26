# The `plm` Package: Linear Models and Tests for Panel Data 

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/plm)](https://CRAN.R-project.org/package=plm)
[![CRAN check](https://cranchecks.info/badges/summary/plm)](https://cran.r-project.org/web/checks/check_results_plm.html)
[![Downloads](https://cranlogs.r-pkg.org/badges/plm)](https://CRAN.R-project.org/package=plm)
<!-- badges: end -->

# About

`plm` is a package for panel data econometrics for the **R** statistical computing
environment. It was first published on **CRAN** in 2006.

It includes:

- a set of functions to estimate models:
  - `plm`: panel data estimators, incl. instrumental-variable estimation (IV).
  - `pvcm`: variable coefficients models,
  - `pgmm`: generalized method of moments (GMM) estimation for panel data,
  - `pggls`: estimation of general feasible generalized least squares models,
  - `pmg`: mean groups (MG), demeaned MG and common correlated effects (CCEMG) estimators,
  - `pcce`: estimators for common correlated effects mean groups (CCEMG) and pooled (CCEP) for panel data with common factors,
  - `pldv`: panel estimators for limited dependent variables.

- a set of testing functions (model specification, serial correlation, 
  cross-sectional dependence, panel unit root, panel Granger (non-)causality),
  
- a set of robust covariance matrix estimators.

- an enhanced data frame, called `pdata.frame`, to deal with data sets for which 
  observations are identified by a combination of two indexes,

# Installation

To install the released version from **CRAN**:

`install.packages("plm")`

The package's CRAN website is https://cran.r-project.org/package=plm.

To install the development version from **GitHub**:

`devtools::install_github("ycroissant/plm")`

The development of package `plm` takes place on GitHub at https://github.com/ycroissant/plm
