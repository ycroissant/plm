# The `plm` Package - Linear Models and Tests for Panel Data 

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/plm)](https://CRAN.R-project.org/package=plm)
[![CRAN check](https://cranchecks.info/badges/summary/plm)](https://cran.r-project.org/web/checks/check_results_plm.html)
[![Downloads](https://cranlogs.r-pkg.org/badges/plm)](https://CRAN.R-project.org/package=plm)
<!-- badges: end -->

## About

`plm` is a package for panel data econometrics for the **R** statistical computing
environment. It was first published on **CRAN** in 2006.

It includes (non-exhaustive overview):

- a set of functions to estimate models:
  - `plm`: panel data estimators (within/fixed effects, random effects, between,
           first-difference, nested random effects), incl. instrumental-variable 
           estimation techniques (IV) and Hausman-Taylor-style models,
  - `pgmm`: generalized method of moments (GMM) estimation for panel data,
  - `pggls`: estimation of general feasible generalized least squares models,
  - `pmg`: mean groups (MG), demeaned MG and common correlated effects (CCEMG) estimators,
  - `pcce`: estimators for common correlated effects mean groups (CCEMG) and pooled (CCEP) for panel data with common factors,
  - `pvcm`: variable coefficients models,
  - `pldv`: panel estimators for limited dependent variables.

- a set of testing functions:
  - model specification (`phtest`, `pFtest`, `pooltest`, `plmtest`, `pwaldtest`, `piest`, `aneweytest`, `mtest`, `sargan`),
  - serial correlation (`pbgtest`, `pwfdtest`, `pbnftest`, `pdwtest`, `pwartest`, `pbsytest`, `pbltest`), 
  - cross-sectional dependence (`pcdtest`),
  - panel unit root (`purtest`, `cipstest`, `phansi`),
  - panel Granger (non-)causality (`pgrangertest`),
  
- a set of robust covariance matrix estimators (incl. various weighting schemes
  for small sample adjustment):
  - `vcovHC`: Arellano (1987), White (1980),
  - `vcovBK`: Beck and Katz (1995) (PCSE),
  - `vcovNW`: Newey and West (1987),
  - `vcovDC`: double-clustering robust (Thompson (2011), Cameron et al. (2011)),
  - `vcovSCC`: Driscoll and Kraay (1998).

- an enhanced data frame, called `pdata.frame`, to deal with data sets for which 
  observations are identified by a combination of two indexes, and panel data 
  transformation functions (e.g, `Within`, `Between`, `lag`, `lead`, `diff`).


## Installation

To install the released version from **CRAN**:

```{r}
install.packages("plm")
```

The package's CRAN website is https://cran.r-project.org/package=plm.


The development of package `plm` takes place on GitHub at https://github.com/ycroissant/plm.
To install the development version from **GitHub**, use, e.g.:
```{r}
# install.packages("remotes")
remotes::install_github("ycroissant/plm")
```


## Documentation

Package plm comes with documentation: Besides the usual help pages for each function,
the vignettes provide a gentle introduction to the package and some functions
(vignettes are available at the package's CRAN website https://cran.r-project.org/package=plm).

New package users are advised to start with the first vignette 
*Panel data econometrics in R: the plm package* for an overview of the package. 
A more in-depth treatment of estimation of error component models and instrument
variable models is in the second vignette *Estimation of error component models 
with the plm function*.

Further, many textbooks treat package `plm` and/or use it in their examples:

* Croissant/Millo, *Panel Data Econometrics with R*, 2019, John Wiley & Sons, Hoboken.

* Kleiber/Zeileis, *Applied Econometrics with R*, 2008, Springer, New York. Esp.
  chapter 3.6.

* Hanck/Arnold/Gerber/Schmelzer, *Econometrics with R*, online book 
  https://www.econometrics-with-r.org/. Esp. chapter 10.

* Heiss, *Using R for Introductory Econometrics*, 2nd edition, 2020,
  Independent Publishing, Düsseldorf, also available online at http://www.urfie.net/.
  A companion book using R to Wooldridge, *Introductory Econometrics*, esp.
  chapters 13-14.
