---
title: NEWS/Changelog for package plm
subtitle: plm - Linear Models for Panel Data - A set of estimators and tests for
          panel data econometrics - <https://cran.r-project.org/package=plm> 
          (development repository <https://github.com/ycroissant/plm/>)
---

# 2.6-6:

### Features:
* `vcovBK`: enabled `type = "sss"` (weighting scheme), the Stata-style small 
            sample adjustment (like for other `vcovXX` functions for a long time).

### Fixes:
* `vcovXX`: for first-difference models with weighting scheme `type = "HC1"`
            fix degrees of freedom used in weighting (was previously only correct 
            for `vcovBK`).
* `predict.plm`: when argument `newdata` is a pdata.frame, the (internal) model 
                 frame creation based on the new data is fixed ([#67](https://github.com/ycroissant/plm/issues/67)).
### Others:

* `sargan`: switch from plain function to generic and method for `pgmm` objects.

***

# 2.6-5

### Fixes:
* `pgmm`: argument `fsm` can now be set to influence the first step's weighting matrix.
* `mtest`:
    * fix test for one-step models with robust covariance matrix and two-steps models.
    * result object's statistic is now a numeric (was 1x1 matrix).
* `vcovXX`: FD models with only one observation per group prior to 
    first-differencing errored ([#58](https://github.com/ycroissant/plm/issues/58)).
* `pggls`: FD models errored with the data constellation as described above for `vcovXX`.
* `is.pdata.frame` (non-exported helper function): fix part of the check if object 
    does not have an index.
    
### Documentation:
* `?mtest` now has more explanation with replication examples.
* First vignette: improved description of `pgmm`'s `effect` argument.


***

# plm 2.6-4

### Features:
* `pvcm`: 
  * implemented parameter homogeneity test for variable coefficients model
    (`model = "random"`), printed when summary is printed.
  * implemented estimation of single unbiased coefficients (incl. variance/std.
    error) for `model = "random"`, can be extracted via 
    `return_value$single.coef`, `return_value$single.vcov`, 
    `return_value$single.std.error`, respectively.

### Speed-up:
* Replaced base R's `duplicated` with faster `collapse::fduplicated(x, all = FALSE)`.
* `pvcm`: faster due to improved code path.

### Fixes:
* `phtest(<.>, method = "aux")` (regression-based Hausman test): errored in case of
  `NA`s and one independent variable (reported by Kurt Schmidheiny).
* `pvcm`:
    * return value now always has a single valid value in `$args$model`.
    * tiny fix for coefficient estimation in case of NA coefficients.
* `pdata.frame`: fix in detection of 3rd index variable (relevant only in quite 
   specific circumstances) ([#50](https://github.com/ycroissant/plm/pull/50), 
   thanks to @MichaelChirico).

### Others:
* Compatibility for `dplyr::arange` by supplying a `pdata.frame` method for `arrange`
  which takes care of pdata.frame's index attribute ([#46](https://github.com/ycroissant/plm/issues/46)
  , thanks to @ssoyounglee for reporting and @zeileis for prompt analysis and hints).
* Estimation functions (e.g., `plm()`, `pcce()`) now warn if a data input claims to 
  be a pdata.frame but has non-compliant properties. This can happen due to data 
  manipulation by non-pdata.frame-aware functions (e.g., `dplyr`'s row-filtering 
  on pdata.frame does not take care of pdata.frame's index attribute).
* `pdata.frame()`:
   * does not stop anymore when a pdata.frame is input, just continues to create
     a new pdata.frame from input.
   * warns if pdata.frame with non-compliant features is input.
* `plm()` passes on ellipsis ("dots" (`...`)) to internal pdata.frame creation, 
  further passed on to internal usage of `data.frame` (if pdata.frame is to be 
  created internally due to non-pdata.frame passed in `plm()`'s `data` argument).
* `pvar.pseries`: when printed, original variable's name is printed 
  (not always `x` anymore).
* `pvcm`: more informative about non-estimable models:
   * `model = "random"`: implemented stopping control for non-estimable model 
     if only one group.
   * improved error message if too few observations.
   * `model = "within"`: be less strict about required number of observations 
     for pure coefficient estimation (#obs = #coefs now allowed, errored before, 
     variance cannot be estimated then, though). Note: this relaxation is not 
     possible for the `model = "random"` case (Swamy (1970)).

***


# plm 2.6-3

### Speed-up:
* `lag`, `lead`, and `diff` on panel data (pseries, pdata.frame) are now faster 
    due to usage of the fast collapse functions internally (`collapse::flag`, 
    `collapse::fdiff`), on large data sets approx. 10x to 20x faster.
* `plm/pggls(., model = "fd")`: faster first-difference model estimation (due to 
   using `collapse::fdiff`).
   NB: FD models are based on row-wise differences, not (yet) time-wise.
* Usages of `split()` substituted with `collapse`'s fast `rsplit()`/`gsplit()`.

### Minors:
* `ciptstest`: error gracefully when argument `lags = 0L` (partly addresses 
   [#39](https://github.com/ycroissant/plm/issues/39)).
* model estimation with dot (`.`) in formula now possible, e.g., `plm(y ~ ., data = <data>)`.
  This needs dependency package `Formula` in version >= 1.2-5, on CRAN since
  2023-02-24 (addresses [#35](https://github.com/ycroissant/plm/issues/35)).
* `has.intercept`: gained new argument `data` to support formulae containing a 
  `.` (dot) (partly addresses [#35](https://github.com/ycroissant/plm/issues/35)).
* `vcovXX.pggls`: stopping break for `vcovHC`, `vcovNW`, `vcovBK`, 
   `vcovSCC`, `vcovDC`, and `vcovG` applied to `pggls` objects implemented via
   newly introduced methods giving an informative error message. (P)GGLS models 
   estimate a parametric covariance, hence it does not make much sense to 
   robustify them.

### Clean-ups:
* `phansitest`: deprecated working alias `phansi` for `phansitest` is now an 
   error (see also NEWS entry for 2.6-0 when `phansi` was renamed to `phansitest`).
* `pFormula`: removed long deprecated class, function, and associated 
   methods (see deprecation note in plm 2.0-0 (2019-05-14)).

***

# plm 2.6-2

### Features:
* `predict.plm`:
  * prediction implemented for fixed effects models incl. support for
    argument `newdata` and out-of-sample prediction. Help page (`?predict.plm`) 
    added to specifically explain the prediction for fixed effects models and the 
    out-of-sample case.
  * change: case without supplied non-`NULL` `newdata` now gives the predicted 
    values of the outer model (before: inner model).

### Speed-up:
* Many functions which split data by an index dimension are now faster on large
  data sets due to more efficient splitting approach, esp. `pcce`, `pggls`, 
  `pmg`, `cipstest`, `pcdtest`.

### Fixes:
* `mtest`: error gracefully if argument `order` specifies a value larger than or 
   equal to the number of available observations ([#23](https://github.com/ycroissant/plm/issues/23)).
* `summary.pgmm` (and associated print method): does not execute `mtest` with 
   `order = 2L` in case there are too few observations ([#23](https://github.com/ycroissant/plm/issues/23)).
* `plm` for IV estimation: respect suppressed intercept in formula's 2nd part of 
   RHS.
* `vcovXX` functions: fix vcov output for first-difference models in case of 
  `cluster = "time"`.
* `pggls`: unbalanced first-difference models do not error anymore.
* `pldv`: variance-covariance fixed for case `model = "fd"` when `objfun == "lsq"` 
   and `sample == "cens"`.
* `fixef(., type = "dfirst"`): for models with `length(fixef(<model_object>)) == 2`, 
  `fixef` does not error anymore and for `length(fixef(<model_object>)) == 1`, 
  the result is more sane (`numeric(0)`).
  
### Others/Minors/Admin:
* `purtest`: errors more informatively when no non-NA cases are left after lagging.
* `plmtest` and `pbsytest`: text in result object's `method` does not 
  contain information about balanced/unbalanced panel anymore and is, thus, not
  printed anymore.
* `print.pseries`: return input object invisibly (prevents double printing when 
  print() is called explicitly on a pseries).
* Package's maintainer changed (for technical reasons only).

***

# plm 2.6-1

### Fixes:
* pmodel.response: `I()` in LHS of formula led to error in estimation with plm
  ([#17](https://github.com/ycroissant/plm/issues/17)).

### Changes:
* summary.pseries: returned object is class "summary.pseries" for all inputs and
  now has added as attribute "SummaryDefault" the summary of the base object
  (e.g., summary(your_numeric)) if the base object is not a factor, logical, or 
  character. For factors, logicals, and characters in a pseries, the 
  summary.pseries object is simply the summary of the base object.
  Printing of the summary object now gives the pseries-specific output as before 
  (if applicable) and additionally summary printing of the base object.

### Clean-ups:
* pdata.frame: argument `stringsAsFactors` set to `FALSE` (was previously
  `default.stringsAsFactors()` to determine value from options, but that 
   will be deprecated as of R 4.2.0).
* make.pbalanced: for argument `balance.type` the value `"shared"` is not 
  accepted anymore, it was allowed for back-compatibility for some time. Use
  `balance.type = "shared.times"` for the same result (see also NEWS entry for 
  version 1.6-6).
* Adjust example benchmark in `?plm.fast` to new behaviour from version 1.4.8
  onwards of package `microbenchmark` 
  (<https://github.com/joshuaulrich/microbenchmark/issues/34>).

### Dependencies:
* Minimum required R version corrected to R 3.2.

***

# plm 2.6-0

### Speed-up:
* Fast mode is now the default for the package: when the package is attached,
  `options("plm.fast" = TRUE)` is set (by R's .onAttach mechanism), requiring 
  package `collapse` as a hard dependency.
* *Recommendation*: Install suggest-dependency package `fixest` or `lfe` as a 
  further significant speed up for the two-ways within transformation (as in 
  two-ways fixed effects models) is gained.
* See `?plm.fast` for more information and a benchmark.

### Features:
* make.dummies: new simple function to conveniently create contrast-coded dummies 
  from a factor.


### Clean-ups:
* phansi: function renamed to phansitest for name consistency, with a *temporary*
  back-compatible solution. [back-compatibility removed in plm 2.6-3]
* phtest: for formula method, argument 'effect' is now explicit as 4th argument 
  (previously, it was extracted from ellipsis (...)).
* detect_lin_dep/detect.lindep: alias detect_lin_dep removed, thus this
  functionality is now only accessible via detect.lindep (function was renamed
  from detect_lin_dep to detect.lindep in CRAN version 1.7-0 (2019-01-04),
  detect_lin_dep was originally introduced in CRAN version 1.6-4 (2016-11-30)).
* has.intercept.plm: removed temporary back-compatible solution for ill-introduced 
  argument 'part', use argument 'rhs' instead (see also NEWS for 2.4-2).
* Within (only matrix method): removed matrix-specific argument 'rm.null'
  (has been defunct since August 2018 anyways).
* plm:
  * error informatively if argument effect = "nested" and model != "random" 
    (previously, this was a warning incl. argument adjustment, see also 
    NEWS for 2.4-2).
  * (as well as pht) for argument 'inst.method', standard R argument matching 
    and error message are used (so no more dedicated message if misspelled value 
    "bmc" instead of "bms" is used, "bmc" was a long-standing typo, then accepted 
    with a warning and later errored with an informative error message, see also 
    NEWS for 2.4-0, 1.6-6).
* pggls:
  * argument model: removed "random" from the list of official argument's
    values as its use is deprecated (model = "pooling" does the same; however,
    value "random" is still accepted and adapted to "pooling" for 
    back-compatibility with a warning).
  * print.summary.pggls: fix printed model name in case default model was selected
    (print only one model name, previously all three model names possible for the
    function were printed).

### Documentation:
* DESCRIPTION file: more comprehensive description of the package, so displayed 
  on CRAN.
* First vignette gained an example for the auxiliary-regression-based Hausman 
  test (`phtest(. , method = "aux")`).

### Dependencies:
 * Shifted package `collapse` from 'Suggests' to 'Imports'.
 * Removed from 'Suggests' as not needed: `bookdown`, `clusterSEs`, `Ecdat`, 
   `foreign`, `pcse`, `pglm`, `spdep`, `splm`, and `stargazer`. This safeguards 
   package `plm` shall the previously suggested packages be removed from CRAN.
 
***

# plm 2.4-3

* Release to pacify CRAN additional checks with various BLAS implementations/platforms:
  Checks moaned about neglectable small numerical differences vs. (at times) platform-specific
  reference output (`.Rout.save` files).
  Moved almost all test files to directory `inst/tests` so they are not run on CRAN.
  Tests can be run manually and by `R CMD check --test-dir=inst/tests plm_VERSION.tar.gz`.

### Admin:
* Source code repository for development is now on GitHub <https://github.com/ycroissant/plm>,
  not on R-Forge anymore.
* Added a REAMDE file to the package giving basic information about package, 
  displayed on CRAN (and on GitHub repository).
* Update one author's e-mail address.

***

# plm 2.4-2

### Speed-up:
 * "Fast mode" is not yet the default. To enable, set
   `options("plm.fast" = TRUE)` manually or in your `.Rprofile` file (see
   `?plm.fast`, also for benchmarks), option introduced in plm version 2.4-0.
   It is planned to default to "fast mode" for the next CRAN release of
   plm (then making package `collapse` a hard dependency).
 * Further speed-up if `options("plm.fast" = TRUE)` is set: In case package
   `fixest` or `lfe` is available locally *in addition* to package `collapse`,
   the two-ways fixed effect transformation is significantly faster compared to 
   the case if only `collapse` is available due to specialised algorithms in these
   two packages, all being fully integrated into the usual plm functions/user
   interfaces (`fixest` is preferred over `lfe`, in this case, plm uses internally
   `collapse::fhdwithin` which in turn uses `fixest::demean`). Thanks to Sebastian
   Krantz for guidance on this.
 * various efficiency gains throughout the package by using more vapply(),
   crossprod(), lm.fit(), better branching, rowSums(., dims = 2L) (instead of 
   apply(., 1:2, sum)), etc., e.g., in plm for non-default random IV cases 
   (cases with `inst.method = "baltagi"` / `"am"` / `"bms"`), pmg, pcce, purtest.

### Features:
 * phansi: new function for Simes (1986) test applied to panels for panel unit
   root testing, as suggested in Hanck (2013) [later renamed to phansitest in 
   plm 2.6].
 * pseriesfy: new function to make each column of a pdata.frame a pseries, see
   `?pseriesfy` for background and useful examples. (Faster version is executed
   if `options("plm.fast" = TRUE)` is set, see `?plm.fast` (then internally
   using `collapse::dapply`)). Thanks to Sebastian Krantz for inspiration.

### Fixes:
 * between (and hence fixef, ranef): order of output is order of *factor levels*
   again (this reverts a change introduced in 2.4-0, there called a fix introducing
   the order of the appearance in the data which is actually not desirable). Change
   is relevant in specific unbalanced data constellations.
 * fixef: for two-ways FE models, fixef does not error anymore if factor is in
   model and not anymore in IV case ([#10](https://github.com/ycroissant/plm/issues/10)).
 * vcovG (hence vcovHC, vcovDC, vcovNW, vcovSCC) and vcovBK: fix bug in case
   of IV estimation with only one regressor (errored previously) 
   ([#4](https://github.com/ycroissant/plm/issues/4)).
 * within_intercept:
     * fix bug which caused an error for FE models with only one regressor 
     ([#4](https://github.com/ycroissant/plm/issues/4)).
     * error informatively for IV models as not suitable.
 * between.matrix: do not coerce result to numeric vector for n x 1 matrix
   input (by using drop = FALSE in extraction) (prior to this fix, estimation
   of the between model with only an intercept errored).
 * pvcm: intercept-only models are now estimable.
 * detect.lindep: argument 'suppressPrint' now correctly passed on/respected
   (methods for data frame and matrix) ([#11](https://github.com/ycroissant/plm/issues/11)).
 * has.intercept.plm: argument 'part' renamed to 'rhs', argument values
   (integer or NULL) aligned with and correctly passed on to 
   has.intercept.Formula (with a *temporary* back-compatible solution 
   [back-compatibility removed in plm 2.6-0]).
 * pcdtest: for formula method, the formula is evaluated in the parent environment.
 * groupGenerics: no more warning in arithmetic operations on pseries when index
   of both operands have same length but different content (e.g., something like
   this does not warn anymore:
   `your_pseries[1:(length(your_pseries)-1)] + your_pseries[2:length(your_pseries)]`).

### Others:
 * plm: for the nested random effect model (`effect = "nested"`), check if
    argument `model = "random"` is set, if not, plm now warns and adjusts 
    accordingly (will become an error in the future).
 * pgmm: printing of summary gives more information about the model estimated
   (print.summary.pgmm).
 * purtest: now checks for NA-values, drops any, and warns about dropping.
 * piest: better printing (handling of 'digits' and 'subset' argument) 
     (print.piest, print.summary.piest).
 * pwaldtest: error informatively if executed on intercept-only model
   (also for such models: do not execute pwaldtest in summary.plm/pvcm and do 
   not print pwaldtest in print.summary.plm/pvcm).
 * mtest:
    * switched to combination of generic and a method for pgmm.
    * has information about user-supplied vcov in its return value's
      method slot (vcov information thus printed as well).
 * various print methods now return the input object invisible (before returned
   NULL).
 * piest, aneweytest: now use demeaning framework by Within() [thus benefiting 
   from fast mode].

   
### Vignettes and Other Documentation:
 * 1st vignette:
    * In section about panel unit root testing:
      * added short intro with overview of available functions/tests and added
        two example cases.
      * added sub-section about new function phansi [later renamed to phansitest 
        in plm 2.6].
    * added a little more information on the use of vcovXX.
 * 2nd vignette: added formula for nested error component model.
  * all vignettes: references updated to include Baltagi (2021), the 6th edition 
    of the textbook; fixed a few typos.
 * pldv: man page extended a little, esp. with examples.
 * vcovXX: man pages extended with examples how to use with plm's own summary method.
  
  
### Dependencies:
 * Added packages `fixest` and `lfe` to 'Suggests'.

***
  
# plm 2.4-1

 * lag: fix export of generic for lag (lost in 2.4-0; the panel-specific lag
   method was executed anyway if base R's lag() encountered a pseries) 
   ([#3](https://github.com/ycroissant/plm/issues/3)).
 * model.frame.pdata.frame: errors informatively if any index dimension has
   NA values.
 * pdata.frame: warns if NA in index dimension is encountered (before, only
   a plain message was printed).
 * Between/between/Sum/Within: Methods which rely on the index attribute
   (\*.pseries and (if with index attribute) \*.matrix) now error informatively
   if NA in any index dimension is encountered.
 * Vignettes: files renamed to start with "A_", "B_", "C_" so that the
   Vignettes are sorted on CRAN's plm page in an order better suited for new
   package users.
 * checkNA.index: new non-exported helper function to check for NA in index
   of a pdata.frame or pseries (all dimensions or a specific one).

***

# plm 2.4-0

### Speed up:
Significant speed improvement (optional, for the time being): A significant
speed-up of the package is available by a newly introduced **option** called
'plm.fast' such that panel model estimations and others run faster. Set option
'plm.fast' to 'TRUE' by `options("plm.fast" = TRUE)` for speed up, switch off by 
`options("plm.fast" = FALSE)` (switched off speed up is current default).
To have it always switched on, put `options("plm.fast" = TRUE)` in your 
.Rprofile file. See documentation `?plm.fast` for more information and a
benchmarked example.

Technically, the speed gains are achieved by weaving in the fast data
transformation functions provided in Sebastian Krantz' package 'collapse',
which needs to be installed ('Suggests' dependency).

Basic functions benefiting from speed-up are currently (used heavily in, e.g.,
plm()): Between, between, Sum, Within.

### Features:
* within_intercept: gains argument 'return.model' (default is FALSE and the
  functions works as previously). If set to TRUE, a full model object is
  returned which is the input's within model with an intercept (see
  documentation for more details).
* fixef: gained new argument value 'effect = "twoways"' to extract the
  sum of individual and time effect (for two-way models).
* plm/ercomp: random effect model estimation with Nerlove's method extended to
  unbalanced panels by weighting of the fixed effects (Cottrell (2017)).
* Sum: is now exported.
* DESCRIPTION file: added line BugReports pointing to a GitHub repository which is
  currently only used for GitHub's issue tracker feature (<https://github.com/ycroissant/plm/issues>).
  [Since version 2.4-3, GitHub is also used as the development platform hosting 
  the package's source code.]

### Fixes:
* fixef: calculation for two-way models fixed; type = "dmean" for unbalanced
  models fixed (by using weighted.mean()).
* between.default: keeps original sequence of elements' occurrence (before,
  compressed output was sorted by the factor's *level* order) [NB: this was
  reverted again in plm 2.4-2].
* Between.matrix and (internal) Tapply.matrix: ellipsis (three dots) is passed on,
  allowing for, e.g., na.rm = TRUE (like already possible for between.matrix etc.).
* Within.pseries/matrix: now handle na.rm argument in ellipsis.
* index: gives warning if argument 'which' contains "confusing" values.
  "confusing": an index variable called by user 'id', 'time', or 'group' if it
  does not refer to the respective index (e.g., time index variable is called 'id'
  in the user's data frame).
* pdata.frame: input 'x' is always pruned by data.frame(x) as a clean data frame
  is needed.
* Access to documentation with a generic defined in another package fixed
  (such as lag, diff, nobs, ...), so that the help systems offers to access the
  plm-specific documentation (regression introduced when pkg plm 2.0-0 adopted
  roxygen2 for documentation).
* ercomp: (cosmetic) if one of theta\$id, theta\$time is 0 => theta\$total must be 
  0 and is set to 0 (before, for some data and platforms, theta$total could be a
  very small positive or negative number, due to limited computational precision).
  This leads to nicer printing for summary outputs as well.
* plm: fix error when fed with a data frame with one (or more) column(s) having
  a 'names' attribute (data frames do not have names attribute for columns!),
  stemming from, e.g., a conversion from a tibble.
* as.data.frame.pdata.frame: clarify argument 'row.names' a bit: FALSE will give
  an integer sequence as row names, TRUE "fancy" row names, and (new) a character
  sets row names to the character's elements (character's length is
  required to match the number of rows).

### Internals:
* Between.\*, between.\*, and Within.\* methods: now use ave() instead of tapply().
* between.matrix and Sum.matrix allow for non-character 'effect' argument in
  non-index case.
* pmg, pcce, cipstest: now use the general Between()/Within() functions of the
  package (instead of "own" between/within transformation implemented inside the
  respective function).
* ercomp: now faster by saving and re-using intermediate results.
* dhat (non-exported function used in vcovXX/vcovG with type = "HC2" to "HC4"):
  now faster as diagonal of the quadratic form is calculated more efficiently.
* pht(., model ="bmc") and plm(., inst.method = "bmc") now error informatively
  (previously gave warnings) as "bms" is to be used for Breusch-Mizon-Schmidt IV
  transformation.

### Dependencies:
 * Added package 'collapse' to 'Suggests'.

***

# plm 2.2-5

* Removed duplicated entries in REFERENCES.bib (dependency Rdpack 2.0 warned).

***

# plm 2.2-4

* ptransform (internal function): check balancedness before pseries index is
  removed (fixes some spurious bug, e.g., when package tibble is used).
* exported/registered again in NAMESPACE after export/registration lost in plm 2.0-0:
  fixef.pggls, Math.pseries, Ops.pseries, Complex.pseries and deprecated
  methods/function formula.dynformula, print.dynformula, pvcovHC.
* Ops.pseries: use of is.vector() was too strict, now uses is.atomic() with
  taking care for additional data types.
* pwaldtest:
   * non-exported function wald() now exported as method pwaldtest.pgmm.
   * for all plm models use approach via
     crossprod(solve(vcov(x), coefs), coefs)), not (tss-ssr)/(ssr/df2)
     anymore.
   * method for pvcm models now allows for pvcm's "within" specification,
     returning a data.frame with test results for each regression.
* pcdtest.pseries: NA values in input variable are now removed before any
  further processing. A warning is issued in case of NA removal.
* mtest, sargan, pwaldtest, piest, aneweytest: added for each a string for
  alternative hypothesis.
  
### Dependencies:
* Removed package 'clubSandwich' from 'Suggests' as it was removed from CRAN
  (archived) [the package was re-added to CRAN at a later point in time but
  not made a 'Suggests' dependency for plm again].

***

# plm 2.2-3

* IGNORE_RDIFF_BEGIN/END added on tests and man pages.

***

# plm 2.2-1

* purtest:
   * tests now support unbalanced panel data, where applicable.
   * gained argument 'ips.stat' to select statistic for IPS test,
     default is "Wtbar" (as before), added "Ztbar" and "tbar".
   * if package 'urca' is available, p-values used in individual (augmented)
     Dicker-Fuller regressions are (for applicable tests) based on
     MacKinnon (1996) instead of MacKinnon (1994) yielding better p-value
     approximations for tau distribution (via urca::punitroot).
   * return value's element 'idres' contains p-values of individual
     ADF-regressions (p.trho) and p-values printed in summary (where applicable).
   * for Levin/Lin/Chu test, object and summary contain short-run and
     long-run variance.
   * for Hadri's test, summary is now applicable.
     
* index.pindex: fixed bug when individual index variable is called "group".

### Minor items:
   * print.fixef: respects / forwards arguments supplied to generic print method.
   * Grunfeld data doc: URL for overview of various Grunfeld data sets updated to
     <https://www.zeileis.org/grunfeld/>. 

### Dependencies:
   * Package 'urca' added to "Suggests".

***

# plm 2.2-0

* Methods for plm.list were not exported, now exported.
* lagt is changed so that it can deal with time factors which
  cannot be coerced to numeric (ex "1950-54", "1955-59", ...).
* cortab was not exported, now exported.
* pvcm failed for random effect models when there are some NA coefficients for
  some individual level OLS regressions, fixed.

***

# plm 2.1-0

* Problems with vignettes fixed (full text was in italics).
* In test file 'test_Estimators.R', L256, tolerance lowered to 1E-04.

***

# plm 2.0-2

* vcovXX.pcce functions exported again (export was lost in plm 2.0-0).
* summary.pcce gained argument 'vcov', summary.pcce object carries robust vcov
  in element 'rvcov'.
* Vignettes switched from bookdown::html_document2 to html_vignette.

***

# plm 2.0-1

* Minor update: tests updated to pacify CRAN's testing procedure with
  OpenBLAS.
* Bug fix in model.frame.pdata.frame: dot previously set to "separate" now set
  to "previous".

***

# plm 2.0-0

* class 'pFormula' is deprecated and will be removed soon.
* model.frame now has a pdata.frame method (instead of a pFormula
    method) and model.matrix has a pdata.frame method (takes as input a model 
    frame as a pdata.frame with a terms attribute and a formula attribute).
    'formula' as an argument in model.matrix was unnecessary as the formula can 
    be retrieved from model frame's pdata.frame.
* A third vignette was added describing the plm model components
    (plmModelComponents.Rmd).
* plm: the informative error message about the deprecated argument
    'instruments' is removed and this argument is no longer supported.
* Man pages and NAMESPACE file are now generated using roxygen2.

***

# plm 1.7-0

* lag, lead, diff for pseries objects: functions now take care of the time
    dimension when shifting observations within each individual. Previously, 
    shifting was performed row-wise within each individual (neglecting the time
    dimension). The argument 'shift' is introduced to control shifting behaviour,
    possible values are "time" (default) and "row" (behaviour up until and incl.
    plm 1.6-6). Note that, however, the diff-ing performed in first-difference 
    model estimation by plm(..., model = "fd") is based on row-wise differences
    of the model matrix per individual.
* pbnftest: new function for (modified) BNF statistic (Durbin-Watson test 
    generalised to panels) and Baltagi/Wu's LBI statistic
    (Bhargava/Franzini/Narendranathan (1982), Baltagi/Wu (1999)).
* pcdtest: bias-corrected scaled LM test implemented (test = "bcsclm") as in
    Baltagi/Feng/Kao (2012).
* summary.plm: for all random models and for all instrumental variable models,
    single coefficient tests are based on std. normal distribution and joint
    coefficient (Wald) test on Chi-square distribution.
* pwaldtest: now handles IV models correctly (Wooldridge (1990)); method for
    random pvcm models added (and used in summary.pvcm).
* pht: fixed estimation (plm 1.6-6 introduced a slight regression).
* summary.pht: waldtest now uses Chi-square distribution (was F distribution).
* Fixed first-difference models (plm(., model = "fd"), pggls(., model = "fd"))
    to have an intercept by default again (disappeared in plm 1.6-6).
* Between.matrix: bug fixed, so that the transformation can be correctly 
    performed for a matrix without index attribute.
* make.pconsecutive for pseries: for consecutive pseries, the argument
    'balanced' was not respected.
* pwfdtest: fixed error when one individual has one observation only.
* pmodel.response, fitted.plm, residuals.plm: for "between" and "fd" models, 
    a pure numeric is returned, not an 'illegal' pseries anymore (these models 
    compress data where having an index/pseries is useless and misleading).
* Between and Within methods for matrices are now exported.
* plm object: gained element 'weights' if weighted estimation was performed.
* groupGenerics now used for 'pseries' objects, implemented as a wrapper 
    for methods in groups 'Math', 'Ops' and 'Complex' (see ?groupGeneric).
    Thus, propagation to a higher or lower data type works correctly when 
    performed on pseries, e.g., c("pseries", "integer") is propagated
    to c("pseries", "numeric") if an operation returns a decimal.
* Vignettes: translated package's original vignette to Rmd format and renamed
    to plmPackage.Rmd; added vignette plmFunction.Rmd for further explanation
    about the estimation of error components models with the plm function;
    in plmPackage.Rmd fixed typo in formula for cross-sectional dependence
    scaled LM test.
  
### Deprecated/renamed:
* pht, plm(., model = "ht"/"am"/"bms"): both uses deprecated, better use instead
    plm(., model="random", random.method ="ht", inst.method="baltagi"/"am"/"bms")
    to estimate Hausman-Taylor-type models.
* summary.plm: removed support for ill-named argument '.vcov', instead use
    'vcov'. '.vcov' has been deprecated in plm 1.6-4 on CRAN since 2016-11-30.
* pvcovHC: function deprecated, use vcovHC for same functionality.
* plm: using the 'instruments' argument errors now (gave deprecation warning).
* dynformula: the long deprecated function now gives a deprecation warning.
* detect.lindep: previously named detect_lin_dep; renamed for consistency in
    function naming (back-compatible solution implemented) [back compatibility 
    removed in plm 2.6-0].
  
### Minor items:

* pvar: added method for pseries.
* pgrangertest: better detection of infeasibility if a series is too short.
* pdata.frame: fixed bug so that pdata.frames with only one column can be 
    created if drop.index = TRUE is set.
* pgmm object: removed element 'df.residual' for now as it contained the
    function of the same name rather than a number; fixed handling of argument
    'lost.ts''s second element.
* as.data.frame.pdata.frame gained argument 'keep.attributes'.
* [.pdata.frame: in case a single column, i.e., a pseries is returned, this 
    pseries now has names (now consistent to extraction by \$.pdata.frame and
    [[.pdata.frame).
* is.pseries: added function to check if an object qualifies as a pseries.
* (internal) is.index, has.index: new non-exported functions to check if an
    object is or has a proper index (in the sense of the plm package).
* pvcm object: element 'residuals' is now of class c("pseries", "numeric") 
    (was: "numeric" for within model); element 'coefficients' is numeric for
    random model (was "matrix" with one column); element 'df.residuals' renamed
    to 'df.residual' (as is standard, cf. lm, plm, ...).
* print.pseries: prettier printing for c("pseries", "complex").
* print.summary.plm: more informative for perfect fits (all residuals are 0).
* plm/ercomp: informative error messages for non-estimable 'swar' and 'amemiya'
    models when between model lacks observations ('swar') and individual or
    time dimension lacks within variation ('amemiya').
* plm/model.matrix.pFormula: informative error message for non-finite values.
* summary.purtest: give informative error message when run on purtest object
    containing result for Hadri's test.
* pcce, pht, pmg, pggls models: proper names in printed summary's topline.
* pcce models: pooled model's element 'coefficients' is numeric (was 1x1 matrix).
* pwaldtest: in all cases, htest object's 'statistic' element is a numeric
    (was 1x1 matrix for some cases).
* Data set 'Crime' extended with pre-computed log values as in original data.  

### Dependencies:
* Added to 'Suggests': knitr, rmarkdown, bookdown.

***

# plm 1.6-6

* ercomp:
    * re-written to be a more general framework.
    * (internal) returned ercomp object: component 'sigma2' is now a numeric;
        component 'theta' is now either a numeric (balanced models) or a list with 
        numerics (unbalanced models), the numerics being of length 1 (one-way models)
        or of length equal to the number of observations (two-ways models).
* model.matrix.*: gained new argument 'cstcovar.rm' to remove specific columns.
* pmodel.response: now returns object of class c("pseries", "numeric") [was "numeric"].
* plm:
    * random effect models: some random methods extended to unbalanced 
        two-ways case (Nerlove's method only supports balanced one-way, two-ways),
        thanks to the more general ercomp framework.
    * nested random effects model implemented (Baltagi/Song/Jung (2001)),
        use effect = "nested", see example in ?plm.
    * two-way fixed effects model for unbalanced panels is faster.
    * new argument 'weights' added.
    * fix backward compatibility for the (deprecated!) argument 'instruments'
        to estimate IV models (but rather use 2-part formulae for IV models).
    * plm gives an informative error message if all terms are dropped in an 
        estimation due to aliasing.
    * argument 'inst.method': value "bmc" renamed to "bms" for the Breusch-Mizon-Schmidt
        method ("bmc" was a typo, back-compatible solution implemented).
* pht: argument 'model': value "bmc" renamed to "bms" (cf. plm).
* purtest:
  * for test = 
    * "madwu": Maddala-Wu test statistic used to be computed using p-values from 
               the normal distribution, fixed now, by using approximated p-values
               for the tau distribution described by MacKinnon (1994).
    * "hadri": 
      * fixed p-value (now based on one-sided test).
      * fixed statistic in non-heteroskedasticity case (Hcons = FALSE).
      * degrees of freedom correction implemented (set dfcor = TRUE).
    * "ips", "levinlin": p-values fixed (now one-sided to the left).
    * new tests: Choi (2001) modified P ("Pm"), inverse normal ("invnormal"),
      logit ("logit").
    * cosmetic: when argument 'lags' is not specified by user, the returned
      object does not contain all three possible values in 'args\$lags' anymore.
* cipstest: for the truncated version of the test, the constants used in the
              calculation were mixed up for type = "none" and "trend". 
* pldv: new function to compute fixed and random effects models for truncated 
          or censored dependent variable.
* pgrangertest: added Granger causality test for panels (Dumitrescu/Hurlin (2012)).
* pbsytest:
  * test = "j": [joint test by Baltagi/Li(1991)]:
        fixed degrees of freedom (now df = 2 instead of df = 1).
  * unbalanced version of all statistics implemented (Sosa-Escudero/Bera (2008)).
  * new argument 're.normal' (only relevant for test = "re"): allows to compute
        the two-sided RE test when set to FALSE (default is TRUE which gives the
        one-sided RE test as before).
* plm.data:
  * use discouraged now (gives warning), use pdata.frame instead.
  * function internally uses pdata.frame now and then adjusts
        to get a "plm.dim" object as before. This way, plm.data
        benefits from bug fixes made previously to pdata.frame.
* pdata.frame:
  * by default, the resulting pdata.frame is now closer to the original data:
    * columns with constant values and all-NA values are not removed anymore,
    * non-finite values are not substituted with NAs anymore,
    * unused factor levels are not dropped anymore (except for those
           variables serving to construct the index).
  * arguments 'drop.NA.series', 'drop.const.series', 'replace.non.finite',
        'drop.unused.levels' introduced to remove/replace columns/values as described
        before (all default to FALSE).
  * warning issued if an index variable is to be constructed that subsequently 
        overwrites an already present column of the same name ('id' and/or 'time').
  * pacified warning in subsetting with non-existent rows and columns due
        to deprecation of 'structure(NULL, *)' in R >= 3.4.0.
  * \$<-.pdata.frame: preserves storage mode and sets correct class if propagation
                         to higher class occurred for a pseries prior to assignment
                         (in plm 1.7-0 this was replaced by the more general approach
                         using groupGenerics).
* ranef: new function to calculate random effects from RE model objects, like
           fixef for FE models.
* aneweytest: changed to use the residuals of the one-way individual within model 
                (was: two-ways within model).
* cortab: new function to compute cross-sectional correlation matrix.
* pwartest, pwfdtest:
    * statistics are labelled as F statistics.
    * calculations now done without car::linearHypothesis().
* diff.pseries:
    * logicals can now be diff'ed (result is integer, mimics base::diff).
    * does not warn anymore if argument 'lag' has length > 1.
* difft.pseries (note the "t") implemented (non-exported), diff-ing with
    taking the value of the time dimension into account, analogous to already
    implemented (non-exported) functions lagt.pseries/leadt.pseries.
* punbalancedness: extended to calculate unbalancedness measures of nested panel 
                     structures as in Baltagi/Song/Jung (2001).
* mtest, sargan: return values gained data.name element.
* pbltest: now accepts pdata.frames with index not in first two columns.
* pwartest.formula (only formula interface): fixed: ellipsis ("dots") was not passed on.
* pwaldtest: fix detection of vcovs generated by package 'clubSandwich' for models 
               with an intercept.
* summary.pseries: better handling of characters, logicals, factors (use base methods).
* summary.piest, summary.pht: table headings now have "z-value" and "Pr(>|z|)" 
                                as the numbers are based on standard normal distribution.
* make.pbalanced: argument 'balanced.type':
  * 'shared' renamed to 'shared.times' (with a back-compatible solution),
  * 'shared.individuals' added as additional option (symmetric case).
* print.pseries: prettier printing for integers.
* print.summary.plm: prints information about dropped coefficients due to singularities
                       (mimics print.summary.lm).
* cosmetic: some print functions now have better aligned whitespacing.

### Dependencies:
  * R version >= 3.1.0 required now.
  * 'Imports': maxLik added, function maxLik::maxLik is re-exported by
    plm.
  * moved from 'Imports' to 'Suggests': car.
  * 'Suggests': added pcse, clusterSEs, clubSandwich, pglm, spdep,
                    splm, statmod, Ecdat, pder, stargazer, texreg,
                    foreign.

***

# plm 1.6-5

* pFtest: disable input model check due to reverse dependency of package AER
    (and to allow arbitrary model comparisons); check was erroneously enabled in
    plm version 1.6-4.

***

# plm 1.6-4

* Argument for supplying a variance-covariance matrix is now consistently named 'vcov'
    across all functions. The old argument '.vcov' of summary.plm is marked as deprecated.
    Deprecation warnings are only issued for those functions which where on CRAN with
    the now deprecated argument '.vcov' (i.e., just for summary.plm, no warnings for
    functions pwaldtest, fixef, within_intercept).
* Snesp.Rd, LaborSupply.Rd: broken links fixed.
* Vignette: updated contact details.

***

# plm 1.6-3

* Function Ftest renamed to pwaldtest (there was never a CRAN release with Ftest
    exported, thus no deprecation warning).
* summary.plm: F statistic uses adjusted df2 parameter in case of robust vcov.

***

# plm 1.6-2

* pcdtest optimized for speed. 'preshape()' added to pcdtest.R to take care of reshaping in wide form
    prior to applying 'cor()'.

***

# plm 1.6-1

* vcovG and vcovBK: added compliance with instrumental variables models (through two-parts formulae).
* plm.Rd: added note on the intercept in 'fd' models, and how to eliminate it.
* make.pbalanced: gained argument 'balance.type' which allows to select whether
    to balance by filling in NA values ("fill", like before) or (now new) also by
    keeping only time periods shared among all individuals ("shared" [in v1.6-6 renamed to "shared.times"]).
* pbsytest: issue warning when applied to an unbalanced model (unbalanced tests still in preparation).
* print.summary.plm: for "between" models: print the no. of obs used in estimation
    (like for "fd" models since 1.5-24).
* pcdtest:
    * returned htest object has correct data.name value now.
    * NaN stemming from non-intersecting periods or only one shared period of 
        pairs of individuals avoided by omission from calculation.
    * pcdtest(some_var ~ 1, data) does not error anymore.
* NEWS file: order of entries changed: latest entries now at top of file.
* pdim.pseries: added method.
* is.pbalanced.*: methods added to determine balancedness for convenience. Same as pdim()\$balanced,
    but avoids calculations performed by pdim() which are not necessary to determine (just) the
    balancedness.
* some functions little more efficient by using is.pbalanced() instead of pdim()\$balanced.

***

# plm 1.5-35

* pbltest.plm: plm interface now respects the 'alternative' argument.
* summary.plm: summary.plm objects gained new component 'df' to mimic summary.lm objects more closely.
* gettvalue() added as (internal) function to quickly extract one or more t values from an estimated model
    (so one can avoid a call to summary() with all the other unnecessary computations).

***

# plm 1.5-34

* Ftest: support for vcovs from package clubSandwich to allow df2 adjustment in robust F test
           (new func trans_clubSandwich_vcov added for this purpose).
* (internal) model.matrix.pFormula: case "pooling" added to twoways/unbalanced condition;
                                    for unknown cases, give meaningful error.
* alias.plm, alias.pFormula: added functions to complement the generic stats::alias to detect
                               linear dependence (much like detect_lin_dep).
* detect_lin_dep.plm: added (complementing previously added detect_lin_dep methods from version 1.5-15). 
  [function was renamed to detect.lindep in version 1.7-0].
* plm objects gained element 'aliased' (a named logical) to indicate any aliased coefficients that are 
    silently dropped during estimation by plm (cf. summary.lm objects).
* fix: vcovXX.plm / vcovG framework now handle plm models with aliased coefficients (linear dependent columns
    in model matrix).
* phtest:
  * better support for between models.
  * for method="aux", argument 'effect' is now extracted from dots.

***

# plm 1.5-33

* (internal) pdiff gained 'effect' argument and thus is more general.
* plm: trying to estimate first-difference (FD) models with effect="time" or "twoways" is now prevented 
         with meaningful error messages; footnote 3 in vignette slightly adapted to reflect this.

***

# plm 1.5-32

* pcdtest: fixed p-value for cross-sectional dependence scaled LM test (pcdtest(..., test = "sclm")).

***

# plm 1.5-31

* fixef: return value is now class c("fixef", "numeric") ("numeric" added).
* summary.fixef: return value is now of class c("summary.fixef", "matrix") ("matrix" added);
                   type and df.residual added as attributes.
    Both class additions allow easier further processing of the return values. 

***

# plm 1.5-30

* lagt.pseries (experimental, non exported function): now handles NA values in time index.
* pdata.frame: warning about NA in time index turned into a note being printed.

***

# plm 1.5-29

* print.pdata.frame: workaround to prevent error when printing pdata.frames with duplicated row names
    (original row names are suppressed for printing in this case).

***

# plm 1.5-28

* phtest (regression based): if only one regressor in formula, the test does not stop anymore.

***

# plm 1.5-27

* model.matrix.pFormula: little speed up of within transformation in the two-ways unbalanced case.
* model.matrix.pFormula: little speed up for some more transformations by using .colMeans instead 
                           of apply(X, 2, mean).

***

# plm 1.5-26

* residuals.plm: residuals extracted by residuals(plm_object) now have class c("pseries", "numeric")
                   (was just "pseries" before).

***

# plm 1.5-25

* fixef:
  * summary.fixef: t distribution is used for p-value calculation (like the heading states).
  * fixef: for the t distribution to be applied for p-value calculation, objects of class "fixef" 
           gained a "df.residual" element.

***

# plm 1.5-24

* print.summary.plm: for FD models: now also prints number of observations used during estimation, because
                       these differ from the number of observations (rows) in the model frame due to diff-ing.
* pres: fixed handling of pggls' FD models and, thus, summary on pggls' FD models does not error anymore.
* pbltest: now has a plm interface (besides the formula interface).

***

# plm 1.5-23

* make.pconsecutive: new function to make a (p)data.frame or pseries consecutive, meaning having consecutive
                       time periods, t, t+1, t+2, ..., where t is an integer (fills in missing time periods).
                       Optionally, by argument 'balanced', (p)data.frames or pseries can be made consecutive 
                       and at the same time also balanced.
* make.pbalanced: new function to make a (p)data.frame or pseries balanced (but not consecutive)
                    (fills in missing time periods).

***

# plm 1.5-22

* pdata.frames are now more consistent after subsetting:
      => identical(pdataframe, pdataframe[1:nrow(pdataframe), ]) yields TRUE now
  * fixed: after subsetting unnecessary information was added to the resulting pdata.frame.
  * fixed: mode of index attribute was changed (unintentionally "simplified").
* pdata.frame is now a little more informative about NA values in either individual or time index
    (a warning is issued when such a pdata.frame is created).
* [.pdata.frame: indexing a pdata.frame is now fully consistent to indexing a data.frame (as documented):
  * fixed: special case of indexing by [i] (missing j) which erroneously returned rows instead of columns.
* pdata.frame's warnings:
  * if duplicate couples or NA values in the index variables are found
    while creating a pdata.frame, the warning now gives users a
    hint how to find those (table(index(your_pdataframe), useNA = "ifany").
  * printed is now "id-time" (was: "time-id") to be consistent with
    order of index variables.

***

# plm 1.5-21

* new function as.list.pdata.frame: Default behaviour is to act
      identical to as.list.data.frame (some code relies on this, do
      not change!).  By setting arg 'keep.attributes = TRUE', one gets
      a list of pseries and can operate (e.g., 'lapply') over this list
      as one would expect for operations on the columns of a
      pdata.frame, e.g., to lag over more than one column and get a
      list of lagged columns back, use: lapply(as.list(pdataframe[ ,
      your_cols], keep.attributes = TRUE), lag).

***

# plm 1.5-20

* vcovXX.pcce methods added by copying the vcovXX.plm ones; work just
    the same, sandwiching the appropriate "meat" in transformed
    data. General reference is Wooldridge, Ch. 7.

***

# plm 1.5-19

* pcce now supports model.matrix and pmodel.response methods, extracting the transformed data so that the 
    estimator can be replicated by lm(pmodel.response(model) ~ model.matrix(model)); this is needed both for
    vcovXX functions and for cluster bootstrapping.
* summary.pcce outputs the R2 calculated according to Holly, Pesaran and Yamagata instead of RSS/TSS.

***

# plm 1.5-18

* pcdtest: small efficiency enhancement (calc only lower.tri of rhos).
* pos.index (internal, not exported): new function to determine column numbers 
  of index vars in a pdata.frame.
* cosmetics:
    * some extraction/subsetting functions doubled 'pseries' in the class of returned value (fixed now).
    * extraction methods for pdata.frame and pseries now preserve order of attributes.
    * class "pindex" for attribute index not lost anymore after subsetting a pdata.frame.

***

# plm 1.5-17

* lagt.pseries: new method respecting "content" of time periods [not yet exported due to testing].
* is.pconsecutive: default method exported so it can be used for arbitrary vectors.

***

# plm 1.5-16

* plmtest:
    * fixed p-values [for type="kw" and "ghm"],
    * unbalanced version of all test statistics implemented,
    * doc update with literature references to unbalanced tests,
    * if requested, the "kw" statistic is now also calculated as one-way ("individual" or "time"), 
      albeit it coincides with the respective "bp" statistic.
* pwtest: formula interface respects 'effect' argument, 'effect' argument now mentioned in doc.
* data set 'Wages': factor 'sex' re-leveled to c("male", "female") to match use in original paper.
* print.summary.plm: suppress printing of 'effects' argument in top line in case of 'pooling' model.
* doc for between, Between, Within extended; doc for lag, lead, diff in separate file now (lag_lead_diff.Rd)
* pdata.frame:
    * fixed bug: do not assume a specific order of data when only individual index is supplied,
    * resulting pdata.frame is ordered by individual, then time index,
    * when duplicate couples (id-time) are created, a warning is issued,
    * new argument 'stringAsFactors'.
* pvar: 
  * fixed warning about var on factor variable (var on factors is deprecated as of R 3.2.3),
  * fixed corner case with one group being all NA and other non-varying,
  * print.pvar: better handling of NA values.
* lag/lead: fixed bug with dropped factor levels, added testfile tests/test_lag_lead_factor_levels.R.
* is.pconsecutive:
    * new function to check if time periods are consecutive per individual,
    * better NA handling by added argument 'rm.na.tindex'.
* pgmm: fixed bugs affecting the instrument matrix in the following cases:
    * GMM instruments are collapsed and restricted in lag length;
    * first lags are used as GMM instruments;
    * GMM instruments are collapsed in system GMM;
    * GMM instruments are restricted in lag length in system GMM.
* punbalancedness: data frame interface gains 'index' argument.
* within_intercept: new function to calculate an overall intercept along its standard error for FE models
                      a la Stata and gretl, accepts argument '.vcov' for user defined vcov.
* added help topic on package as a whole (?`plm-package`) for sake of completeness.
* summary.plm: 
  * argument '.vcov' can also be a function (before, only matrix was possible).
  * internal: the furnished vcov is saved in the summary.plm object in
       object\$rvcov (vcov function name in attr(object\$rvcov, which="rvcov.name").
* Ftest: 
  * gained '.vcov' argument, which enables robust F test and chi-sq
         test computations [robust versions not yet weaved in
         summary.plm].
  * now exported and has documentation (.Rd file).
  * returned htest object has vcov's name in 'method' element (if vcov was supplied).
  * Ftest later renamed to pwaldtest (in version 1.6-3).
* (internal) vcovXX functions: furnished vcovs gain attribute
                                 "cluster" which give info about
                                 clustering, e.g., "group" or "time".
* fixef: gains new argument '.vcov'.

***

# plm 1.5-15

* punbalancedness: new function for unbalancedness measures for panel data as
    in Ahrens/Pincus (1981); added doc and testfile for punbalancedness.
* DESCRIPTION: added URLs for package on CRAN and package on R-Forge.
* model.matrix.pFormula and plm.fit: include 'assign' and 'contrasts' attributes (if any) in model fit
* Vignette: summary(lme_mod)\$coef\$fixed -> summary(lme_mod)\$coefficients\$fixed to avoid
    partial matching of 'coef' to 'coefficients' by "\$" operator.
* r.squared: adjusted R squared fixed (at least for models with intercept).
* model.matrix.pFormula and pmodel.response: ensured that 'data' gets reduced to the corresponding
    model frame if not already a model frame (now mimics stats::model.matrix in this regard);
    fixes corner cases with specific NA patterns when model.matrix.pFormula or pmodel.response are
    called directly and 'data' not being a model frame (despite being required so)
    [see tests/test_model.matrix_pmodel.response_NA.R].
* detect_lin_dep: new function as a little helper function to detect linear dependent
    columns, esp. in model matrices; incl. doc with two examples about how linear
    dependent columns can be induced by the within transformation [function was 
    renamed to detect.lindep in version 1.7-0].
* doc pFormula.Rd extended (especially examples) and split up in two files to better accommodate
    different return values and input values in the documentation
    (new file added: man/model.frame_model.matrix.Rd).

***

# plm 1.5-14


* lag.pseries: modified to handle negative lags (=leading values).
* lead.pseries: added function as a wrapper for lag.pseries(x, k = -1) for convenience,
    i.e., lag(x, k = -1) == lead(x, k = 1).
* pmodel.response.pFormula: make sure supplied formula is a pFormula before we continue
    (coerce to pFormula), fixes "bugs" (rather unexpected, but documented behaviour) like:
      pmodel.response.pFormula(regular_formula, data = dat, model = "pooling")
       # Error in names(y) <- namesy :
       # 'names' attribute [482] must be the same length as the vector [0]
* diff.pseries: prevented negative lags as input to avoid confusion.
* doc for pseries functions are made available under their name, e.g., ?lag now displays helpfile for
    lag.pseries in the help overview (besides, e.g., stats::lag).
* pdim.default: make error message "duplicate couples (time-id)" printed as proper error message
    (removed cat() around the message in stop(), was printed as regular string on screen before).
* plm.data: slight improvement for printed outputs (spelling and spacing).
* indexes: fixed return value (was always NULL).
* doc updates: ?pdim: added section about possible different return values for pdim(pdata.frame) and
    pdim(panelmodel_object); others: linkage to base functions enabled, spelling.
* mylm: added commented (i.e., inactive) warning about dropped coefficients in estimation.
* fitted.plm: added commented (i.e., inactive) warning in about dropped coefficients in
    estimated model compared to specified model.matrix.
* added testfile tests/test_fitted.plm.R (some of those test currently do not run
    (commented, i.e., inactive)).
* some testfiles: fixed weird encodings.

***

# plm 1.5-13

* fixed bug in vcovHC(..., method="white") from degenerating diag() if any group has
    only 1 element.
* vcovG framework: reintroduced "white2" method.

***

# plm 1.5-12

* dataset Produc: added variable 'region' as 3rd column and fixed variable 
    descriptions: 'pcap' is public capital (not private) while 'pc' is private 
    capital (not public).
* added importFrom as per CRAN check for submission.

***

# plm 1.5-11

* added RiceFarms to datasets to eliminate non-CRAN dependencies, (temporarily) removed
    'pder' from suggested, uncommented (fixed) example in pdwtest.Rd.

***

# plm 1.5-9

* fixed bug in vcovG for lagged White terms (make pseudo-diagonal in E(u,v)); affected vcovNW.

* documentation updates (mostly text books of Baltagi and Wooldridge to latest editions,
    references are also more specific now).
  
* pbgtest: fixed/enabled type="F" (F test of lmtest::bgtest) in wrapper pbgtest() and
    fixed/enabled passing of argument 'order.by' [additional argument in
    lmtest::bgtest] [tests/testpbgtest.R added, docs updated and extended].

* phtest(., method="aux") [Hausman regression-based test]: fixed bug when data contain more
    cases than there are used in model estimation (due to NA values); avoid calc. of RE model
    a second time; phtest now also works with between model correctly (fixed degrees of freedom)
    tests/test_phtest_Hausman_regression.R added.
 
* plm(): original row names of input data are preserved in plm_object\$model,
    so functions like pmodel.response(), model.frame(), model.matrix(), residuals()
    return the original row names (and thus fancy row names if those were to be computed by
    pdata.frame)

* as.data.frame.pdata.frame(): respects 'row.names' argument.

***

# plm 1.5-8

* phtest (Hausman test): introduced new regression-based test, allowing for
  robust vcov (via argument method = "aux").
* fixed bugs in pdwtest.

***

# plm 1.5-6

* pruned 'require' calls to 'lmtest', 'car', 'lattice' and substituted them with proper
    entries in NAMESPACE.

* temporarily commented problematic examples in pbgtest and pdwtest.

***

# plm 1.5-5

* fixed bug affecting vcovG on unbalanced datasets (thx Liviu Andronic) from propagation
    of NAs in final by-group matrix multiplication.

***

# plm 1.5-4

* fixed testErrors.R with plm.data instead of pdata.frame.

***

# plm 1.5-3

* reintroduced plm.data eliminated by mistake.

***

# plm 1.5-1

* fixed "already a pdata.frame" bug in pcdtest.formula.

* implemented fixef() method for objects estimated by pggls().

***

# plm 1.5-0

* added generic building block and high-level wrappers vcovNW and vcovDC to the namespace.

* dataset Parity added to /data.

***

# plm 1.4-0

* substituted vcovHC, vcovSCC with the new framework based on vcovG and wrapper functions.

***

# plm 1.3-1

* a 'subset' argument is added to print.summary.plm and summary.pht
    so that a subset of coefficients may be selected.

* fixed a small problem on the printing of the typology of the
    variables for pht models.

* the "name" of the tests is now the formula truncated so that it
    prints on only one line.

* 'restrict.matrix' argument added to deal with linear restrictions.

* a 'vcov' argument is added to summary.plm so that a variance matrix can be supplied.

* the deviance method for panelmodel objects is now exported (12/02).

* the Hausman-Taylor now supports the Amemiya-MaCurdy and the Breusch-Mizon-Schmidt version.

* a small bug is fixed on the var2lev function to deal the case when there are no factors.

***

# plm 1.3-0

* an update method for panelmodel objects is provided.

* the Wages example is removed from the pvar man page because it's time consuming.

***

# plm 1.2-10

* for unbalanced Hausman-Taylor model, the printing of the error
    components was wrong, it is now fixed.

* the printing of the removed variables (cst or NA) is improved.

* the pgmm function has been improved to deal correctly with holes
    in the cross-sections ; a 'collapse' argument is added to limit the
    number of GMM instruments.

* the CoefTable element of summary.pgmm objects is renamed to
    coefficients, so that it can easily be extracted using the coef method.

* the default value for robust is now TRUE in the summary.pgmm method.

* a new argument 'lost.ts' is added to pgmm to select manually the
    number of lost time series.

* almost null columns of instruments are removed (this happens when
    within/between transformation is performed on between/within series.

* plm now accepts three part formulas, the last part being for
    instruments introduced only in the within form.

* the predict method for panelmodel objects is now exported.

* plm now estimates systems of equations if a list of formulas is
    provided as the first argument.

***

# plm 1.2-9	

* the pccep function, estimating CCEP models a la Pesaran, has
    been added together with summary and print.summary methods. The
    function generates objects of a class of their own ("pccep"), much
    like 'pggls', together with 'panelmodel' ['pccep' was later renamed to 
    'pcce'.]

* the pmg function, estimating MG, DMG and CCEMG models a la
    Pesaran, has been added together with summary and print.summary
    methods. The function generates objects of a class of their own
    ("pmg"), much like 'pggls', together with 'panelmodel'. In the
    future must consider possible merger with 'pvcm'.

* the new cipstest function performs a second-generation CIPS test
    for unit roots on pseries objects.

* the new (non-exported) function pmerge is used internally by cipstest
    to merge lags and differences of a pseries into the original
    pdata.frame. Will possibly be added to the user space in the future.

***

# plm 1.2-8	

* an index method is added for panelmodel, pdata.frame and pseries.

* a bug in the typology of the variables in pht is fixed.

* a bug in vcovBK (matrices degenerating into vectors) is fixed
    (thx to David Hugh-Jones for bug report).

* the Between function now returns a pseries object.

* the resid and fitted method now return a pseries object.

* the pgmm method has been rewritten; the data frame is first
    balanced and NAs are then overwritten by 0s.

***

# plm 1.2-7

* a typo is corrected in the man page of plm.data.

* package AER is now suggested.

***

# plm 1.2-6

* a bug in mtest for pgmm models with effect="individual" and
    transformation="ld" *and* for the wald test for time.dummies for
    model with effect="twoways" and transformation="ld" is fixed by
    modifying namest in pgmm.

* there was a bug in pgmm for models with different lags for GMM
    instruments. The number of time series lost is now the min (and
    not the max) of the first lags for GMM instruments.

* some parts of summary.pgmm only worked correctly for models with
    time-dummies. It now deals correctly for models with 'individual'
    effects.

* the *.rda files are now compressed.

* p-values for the two-tailed versions of plmtest() were wrong and
    have been fixed. They were divided by 2 instead of multiplied.

***

# plm 1.2-5

* fixed error in pggls, model="within" (FEGLS). Added model="fd" (FDGLS).

* changed dependency from package 'kinship' to 'bdsmatrix' (as suggested by
    Terry Therneau after his reorganization of the packages).

* fixed DESCRIPTION and NAMESPACE accordingly.

* fixed the example in pggls.Rd.

***

# plm 1.2-4

* bug corrected in pgmm: ud <- cbind(ud, td.gmm.level) is
    relevant only for twoways models.

* in fitted.plm, the extraction of the index is updated.

* the residuals.plm method now has a 'model' argument.

* new function r.squared introduced.

* pmodel.response.plm is modified: no explicit 'effect' and 'model'
    arguments anymore (like in model.matrix.plm).

***

# plm 1.2-3

* lag.pseries now returns relevant names for the returned factor.

* pFormula is modified so that it can handle correctly Formula
    objects, and not only formula.

* pgmm has been completely rewritten with a new 'Formula'
    interface. Old formula and dynformula interfaces are kept for
    backward compatibility but won't be maintained in the future.

* 'subset' and 'na.action' are added to the list of arguments of pgmm and oldpgmm.

* lag.pseries is now able to deal with vector arguments for lags, 
    e.g., lag(x, c(1,3)).

* suml(x) is replaced by Reduce("+", x).

***

# plm 1.2-2

* the documentation has been improved.

* the pvalue for purtest(..., type = "madwu") was in error (by a
    factor of 2).

* in formula(dynformula), a bug is fixed so that the endog
    variable doesn't appear on the RHS if there are no lags.

* in pgmm, the extract.data has been rewritten and is *much* faster.

* two new functions vcovBK and vcovSCC have been added.

* a 'model' argument is added to pgmm.sys and pgmm.diff
    (previously, the model name was extracted from the call).

* in pgmm, Kt is fixed to 0 when effect="individual".

***

# plm 1.2-1

* a new purtest function to perform panel unit root tests.

* [[.pdata.frame is modified so that NULL (and not an error
    message) is returned if an unknown column is selected.

***

# plm 1.2-0	

* the as.matrix and print methods for pserie objects are now
    exported.

* in summary.plm, the p-value is now computed using a Student
    distribution and not a normal one.

* 'pserie' is renamed 'pseries'.

* the lag.pseries method is modified so that it deals correctly
    with factors, and not only with numeric vectors. The diff.pseries
    method returns an error if its argument is not numeric.

* the instruments-"backward compatibility" stuff in plm is
    simplified thanks to the new features of Formula.

* a THANKS file is added.

* the `[.pdata.frame` function is modified so that [, j] returns a
    pseries and not a pdata.frame when j is a single integer, and a
    backward compatibility feature is added for the "index" attribute.

***

# Change since version 1-1.4

* an 'args' argument is added to plm objects, and the internal
    function relies now on it (and not on the call as previously).

* more attention is paid when one of the estimated components of
    the variance is negative (warning or error messages result).

* pdata.frame objects are re-introduced. They are used to compute
    model.frames. Extraction from pdata.frames results in 'pserie'
    objects which have an index attribute.

* the print method of ercomp is now exported.

* the first argument of pgmm may now be a formula. A lag.form must
    be provided in this case.

* Hausman-Taylor estimation is now performed by the pht
    function. For backward compatibility reasons, it is still
    possible to estimate this model with plm.

***

# plm 1-1.3

* a relevant error message is added when a within model is
    estimated with no time-varying variable.

* the formula method for dynformula objects is now exported.

* a misleading notation was corrected for plm.ht model.

* the definition of sigma2\$id for unbalanced ht model is
    corrected, a harmonic mean of Ti being used.

* the definition of tss.default is simplified.

* the fitted.values element was missing for plm objects and has
    been added.

***

# plm 1-1.2

* the /inst directory was missing, it has been added again.

***

# plm 1-1.1

* part of the "details" section of the fixef.plm man page is
    removed.

* a fitted.value method is now available for plm objects. It
    returns the fitted values of the untransformed response.

* in pdiff, a drop=FALSE is added. This omission was responsible
    for a bug while estimating a model like plm(inv~value-1, data =
    Grunfeld, model = "fd").

* the lev2var is changed so that it doesn't result in an error
    when the data.frame contains no factor: this was responsible for a
    bug in plm.ht.

***

# plm 1-1.0

* in fixef, the 'effect' argument default is now NULL:
    fixef(model_with_time_effect) now works correctly.

* in pFtest, the error message "the arguments should be a within
    and a pooling model" is removed so that two within models may be
    provided.

* for backward compatibility reasons, function pvcovHC is reintroduced.

* for backward compatibility reasons, argument 'test' of pbsytest
    may be indicated in upper case.

* we switched back to old names for two arguments of plm ercomp ->
    random.methods ivar -> inst.method -> ivar.

* amemiya method is not implemented for unbalanced panels: an
    error message is now returned.

***

# plm 1-0.1

* the plm function has been completely rewritten.
  
* the names of some arguments have changed (random.methods ->
    ercomp, inst.method -> ivar), the old names are accepted with a
    warning.

* the 'instruments' argument is removed, instrumental variable
    estimation is performed using extended formula. The 'instruments'
    argument is still accepted with a warning.

* the 'model' element of plm objects are now ordinary data.frame,
    and not data.frame with elements y and X. Moreover, this
    data.frame contains untransformed data.

* the data sets which are relevant for panel data estimation that
    where previously in the 'Ecdat' package are now in the plm package.

* in pvcm a bug when the estimation was made on a subset is fixed.

* ercomp is a stand alone function which returns the estimation of
    the components of the variance of the errors.

* the estimation of two-ways within model is now implemented for
    unbalanced panels.

* the fixef method is much improved, the fixed effects may be
    computed in levels, in deviation from the first level, and in
    deviation from the overall mean.

* in pbsytest, the arguments test are now in lowercase.

* the pvcovHC function is replaced by suitable vcovHC methods for
    panelmodel and pgmm models.

***

# plm 1-0.0

* lag and diff methods for pseries are now exported and therefore
    behave correctly.

* for two-ways within models with instrumental variables, K is
    replaced by K+1 for the computation of the overall mean matrix of
    the instruments. Time fixef are now computed. The error message
    "impossible ..." is removed.

* a bug in the time random effect model is corrected.

* a model.matrix method for panelmodel is provided.

* models without intercept (-1 in the formula) should now be
    consistently estimated with plm, pggls and pvcm.

* plm depends now on the 'Formula' package which provides useful
    tools for formula with two parts.

***

# plm 0-3.2

* a lot of typos of the man pages have been fixed.

* functions pcdtest, pcdres have been added.

* for Hausman-Taylor model, summary now prints the variables
    and not the effects.

* the estimation of a model with only one explanatory variable
    using plm with method = "fd" is now working correctly.

***

# plm 0-3.1

* in plm.formula, [int.row.names] is replaced by
    [as.character(int.row.names)].

* the degrees of freedom for a within time effect model was wrong
    and has been corrected.

* the arguments 'type' and 'weights' in pvcovHC.panelmodel are renamed
    to 'method' and 'type', respectively. The default method (type in previous
    versions) is "arellano"" and not "white1".

* honda is now the default option for plmtest.

***

# plm 0-2.2

* the coefficients method for objects of class pgmm is now
    exported.

* a bug in the plm method of plmtest has been fixed.

* in plmtest, for argument 'effect' value "id" is renamed "individual".

* three testing functions are added : pbsytest (Bera,
    Sosa-Escudero and Yoon test), pARtest (Breusch-Godfrey test) and
    pDWtest (Durbin-Watson test) (later renamed to pdwtest), pwartest,
    pBGtest (later renamed to pbgtest), pwtest, and pbltest.

* plm, pvcm and pggls now have arguments "subset" and "na.action".

* phtest, pFtest, plmtest now have a formula method.

* a bug is fixed for the vcov of the within model.

* the pdata.frame function and class is suppressed. The package
    now use ordinary data.frames.

***

# plm 0-2.1

* pdata.frame is much faster thanks to a modification of the pvar
    function.

* series with only NA values or with constants values are now
    removed by pdata.frame.

* observations are ordered by id and time by pdata.frame.

* a pfix function is added to edit a pdata.frame [at some later point this
  function was removed from the package].

* a as.data.frame function is provided to coerce a pdata.frame to
    a data.frame.

* the dependency to the 'Matrix' package has been removed and pgmm
    is much faster now.

* phtest has been fixed to return only positive values of the
    statistic.

* pgmm objects now inherit from panelmodel like other estimators
    and print correctly.

* a bug in summary.pgmm has been fixed.

***

# plm 0-1.2

* Models with only one explanatory variable resulted in an
    error. This has been fixed.

* Estimation methods are now available with these four functions : 
    plm, pvcm, pggls, and pgmm instead of one (plm) in the previous version.

* pvcm is a new function which estimates variable coefficients
    models. The "nopool" model is now part of it.

* pggls is a new function which enables the estimation of general
    FGLS.

* pgmm is a new function for general method of moments estimator.

* for all estimation functions, the first four arguments are now
    'formula', 'data', 'effect', 'model'.

* robust inference is now provided by the pvcovHC function.
