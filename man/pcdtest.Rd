\name{pcdtest}
\alias{pcdtest}
\alias{pcdtest.formula}
\alias{pcdtest.panelmodel}
\title{Tests of cross-section dependence for panel models}

\description{
 Pesaran's CD or Breusch-Pagan's LM (local or global) tests for cross sectional dependence in panel models
}

\usage{
pcdtest(x, ...)
\method{pcdtest}{panelmodel}(x, test = c("cd", "sclm", "lm", "rho", "absrho"),
                             w = NULL, ...)
\method{pcdtest}{formula}(x, data, index = NULL, model = NULL,
                          test = c("cd", "sclm", "lm", "rho", "absrho"),
                          w = NULL, ...)
}

\arguments{
 \item{x}{an object of class \code{formula} or \code{panelmodel}, describing the model to be tested}
 \item{data}{a \code{data.frame}}
 \item{index}{an optional numerical index, in case \code{data} has to be formatted by \code{plm.data}}
 \item{model}{an optional character string indicating which type of model to estimate;
 if left to \code{NULL}, the original heterogeneous specification of Pesaran is used}
 \item{test}{the type of test statistic to be returned. One of "cd" for Pesaran's CD statistic,
  "lm" for Breusch and Pagan's original LM statistic,
  "sclm" for the scaled version of Breusch and Pagan's LM statistic, or
 "rho", "absrho" for average (absolute) correlation coefficient}
 \item{w}{an \code{n x n} \code{matrix} describing proximity between observations, with \eqn{w_ij = a} where \eqn{a} is any number such that \code{as.logical(a)=TRUE}, if \eqn{i,j} are
 neighbours, \eqn{0} or any number \eqn{b} such that \code{as.logical(b)=FALSE} elsewhere}        
 \item{...}{further arguments to be passed on to \code{plm}, such as e.g. \code{effect} or \code{random.method}}
}

\value{
An object of class \code{"htest"}.
}

\details{
 These tests are originally meant to use the residuals of separate estimation of one time-series regression for each cross-sectional unit in order to check for cross-sectional dependence. If a different model specification (\code{within}, \code{random}, ...) is assumed consistent, one can resort to its residuals for testing (which is common, e.g., when the time dimension's length is insufficient for estimating the heterogeneous model). If the time dimension is insufficient and \code{model=NULL}, the function defaults to estimation of a \code{within} model and issues a warning. The main argument of this function may be either a model of class \code{panelmodel} or a \code{formula} and \code{dataframe}; in the second case, unless \code{model} is set to \code{NULL}, all usual parameters relative to the estimation of a \code{plm} model may be passed on. The test is compatible with any consistent \code{panelmodel} for the data at hand, with any specification of \code{effect}. E.g., specifying \code{effect=''time''} or \code{effect=''twoways''} allows to test for residual cross-sectional dependence after the introduction of time fixed
 effects to account for common shocks. A \emph{local} version of either test can be computed supplying a proximity matrix (coercible to \code{logical}) providing information on whether any pair of observations are neighbours or not. If \code{w} is supplied, only neighbouring pairs will be used in computing the test; else, \code{w} will default to \code{NULL} and all observations will be used. The matrix need not be binary, so commonly used ``row-standardized'' matrices can be employed as well. \code{nb} objects from \pkg{spdep} must instead be transformed into matrices by \code{nb2mat} before using. 
}

\references{
 Breusch, T.S. and A.R. Pagan (1980), The Lagrange multiplier test and its applications to model specification in econometrics, \emph{Review of Economic Studies}, 47, pp. 239--253.

 Pesaran, H. (2004), General Diagnostic Tests for Cross Section Dependence in Panels, \emph{CESIfo} Working Paper 1229. 
 \url{http://www.cesifo-group.de/DocCIDL/cesifo1_wp1229.pdf}
}

\examples{
data(Grunfeld, package = "plm")
## test on heterogeneous model (separate time series regressions)
pcdtest(inv ~ value + capital, data=Grunfeld,
        index = c("firm","year"))
## test on two-way fixed effects homogeneous model
pcdtest(inv ~ value + capital, data=Grunfeld, model="within",
        effect="twoways", , index = c("firm","year"))
## test on model object
g <- plm(inv ~ value + capital, data=Grunfeld, index = c("firm","year"))
pcdtest(g)
## scaled LM test
pcdtest(g,test="sclm")
}

\keyword{htest}
