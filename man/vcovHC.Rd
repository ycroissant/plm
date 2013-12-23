\name{vcovHC}
\alias{vcovHC}
\alias{vcovHC.plm}
\alias{vcovHC.pgmm}
\alias{pvcovHC}

\title{Robust Covariance Matrix Estimators}

\description{
  Robust covariance matrix estimators \emph{a la White} for panel models.
}

\usage{
\method{vcovHC}{plm}(x, method = c("arellano", "white1", "white2"),
                        type = c("HC0", "HC1", "HC2", "HC3", "HC4"),
                        cluster = c("group","time"), ...)
\method{vcovHC}{pgmm}(x, ...)
}

\arguments{
  \item{x}{an object of class \code{"plm"} which should be the result of a 
           random effects or a within model or a model of class \code{"pgmm"},}
  \item{method}{one of \code{"arellano","white1","white2"},}
  \item{type}{one of \code{"HC0","HC1","HC2","HC3","HC4"},}
  \item{cluster}{one of \code{"group","time"}}
  \item{\dots}{further arguments.}
}

\value{
  An object of class \code{"matrix"} containing the estimate of the asymptotic covariance matrix of coefficients.
}

\details{
\code{vcovHC} is a function for estimating a robust covariance matrix of
parameters for a fixed effects or random effects panel model according
to the White method (White 1980, 1984; Arellano 1987). Observations may
be clustered by \code{"group"} (\code{"time"}) to account for serial
(cross-sectional) correlation.

All types assume no intragroup (serial) correlation between errors and
allow for heteroskedasticity across groups (time periods). As for the
error covariance matrix of every single group of observations,
\code{"white1"} allows for general heteroskedasticity but no serial
(cross-sectional) correlation; \code{"white2"} is \code{"white1"}
restricted to a common variance inside every group (time period) (see
Greene (2003), 13.7.1-2 and Wooldridge (2002), 10.7.2);
\code{"arellano"} (see ibid. and the original ref. Arellano (1987))
allows a fully general structure w.r.t. heteroskedasticity and serial
(cross-sectional) correlation.

Weighting schemes are analogous to those in \code{vcovHC} in package
\code{sandwich} and are justified theoretically (although in the context
of the standard linear model) by MacKinnon and White (1985) and
Cribari-Neto (2004) (see Zeileis, 2004).

The main use of \code{vcovHC} is to be an argument to other functions,
e.g. for Wald-type testing: as \code{vcov} to \code{coeftest()},
\code{waldtest()} and other methods in the \code{lmtest} package; and as
\code{vcov} to \code{linearHypothesis()} in the \code{car} package (see
the examples). Notice that the \code{vcov} argument allows to supply a
function (which is the safest) or a matrix (see Zeileis (2004), 4.1-2
and examples below).

A special procedure for \code{pgmm} objects, proposed by Windmeijer
(2005), is also provided.

}

\author{Giovanni Millo \& Yves Croissant}

\references{
Arellano, M. (1987) Computing robust standard errors for within group estimators,
\emph{Oxford Bulletin of Economics and Statistics}, \bold{49}, 431--434.

Cribari-Neto, F. (2004) Asymptotic inference under heteroskedasticity
of unknown form. \emph{Computational Statistics \& Data Analysis}
\bold{45}, 215--233.

Greene, W. H. (1993) \emph{Econometric Analysis}, 2nd ed. Macmillan
Publishing Company, New York.

MacKinnon, J. G. and  White H. (1985) Some heteroskedasticity-consistent
covariance matrix estimators with improved finite sample properties.
\emph{Journal of Econometrics} \bold{29}, 305--325.

Windmeijer, F. (2005) A finite sample correction for the variance of
linear efficient two--step GMM estimators, \emph{Journal of
  Econometrics}, \bold{126}, pp.25--51.

White H. (1980) \emph{Asymptotic Theory for Econometricians}, Ch. 6, Academic Press, Orlando (FL).

White H. (1984) A heteroskedasticity-consistent covariance matrix and
a direct test for heteroskedasticity. \emph{Econometrica} \bold{48},
817--838.

Wooldridge J. M. (2002) \emph{Econometric Analysis of Cross Section and
Panel Data}, MIT Press, Cambridge (MA).

Zeileis A. (2004) Econometric Computing with HC and HAC Covariance Matrix
Estimators. \emph{Journal of Statistical Software}, \bold{11}(10), 1--17.
URL \url{http://http://www.jstatsoft.org/v11/i10/}.


}

\examples{
library(lmtest)
library(car)
data("Produc", package = "plm")
zz <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
          data = Produc, model = "random")
## standard coefficient significance test
coeftest(zz)
## robust significance test, cluster by group
## (robust vs. serial correlation)
coeftest(zz, vcov=vcovHC)
## idem with parameters, pass vcov as a function argument
coeftest(zz, vcov=function(x) vcovHC(x, method="arellano", type="HC1"))
## idem, cluster by time period
## (robust vs. cross-sectional correlation)
coeftest(zz, vcov=function(x) vcovHC(x, method="arellano",
 type="HC1", cluster="group"))
## idem with parameters, pass vcov as a matrix argument
coeftest(zz, vcov=vcovHC(zz, method="arellano", type="HC1"))
## joint restriction test
waldtest(zz, update(zz, .~.-log(emp)-unemp), vcov=vcovHC)
## test of hyp.: 2*log(pc)=log(emp)
linearHypothesis(zz, "2*log(pc)=log(emp)", vcov=vcovHC)

## Robust inference for GMM models
data("EmplUK", package="plm")
ar <- pgmm(dynformula(log(emp) ~ log(wage) + log(capital) + log(output),
           list(2, 1, 2, 2)), data = EmplUK, effect = "twoways",
           model = "twosteps", gmm.inst = ~ log(emp),
           lag.gmm = list(c(2, 99)))
rv <- vcovHC(ar)
mtest(ar, order = 2, vcov = rv)

}

\keyword{regression}
