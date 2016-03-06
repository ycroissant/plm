\name{vcovBK}
\alias{vcovBK}
\alias{vcovBK.plm}

\title{Beck and Katz Robust Covariance Matrix Estimators}
\description{
  Unconditional Robust covariance matrix estimators \emph{a la Beck and Katz} for panel models.
}
\usage{
\method{vcovBK}{plm}(x, type = c("HC0", "HC1", "HC2", "HC3", "HC4"),
                              cluster = c("group", "time"),
                              diagonal = FALSE,
                              ...)
}

\arguments{
  \item{x}{an object of class \code{"plm"}}
  \item{type}{one of \code{"HC0"}, \code{"HC1"}, \code{"HC2"}, \code{"HC3"}, \code{"HC4"},}
  \item{cluster}{one of \code{"group"}, \code{"time"}}
  \item{diagonal}{a logical value specifying whether to force
  nondiagonal elements to zero}
  \item{\dots}{further arguments.}
}

\value{
  An object of class \code{"matrix"} containing the estimate of the covariance matrix of coefficients.
}

\details{
\code{vcovBK} is a function for estimating a robust covariance matrix of
parameters for a panel model according to the Beck and Katz (1995)
method, a.k.a. Panel Corrected Standard Errors (PCSE), which uses an
unconditional estimate of the error covariance across time
periods (groups) inside the standard formula for coefficient covariance. Observations may
be clustered either by \code{"group"} to account for timewise
heteroskedasticity and serial correlation
or by \code{"time"} to account for cross-sectional heteroskedasticity
and correlation. It must
be borne in mind that the Beck and Katz formula is based on N- (T-)
asymptotics and will not be appropriate elsewhere.

The \code{diagonal} logical argument can be used, if set to \code{TRUE},
to force to zero all nondiagonal elements in the estimated error
covariances; this is appropriate if both serial and cross-sectional correlation
are assumed out, and yields a timewise- (groupwise-) heteroskedasticity-consistent
estimator.

Weighting schemes are analogous to those in \code{\link[sandwich]{vcovHC}} in package \code{sandwich} and are justified theoretically (although in the context of the standard linear model) by MacKinnon and White (1985) and Cribari-Neto (2004) (see Zeileis (2004)).

The main use of \code{vcovBK} is to be an argument to other functions,
e.g. for Wald-type testing: as \code{vcov} to \code{coeftest()},
\code{waldtest()} and other methods in the \code{lmtest} package; and as
\code{vcov} to \code{linearHypothesis()} in the \code{car} package (see the examples). Notice that the \code{vcov} argument allows to supply a function (which is the safest) or a matrix (see Zeileis (2004), 4.1-2 and examples below).

}

\seealso{
  \code{\link[sandwich]{vcovHC}} from the sandwich package for weighting schemes (\code{type} argument).
}

\author{Giovanni Millo}

\references{
  Beck, N. and Katz, J. (1995) What to do (and not to do) with
  time-series-cross-section data in comparative politics. \emph{American
    Political Science Review}, \bold{89(3)}, pp. 634--647.
    
  Cribari-Neto, F. (2004) Asymptotic inference under heteroskedasticity
  of unknown form. \emph{Computational Statistics & Data Analysis}
  \bold{45}, pp. 215--233.

  Greene, W. H. (2003) \emph{Econometric Analysis}, 5th ed., Prentice Hall/Pearson,
  Upper Saddle River, New Jersey, Sec. 13.9.3, 13.9.7.
  
  MacKinnon, J. G. and  White, H. (1985) Some heteroskedasticity-consistent
  covariance matrix estimators with improved finite sample properties.
  \emph{Journal of Econometrics} \bold{29}, pp. 305--325.
  
  Zeileis, A. (2004) Econometric Computing with HC and HAC Covariance Matrix
  Estimators. \emph{Journal of Statistical Software}, \bold{11}(10), pp. 1--17.
  URL \url{http://www.jstatsoft.org/v11/i10/}.
}

\examples{
library(lmtest)
library(car)
data("Produc", package="plm")
zz <- plm(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp, data=Produc, model="random")
## standard coefficient significance test
coeftest(zz)
## robust significance test, cluster by group
## (robust vs. serial correlation), default arguments
coeftest(zz, vcov=vcovBK)
## idem with parameters, pass vcov as a function argument
coeftest(zz, vcov=function(x) vcovBK(x, type="HC1"))
## idem, cluster by time period
## (robust vs. cross-sectional correlation)
coeftest(zz, vcov=function(x) vcovBK(x,
 type="HC1", cluster="time"))
## idem with parameters, pass vcov as a matrix argument
coeftest(zz, vcov=vcovBK(zz, type="HC1"))
## joint restriction test
waldtest(zz, update(zz, .~.-log(emp)-unemp), vcov=vcovBK)
## test of hyp.: 2*log(pc)=log(emp)
linearHypothesis(zz, "2*log(pc)=log(emp)", vcov=vcovBK)
}
\keyword{regression}

