\name{vcovSCC}
\alias{vcovSCC}
\alias{vcovSCC.plm}

\title{Driscoll and Kraay (1998) Robust Covariance Matrix Estimator}
\description{
  Nonparametric robust covariance matrix estimators \emph{a la Driscoll
    and Kraay} for panel models with cross-sectional \emph{and} serial
  correlation.
  }
\usage{
\method{vcovSCC}{plm}(x, type = c("HC0", "sss", "HC1", "HC2", "HC3", "HC4"),
                      cluster="time",
                      maxlag=NULL,
                      inner=c("cluster","white"),
                      wj=function(j, maxlag) 1-j/(maxlag+1),
                      ...)
}

\arguments{
  \item{x}{an object of class \code{"plm"}}
  \item{type}{one of \code{"HC0"}, \code{"sss"}, \code{"HC1"}, \code{"HC2"}, \code{"HC3"}, \code{"HC4"},}
  \item{cluster}{switch for vcovG; set at \code{"time"} here,}
  \item{maxlag}{either \code{NULL} or a positive integer specifying the
  maximum lag order before truncation}
  \item{inner}{the function to be applied to the residuals inside the 
  sandwich: \code{"cluster"} for SCC, \code{"white"} for Newey-West,}
  \item{wj}{weighting function to be applied to lagged terms,}
  \item{...}{further arguments}.
}

\value{
  An object of class \code{"matrix"} containing the estimate of the covariance matrix of coefficients.
}

\details{
\code{vcovSCC} is a function for estimating a robust covariance matrix of
parameters for a panel model according to the Driscoll and Kraay (1998)
method, which is consistent with cross-sectional and serial correlation
in a T-asymptotic setting and irrespective of the N dimension. The use
with random effects models is undocumented.

Weighting schemes are analogous to those in \code{vcovHC} in package \code{sandwich} and are justified theoretically (although in the context of the standard linear model) by MacKinnon and White (1985) and Cribari-Neto (2004) (see Zeileis (2004)).

The main use of \code{vcovSCC} is to be an argument to other functions,
e.g. for Wald-type testing: as \code{vcov} to \code{coeftest()},
\code{waldtest()} and other methods in the \code{lmtest} package; and as
\code{vcov} to \code{linearHypothesis()} in the \code{car} package (see the examples). Notice that the \code{vcov} argument may be supplied a function (which is the safest) or a matrix (see Zeileis (2004), 4.1-2 and examples below).

}

\author{Giovanni Millo, partially ported from Daniel Hoechle's (2007) Stata code}

\references{
 Driscoll, J.C. and Kraay, A.C. (1998) Consistent Covariance Matrix
 Estimation with Spatially Dependent Panel Data. \emph{Review of
   Economics and Statistics} \bold{80}, pp. 549--560.

 Hoechle, D. (2007) Robust standard errors for panel regressions with
  cross-sectional dependence. \emph{Stata Journal}, \bold{7(3)},
  pp. 281--312.
  }

\examples{
library(lmtest)
library(car)
data("Produc", package="plm")
zz <- plm(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp, data=Produc, model="pooling")
## standard coefficient significance test
coeftest(zz)
## SCC robust significance test, default
coeftest(zz, vcov=vcovSCC)
## idem with parameters, pass vcov as a function argument
coeftest(zz, vcov=function(x) vcovSCC(x, type="HC1", maxlag=4))
## joint restriction test
waldtest(zz, update(zz, .~.-log(emp)-unemp), vcov=vcovSCC)
## test of hyp.: 2*log(pc)=log(emp)
linearHypothesis(zz, "2*log(pc)=log(emp)", vcov=vcovSCC)
}
\keyword{regression}

