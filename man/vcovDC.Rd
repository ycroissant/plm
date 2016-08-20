\name{vcovDC}
\alias{vcovDC}
\alias{vcovDC.plm}
\alias{vcovDC.pcce}

\title{Double-Clustering Robust Covariance Matrix Estimator}
\description{
  High-level convenience wrapper for double-clustering robust covariance 
  matrix estimators \emph{a la} Thompson (2011) and Cameron, Gelbach and
  Miller (2011) for panel models.
  }
\usage{
\method{vcovDC}{plm}(x, type = c("HC0", "sss", "HC1", "HC2", "HC3", "HC4"),
                      ...)
}

\arguments{
  \item{x}{an object of class \code{"plm"} or \code{"pcce"}}
  \item{type}{one of \code{"HC0"}, \code{"sss"}, \code{"HC1"}, \code{"HC2"}, \code{"HC3"}, \code{"HC4"},}
  \item{...}{further arguments}.
}

\value{
  An object of class \code{"matrix"} containing the estimate of the covariance matrix of coefficients.
}

\details{
\code{vcovDC} is a function for estimating a robust covariance matrix of
parameters for a panel model with errors clustering along both dimensions.
The function is a convenience wrapper simply summing a group- and a
time-clustered covariance matrix and subtracting a diagonal one \emph{a la}
White.

Weighting schemes are analogous to those in \code{\link[sandwich]{vcovHC}} in package \code{sandwich} and are justified theoretically (although in the context of the standard linear model) by MacKinnon and White (1985) and Cribari-Neto (2004) (see Zeileis (2004)).

The main use of \code{vcovDC} is to be an argument to other functions,
e.g. for Wald-type testing: argument \code{vcov.} to \code{coeftest()}, argument \code{vcov} to
\code{waldtest()} and other methods in the \code{lmtest} package; and argument
\code{vcov.} to \code{linearHypothesis()} in the \code{car} package (see the examples). Notice that the \code{vcov} and \code{vcov.} arguments allow to supply a function (which is the safest) or a matrix (see Zeileis (2004), 4.1-2 and examples below).

}

\seealso{
  \code{\link[sandwich]{vcovHC}} from the sandwich package for weighting schemes (\code{type} argument).
}

\author{Giovanni Millo}

\references{
Cameron, A.C., Gelbach, J.B., & Miller, D.L. (2011) Robust inference with 
 multiway clustering, \emph{Journal of Business and Economic Statistics}
 \bold{29(2)}, pp. 238--249.
 
Cribari-Neto, F. (2004) Asymptotic inference under heteroskedasticity
 of unknown form. \emph{Computational Statistics & Data Analysis}
 \bold{45(2)}, pp. 215--233.
 
MacKinnon, J. G. and  White, H. (1985) Some heteroskedasticity-consistent
 covariance matrix estimators with improved finite sample properties.
 \emph{Journal of Econometrics} \bold{29(3)}, pp. 305--325.
 
Thompson, S.B. (2011) Simple formulas for standard errors that cluster by
 both firm and time, \emph{Journal of Financial Economics} \bold{99(1)}, pp. 1--10.
 
Zeileis, A. (2004) Econometric Computing with HC and HAC Covariance Matrix
 Estimators. \emph{Journal of Statistical Software}, \bold{11}(10), pp. 1--17.
 URL \url{http://www.jstatsoft.org/v11/i10/}.
}

\examples{
library(lmtest)
library(car)
data("Produc", package="plm")
zz <- plm(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp, data=Produc, model="pooling")
## standard coefficient significance test
coeftest(zz)
## DC robust significance test, default
coeftest(zz, vcov.=vcovDC)
## idem with parameters, pass vcov as a function argument
coeftest(zz, vcov.=function(x) vcovDC(x, type="HC1", maxlag=4))
## joint restriction test
waldtest(zz, update(zz, .~.-log(emp)-unemp), vcov=vcovDC)
## test of hyp.: 2*log(pc)=log(emp)
linearHypothesis(zz, "2*log(pc)=log(emp)", vcov.=vcovDC)
}
\keyword{regression}

