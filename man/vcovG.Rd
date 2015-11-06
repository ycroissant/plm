\name{vcovG}
\alias{vcovG}
\alias{vcovG.plm}

\title{Generic Lego building block for Robust Covariance Matrix Estimators}
\description{
  Generic Lego building block for robust covariance matrix estimators of
  the vcovXX kind for panel models.
  }
\usage{
\method{vcovG}{plm}(x, type = c("HC0", "sss", "HC1", "HC2", "HC3", "HC4"),
                      cluster=c("group", "time"),
                      l=0,
                      inner=c("cluster", "white"),
                      ...)
}

\arguments{
  \item{x}{an object of class \code{"plm"}}
  \item{type}{one of \code{"HC0"}, \code{"sss"}, \code{"HC1"}, \code{"HC2"}, \code{"HC3"}, \code{"HC4"},}
  \item{cluster}{one of \code{"group"}, \code{"time"},}
  \item{l}{lagging order, defaulting to zero}
  \item{inner}{the function to be applied to the residuals inside the 
  sandwich: one of \code{"cluster"} or \code{"white"},}
  \item{...}{further arguments}.
}

\value{
  An object of class \code{"matrix"} containing the estimate of the covariance matrix of coefficients.
}

\details{
  \code{vcovG} is the generic building block for use by higher-level
  wrappers vcovHC, vcovSCC, vcovDC and vcovNW.
The main use of \code{vcovG} is to be used internally by the former, but
  it is made available in the user space for use in non-standard combinations.
}

\author{Giovanni Millo}

\references{
 Millo, G. (2014) Robust standard error estimators for panel models: a
 unifying approach \emph{Unpublished manuscript}.
  }

\examples{
data("Produc", package="plm")
zz <- plm(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp, data=Produc,
model="pooling")
## reproduce Arellano's covariance matrix
vcovG(zz, cluster="group", inner="cluster", l=0)
## use in coefficient significance test
library(lmtest)
## define custom covariance function
## (in this example, same as vcovHC)
myvcov <- function(x) vcovG(x, cluster="group", inner="cluster", l=0)
## robust significance test
coeftest(zz, vcov=myvcov)
}
\keyword{regression}

