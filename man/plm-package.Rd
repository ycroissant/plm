\name{plm-package}
\alias{plm-package}
\docType{package}
\title{
  plm package: linear models for panel data
}
\description{
plm is a package for R which intends to make the estimation of linear panel models straightforward.
plm provides functions to estimate a wide variety of models and to make (robust) inference.}

\details{
  For a gentle and comprehensive introduction to the package, please see the package's vignette.
  
  The main functions to estimate models are:
  \tabular{ll}{
    \code{plm} \tab panel data estimators using \code{lm} on transformed data,
  }
  \tabular{ll}{
    \code{pgmm} \tab generalized method of moments (GMM) estimation for panel data,
  }
  \tabular{ll}{
    \code{pvcm} \tab variable coefficients models for panel data.
  }

  Next to the model estimation function, the package offers several functions for statistical tests related to panel data/models.
  
  Multiple functions for (robust) variance--covariance matrices are at hand as well.

  The package also provides data sets to demonstrate functions and to replicate some text book/paper results.
  Use \code{data(package="plm")} to view a list of available data sets in the package.
}

\keyword{package}
\keyword{model}
\keyword{regression}

\examples{
data("Produc", package = "plm")
zz <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
          data = Produc, index = c("state","year"))
summary(zz)

# replicates some results from Baltagi (2013), table 3.1
data("Grunfeld", package = "plm")
p <- plm(inv ~ value + capital,
         data = Grunfeld, model="pooling")

wi <- plm(inv ~ value + capital,
          data = Grunfeld, model="within", effect = "twoways")

swar <- plm(inv ~ value + capital,
            data = Grunfeld, model="random", effect = "twoways")
          
amemiya <- plm(inv ~ value + capital,
               data = Grunfeld, model = "random", random.method = "amemiya",
               effect = "twoways")
                
walhus <- plm(inv ~ value + capital,
              data = Grunfeld, model = "random", random.method = "walhus",
              effect = "twoways")
}