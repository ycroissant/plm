\name{piest}
\alias{piest}
\alias{summary.piest}
\alias{print.summary.piest}
\title{Chamberlain estimator and test for fixed effects}

\description{
  General estimator usefull for testing the within specification
}

\usage{
piest(formula, data, subset, na.action, index = NULL, robust = TRUE, ...)
\method{summary}{piest}(object, ...)
\method{print}{summary.piest}(x, ...)

}

\arguments{
  \item{formula}{a symbolic description for the model to be estimated,}
  \item{object,x}{an object of class \code{"plm"},}
  \item{data}{a \code{data.frame},}
  \item{subset}{see \code{\link{lm}},}
  \item{na.action}{see \code{\link{lm}},}
  \item{index}{the indexes,}
  \item{robust}{if \code{FALSE}, the error as assumed to be spherical,
    otherwise, a robust estimation of the covariance matrix is computed,}
 \item{\dots}{further arguments.}

}

\value{
  An object of class \code{"piest"}.
}

\details{ The Chamberlain method consists on using the covariates of all
  the periods as regressors. It unables to test the within
  specification.  }

\references{
Chamberlain, G. (1982) Multivariate regression for panel data,
\emph{Journal of Econometrics}, \bold{18}, pp. 5--46.
}

\author{Yves Croissant}
  
\examples{
data("RiceFarms", package = "pder")
pirice <- piest(log(goutput) ~ log(seed) + log(totlabor) + log(size), RiceFarms, index = "id")
summary(pirice)
}

\keyword{piest}
