\name{sargan}
\alias{sargan}

\title{Hansen--Sargan Test of Overidentifying Restrictions}

\description{
A test of overidentifying restrictions for models estimated by GMM.
}

\usage{
sargan(object, weights = c("twosteps", "onestep"))
}

\arguments{
  \item{object}{an object of class \code{"pgmm"}.}
  \item{weights}{}
}

\details{
The Hansen--Sargan test calculates the quadratic form of the moment restrictions that is minimized while computing the GMM estimator. It follows asymptotically a chi-square distribution with number of degrees of freedom equal to the difference between the number of moment conditions and the number of coefficients.
}

\value{
An object of class \code{"htest"}.
}

\author{Yves Croissant}
  
\references{
Hansen, L.P. (1982), Large Sample Properties of Generalized Methods of Moments Estimators, \emph{Econometrica}, 50, 1029--54.
  
Sargan, J.D. (1958), The Estimation of Economic Relationships using Instrumental Variables, \emph{Econometrica}, 26, pp.393--415.
}

\seealso{\code{\link{pgmm}}}

\examples{
data("EmplUK", package = "plm")
ar <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1) +
           lag(log(capital), 0:2) + lag(log(output), 0:2) | lag(log(emp), 2:99),
           data = EmplUK, effect = "twoways", model = "twosteps")
sargan(ar)
}

\keyword{htest}
