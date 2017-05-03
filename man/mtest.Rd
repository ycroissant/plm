\name{mtest}
\alias{mtest}
\title{Arellano--Bond test of Serial Correlation}

\description{
 Test of serial correlation for models estimated by GMM
}

\usage{
mtest(object, order = 1, vcov = NULL)
}

\arguments{
  \item{object}{an object of class \code{"pgmm"},}
  \item{order}{the order of the serial correlation (1 or 2),}
  \item{vcov}{a matrix of covariance for the coefficients or a function to compute it.}
}

\value{
  An object of class \code{"htest"}.
}

\details{
  The Arellano--Bond test is a test of correlation based on the
  residuals of the estimation. By default, the computation is 
  done with the standard covariance matrix of the coefficients.
  A robust estimator of this covariance matrix can be supplied
  with the \code{vcov} argument.
}

\references{
Arellano, M. and Bond, S. (1991), Some Tests of Specification for Panel Data: Monte Carlo Evidence and an Application to Employment Equations, \emph{The Review of Economic Studies}, \bold{58}(2), 1991, pp. 227--297.
}

\seealso{\code{\link{pgmm}}}

\author{Yves Croissant}
  
\examples{
data("EmplUK", package = "plm")
ar <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1) +
           lag(log(capital), 0:2) + lag(log(output), 0:2) | lag(log(emp), 2:99),
           data = EmplUK, effect = "twoways", model = "twosteps")
mtest(ar, order = 1)
mtest(ar, order = 2, vcov = vcovHC)
}

\keyword{htest}
