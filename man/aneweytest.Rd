\name{aneweytest}
\alias{aneweytest}
\alias{summary.aneweytest}
\alias{print.summary.aneweytest}
\title{Chamberlain estimator and test for fixed effects}

\description{
  Angrist and Newey's version of the Chamberlain test
}

\usage{
aneweytest(formula, data, subset, na.action, index = NULL, ...)
}

\arguments{
  \item{formula}{a symbolic description for the model to be estimated,}
  \item{data}{a \code{data.frame},}
  \item{subset}{see \code{\link{lm}},}
  \item{na.action}{see \code{\link{lm}},}
  \item{index}{the indexes,}
 \item{\dots}{further arguments.}
 
}

\value{
  An object of class \code{"htest"}.
}

\details{ Angrist and Newey's test is based on the results of the
  artifactual regression of the within residuals on the covariates for
  all the period.}

\references{
Angrist, J.D. and Newey, W.K. (1991). Over-identification tests in
earnings functions with fixed effects, \emph{Journal of Business \&
  Economic Statistics}, \bold{9(3)}, pp. 317--323.
  
}

\author{Yves Croissant}
  
\examples{
data("RiceFarms", package = "pder")
aneweytest(log(goutput) ~ log(seed) + log(totlabor) + log(size), RiceFarms, index = "id")
}
\keyword{aneweytest}
