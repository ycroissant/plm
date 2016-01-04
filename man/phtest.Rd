\name{phtest}
\alias{phtest}
\alias{phtest.formula}
\alias{phtest.panelmodel}
\title{Hausman Test for Panel Models}

\description{
Specification test for panel models.
}

\usage{
phtest(x, ...)
\method{phtest}{panelmodel}(x, x2, ...)
\method{phtest}{formula}(x, data, model = c("within", "random"),
                method = c("chisq", "aux"),
                index = NULL, vcov = NULL, ...)
}

\arguments{
 \item{x}{an object of class  \code{"panelmodel"} or \code{"formula"},}
 \item{x2}{an object of class \code{"panelmodel"},}
 \item{model}{a vector containing the names of two models,}
 \item{data}{a \code{data.frame},}
 \item{method}{one of \code{"chisq"} or \code{"aux"},}
 \item{index}{an optional vector of index variables,}
 \item{vcov}{an optional covariance function}
 \item{\dots}{further arguments passed to \code{plm}}
}

\value{
An object of class \code{"htest"}.
}

\details{
The Hausman test (sometimes also called Durbin--Wu--Hausman test) is
based on the difference of the vectors of coefficients of two different
models. The \code{panelmodel} method computes the original version of
the test based on a quadratic form. The \code{formula} method, if
\code{method="chisq"} (default), computes
the original version of the test based on a quadratic form;
if \code{method="aux"} then the auxiliary-regression-based version in
Wooldridge (2010, Sec. 10.7.3.) is computed instead. Only the latter can
be robustified specifying a robust covariance estimator as a function
through the argument \code{vcov}. 
}

\references{
Hausman, J.A. (1978), Specification tests in econometrics,
\emph{Econometrica}, \bold{46}, pp. 1251--1271.

Wooldridge, Jeffrey M. (2010), \emph{Econometric analysis of
  cross-section and panel data}, 2nd ed., MIT Press, Sec. 10.7.3., pp. 328--334.
  
Baltagi, Badi H. (2013), \emph{Econometric Analysis of Panel Data}, 5th ed., John Wiley and Sons., Sec. 4.3.
}

\author{Yves Croissant, Giovanni Millo}
  
\examples{
data("Gasoline", package = "plm")
form <- lgaspcar ~ lincomep + lrpmg + lcarpcap
wi <- plm(form, data = Gasoline, model = "within")
re <- plm(form, data = Gasoline, model = "random")
phtest(wi, re)
phtest(form, data = Gasoline)
phtest(form, data = Gasoline, method = "aux")

# robust Hausman test (regression-based)
phtest(form, data = Gasoline, method = "aux", vcov = vcovHC)

# robust Hausman test with vcov supplied as a
# function and additional parameters
phtest(form, data = Gasoline, method = "aux",
  vcov = function(x) vcovHC(x, method="white2", type="HC3"))
}

\keyword{htest}
