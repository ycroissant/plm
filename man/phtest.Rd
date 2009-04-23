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
\method{phtest}{formula}(x, data, ..., model = c("within","random"))
}

\arguments{
  \item{x}{an object of class  \code{"panelmodel"} or \code{"formula"},}
  \item{x2}{an object of class \code{"panelmodel"},}
  \item{model}{a vector containing the names of two models,}
  \item{data}{a \code{data.frame},}
  \item{\dots}{further arguments passed to \code{plm}}
}

\value{
  An object of class \code{"htest"}.
}

\details{
  The Hausman test is based on the difference of the vectors of
  coefficients of two different models.
}
\references{
  Hausman, J.A. (1978), Specification tests in econometrics,
  \emph{Econometrica}, \bold{46}, pp.1251--1271.
}

\author{Yves Croissant}
  
\examples{
data("Gasoline",package="Ecdat")
form <- lgaspcar~lincomep+lrpmg+lcarpcap
wi <-  plm(form, data=Gasoline, model="within")
re <-  plm(form, data=Gasoline, model="random")
phtest(wi, re)
phtest(form, data=Gasoline)
}
\keyword{htest}
