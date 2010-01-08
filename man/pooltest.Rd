\name{pooltest}
\alias{pooltest}
\alias{pooltest.plm}
\alias{pooltest.formula}

\title{Test of Poolability}

\description{
A Chow  test for the poolability of the data.
}

\usage{
pooltest(x, ...)
\method{pooltest}{plm}(x, z, ...)
\method{pooltest}{formula}(x, data, ...)
}

\arguments{
 \item{x}{an object of class  \code{"plm"},}
 \item{z}{an object of class \code{"pvcm"} obtained with
    \code{model="within"},}
 \item{data}{a \code{data.frame},}
 \item{\dots}{further arguments passed to plm.}
}

\details{ \code{pooltest} is an \emph{F} test of stability (or Chow  test) for the coefficients of a panel model. The estimated \code{plm} object should be a \code{"pooling"} model or a \code{"within"} model (the default); intercepts are assumed to be identical in the first case and different in the second case.
}

\value{
An object of class \code{"htest"}.
}

\author{Yves Croissant}

\examples{
data("Gasoline", package = "plm")
form <- lgaspcar ~ lincomep + lrpmg + lcarpcap
gasw <-  plm(form, data = Gasoline, model = "within")
gasp <-  plm(form, data = Gasoline, model = "pooling")
gasnp <-  pvcm(form, data = Gasoline, model = "within")
pooltest(gasw, gasnp)
pooltest(gasp, gasnp)

pooltest(form, data = Gasoline, effect = "individual", model = "within")
pooltest(form, data = Gasoline, effect = "individual", model = "pooling")
}

\keyword{htest}
