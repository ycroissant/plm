\name{r.squared}
\alias{r.squared}
\title{R squared for panel data}
\description{
  This function computes the R squared for plm objects. It allows to
  define on which transformation of the data the R squared has to be
  computed and which formula should be used
}
\usage{
r.squared(object, model = NULL, type = c('cor', 'rss', 'ess'), dfcor = FALSE)
}
\arguments{
  \item{object}{an object of class \code{"plm"},}
  \item{model}{on which transformation of the data the R-squared has to
    be computed ? If null the transformation used to estimate the model
    is also used for the computation of the R squared.}
  \item{type}{should the R-squaredcomputed using the residual sum of
    squares (\code{"rss"}), the explained sum of squares (\code{"ess"})
    or the coefficient of correlation between the fitted values and the
    response (\code{cor}).}
  \item{dfcor}{if \code{TRUE}, the adjusted R squared is computed.}
}
\value{
  a numerical value
}

\keyword{htest}
