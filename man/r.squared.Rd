\name{r.squared}
\alias{r.squared}
\title{R squared and adjusted R squared for panel models}
\description{
  This function computes R squared or adjusted R squared for plm objects.
  It allows to define on which transformation of the data the (adjusted)
  R squared is to be computed and which method for calculation is used.
}
\usage{
r.squared(object, model = NULL, type = c("cor", "rss", "ess"), dfcor = FALSE)
}
\arguments{
  \item{object}{an object of class \code{"plm"},}
  \item{model}{on which transformation of the data the R-squared it to
    be computed. If \code{NULL}, the transformation used to estimate the
    model is also used for the computation of R squared,}
  \item{type}{inicates  method which is used to compute R squared. One of\cr
    \code{"rss"} (residual sum of squares),\cr
    \code{"ess"} (explained sum of squares), or\cr
    \code{"cor"} (coefficient of correlation between the fitted values and the
                  response),}
  \item{dfcor}{if \code{TRUE}, the adjusted R squared is computed.}
}
\value{
  A numerical value (R squared or adjusted R squared).
}

\keyword{htest}
