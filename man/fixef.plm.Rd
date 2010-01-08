\name{fixef.plm}
\alias{fixef.plm}
\alias{fixef}
\alias{print.fixef}
\alias{summary.fixef}
\alias{print.summary.fixef}
\title{Extract the Fixed Effects}

\description{
  This function extracts the fixed effects from a \code{plm} object.
}

\usage{
\method{fixef}{plm}(object, effect = NULL,
                      type = c("level", "dfirst", "dmean"), ...)
\method{print}{fixef}(x, digits = max(3, getOption("digits") - 2),
width = getOption("width"),...)
\method{summary}{fixef}(object, ...)
\method{print}{summary.fixef}(x, digits = max(3, getOption("digits") -
2), width = getOption("width"),...)
}

\arguments{
  \item{x,object}{an object of class \code{"plm"}, an object
    of class \code{"fixef"} for the \code{print} and the \code{summary} method,}
  \item{effect}{one of \code{"individual"} or \code{"time"}, only
    relevant in case of two--ways effects models,}
  \item{type}{one of \code{level}, \code{dmean} or \code{dfirst}.}
  \item{digits}{digits,}
  \item{width}{the maximum length of the lines in the print output,}
  \item{\dots}{further arguments.}
}

\value{
  An object of class \code{"fixef"}. It is a numeric vector containing the fixed effects with two attributes: \code{se} which contains the standard errors and \code{intercept} which is the overall intercept. With the \code{type} argument, the fixed effects may be returned in levels, as deviations from the overall mean or as deviations from the first value of the index.
}

\details{
  The summary method prints the effects in deviation from the overallintercept, the standard error and the t--values.
}

\seealso{
  \code{\link{plm}}
}

\author{Yves Croissant}

\examples{
data("Grunfeld", package = "plm")
gi <- plm(inv ~ value + capital, data = Grunfeld, model = "within")
fixef(gi)
summary(fixef(gi))
# extract time effects in a twoways effects model
gi <- plm(inv ~ value + capital, data = Grunfeld,
          model = "within", effect = "twoways")
fixef(gi,effect = "time")
}

\keyword{regression}
