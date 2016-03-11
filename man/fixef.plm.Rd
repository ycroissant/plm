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
  \item{type}{one of \code{"level"}, \code{"dfirst"}, or \code{"dmean"},}
  \item{digits}{digits,}
  \item{width}{the maximum length of the lines in the print output,}
  \item{\dots}{further arguments.}
}

\value{
  An object of class \code{"fixef"}. It is a numeric vector containing the fixed effects with attribute: \code{se} which contains the standard errors. 
  % TODO: this not true anymore/yet:  "and [attribute] \code{intercept} which is the overall intercept.""
  
  With the \code{type} argument, the fixed effects may be returned in levels (\code{"level"}), as deviations from the first value of the index (\code{"dfirst"}), or as deviations from the overall mean (\code{"dmean"}).
}

\details{
  The summary method prints the fixed effects (in the way they were requested with the '\code{type}' argument 
  (\code{"level"}, \code{"dfirst"}, or \code{"dmean"}), their standard errors and associated t--values and p--values.
}

\seealso{
  \code{\link{within_intercept}} for the overall intercept along its standard error, \code{\link{plm}} for plm objects and within models in general.
}


\author{Yves Croissant}

\examples{
data("Grunfeld", package = "plm")
gi <- plm(inv ~ value + capital, data = Grunfeld, model = "within")
fixef(gi)
summary(fixef(gi))

# relationshop of type="dmean", "level" and overall intercept
fx_level <- fixef(gi, type = "level")
fx_dmean <- fixef(gi, type = "dmean")
overallint <- within_intercept(gi)
all.equal(overallint + fx_dmean, fx_level, check.attributes = FALSE) # TRUE

# extract time effects in a twoways effects model
gi_tw <- plm(inv ~ value + capital, data = Grunfeld,
          model = "within", effect = "twoways")
fixef(gi_tw, effect = "time")

# calc. fitted values of oneway within model:
fixefs <- fixef(gi)[index(gi, which = "id")]
fitted_by_hand <- fixefs + gi$coefficients["value"] * gi$model$value +
                           gi$coefficients["capital"] * gi$model$capital

}

\keyword{regression}
