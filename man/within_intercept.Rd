\name{within_intercept}
\alias{within_intercept}
\alias{within_intercept.plm}

\title{Overall Intercept for Within Models Along its Standard Error}

\description{This function gives an overall intercept for within models and its standard error}

\usage{
within_intercept(x, ...)
\method{within_intercept}{plm}(x, ...)
}

\arguments{
 \item{x}{object of class \code{plm} which must be a within model (fixed effects model),}
 \item{\dots}{further arguments (currently none).}
}

\value{ 
A named \code{numeric} vector of length one: the overall intercept for the estimated within model along
attribute "se" which contains the standard error for the intercept.
}

\details{
The (somewhat artificial) intercept for within models (fixed effects models) was made popular by
Stata of StataCorp (see Gould (2013)) and later also adopted by Gretl (see Cottrell/Lucchetti (2016), p. 160-161 (example 18.1)).
It can be considered an overall intercept in the within model framework and is the weighted mean of fixed effects.
}

\references{
Gould, W. (2013), \dQuote{How can there be an intercept in the fixed-effects model estimated by xtreg, fe?},
\url{http://www.stata.com/support/faqs/statistics/intercept-in-fixed-effects-model/}.

Cottrell, A./Lucchetti, R., Gretl User's Guide, \url{http://gretl.sourceforge.net/gretl-help/gretl-guide.pdf}.
}

\seealso{
 \code{\link{fixef}} to extract the fixed effects of a within model.
}

\author{Kevin Tappe}

\examples{
# estimate some within model (with unbalanced data)
data("Hedonic", package = "plm")
mod_fe <- plm(mv ~ age + crim, data = Hedonic, index = "townid")

overallint <- within_intercept(mod_fe)

# extract standard error for intercept
attr(overallint, "se")

# overall intercept is the weighted mean of fixed effects
weighted.mean(fixef(mod_fe) , as.numeric(table(index(mod_fe)[[1]])))


# relationshop of type="dmean", "level" and within_intercept
data("Grunfeld", package = "plm")
gi <- plm(inv ~ value + capital, data = Grunfeld, model = "within")
fx_level <- fixef(gi, type = "level")
fx_dmean <- fixef(gi, type = "dmean")
overallint <- within_intercept(gi)
all.equal(overallint + fx_dmean, fx_level, check.attributes = FALSE) # TRUE

}

\keyword{attribute}
