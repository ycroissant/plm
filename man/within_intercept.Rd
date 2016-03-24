\name{within_intercept}
\alias{within_intercept}
\alias{within_intercept.plm}

\title{Overall Intercept for Within Models Along its Standard Error}

\description{This function gives an overall intercept for within models and its accompanying standard error}

\usage{
within_intercept(object, ...)
\method{within_intercept}{plm}(object, .vcov = NULL, ...)
}

\arguments{
 \item{object}{object of class \code{plm} which must be a within model (fixed effects model),}
 \item{.vcov}{if not \code{NULL} (default), a function to calculate a user defined variance--covariance matrix (function for robust vcov),}
 \item{\dots}{further arguments (currently none).}
}


\details{
The (somewhat artificial) intercept for within models (fixed effects models) was made popular by
Stata of StataCorp (see Gould (2013)) and later also adopted by Gretl (see Cottrell/Lucchetti (2016), p. 160-161 (example 18.1)).
It can be considered an overall intercept in the within model framework and is the weighted mean of fixed effects, see examples 
for the relationship.

Users can set argument \code{.vcov} to a function to calculate a specific (robust) variance--covariance matrix and get the 
respective (robust) standard error for the overall intercept, e.g. the function \code{\link[plm]{vcovHC}}, 
see examples for usage. Note: The argument \code{.vcov} must be a function, not a matrix, because the model 
to calculate the overall intercept for the within model is different from the within model itself.
}

\value{ 
A named \code{numeric} vector of length one: The overall intercept for the estimated within model along
attribute "se" which contains the standard error for the intercept.
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
# estimate within model (unbalanced data)
data("Hedonic", package = "plm")
mod_fe <- plm(mv ~ age + crim, data = Hedonic, index = "townid")
overallint <- within_intercept(mod_fe)
attr(overallint, "se") # standard error

# overall intercept is the weighted mean of fixed effects
weighted.mean(fixef(mod_fe) , as.numeric(table(index(mod_fe)[[1]])))

# relationship of type="dmean", "level" and within_intercept
data("Grunfeld", package = "plm")
gi <- plm(inv ~ value + capital, data = Grunfeld, model = "within")
fx_level <- fixef(gi, type = "level")
fx_dmean <- fixef(gi, type = "dmean")
overallint <- within_intercept(gi)
all.equal(overallint + fx_dmean, fx_level, check.attributes = FALSE) # TRUE

# overall intercept with robust standard error
within_intercept(gi, .vcov = function(x) vcovHC(x, method="arellano", type="HC0"))
}

\keyword{attribute}
