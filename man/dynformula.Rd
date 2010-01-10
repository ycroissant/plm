\name{dynformula}
\alias{dynformula}
\alias{print.dynformula}
\alias{formula.dynformula}
\title{Dynamic Formula}

\description{
 A function to easily create a formula with lags and differences
}

\usage{
dynformula(formula, lag.form = NULL, diff.form = NULL, log.form = NULL)
}

\arguments{
  \item{formula}{a formula,}
  \item{lag.form}{a list containing the lag structure of each variable in the formula,}
  \item{diff.form}{a vector (or a list) of logical values indicating whether variables should be differenced,}
  \item{log.form}{a vector (or a list) of logical values indicating whether variables should be in logarithms.}  }

\value{
  An object of class \code{c("dynformula","formula")}, which is a
  formula with four additional attributes: \code{var}, the names of the variables in the formula, \code{lag}, \code{diff}, and \code{log}, which store the information about lags, differences and logs, respectively.

  A \code{formula} method coerces the \code{dynformula} object to a standard \code{formula}.
}

\details{
  \code{lag.form} is a list, \code{diff.form} and \code{log.form} are vectors (or lists) that should be of length equal to the total number of variables. Each element of these lists/vectors is:
  
  \itemize{
    \item either a vector of length 2 (\code{c(1,4)} means lags 1,2,3 and 4) or a scalar (\code{3} means lags 0,1,2,3 except for the left--hand side variable for which it is 1,2,3) for \code{lag.form}.
    \item logical values for \code{diff.form} and \code{log.form}.
  }
  
  It can also be an incomplete named list/vector (for example, to apply the transformation for only some variables) with eventually an unnamed element which then is the default value.
}
  
\author{Yves Croissant}

\examples{
# all variables in log, x1, x2 and x3 laged twice, y laged once and x3 differenced
z <- dynformula(y ~ x1 + x2 + x3, lag.form = list(2, y = 1),
                diff.form = c(x3 = TRUE), log.form = TRUE)
formula(z)
}

\keyword{classes}
