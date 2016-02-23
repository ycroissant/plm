\name{lag}
\alias{lag}
\alias{lead}
\alias{diff}
\alias{lag.pseries}
\alias{lead.pseries}
\alias{diff.pseries}

\title{lag, lead, and diff for panel data}

\description{
lag, lead, and diff functions for class pseries.
}

\usage{
\method{lag}{pseries}(x, k = 1, ...)
\method{lead}{pseries}(x, k = 1, ...)
\method{diff}{pseries}(x, lag = 1, ...)
}

\arguments{
 \item{x}{a \code{pseries} object,}
 \item{k}{an integer vector, the number of lags for the \code{lag} and \code{lead} methods (can also be negative).
          For the \code{lag} method, a positive (negative) \code{k} gives lagged (leading) values.
          For the \code{lead} method, a positive (negative) \code{k} gives leading (lagged) values,
          thus, \code{lag(x, k = -1)} yields the same as \code{lead(x, k = 1)}.
          If \code{k} is an integer vector with length > 1 (\code{k = c(k1, k2, ...)}) a \code{matrix} with multiple lagged \code{pseries} is returned,}
 \item{lag}{the number of lags for the \code{diff} method (only non--negative values for parameter \code{lag} are allowed in \code{diff}),}
 \item{\dots}{further arguments.}
}

\value{
These functions return an object of class \code{pseries} except if \code{lag} (thus also \code{lead}) is called with more than one lag (lead), i. e. \code{length(k) > 1}, a matrix is returned.
}

\note{The sign of \code{k} in \code{lag.pseries} results in inverse behavior compared to \code{\link[stats]{lag}} and \code{\link[zoo]{lag.zoo}}.}

% \references{
% }

\seealso{
For further function for 'pseries' objects: \code{\link[plm]{between}}, \code{\link[plm]{Between}}, 
\code{\link[plm]{Within}}, \code{\link[plm]{summary.pseries}}, \code{\link[plm]{print.summary.pseries}},
 \code{\link[plm]{as.matrix.pseries}}.
}

\examples{
# First, create a pdata.frame
data("EmplUK", package = "plm")
Em <- pdata.frame(EmplUK)

# Then extract a series, which becomes additionally a pseries
z <- Em$output
class(z)

# compute the first and third lag, and the difference lagged twice
lag(z)
lag(z, 3)
diff(z, 2)

# compute negative lags (= leading values)
lag(z, -1)
lead(z, 1) # same as line above
identical(lead(z, 1), lag(z, -1)) # TRUE
 
# compute more than one lag at once
lag(z, c(1,2))
}

\author{Yves Croissant}

\keyword{classes}