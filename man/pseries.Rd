\name{pseries}
\alias{pseries}
\alias{Between}
\alias{between}
\alias{Within}
\alias{lag}
\alias{lead}
\alias{diff}
\alias{Between.pseries}
\alias{between.pseries}
\alias{Within.pseries}
\alias{lag.pseries}
\alias{lead.pseries}
\alias{diff.pseries}
\alias{summary.pseries}
\alias{print.summary.pseries}
\alias{as.matrix.pseries}

\title{panel series}

\description{
A class for panel series for which several useful computations are available.
}

\usage{
between(x, ...)
Between(x, ...)
Within(x, ...)
\method{lag}{pseries}(x, k = 1, ...)
\method{lead}{pseries}(x, k = 1, ...)
\method{diff}{pseries}(x, lag = 1, ...)
\method{as.matrix}{pseries}(x, idbyrow = TRUE, ...)
\method{between}{pseries}(x, effect = c("individual", "time"), ...)
\method{Between}{pseries}(x, effect = c("individual", "time"), ...)
\method{Within}{pseries}(x, effect = c("individual", "time"), ...)
\method{summary}{pseries}(object, ...)
\method{print}{summary.pseries}(x, ...)
}

\arguments{
 \item{x, object}{a \code{pseries} or a \code{summary.pseries} object,}
 \item{effect}{character string indicating the \code{"individual"} or \code{"time"} effect,}
 \item{k}{the number of lags for the \code{lag} and \code{lead} methods (can also be negative).
          For the \code{lag} method, a positive (negative) \code{k} gives lagged (leading) values.
          For the \code{lead} method, a positive (negative) \code{k} gives leading (lagged) values,
          thus, \code{lag(x, k = -1)} yields the same as \code{lead(x, k = 1)}.
          If \code{k} is a vector (\code{k = c(k1,k2, ...)}) a \code{matrix} with multiple lagged \code{pseries} is returned,}
 \item{lag}{the number of lags for the \code{diff} method (only non-negative values for parameter \code{lag} are allowed in \code{diff}),}
 \item{idbyrow}{if \code{TRUE} in the \code{as.matrix} method, the lines of the matrix are the individuals,}
 \item{\dots}{further arguments.}
}

\value{
All these functions return an object of class \code{pseries}, except:\cr
\code{between}, which returns a numeric vector,\cr
\code{as.matrix}, which returns a matrix, and\cr
\code{lag} (thus also \code{lead}), which returns a matrix if called with more than one lag (lead), i. e. \code{length(k) > 1}.
}

\details{
A \code{pseries} is obtained when a series is extracted from a
\code{pdata.frame} object. It consists of the original series with the \code{index} attribute of the \code{pdata.frame}. Specific transformations (\code{within} and \code{between}) are available and special \code{lag}, \code{lead} and \code{diff} methods are provided.
}

\note{The sign of \code{k} in \code{lag.pseries} behaves inversely compared to \code{\link[stats]{lag}} and \code{\link[zoo]{lag.zoo}}.}

% \references{
% }

% \seealso{
% }

\examples{
# First, create a pdata.frame
 data("EmplUK", package = "plm")
 Em <- pdata.frame(EmplUK)
# Then extract a series, which becomes additionally a pseries
 z <- Em$output
 class(z)

# obtain the matrix representation
 as.matrix(z)

# compute the between and within transformations
 between(z)
 Within(z)

# Between replicates the values for each time observations
 Between(z)

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