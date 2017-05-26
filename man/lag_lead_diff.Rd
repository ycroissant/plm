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
\method{lag}{pseries}(x, k = 1, \dots)
\method{lead}{pseries}(x, k = 1, \dots)
\method{diff}{pseries}(x, lag = 1, \dots)
}

\arguments{
 \item{x}{a \code{pseries} object,}
 \item{k}{an integer vector, the number of lags for the \code{lag} and \code{lead} methods (can also be negative).
          For the \code{lag} method, a positive (negative) \code{k} gives lagged (leading) values.
          For the \code{lead} method, a positive (negative) \code{k} gives leading (lagged) values,
          thus, \code{lag(x, k = -1)} yields the same as \code{lead(x, k = 1)}.
          If \code{k} is an integer vector with length > 1 (\code{k = c(k1, k2, \dots)}) 
          a \code{matrix} with multiple lagged \code{pseries} is returned,}
 \item{lag}{the number of lags for the \code{diff} method, can also be a vector of length > 1 (see argument \code{k}) (only non--negative values in argument \code{lag} are allowed for \code{diff}),}
 \item{\dots}{further arguments.}
}

\value{

 \item{*}{An object of class \code{pseries}, if the argument specifying the lag has length 1 
 (argument \code{k} in functions \code{lag} and \code{lead}, argument \code{lag} in function \code{diff}).}
 \item{*}{A matrix containing the various series in its columns, if the argument specifying the lag has length > 1.}
}

\note{The sign of \code{k} in \code{lag.pseries} results in inverse behavior compared to \code{\link[stats]{lag}} and \code{\link[zoo]{lag.zoo}}.}

% \references{
% }

\seealso{
For further function for 'pseries' objects: \code{\link[plm]{between}}, \code{\link[plm]{Between}}, 
\code{\link[plm]{Within}}, \code{\link[plm]{summary.pseries}}, \code{\link[plm]{print.summary.pseries}}, 
\code{\link[plm]{as.matrix.pseries}}. To check if the time periods are consecutive per individual, 
see \code{\link[plm]{is.pconsecutive}}.
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
 
# compute more than one lag and diff at once
# (each returns matrix)
lag(z, c(1,2))
diff(z, c(1,2))
}

\author{Yves Croissant}

\keyword{classes}