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
\method{lag}{pseries}(x, k = 1, shift = c("time", "row"), \dots)
\method{lead}{pseries}(x, k = 1, shift = c("time", "row"), \dots)
\method{diff}{pseries}(x, lag = 1, shift = c("time", "row"), \dots)
}

\arguments{
 \item{x}{a \code{pseries} object,}
 \item{k}{an integer, the number of lags for the \code{lag} and \code{lead} methods (can also be negative).
          For the \code{lag} method, a positive (negative) \code{k} gives lagged (leading) values.
          For the \code{lead} method, a positive (negative) \code{k} gives leading (lagged) values,
          thus, \code{lag(x, k = -1)} yields the same as \code{lead(x, k = 1)}.
          If \code{k} is an integer with length > 1 (\code{k = c(k1, k2, \dots)}) 
          a \code{matrix} with multiple lagged \code{pseries} is returned,}
 \item{lag}{the number of lags for the \code{diff} method, can also be of length > 1 (see argument \code{k}) 
            (only non--negative values in argument \code{lag} are allowed for \code{diff}),}
 \item{shift}{character, either \code{"time"} (default) or \code{"row"} determining how the shifting
              in the \code{lag}/\code{lead}/\code{diff} functions is performed (see Details and Examples).}
 \item{\dots}{further arguments (currently none evaluated).}
}

\details{
This set of functions perform lagging, leading (lagging in the opposite direction), and differencing operations
on \code{pseries} objects, i. e., they take the panel structure of the data into account by performing the
operations per individual.

Argument \code{shift} controls the shifting of observations to be used by methods \code{lag}, \code{lead}, and 
\code{diff}:

\itemize{
\item{\code{shift = "time"} (default): }{Methods respect the numerical value in the time dimension of the index. 
The time dimension needs to be interpretable as a sequence t, t+1, t+2, \ldots where t is an integer (from a 
technical viewpoint, \code{as.numeric(as.character(index(your_pdata.frame)[[2]]))} needs to result in a meaningful
integer).}

\item{\code{shift = "row": }}{Methods perform the shifting operation based solely on the "physical position" of the 
observations, i.e. neighbouring rows are shifted per individual. The value in the time index is not relevant in this
case.}
}

For consecutive time periods per individual, a switch of shifting behaviour results in no difference. Different 
return values will occur for non-consecutive time periods per individual ("holes in time"), see also Examples.


}

\value{
\itemize{
 \item{An object of class \code{pseries}, if the argument specifying the lag has length 1 
 (argument \code{k} in functions \code{lag} and \code{lead}, argument \code{lag} in function \code{diff}).}
 \item{A matrix containing the various series in its columns, if the argument specifying the lag has length > 1.}
}
}

\note{
  The sign of \code{k} in \code{lag.pseries} results in inverse behaviour compared to \code{\link[stats]{lag}} 
  and \code{\link[zoo]{lag.zoo}}.
}

\note{
  Up to and including version 1.6-6 of package \pkg{plm}, the lag, lead and diff functions for pseries
  performed the shifting based on rows and there was no argument \code{shift}. Beginning with version 1.7-0, 
  the default shifting in these functions respects the time dimension (argument \code{shift} introduced with
  default value \code{"time"}.)
}

% \references{
% }

\seealso{
To check if the time periods are consecutive per individual, see \code{\link[plm]{is.pconsecutive}}.

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
 
# compute more than one lag and diff at once (matrix returned)
lag(z, c(1,2))
diff(z, c(1,2))

## demonstrate behaviour of shift = "time" vs. shift = "row"
# delete 2nd time period for first individual (1978 is missing (not NA)):
Em_hole <- Em[-2, ]
is.pconsecutive(Em_hole) # check: non-consecutive for 1st individual now

# original non-consecutive data:
head(Em_hole$emp, 10) 
# for shift = "time", 1-1979 contains the value of former 1-1977 (2 periods lagged):
head(lag(Em_hole$emp, k = 2, shift = "time"), 10)
# for shift = "row", 1-1979 contains NA (2 rows lagged (and no entry for 1976):
head(lag(Em_hole$emp, k = 2, shift = "row"), 10)
}

\author{Yves Croissant and Kevin Tappe}

\keyword{classes}