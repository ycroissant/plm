\name{is.pseries}
\alias{is.pseries}

\title{Check if an object is a pseries}

\description{This function checks if an object qualifies as a pseries}

\usage{
is.pseries(object)
}

\arguments{
 \item{object}{object to be checked for pseries features}
% \item{\dots}{further arguments.}
}

\value{
A logical indicating whether the object is a pseries (\code{TRUE}) or not (\code{FALSE}).
}

\details{
A \code{"pseries"} is a wrapper around a "basic class" (numeric, factor, logical, or character).

To qualify as a pseries, an object needs to have the following features:
\itemize{
\item class contains \code{"pseries"} and there are at least two classes (\code{"pseries"} and the basic class),
\item have an appropriate index attribute (defines the panel structure),
\item any of \code{is.numeric}, \code{is.factor}, \code{is.logical}, \code{is.character} is \code{TRUE}.
}

}

\seealso{
 \code{\link{pseries}} for some computations on pseries and some further links.
}

\examples{
# Create a pdata.frame and extract a series, which becomes a pseries
data("EmplUK", package = "plm")
Em <- pdata.frame(EmplUK)
z <- Em$output

class(z) # pseries as indicated by class
is.pseries(z) # and confirmed by check

# destroy index of pseries and re-check
attr(z, "index") <- NA
is.pseries(z) # now FALSE
}

\keyword{attribute}
