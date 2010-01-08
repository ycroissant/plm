\name{pseries}
\alias{pseries}
\alias{Between}
\alias{between}
\alias{Within}
\alias{Between.pseries}
\alias{between.pseries}
\alias{Within.pseries}
\alias{lag.pseries}
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
 \item{effect}{the (individual or time) effect,}
 \item{k, lag}{the number of lags for the \code{lag} and \code{diff}
  methods,}
 \item{idbyrow}{if TRUE, the lines of the matrix are the individuals,}
 \item{\dots}{further arguments.}
}

\value{
All these functions return an object of class \code{pseries}, except \code{between} which is a numeric vector.
}

\details{
A \code{pseries} is obtained when a series is extracted from a
\code{pdata.frame} object. It consists on the original series with the \code{index} attribute of the \code{pdata.frame}. Specific transformations (within and between) are available and special \code{lag} and \code{diff} methods are provided.
}

% \references{
% }

% \seealso{
% }

\examples{
# Create first a pdata.frame
data("EmplUK", package = "plm")
Em <- pdata.frame(EmplUK)
# Then extract a serie, which becomes a pseries
z <- Em$output
class(z)
# obtain the matrix representation
as.matrix(z)
# compute the between, within transformation
between(z)
Within(z)
# Between replicate the values for each time observations
Between(z)
# compute the first and third lag, and the difference laged twice
lag(z)
lag(z, 3)
diff(z, 2)
}

\author{Yves Croissant}

\keyword{classes}
