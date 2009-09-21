\name{pserie}
\alias{pserie}
\alias{Between}
\alias{between}
\alias{within}
\alias{Between.pserie}
\alias{between.pserie}
\alias{within.pserie}
\alias{lag.pserie}
\alias{diff.pserie}
\alias{summary.pserie}
\alias{print.summary.pserie}
\alias{as.matrix.pserie}

\title{panel series}
\description{
  a class for panel series for which several usefull computations are available
}
\usage{
between(x, ...)
Between(x, ...)
within(x, ...)
\method{lag}{pserie}(x, k = 1, ...)
\method{diff}{pserie}(x, lag = 1, ...)
\method{as.matrix}{pserie}(x, idbyrow = TRUE, ...)
\method{between}{pserie}(x, effect = c("individual", "time"), ...)
\method{Between}{pserie}(x, effect = c("individual", "time"), ...)
\method{within}{pserie}(x, effect = c("individual", "time"), ...)
\method{summary}{pserie}(object, ...)
\method{print}{summary.pserie}(x, ...)
}
\arguments{
  \item{x, object}{a \code{pserie} or a \code{summary.pserie} object,}
  \item{effect}{the (individual or time) effect,}
  \item{k, lag}{the number of lags for the \code{lag} and \code{diff}
  methods,}
  \item{idbyrow}{if TRUE, the lines of the matrix are the individuals,}
  \item{\dots}{further arguments.}
}
\value{
  All these functions return an object of class \code{pserie}, except
  \code{between} which is a numeric vector.
}
\details{
  A \code{pserie} is obtained when a serie is extracted from a
  \code{pdata.frame} object. It consists on the original serie with the
  \code{index} attribute of the \code{pdata.frame}. Specific transformations
  (within and between) are available and spectial \code{lag} and
  \code{diff} methods are provided.
}
% \references{
% }
% \seealso{
% }
\examples{
# Create first a pdata.frame
data("EmplUK", package = "plm")
Em <- pdata.frame(EmplUK)
# Then extract a serie, which becomes a pserie
z <- Em$output
class(z)
# obtain the matrix representation
as.matrix(z)
# compute the between, within transformation
between(z)
within(z)
# Between replicate the values for each time observations
Between(z)
# compute the first and third lag, and the difference laged twice
lag(z)
lag(z, 3)
diff(z, 2)
}
\author{Yves Croissant}
\keyword{classes}
