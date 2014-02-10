\name{pvar}
\alias{pvar}
\alias{pvar.data.frame}
\alias{pvar.pdata.frame}
\alias{print.pvar}

\title{Check for Cross-Sectional and Time Variation}

\description{This function checks for each variable of a panel if it
varies cross-sectionally and over time.}

\usage{
pvar(x, ...)
\method{pvar}{data.frame}(x, index = NULL, ...)
\method{pvar}{pdata.frame}(x, ...)
\method{print}{pvar}(x, ...)
}

\arguments{
 \item{x}{a \code{data.frame},}
 \item{index}{see \code{\link{pdata.frame}},}
 \item{\dots}{further arguments.}
}

\details{
 \code{pvar} can be time consuming for ``big'' panels.
}

\value{ 
An object of class \code{pvar} containing the following elements:

 \item{id.var}{a logical vector with \code{TRUE} values if the
  variable has individual variation, \code{FALSE} otherwise,}
 \item{time.var}{a logical vector with \code{TRUE} values if the
  variable has time variation, \code{FALSE} otherwise,} }

\author{Yves Croissant}

\examples{

# Gasoline contains two variables which are individual and time indexes
# and are the first two variables
data("Gasoline", package = "plm")
pvar(Gasoline)

# Hedonic is an unbalanced panel, townid is the individual index;
# the drop.index argument is passed to pdata.frame
data("Hedonic", package = "plm")
pvar(Hedonic,"townid", drop.index = TRUE)

# same using pdata.frame
Hed <- pdata.frame(Hedonic, "townid", drop.index = TRUE)
pvar(Hed)
}

\keyword{attribute}
