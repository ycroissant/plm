\name{pvar}
\alias{pvar}
\alias{pvar.default}
\alias{pvar.data.frame}
%\alias{pvar.matrix}
\alias{print.pvar}
\title{Check Whether Variables of a Panel Have Individual and Time Variations}
\description{This function checks for each variable of a panel if it has an
  individual and a time variation.}
\usage{
pvar(x, ...)
\method{pvar}{data.frame}(x, indexes = NULL, ...)
%\method{pvar}{matrix}(x,id,time, ...)
%\method{pvar}{default}(x,id,time, ...)
}
\arguments{
  \item{x}{a \code{data.frame},}
  \item{indexes}{see \code{\link{plm.data}},}
  \item{\dots}{further arguments.}
}

\details{

  \code{pvar} is called by \code{\link{plm}}, except if
  \code{pvar=FALSE}. It is used to omit time--invariant variables
  from the estimation. It is therefore
  useful for whithin models, but also for random effect models
  estimated by the Swamy and Arora method (which uses the results of the
  within model). It can be time consuming for ``big'' panels.

}


\value{ an object of class \code{pvar} containing the following elements
  : \item{id.var}{ a logical vector with \code{TRUE} values if the
  variable has individual variation, \code{FALSE} otherwise,}
  \item{time.var}{ a logical vector with \code{TRUE} values if the
    variable has time variation, \code{FALSE} otherwise,} }

\author{Yves Croissant}


\seealso{\code{\link{plm}}}
\examples{


#There are 595 individuals

data("Wages",package="Ecdat")
pvar(Wages,595)

# Gasoline contains two variables which are individual and time indexes
# and are the first two variables
data("Gasoline",package="Ecdat")
pvar(Gasoline)

# Hedonic is an unbalanced panel, townid is the individual index
data("Hedonic",package="Ecdat")
pvar(Hedonic,"townid")

}
\keyword{attribute}
