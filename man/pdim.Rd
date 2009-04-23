\name{pdim}
\alias{pdim}
\alias{pdim.data.frame}
\alias{pdim.plm}
\alias{pdim.default}
\alias{print.pdim}

\title{Check for the Dimensions of the Panel}
\description{This function checks the number of individuals and time
  observations in the panel and whether it is balanced or not.}
\usage{
pdim(x,...)
\method{pdim}{data.frame}(x, indexes = NULL, ...)
%\method{pdim}{default}(x,y,...)

}
\arguments{
  \item{x}{a \code{"data.frame"} or a \code{"plm"} object,}
%  or a vector for the default method}
%  \item{y}{a vector,}
  \item{indexes}{see \code{plm.data},}
  \item{\dots}{further arguments.}
}

\value{
  an object of class \code{pdim} containing the following elements :
  \item{nT}{ a list containing \code{n}, the number of individuals,
    \code{T}, the number of time observations, \code{N} the total number
    of observations,}
  \item{TinT}{ a list containing two vectors : \code{Ti} gives the
    number of observations for each individuals and \code{nt} gives
    the number of individuals observed for each period,}
  \item{balanced}{ a logical value : \code{TRUE} for a balanced panel,
    \code{FALSE} for an unbalanced panel,}
  \item{panel.names}{a list of character vectors : \code{id.names}
    contains the names of each individual and \code{time.names}
    contains the names of each period.}
    }
\details{
\code{pdim} is called by the estimation functions.
}

\author{Yves Croissant}

\seealso{\code{\link{plm.data}}}

\examples{

#There are 595 individuals

data("Wages", package="Ecdat")
pdim(Wages, 595)

# Gasoline contains two variables which are individual and time indexes
# and are the first two variables
data("Gasoline", package="Ecdat")
pdim(Gasoline)

# Hedonic is an unbalanced panel, townid is the individual index
data("Hedonic", package="Ecdat")
pdim(Hedonic, "townid")


}
\keyword{attribute}
