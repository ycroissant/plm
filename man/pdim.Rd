\name{pdim}
\alias{pdim}
\alias{pdim.data.frame}
\alias{pdim.pdata.frame}
\alias{pdim.panelmodel}
\alias{pdim.default}
\alias{print.pdim}

\title{Check for the Dimensions of the Panel}

\description{This function checks the number of individuals and time
  observations in the panel and whether it is balanced or not.}

\usage{
pdim(x,...)
\method{pdim}{data.frame}(x, index = NULL, ...)
\method{pdim}{panelmodel}(x, ...)
\method{pdim}{pdata.frame}(x, ...)
}

\arguments{
 \item{x}{a \code{data.frame}, a \code{pdata.frame} or a \code{panelmodel} object,}
 \item{index}{see \code{\link{pdata.frame}},}
 \item{\dots}{further arguments.}
}

\value{
 An object of class \code{pdim} containing the following elements:

 \item{nT}{a list containing \code{n}, the number of individuals, \code{T},
  the number of time observations, \code{N} the total number of observations,}
 \item{Tint}{a list containing two vectors : \code{Ti} gives the
  number of observations for each individuals and \code{nt} gives
  the number of individuals observed for each period,}
 \item{balanced}{a logical value : \code{TRUE} for a balanced panel,
  \code{FALSE} for an unbalanced panel,}
 \item{panel.names}{a list of character vectors : \code{id.names}
  contains the names of each individual and \code{time.names}
  contains the names of each period.}
}

\note{calling \code{pdim} on an estimated \code{panelmodel} object and on the corresponding \code{pdata.frame}
  used for this estimation does not necessarily yield the same result. When called on an estimated \code{panelmodel},
  the number of observations (individual, time) actually used for model estimation are taken into account.
  When called on a \code{pdata.frame}, the rows in the \code{pdata.frame} are considered, disregarding any
  NA values in the variables dependent or independent variable(s) which would be dropped during model estimation.
}

\details{
\code{pdim} is called by the estimation functions.
}

\author{Yves Croissant}

\seealso{\code{\link{nobs}}, \code{\link{pdata.frame}}}

\examples{

# There are 595 individuals

data("Wages", package = "plm")
pdim(Wages, 595)

# Gasoline contains two variables which are individual and time indexes
# and are the first two variables
data("Gasoline", package="plm")
pdim(Gasoline)

# Hedonic is an unbalanced panel, townid is the individual index
data("Hedonic", package = "plm")
pdim(Hedonic, "townid")

# An example of the panelmodel method
data("Produc", package = "plm")
z <- plm(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp,data=Produc,
         model="random", subset = gsp > 5000)
pdim(z)
}

\keyword{attribute}
