\name{index.plm}
\alias{index}
\alias{index.pdata.frame}
\alias{index.pindex}
\alias{index.pseries}
\alias{index.panelmodel}
\title{Extract the indexes of panel data}

\description{
  This function extracts the information about the structure of the
  individual and time dimensions of panel data.
}

\usage{
\method{index}{pindex}(x, which = NULL, ...)
\method{index}{pdata.frame}(x, which = NULL, ...)
\method{index}{pseries}(x, which = NULL, ...)
\method{index}{panelmodel}(x, which = NULL, ...)
}

\arguments{
  \item{x}{an object of class \code{"pindex"}, \code{"pdata.frame"},
  \code{"pseries"} or \code{"panelmodel"},}
  \item{which}{the index(es) to be extracted (see details),}
  \item{\dots}{further arguments.}
}

\value{
  a vector or a \code{"data.frame"} containing either one or both
  indexes.
}

\details{
  panel data are stored in a \code{"pdata.frame"} which has an
  \code{"index"} attribute. Fitted models in \code{"plm"} has a
  \code{"model"} element which is also a \code{"pdata.frame"} and
  therefore also have an \code{"index"} attribute. Finally, each series
  in a \code{"pdata.frame"} is of class \code{"pseries"}, which also
  have this \code{"index"} attribute. \code{"index"} methods are
  available for all these objects. The argument \code{"which"} indicates
  which index should be extracted. If \code{NULL}, both indexes are
  extracted, but \code{"which"} can also be a vector of lenght 1 or 2
  containing either characters (the names of the individual and of the
  time periods or \code{"id"} and \code{"time"}) or integers (1 for the
  individual index and 2 for the time index.)
}
  
\seealso{
  \code{\link{plm}}
}

\author{Yves Croissant}

\examples{
data("Grunfeld", package = "plm")
Gr <- pdata.frame(Grunfeld, index = c("firm", "year"))
m <- plm(inv ~ value + capital, data = Gr)
index(Gr, "firm")
index(Gr, "time")
index(Gr$inv, c(2, 1))
index(m, "id")
}

\keyword{attribute}
