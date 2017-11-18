\name{pdata.frame}
\alias{pdata.frame}
\alias{print.pdata.frame}
\alias{[.pdata.frame}
\alias{[[.pdata.frame}
\alias{$.pdata.frame}
\alias{as.data.frame.pdata.frame}
\alias{as.list.pdata.frame}

\title{data.frame for panel data}

\description{
  An object of class 'pdata.frame' is a data.frame with an index attribute that
  describes its individual and time dimensions.
}

\usage{
pdata.frame(x, index = NULL, drop.index = FALSE, row.names = TRUE,
               stringsAsFactors = default.stringsAsFactors(),
               replace.non.finite = FALSE,
               drop.NA.series = FALSE, drop.const.series = FALSE,
               drop.unused.levels = FALSE)
\method{[}{pdata.frame}(x, i, j, drop)
\method{[[}{pdata.frame}(x, y)
\method{$}{pdata.frame}(x, y)
\method{print}{pdata.frame}(x, \dots)
\method{as.data.frame}{pdata.frame}(x, row.names = NULL, optional = FALSE, keep.attributes = TRUE, \dots)
\method{as.list}{pdata.frame}(x, keep.attributes = FALSE, \dots)
}
\arguments{
  \item{x}{ a \code{data.frame} for the \code{pdata.frame} function and a
    \code{pdata.frame} for the methods,}
  \item{i}{see \code{\link{Extract}},}
  \item{j}{see \code{\link{Extract}},}
  \item{y}{one of the columns of the \code{data.frame},}
  \item{index}{this argument indicates the individual and time
    indexes. See \bold{Details},}
  \item{drop}{see \code{\link{Extract}},}
  \item{drop.index}{logical, indicates whether the indexes are to be excluded from the resulting pdata.frame,}
  \item{optional}{see \code{\link{as.data.frame}},}
  \item{row.names}{\code{NULL} or logical, indicates whether ``fancy'' row names (a combination of individual
  index and time index) are to be added to the returned (p)data.frame (\code{NULL} and \code{FALSE} have the same meaning),}
  \item{stringsAsFactors}{logical, indicating whether character vectors are to be converted to factors,}
  \item{replace.non.finite}{logical, indicating whether values for which \code{is.finite()} yields \code{TRUE} are to be
  replaced by \code{NA} values, except for character variables (defaults to \code{FALSE}),}
  \item{drop.NA.series}{logical, indicating whether all-NA columns are to be removed from the pdata.frame (defaults to \code{FALSE}),}
  \item{drop.const.series}{logical, indicating whether constant columns are to be removed from the pdata.frame (defaults to \code{FALSE}),}
  \item{drop.unused.levels}{logical, indicating whether unused levels of factors are to be dropped (defaults to \code{FALSE})
    (unused levels are always dropped from variables serving to construct the index variables),}
  \item{keep.attributes}{logical, only for as.list and as.data.frame methods, indicating weather the elements of the returned list/columns of the data.frame should have the pdata.frame's attributes added (default: FALSE for as.list, TRUE for as.data.frame),}
  \item{\dots}{further arguments}
}

\value{
  a \code{pdata.frame} object: this is a \code{data.frame} with an
  \code{index} attribute which is a \code{data.frame} with two
  variables, the individual and the time indexes, both being factors.
  The resulting pdata.frame is sorted by the individual index, then
  by the time index.
}

\details{
  The \code{index} argument indicates the dimensions of the panel. It can be:
  \itemize{
    \item a vector of two character strings which contains the names of
    the individual and of the time indexes,
    \item a character string which is the name of the individual index
    variable. In this case, the time index is created automatically and
    a new variable called ``time'' is added, assuming consecutive and ascending
    time periods in the order of the original data,
    \item an integer, the number of individuals. In this case, the data
    need to be a balanced panel and be organized as a stacked time series
    (successive blocks of individuals, each block being a time series for
    the respective individual) assuming consecutive and ascending
    time periods in the order of the original data. Two new variables are added:
    ``id'' and ``time'' which contain the individual and the time indexes.
    }
  
  The \code{"[["} and \code{"$"} extract
  a series from the \code{pdata.frame}. The \code{"index"} attribute is
  then added to the series and a class attribute \code{"pseries"} is
  added. The \code{"["} method behaves as for \code{data.frame}, except
  that the extraction is also applied to the \code{index} attribute.
  A safe way to extract the index attribute is to use the function
  \code{\link{index}} for 'pdata.frames' (and other objects).
  
  \code{as.data.frame} removes the index from the \code{pdata.frame} and
  adds it to each column.
  
  \code{as.list} behaves by default identical to \code{\link[base]{as.list.data.frame}}
  which means it drops the attributes specific to a pdata.frame; if a list of pseries
  is wanted, the attribute \code{keep.attributes} can to be set to \code{TRUE}. This also
  makes \code{lapply} work as expected on a pdata.frame (see also \bold{Examples}).
}

\seealso{
 \code{\link{index}} to extract the index variables from a 'pdata.frame' (and other objects),
 \code{\link{pdim}} to check the dimensions of a 'pdata.frame' (and other objects), 
 \code{\link{pvar}} to check for each variable if it varies cross-sectionally and over time. 
 To check if the time periods are consecutive per individual, see \code{\link[plm]{is.pconsecutive}}.
}

\examples{
# Gasoline contains two variables which are individual and time indexes
data("Gasoline", package = "plm")
Gas <- pdata.frame(Gasoline, index = c("country", "year"), drop.index = TRUE)

# Hedonic is an unbalanced panel, townid is the individual index
data("Hedonic", package = "plm")
Hed <- pdata.frame(Hedonic, index = "townid", row.names = FALSE)

# In case of balanced panel, it is sufficient to give number of individuals 
# data set 'Wages' is organized as a stacked time series
data("Wages", package = "plm")
Wag <- pdata.frame(Wages, 595)

# lapply on a pdata.frame by making it a list of pseries first
lapply(as.list(Wag[ , c("ed", "lwage")], keep.attributes = TRUE), lag)

}

\author{Yves Croissant}

\keyword{classes}
