\name{pdata.frame}
\alias{pdata.frame}
\alias{print.pdata.frame}
\alias{[.pdata.frame}
\alias{[[.pdata.frame}
\alias{$.pdata.frame}
\alias{as.data.frame.pdata.frame}

\title{data.frame for panel data}

\description{
  An object of this class is a data.frame with an attribute that
  describes its time and individual dimensions.
}

\usage{
pdata.frame(x, index = NULL, drop.index = FALSE, row.names = TRUE)
\method{[}{pdata.frame}(x, i, j, drop)
\method{[[}{pdata.frame}(x, y)
\method{$}{pdata.frame}(x, y)
\method{print}{pdata.frame}(x, ...)
\method{as.data.frame}{pdata.frame}(x, row.names = NULL, optional = FALSE, ...)
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
  \item{row.names}{\code{NULL} or logical, indicates whether ``fancy'' row names (a combination of individual index and time index) are to be added to the returned (p)data.frame (\code{NULL} and \code{FALSE} have the same meaning),}
  \item{...}{further arguments}
}

\value{
  a \code{pdata.frame} object: this is a \code{data.frame} with an
  \code{index} attribute which is a \code{data.frame} with two
  variables, the individual and the time indexes.
}

\details{
  The \code{index} argument indicates the dimensions of the panel. It can be:
  \itemize{
    \item a vector of two character strings which contains the names of
    the individual and of the time indexes,
    \item a character string which is the name of the individual index
    variable. In this case, the time index is created automatically and
    a new variable called ``time'' is added, assuming consecutive time periods,
    \item an integer, the number of individuals. In this case, the data
    need to be a balanced panel and be organized as a stacked time series
    (sucessive blocks of individuals, each block being a time series for
    the respective individual,). Two new variables are added:
    ``time'' and ``id'' which contain the individual and the time indexes,
    the time periods are assumed to be consecutive.
}

The \code{"[["} and \code{"$"} extract
a series from the \code{pdata.frame}. The \code{"index"} attribute is
then added to the series and a class attribute \code{"pseries"} is
added. The \code{"["} method behaves as for \code{data.frame}, except
that the extraction is also applied to the \code{index} attribute.
\code{as.data.frame} removes the index from the \code{pdata.frame} and
adds it to every series.
}

% \seealso{
% }

\examples{
# Gasoline contains two variables which are individual and time indexes
data("Gasoline", package = "plm")
Gas <- pdata.frame(Gasoline, index = c("country", "year"), drop = TRUE)

# Hedonic is an unbalanced panel, townid is the individual index
data("Hedonic", package = "plm")
Hed <- pdata.frame(Hedonic, index = "townid", row.names = FALSE)

# In case of balanced panel, it is sufficient to give number of individuals 
# data set 'Wages' is organized as a stacked time series
data("Wages", package = "plm")
Wag <- pdata.frame(Wages, 595)
}

\author{Yves Croissant}

\keyword{classes}
