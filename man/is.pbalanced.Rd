\name{is.pbalanced}
\alias{is.pbalanced}
\alias{is.pbalanced.data.frame}
\alias{is.pbalanced.pdata.frame}
\alias{is.pbalanced.pseries}
\alias{is.pbalanced.panelmodel}
\alias{is.pbalanced.pgmm}
\alias{is.pbalanced.default}

\title{Check if data are balanced}

\description{This function checks if the data are balanced, i.e. each individual has the same time periods}

\usage{
\method{is.pbalanced}{pdata.frame}(x, \dots)
\method{is.pbalanced}{data.frame}(x, index = NULL, \dots)
\method{is.pbalanced}{pseries}(x, \dots)
\method{is.pbalanced}{panelmodel}(x, \dots)
\method{is.pbalanced}{pgmm}(x, \dots)
}

\arguments{
 \item{x}{an object of class \code{pdata.frame}, \code{data.frame}, \code{pseries}, \code{panelmodel}, \code{pgmm};}
 \item{index}{only relevant for \code{data.frame} interface; if \code{NULL}, the first two columns of the data.frame are 
              assumed to be the index variables; if not \code{NULL}, both dimensions ('individual', 'time') need to be 
              specified by \code{index} as character of length 2 for data frames, for further details see \code{\link{pdata.frame}},}
 \item{\dots}{further arguments.}
}

\value{
A logical indicating whether the data associated with object \code{x} are balanced (\code{TRUE}) or not (\code{FALSE}).
}

\details{
 Balanced data are data for which each individual has the same time periods.
 The returned values of the \code{is.pbalanced(object)} methods are identical to \code{pdim(object)$balanced}.
 \code{is.pbalanced} is provided as a short cut and is faster than \code{pdim(object)$balanced} because it
 avoids those computations performed by \code{pdim} which are unnecessary to determine the balancedness
 of the data.
}

\seealso{
 \code{\link{punbalancedness}} for two measures of unbalancedness,
 \code{\link{make.pbalanced}} to make data balanced; 
 \code{\link{is.pconsecutive}} to check if data are consecutive; \code{\link{make.pconsecutive}} to make data consecutive 
 (and, optionally, also balanced).\cr
 \code{\link{pdim}} to check the dimensions of a 'pdata.frame' (and other objects), 
 \code{\link{pvar}} to check for individual and time variation of a 'pdata.frame' (and other objects),
 \code{\link{pseries}}, \code{\link{data.frame}}, \code{\link{pdata.frame}}.
}

\examples{
# take balanced data and make it unbalanced
# by deletion of 2nd row (2nd time period for first individual)
data("Grunfeld", package = "plm")
Grunfeld_missing_period <- Grunfeld[-2, ]
is.pbalanced(Grunfeld_missing_period)     # check if balanced: FALSE
pdim(Grunfeld_missing_period)$balanced    # same

# pdata.frame interface
pGrunfeld_missing_period <- pdata.frame(Grunfeld_missing_period)
is.pbalanced(Grunfeld_missing_period)

# pseries interface
is.pbalanced(pGrunfeld_missing_period$inv)
}

\keyword{attribute}
