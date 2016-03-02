\name{is.pconsecutive}
\alias{is.pconsecutive}
\alias{is.pconsecutive.data.frame}
\alias{is.pconsecutive.pdata.frame}
\alias{is.pconsecutive.panelmodel}
\alias{is.pconsecutive.pseries}

\title{Check if time periods are consecutive}

\description{This function checks for each individual if its associated time periods 
are consecutive (no "gaps" in time)}

\usage{
is.pconsecutive(x, ...)
\method{is.pconsecutive}{pdata.frame}(x, ...)
\method{is.pconsecutive}{panelmodel}(x, ...)
\method{is.pconsecutive}{pseries}(x, ...)
\method{is.pconsecutive}{data.frame}(x, index = NULL, ...)
}

\arguments{
 \item{x}{object of class \code{pdata.frame}, \code{data.frame}, \code{pseries}, or a an estimated \code{panelmodel},}
 \item{index}{only relevant for \code{data.frame} interface; if not \code{NULL}, both dimensions ('individual', 'time')
              need to be specified by \code{index} for \code{is.pconsecutive} on data frames,
              for further details see \code{\link{pdata.frame}},}
 \item{\dots}{further arguments.}
}

\value{ 
A named \code{logical} vector (names are those of the individuals).
i-th element of the vector is \code{TRUE} if the i-th individual has consecutive time periods,
\code{FALSE} if it has non-consecutive time periods.
}

\details{
 (p)data.frame, pseries and estimated panelmodel objects can be tested if their time periods are consecutive per individual.
 Note: Only the presence of the time period itself in the object is tested, not if there are any other variables.
}

\seealso{
 \code{\link{pdim}} to check the dimensions of a 'pdata.frame' (and other objects), 
 \code{\link{pvar}} to check for individual and time variation of a 'pdata.frame' (and other objects).\cr
 
 \code{\link{pseries}}, \code{\link{data.frame}}, \code{\link{pdata.frame}}, for class 'panelmodel' see \code{\link{plm}} and \code{\link{pgmm}}.
}

\author{Kevin Tappe}

\examples{
data("Grunfeld", package = "plm")
is.pconsecutive(Grunfeld)
is.pconsecutive(Grunfeld, index=c("firm", "year"))

# delete 2nd row (2nd time period for first individual)
# -> non consecutive 
Grunfeld_missing_period <- Grunfeld[-2, ]
is.pconsecutive(Grunfeld_missing_period)
all(is.pconsecutive(Grunfeld_missing_period)) # FALSE

# delete rows 1 and 2 (1st and 2nd time period for first individual)
# -> consecutive
Grunfeld_missing_period_other <- Grunfeld[-c(1,2), ]
is.pconsecutive(Grunfeld_missing_period_other) # all TRUE


# pdata.frame interface
pGrunfeld <- pdata.frame(Grunfeld)
pGrunfeld_missing_period <- pdata.frame(Grunfeld_missing_period)
is.pconsecutive(pGrunfeld) # all TRUE
is.pconsecutive(pGrunfeld_missing_period) # first FALSE, others TRUE


# panelmodel interface (first, estimate some models)
mod_pGrunfeld <- plm(inv ~ value + capital, data = Grunfeld)
mod_pGrunfeld_missing_period <- plm(inv ~ value + capital, data = Grunfeld_missing_period)

is.pconsecutive(mod_pGrunfeld)
is.pconsecutive(mod_pGrunfeld_missing_period)

nobs(mod_pGrunfeld) # 200
nobs(mod_pGrunfeld_missing_period) # 199


# pseries interface
pinv <- pGrunfeld$inv
pinv_missing_period <- pGrunfeld_missing_period$inv

is.pconsecutive(pinv)
is.pconsecutive(pinv_missing_period)
}

\keyword{attribute}
