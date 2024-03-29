% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make.pconsecutive_pbalanced.R
\name{make.pconsecutive}
\alias{make.pconsecutive}
\alias{make.pconsecutive.data.frame}
\alias{make.pconsecutive.pdata.frame}
\alias{make.pconsecutive.pseries}
\title{Make data consecutive (and, optionally, also balanced)}
\usage{
make.pconsecutive(x, ...)

\method{make.pconsecutive}{data.frame}(x, balanced = FALSE, index = NULL, ...)

\method{make.pconsecutive}{pdata.frame}(x, balanced = FALSE, ...)

\method{make.pconsecutive}{pseries}(x, balanced = FALSE, ...)
}
\arguments{
\item{x}{an object of class \code{pdata.frame}, \code{data.frame},
or \code{pseries},}

\item{\dots}{further arguments.}

\item{balanced}{logical, indicating whether the data should
\emph{additionally} be made balanced (default: FALSE),}

\item{index}{only relevant for \code{data.frame} interface; if
\code{NULL}, the first two columns of the data.frame are
assumed to be the index variables; if not \code{NULL}, both
dimensions ('individual', 'time') need to be specified by
\code{index} as character of length 2 for data frames, for
further details see \code{\link[=pdata.frame]{pdata.frame()}},}
}
\value{
An object of the same class as the input \code{x}, i.e., a
pdata.frame, data.frame or a pseries which is made
time--consecutive based on the index variables. The returned
data are sorted as a stacked time series.
}
\description{
This function makes the data consecutive for each individual (no "gaps" in
time dimension per individual) and, optionally, also balanced
}
\details{
(p)data.frame and pseries objects are made consecutive, meaning their time
periods are made consecutive per individual.  For consecutiveness, the time
dimension is interpreted to be numeric, and the data are extended to a
regularly spaced sequence with distance 1 between the time periods for each
individual (for each individual the time dimension become a sequence t, t+1,
t+2, \ldots{}, where t is an integer). Non--index variables are filled with
\code{NA} for the inserted elements (rows for (p)data.frames, vector
elements for pseries).

With argument \code{balanced = TRUE}, additionally to be made consecutive,
the data also can be made a balanced panel/pseries.  Note: This means
consecutive AND balanced; balancedness does not imply consecutiveness. In
the result, each individual will have the same time periods in their time
dimension by taking the min and max of the time index variable over all
individuals (w/o \code{NA} values) and inserting the missing time periods.
Looking at the number of rows of the resulting (pdata.frame) (elements for
pseries), this results in \verb{nrow(make.pconsecutive(<.>, balanced = FALSE))} <=
\verb{nrow(make.pconsecutive(<.>, balanced = TRUE))}. For making the data only
balanced, i.e., not demanding consecutiveness at the same time, use
\code{\link[=make.pbalanced]{make.pbalanced()}} (see \strong{Examples} for a comparison)).

Note: rows of (p)data.frames (elements for pseries) with \code{NA} values in
individual or time index are not examined but silently dropped before the
data are made consecutive. In this case, it is not clear which individual or
time period is meant by the missing value(s). Especially, this means: If
there are \code{NA} values in the first/last position of the original time
periods for an individual, which usually depicts the beginning and ending of
the time series for that individual, the beginning/end of the resulting time
series is taken to be the min and max (w/o \code{NA} values) of the original
time series for that individual, see also \strong{Examples}. Thus, one might
want to check if there are any \code{NA} values in the index variables
before applying \code{make.pconsecutive}, and especially check for \code{NA} values
in the first and last position for each individual in original data and, if
so, maybe set those to some meaningful begin/end value for the time series.
}
\examples{

# take data and make it non-consecutive
# by deletion of 2nd row (2nd time period for first individual)
data("Grunfeld", package = "plm")
nrow(Grunfeld)                             # 200 rows
Grunfeld_missing_period <- Grunfeld[-2, ]
is.pconsecutive(Grunfeld_missing_period)   # check for consecutiveness
make.pconsecutive(Grunfeld_missing_period) # make it consecutiveness


# argument balanced:
# First, make data non-consecutive and unbalanced
# by deletion of 2nd time period (year 1936) for all individuals
# and more time periods for first individual only
Grunfeld_unbalanced <- Grunfeld[Grunfeld$year != 1936, ]
Grunfeld_unbalanced <- Grunfeld_unbalanced[-c(1,4), ]
all(is.pconsecutive(Grunfeld_unbalanced)) # FALSE
pdim(Grunfeld_unbalanced)$balanced        # FALSE

g_consec_bal <- make.pconsecutive(Grunfeld_unbalanced, balanced = TRUE)
all(is.pconsecutive(g_consec_bal)) # TRUE
pdim(g_consec_bal)$balanced        # TRUE
nrow(g_consec_bal)                 # 200 rows
head(g_consec_bal)                 # 1st individual: years 1935, 1936, 1939 are NA

g_consec <- make.pconsecutive(Grunfeld_unbalanced) # default: balanced = FALSE
all(is.pconsecutive(g_consec)) # TRUE
pdim(g_consec)$balanced        # FALSE
nrow(g_consec)                 # 198 rows
head(g_consec)                 # 1st individual: years 1935, 1936 dropped, 1939 is NA 


# NA in 1st, 3rd time period (years 1935, 1937) for first individual
Grunfeld_NA <- Grunfeld
Grunfeld_NA[c(1, 3), "year"] <- NA
g_NA <- make.pconsecutive(Grunfeld_NA)
head(g_NA)        # 1936 is begin for 1st individual, 1937: NA for non-index vars
nrow(g_NA)        # 199, year 1935 from original data is dropped


# pdata.frame interface
pGrunfeld_missing_period <- pdata.frame(Grunfeld_missing_period)
make.pconsecutive(Grunfeld_missing_period)


# pseries interface
make.pconsecutive(pGrunfeld_missing_period$inv)


# comparison to make.pbalanced (makes the data only balanced, not consecutive)
g_bal <- make.pbalanced(Grunfeld_unbalanced)
all(is.pconsecutive(g_bal)) # FALSE
pdim(g_bal)$balanced        # TRUE
nrow(g_bal) # 190 rows

}
\seealso{
\code{\link[=is.pconsecutive]{is.pconsecutive()}} to check if data are
consecutive; \code{\link[=make.pbalanced]{make.pbalanced()}} to make data only
balanced (not consecutive).\cr \code{\link[=punbalancedness]{punbalancedness()}}
for two measures of unbalancedness, \code{\link[=pdim]{pdim()}} to check
the dimensions of a 'pdata.frame' (and other objects),
\code{\link[=pvar]{pvar()}} to check for individual and time variation
of a 'pdata.frame' (and other objects), \code{\link[=lag]{lag()}} for
lagged (and leading) values of a 'pseries' object.\cr
\code{\link[=pseries]{pseries()}}, \code{\link[=data.frame]{data.frame()}},
\code{\link[=pdata.frame]{pdata.frame()}}.
}
\author{
Kevin Tappe
}
\keyword{attribute}
