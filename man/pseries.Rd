\name{pseries}
\alias{pseries}
\alias{Between}
\alias{between}
\alias{Within}
\alias{Between.pseries}
\alias{between.pseries}
\alias{Within.pseries}
\alias{summary.pseries}
\alias{print.summary.pseries}
\alias{as.matrix.pseries}

% lag, lead, diff is now in separate file lag_lead_diff.Rd

\title{panel series}

\description{
A class for panel series for which several useful computations and data transformations are available.
}

\usage{
between(x, \dots)
Between(x, \dots)
Within(x, \dots)
\method{between}{pseries}(x, effect = c("individual", "time", "group"), \dots)
\method{Between}{pseries}(x, effect = c("individual", "time", "group"), \dots)
\method{Within}{pseries}(x, effect = c("individual", "time", "group"), \dots)
\method{summary}{pseries}(object, \dots)
\method{print}{summary.pseries}(x, \dots)
\method{as.matrix}{pseries}(x, idbyrow = TRUE, \dots)
}

\arguments{
 \item{x, object}{a \code{pseries} or a \code{summary.pseries} object,}
 \item{effect}{character string indicating the \code{"individual"} or \code{"time"} effect,}
 \item{idbyrow}{if \code{TRUE} in the \code{as.matrix} method, the lines of the matrix are the individuals,}
 \item{\dots}{further arguments, e. g. \code{na.rm = TRUE} for transformation functions like 
 \code{beetween}, see \bold{Details} and \bold{Examples}.}
}

\value{
All these functions return an object of class \code{pseries}, except:\cr
\code{between}, which returns a numeric vector, \code{as.matrix}, which returns a matrix.
}

\details{
The functions \code{between}, \code{Between}, and \code{Within} perform specific
data transformations, i. e. the between and within transformation.

\code{between} returns a vector containing the individual means (over time) with the length of the
vector equal to the number of individuals (if \code{effect = "individual"} (default); if \code{effect = "time"},
it returns the time means (over individuals)). \code{Between} duplicates the values and returns a vector which 
length is the number of total observations. \code{Within} returns a vector containing the 
values in deviation from the individual means (if \code{effect = "individual"}, from time means if \code{effect = "time"}),
the so called demeaned data.

For \code{between}, \code{Between}, and \code{Within} in presence of NA values it can
be useful to supply \code{na.rm = TRUE} as an additional argument to
keep as many observations as possible in the resulting transformation, see
also \bold{Examples}.
}


% \references{
% }

\seealso{
For more functions on class 'pseries' see \code{\link[plm]{lag}}, \code{\link[plm]{lead}}, 
\code{\link[plm]{diff}} for lagging values, leading values (negative lags) and differencing.
}

\examples{
# First, create a pdata.frame
data("EmplUK", package = "plm")
Em <- pdata.frame(EmplUK)

# Then extract a series, which becomes additionally a pseries
z <- Em$output
class(z)

# obtain the matrix representation
as.matrix(z)

# compute the between and within transformations
between(z)
Within(z)

# Between replicates the values for each time observation
Between(z)

# between, Between, and Within transformations on other dimension
between(z, effect = "time")
Between(z, effect = "time")
Within(z, effect = "time")

# NA treatment for between, Between, and Within
z2 <- z
z2[length(z2)] <- NA # set last value to NA
between(z2, na.rm = TRUE) # non-NA value for last individual
Between(z2, na.rm = TRUE) # only the NA observation is lost
Within(z2, na.rm = TRUE)  # only the NA observation is lost

sum(is.na(Between(z2))) # 9 observations lost due to one NA value
sum(is.na(Between(z2, na.rm = TRUE))) # only the NA observation is lost
sum(is.na(Within(z2))) # 9 observations lost due to one NA value
sum(is.na(Within(z2, na.rm = TRUE))) # only the NA observation is lost
}

\author{Yves Croissant}

\keyword{classes}