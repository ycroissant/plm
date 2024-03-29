% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tool_misc.R
\name{punbalancedness}
\alias{punbalancedness}
\alias{punbalancedness.pdata.frame}
\alias{punbalancedness.data.frame}
\alias{punbalancedness.panelmodel}
\title{Measures for Unbalancedness of Panel Data}
\usage{
punbalancedness(x, ...)

\method{punbalancedness}{pdata.frame}(x, ...)

\method{punbalancedness}{data.frame}(x, index = NULL, ...)

\method{punbalancedness}{panelmodel}(x, ...)
}
\arguments{
\item{x}{a \code{panelmodel}, a \code{data.frame}, or a \code{pdata.frame} object,}

\item{\dots}{further arguments.}

\item{index}{only relevant for \code{data.frame} interface, for details
see \code{\link[=pdata.frame]{pdata.frame()}},}
}
\value{
A named numeric containing either two or three entries,
depending on the panel structure inputted:
\itemize{
\item For the two-dimensional panel structure, the entries are called
\code{gamma} and \code{nu},
\item For a nested panel structure, the entries are called \code{c1}, \code{c2},
\code{c3}.
}
}
\description{
This function reports unbalancedness measures for panel data as
defined in \insertCite{AHRE:PINC:81;textual}{plm} and
\insertCite{BALT:SONG:JUNG:01;textual}{plm}.
}
\details{
\code{punbalancedness} returns measures for the unbalancedness of a
panel data set.
\itemize{
\item For two-dimensional data:\cr The two measures of
\insertCite{AHRE:PINC:81;textual}{plm} are calculated, called
"gamma" (\eqn{\gamma}) and "nu" (\eqn{\nu}).
}

If the panel data are balanced, both measures equal 1. The more
"unbalanced" the panel data, the lower the measures (but > 0). The
upper and lower bounds as given in \insertCite{AHRE:PINC:81;textual}{plm}
are:\cr
\eqn{0 < \gamma, \nu \le 1}, and for \eqn{\nu} more precisely
\eqn{\frac{1}{n} < \nu \le 1}{1/n < \nu \le 1}, with \eqn{n} being
the number of individuals (as in \code{pdim(x)$nT$n}).
\itemize{
\item For nested panel data (meaning including a grouping variable):\cr
The extension of the above measures by
\insertCite{BALT:SONG:JUNG:01;textual}{plm}, p. 368, are
calculated:\cr
\itemize{
\item c1: measure of subgroup (individual) unbalancedness,
\item c2: measure of time unbalancedness,
\item c3: measure of group unbalancedness due to each group size.
}
}

Values are 1 if the data are balanced and become smaller as the
data become more unbalanced.

An application of the measure "gamma" is found in e. g.
\insertCite{BALT:SONG:JUNG:01;textual}{plm}, pp. 488-491, and
\insertCite{BALT:CHAN:94;textual}{plm}, pp. 78--87, where it is
used to measure the unbalancedness of various unbalanced data sets
used for Monte Carlo simulation studies. Measures c1, c2, c3 are
used for similar purposes in
\insertCite{BALT:SONG:JUNG:01;textual}{plm}.

In the two-dimensional case, \code{punbalancedness} uses output of
\code{\link[=pdim]{pdim()}} to calculate the two unbalancedness measures, so inputs to
\code{punbalancedness} can be whatever \code{pdim} works on. \code{pdim} returns
detailed information about the number of individuals and time
observations (see \code{\link[=pdim]{pdim()}}).
}
\note{
Calling \code{punbalancedness} on an estimated \code{panelmodel} object
and on the corresponding \verb{(p)data.frame} used for this
estimation does not necessarily yield the same result (true
also for \code{pdim}). When called on an estimated \code{panelmodel}, the
number of observations (individual, time) actually used for
model estimation are taken into account. When called on a
\verb{(p)data.frame}, the rows in the \verb{(p)data.frame} are
considered, disregarding any \code{NA} values in the dependent or
independent variable(s) which would be dropped during model
estimation.
}
\examples{

# Grunfeld is a balanced panel, Hedonic is an unbalanced panel
data(list=c("Grunfeld", "Hedonic"), package="plm")

# Grunfeld has individual and time index in first two columns
punbalancedness(Grunfeld) # c(1,1) indicates balanced panel
pdim(Grunfeld)$balanced   # TRUE

# Hedonic has individual index in column "townid" (in last column)
punbalancedness(Hedonic, index="townid") # c(0.472, 0.519)
pdim(Hedonic, index="townid")$balanced   # FALSE

# punbalancedness on estimated models
plm_mod_pool <- plm(inv ~ value + capital, data = Grunfeld)
punbalancedness(plm_mod_pool)

plm_mod_fe <- plm(inv ~ value + capital, data = Grunfeld[1:99, ], model = "within")
punbalancedness(plm_mod_fe)

# replicate results for panel data design no. 1 in Ahrens/Pincus (1981), p. 234
ind_d1  <- c(1,1,1,2,2,2,3,3,3,3,3,4,4,4,4,4,4,4,5,5,5,5,5,5,5)
time_d1 <- c(1,2,3,1,2,3,1,2,3,4,5,1,2,3,4,5,6,7,1,2,3,4,5,6,7)
df_d1 <- data.frame(individual = ind_d1, time = time_d1)
punbalancedness(df_d1) # c(0.868, 0.887)

# example for a nested panel structure with a third index variable
# specifying a group (states are grouped by region) and without grouping
data("Produc", package = "plm")
punbalancedness(Produc, index = c("state", "year", "region"))
punbalancedness(Produc, index = c("state", "year")) 

}
\references{
\insertRef{AHRE:PINC:81}{plm}

\insertRef{BALT:CHAN:94}{plm}

\insertRef{BALT:SONG:JUNG:01}{plm}

\insertRef{BALT:SONG:JUNG:02}{plm}
}
\seealso{
\code{\link[=nobs]{nobs()}}, \code{\link[=pdim]{pdim()}}, \code{\link[=pdata.frame]{pdata.frame()}}
}
\author{
Kevin Tappe
}
\keyword{attribute}
