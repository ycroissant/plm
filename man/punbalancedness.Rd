\name{punbalancedness}
\alias{punbalancedness}
\alias{punbalancedness.data.frame}
\alias{punbalancedness.pdata.frame}
\alias{punbalancedness.panelmodel}

\title{Measures for Unbalancedness of Panel Data}

\description{This function reports unbalancedness measures for panel data
as defined in Ahrens/Pincus (1981) and Baltagi/Song/Jung (2001).}

\usage{
punbalancedness(x, \dots)
\method{punbalancedness}{panelmodel}(x, \dots)
\method{punbalancedness}{data.frame}(x, index = NULL, \dots)
\method{punbalancedness}{pdata.frame}(x, \dots)
}

\arguments{
 \item{x}{a \code{panelmodel}, a \code{data.frame}, or a \code{pdata.frame} object,}
 \item{index}{only relevant for \code{data.frame} interface, for details see \code{\link{pdata.frame}},}
 \item{\dots}{further arguments.}
}


\details{
 \code{punbalancedness} returns measures for the unbalancedness of a
 panel data set.

\itemize{
 \item{For two-dimensional data:\cr
 The two measures of Ahrens/Pincus (1981) are calculated,
 called "gamma" (\eqn{\gamma}) and "nu" (\eqn{\nu}).
 
 If the panel data are balanced, both measures equal 1. The more "unbalanced" the
 panel data, the lower the measures (but > 0). The upper and lower bounds
 as given in Ahrens/Pincus (1981) are:\cr
 \eqn{0 < \gamma, \nu \le 1}, and for \eqn{\nu} more precisely \eqn{\frac{1}{n} < \nu \le 1}{%
 1/n < \nu \le 1}, with \eqn{n} being the number of individuals (as in \code{pdim(x)$nT$n}).
 }
 \item{
 For nested panel data (meaning including a grouping variable):\cr
 The extension of the above measures by Baltagi/Song/Jung (2001), p. 368, are calculated:\cr
 \itemize{
   \item{c1: measure of subgroup (individual) unbalancedness,}
   \item{c2: measure of time unbalancedness,}
   \item{c3: measure of group unbalancedness due to each group size.}
 }
 Values are 1 if the data are balanced and become smaller as the data become more unbalanced.
 }
 } % itemize
 
 An application of the measure "gamma" is found in e. g.
 Baltagi/Song/Jung (2002), pp. 488-491, and Baltagi/Chang (1994), pp. 78--87,
 where it is used to measure the unbalancedness of various unbalanced data
 sets used for Monte Carlo simulation studies. Measures c1, c2, c3 are used
 for similar purposes in Baltagi/Song/Jung (2001).


 In the two-dimensional case, \code{punbalancedness} uses output of \code{\link{pdim}} 
 to calculate the two unbalancedness measures, so inputs to \code{punbalancedness} can 
 be whatever \code{pdim} works on. \code{pdim} returns detailed information about the 
 number of individuals and time observations (see \code{\link{pdim}}).
}


\value{
 A named numeric containing either two or three entries, depending on the 
 panel structure inputted:
 
 \item{*}{For the two-dimensional panel structure, the entries are called \code{gamma} and \code{nu},}
 \item{*}{For a nested panel structure, the entries are called \code{c1}, \code{c2}, \code{c3}.}
}

\note{Calling \code{punbalancedness} on an estimated \code{panelmodel} object and
  on the corresponding \code{(p)data.frame} used for this estimation does not
  necessarily yield the same result (true also for \code{pdim}). When called on an estimated
  \code{panelmodel}, the number of observations (individual, time) actually used for
  model estimation are taken into account. When called on a \code{(p)data.frame}, the
  rows in the \code{(p)data.frame} are considered, disregarding any NA values in the
  dependent or independent variable(s) which would be dropped during model estimation.
}

\author{Kevin Tappe}

\references{
Ahrens, H.; Pincus, R. (1981), \dQuote{On two measures of unbalancedness in a one-way model
and their relation to efficiency}, \emph{Biometrical Journal}, \bold{23}(3), pp. 227--235.

Baltagi, Badi H.; Chang, Young-Jae (1994), \dQuote{Incomplete panels: A comparative study of
alternative estimators for the unbalanced one-way error component regression model},
\emph{Journal of Econometrics}, \bold{62}(2), pp. 67--89.

Baltagi, Badi H.; Song, Seuck Heun; Jung, Byoung Cheol (2001), \dQuote{The unbalanced nested error
component regression model}, \emph{Journal of Econometrics}, \bold{101}(2), pp. 357--381.

Baltagi, Badi H.; Song, Seuck H.; Jung, Byoung C. (2002), \dQuote{A comparative study of alternative
estimators for the unbalanced two-way error component regression model}, \emph{Econometrics Journal},
\bold{5}(2), pp. 480--493.
}

\seealso{\code{\link{nobs}}, \code{\link{pdim}}, \code{\link{pdata.frame}}}

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

\keyword{attribute}
