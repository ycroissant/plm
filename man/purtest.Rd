\name{purtest}
\alias{purtest}
\alias{print.purtest}
\alias{summary.purtest}
\alias{print.summary.purtest}
\title{Unit root tests for panel data}
\description{
  \code{purtest} implements different testing procedures that have been
  proposed to test the hypothesis of unit root with panel data
}
\usage{
purtest(object, test= c("levinlin", "ips", "madwu"),
        type = c("none", "intercept", "trend"),
        lags = c("SIC", "AIC", "Hall"), pmax = 10,
        q = NULL, dfcor = FALSE, fixedT = TRUE, ...)
\method{print}{purtest}(x, ...)
\method{summary}{purtest}(object, ...)
\method{print}{summary.purtest}(x, ...)
}
\arguments{
  \item{object, x}{a data.frame containing the time series, a
    \code{'purtest'} object for the print and summary methods}
  \item{test}{the test to be computed : one of \code{levinlin} for
    Levin, Lin and Chu (2002), \code{ips} for Im, Pesaran and Shim
    (2003) and \code{madwu} for Maddala and Wu (1999),}
  \item{type}{the exogenous variables to introduce in the augmented
    Dickey Fuller regressions : this can be nothing (\code{'none'}),
    individual intercepts (\code{'intercept'}) or individual intercepts
    and trend (\code{'trend'}),}
  \item{lags}{the number of lags to be used for the augmented Dickey
    Fuller regressions : whether an integer (the number of lags for all
    time-series), a vector of integers (one for each time-series), or a
    character string for an automatic computation of the number of lags,
    whether based on the AIC (\code{'AIC'}), SIC (\code{'SIC'}) or using
    Hall's method (\code{'Hall'}),}
  \item{pmax}{maximum number of lags,}
  \item{q}{the bandwidth for the estimation of the long-run variance,}
  \item{dfcor}{should the standard deviation of the regressions be
    computed using a degree of freedom correction ?}
  \item{fixedT}{should the different adf regressions be computed using
    the same number of observations,}
  \item{\dots}{further arguments.}
}
\value{
  An object of class \code{'purtest'} : a list containing
  \code{'statistic'} which is a \code{'htest'} object, \code{'call'},
  \code{'args'}, \code{'idres'} which contains results from the
  individual regressions and \code{'adjval'} which contains the
  simulated means and variances needed to compute the statistics.
}
\details{
  All these tests are based on the estimation of augmented Dikey-Fuller
  regressions for each time-series. A statistic is then computed using
  the t-statistic associated to the lag value of the variable.
}
\references{
  
Hadri K. (2000). ``Testing for Unit Roots in Heterogeneous Panel Data'',
Econometrics Journal, 3, pp. 148-161.

Im K.S., Pesaran M.H. et Shin Y. (2003). ``Testing for Unit Roots in
Heterogeneous Panels'', Journal of Econometrics, 115, 1, pp. 53-74.

Levin A., Lin C.F. et Chu C.S.J. (2002). ``Unit Root Test in Panel Data:
Asymptotic and Finite Sample Properties'', Journal of Econometrics, 108,
pp. 1-24.

Maddala G.S. et Wu S. (1999). ``A Comparative Study of Unit Root Tests
with Panel Data and a New Simple Test'', Oxford Bulletin of Economics
and Statistics, special issue, pp. 631-652.

}
\seealso{
}
\examples{

data("Grunfeld", package = "plm")
y <- data.frame(split(Grunfeld$inv, Grunfeld$firm))
purtest(y, pmax=4, type="intercept", test = "madwu")
}
\author{Yves Croissant}
\keyword{htest}
