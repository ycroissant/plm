\name{pwtest}
\alias{pwtest}
\alias{pwtest.panelmodel}
\alias{pwtest.formula}

\title{Wooldridge's Test for Unobserved Effects in Panel Models}

\description{
Semi-parametric test for the presence of (individual or time) unobserved effects in panel models.
}

\usage{
pwtest(x,...)
\method{pwtest}{formula}(x, data, effect = c("individual", "time"), ...)
\method{pwtest}{panelmodel}(x, effect = c("individual", "time"), ...)
}

\arguments{
\item{x}{an object of class \code{"formula"}, or an estimated model of class
        \code{panelmodel},}
\item{effect}{the effect to be tested for, one of \code{"individual"} (default)
        or \code{"time"},}
\item{data}{a \code{data.frame},}
\item{\dots}{further arguments passed to \code{plm}.}
}

\details{
This semi-parametric test checks the null hypothesis of zero
correlation between errors of the same group. Therefore, it
has power both against individual effects and, more generally,
any kind of serial correlation.

The test relies on large-N asymptotics. It is valid under error
heteroskedasticity and departures from normality.

The above is valid if \code{effect="individual"}, which is the
most likely usage. If \code{effect="time"}, symmetrically, the
test relies on large-T asymptotics and has power against time
effects and, more generally, against cross-sectional correlation.

If the panelmodel interface is used, the inputted model must be
a pooling model.
}

\value{
An object of class \code{"htest"}.
}

\author{Giovanni Millo}

\references{
Wooldridge, J.M. (2002) \emph{Econometric Analysis of Cross-Section and Panel Data}, MIT Press, Sec. 10.4.4., pp. 264--265.

Wooldridge, J.M. (2010) \emph{Econometric Analysis of Cross-Section and Panel Data}, 2nd ed., MIT Press, Sec. 10.4.4, pp. 299--230.

}

\seealso{
 \code{\link{pbltest}}, \code{\link{pbgtest}}, \code{\link{pdwtest}},
 \code{\link{pbsytest}}, \code{\link{pwartest}}, \code{\link{pwfdtest}}
 for tests for serial correlation in panel models. \code{\link{plmtest}}
 for tests for random effects.
}

\examples{
data("Produc", package = "plm")
## formula interface
pwtest(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc)
pwtest(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, effect = "time")

## panelmodel interface
# first, estimate a pooling model, than compute test statistics
form <- formula(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp)
pool_prodc <- plm(form, data = Produc, model = "pooling")
pwtest(pool_prodc) # == effect="individual"
pwtest(pool_prodc, effect="time")
}

\keyword{htest}
