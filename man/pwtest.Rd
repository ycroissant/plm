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
\method{pwtest}{panelmodel}(x, ...)
\method{pwtest}{formula}(x, data, ...)
}

\arguments{
\item{x}{an object of class \code{"formula"},}
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
pwtest(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc)
pwtest(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, effect="time")
}

\keyword{htest}
