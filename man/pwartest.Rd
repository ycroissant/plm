\name{pwartest}
\alias{pwartest}
\alias{pwartest.panelmodel}
\alias{pwartest.formula}

\title{Wooldridge Test for AR(1) Errors in FE Panel Models}

\description{
Test of serial correlation for (the idiosyncratic component of) the errors in fixed-effects panel models.
}

\usage{
pwartest(x,...)
\method{pwartest}{panelmodel}(x, ...)
\method{pwartest}{formula}(x, data, ...)
}

\arguments{
 \item{x}{an object of class \code{formula} or of class \code{panelmodel},}
 \item{data}{a \code{data.frame},}
 \item{...}{further arguments to be passed on to \code{linearHypothesis} or to \code{vcovHC}.}
}

\details{
As Wooldridge (2003/2010, Sec. 10.5.4) observes, under the null of no serial correlation in the errors, the residuals of a FE model must be negatively serially correlated, with \eqn{cor(\hat{u}_{it}, \hat{u}_{is})=-1/(T-1)} for each \eqn{t,s}. He suggests basing a test for this null hypothesis on a pooled regression of FE residuals on their first lag: \eqn{\hat{u}_{i,t} = \alpha + \delta \hat{u}_{i,t-1} + \eta_{i,t}}. Rejecting the restriction \eqn{\delta = -1/(T-1)} makes us conclude against the original null of no serial correlation.

\code{pwartest} estimates the \code{within} model and retrieves
residuals, then estimates an AR(1) \code{pooling} model on them. The test statistic is obtained by applying \code{linearHypothesis()} to the latter model to test the above restriction on \eqn{\delta}, setting the covariance matrix to \code{vcovHC} with the option \code{method="arellano"} to control for serial correlation.

Unlike the \code{\link{pbgtest}} and \code{\link{pdwtest}}, this test does not rely on large-T asymptotics and has therefore good properties in ``short'' panels. Furthermore, it is robust to general heteroskedasticity.
}

\value{
An object of class \code{"htest"}.
}

\author{Giovanni Millo}

\references{
Wooldridge, J.M. (2002) \emph{Econometric Analysis of Cross-Section and Panel Data}, MIT Press, Sec. 10.5.4, pp. 274--276.
Wooldridge, J.M. (2010) \emph{Econometric analysis of cross-section and panel data}, 2nd ed., MIT Press, Sec. 10.5.4, pp. 310--312.
}

\seealso{
 \code{\link{pwfdtest}}, \code{\link{pdwtest}}, \code{\link{pbgtest}}, \code{\link{pbltest}},
 \code{\link{pbsytest}}.
}

\examples{
data("EmplUK", package = "plm")
pwartest(log(emp) ~ log(wage) + log(capital), data = EmplUK)
}

\keyword{htest}
