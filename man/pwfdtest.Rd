\name{pwfdtest}
\alias{pwfdtest}
\alias{pwfdtest.formula}
\alias{pwfdtest.panelmodel}

\title{Wooldridge first--difference--based test for AR(1) errors in levels or first--differenced panel models}

\description{
First-differencing-based test of serial correlation for (the idiosyncratic component of) the errors in either levels or first--differenced panel models.
}

\usage{
pwfdtest(x,...)
\method{pwfdtest}{panelmodel}(x, ..., h0 = c("fd","fe"))
\method{pwfdtest}{formula}(x, data, ..., h0 = c("fd","fe"))
}

\arguments{
  \item{x}{an object of class \code{formula},}
  \item{data}{a \code{data.frame},}
  \item{h0}{the null hypothesis: one of "fd","fe"}
  \item{...}{further arguments to be passed on to
  \code{linearHypothesis} or to \code{vcovHC}}
}

\details{
As Wooldridge (2003/2010, Sec. 10.6.3) observes, if the idiosyncratic errors in the model in levels are uncorrelated (which we label hypothesis \code{"fe"}), then the errors of the model in first differences (FD) must be serially correlated with \eqn{cor(\hat{e}_{it}, \hat{e}_{is})=-0.5} for each \eqn{t,s}.  If on the contrary the levels model's errors are a random walk, then there must be no serial correlation in the FD errors (hypothesis \code{"fd"}). Both the fixed effects (FE) and the first--differenced (FD) estimators remain consistent under either assumption, but the relative efficiency changes: FE is more efficient under \code{"fe"}, FD under \code{"fd"}.

Wooldridge (ibid.) suggests basing a test for either hypothesis on a pooled regression of FD residuals on their first lag:
\eqn{\hat{e}_{i,t}=\alpha + \delta \hat{e}_{i,t-1} + \eta_{i,t}}. Rejecting the restriction \eqn{\delta = -0.5} makes us conclude against the null of no serial correlation in errors of the levels equation (\code{"fe"}). The null hypothesis of no serial correlation in differenced errors (\code{"fd"}) is tested in a similar way, but based on the zero restriction on \eqn{\delta}. Rejecting \code{"fe"} favours the use of the first--differences  estimator and the contrary, although it is possible that both be rejected.

\code{pwfdtest} estimates the \code{fd} model and retrieves residuals, then estimates an AR(1) \code{pooling} model on them. The test statistic is obtained by applying \code{linearHypothesis()} to the latter model to test the relevant restriction on \eqn{\delta}, setting the covariance matrix to \code{vcovHC} with the option \code{method=''arellano''} to control for serial correlation.

Unlike the \code{pbgtest} and \code{pdwtest}, this test does not rely on large--T asymptotics and has therefore good properties in ''short'' panels. Furthermore, it is robust to general heteroskedasticity. The \code{"fe"} version can be used to test for error autocorrelation regardless of whether the maintained specification has fixed or random effects (see Drukker, 2003).
}

\value{
An object of class \code{"htest"}.
}

\author{Giovanni Millo}

\references{
Drukker, D.M. (2003) Testing for serial correlation in linear
panel--data models, \emph{The Stata Journal}, \bold{3(2)}, pp. 168--177.

Wooldridge, J.M. (2003) \emph{Econometric analysis of cross--section and
panel data}, MIT Press, Sec. 10.6.3, pp. 282--283.

Wooldridge, J.M. (2010) \emph{Econometric analysis of cross--section and
panel data}, 2nd ed., MIT Press, Sec. 10.6.3, pp. 319--320.


}

\seealso{
\code{pdwtest}, \code{pbgtest}, \code{pwartest}
}

\examples{
data(EmplUK)
pwfdtest(log(emp) ~ log(wage) + log(capital), data = EmplUK)
pwfdtest(log(emp) ~ log(wage) + log(capital), data = EmplUK, h0 = "fe")
}

\keyword{htest}
