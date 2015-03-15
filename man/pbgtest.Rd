\name{pbgtest}
\alias{pbgtest}
\alias{pbgtest.panelmodel}
\alias{pbgtest.formula}
\title{Breusch--Godfrey Test for Panel Models}

\description{
 Test of serial correlation for (the idiosyncratic component of) the errors in panel models.
}

\usage{
pbgtest(x,...)
\method{pbgtest}{panelmodel}(x, order = NULL, ...)
\method{pbgtest}{formula}(x, ..., order = NULL)
}

\arguments{
 \item{x}{an object of class \code{"panelmodel"} or of class \code{"formula"},}
 \item{order}{an integer indicating the order of serial correlation to be tested for. Defaults to the minimum number of observations over the time dimension,}
 \item{\dots}{further arguments.}
}

\value{
 An object of class \code{"htest"}.
}

\author{Giovanni Millo}

\details{
This Lagrange multiplier test uses the auxiliary model on
(quasi-)demeaned data taken from a model of class \code{plm} which may be a \code{pooling} (the default), \code{random} or \code{within} model. It performs a Breusch--Godfrey test (using \code{bgtest} from package \code{lmtest}) on the residuals of the (quasi-)demeaned model, which should be serially uncorrelated under the null of no serial correlation in idiosyncratic
errors, as illustrated in Wooldridge (2002). The function takes the demeaned data, estimates the model and calls \code{bgtest}.  

Unlike most other tests for serial correlation in panels, this one allows to choose the order of correlation to test for.
}

\references{
 Breusch, T. (1978)  Testing for autocorrelation in dynamic linear models, \emph{Australian Economic Papers}, \bold{17}, pp.334--355.

 Godfrey, L. (1978) Testing against general autoregressive and moving average error models when the regressors include lagged dependent variables, \emph{Econometrica}, \bold{46}; pp. 1293--1302.
  
 Wooldridge, J.M. (2002) \emph{Econometric Analysis of Cross-Section and Panel Data}, MIT Press, p. 288.
}

\seealso{
\code{\link{pdwtest}} for the analogous panel Durbin--Watson test, \code{bgtest} for the Breusch--Godfrey test for serial correlation in the linear model. \code{\link{pbltest}}, \code{\link{pbsytest}}, \code{\link{pwartest}} and \code{\link{pwfdtest}} for other serial correlation tests for panel models.
}

\examples{
#data("Grunfeld", package = "plm")
#g <- plm(inv ~ value + capital, data = Grunfeld, model = "random")
#pbgtest(g)
#pbgtest(g, order = 4)

## formula interface
#pbgtest(inv ~ value + capital, data = Grunfeld, model = "random")
}

\keyword{htest}
