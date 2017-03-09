\name{pbgtest}
\alias{pbgtest}
\alias{pbgtest.panelmodel}
\alias{pbgtest.formula}
\title{Breusch--Godfrey Test for Panel Models}

\description{
 Test of serial correlation for (the idiosyncratic component of) the errors in panel models.
}

\usage{
pbgtest(x, \dots)
\method{pbgtest}{panelmodel}(x, order = NULL, type = c("Chisq", "F"), \dots)
\method{pbgtest}{formula}(x, order = NULL, type = c("Chisq", "F"),
           data, model=c("pooling", "random", "within"), \dots)
}

\arguments{
 \item{x}{an object of class \code{"panelmodel"} or of class \code{"formula"},}
 \item{order}{an integer indicating the order of serial correlation to be tested for. \code{NULL} (default) uses the minimum number of observations over the time dimension (see also section \bold{Details} below),}
 \item{type}{type of test statistic to be calculated; either \code{"Chisq"} (default) for the Chi-squared test statistic or \code{"F"} for the F test statistic,}
 \item{data}{only relevant for formula interface: data set for which the respective panel model (see \code{model}) is to be evaluated,}
 \item{model}{only relevant for formula interface: compute test statistic for model \code{pooling} (default), \code{random}, or \code{within}.
              When \code{model} is used, the \code{data} argument needs to be passed as well,}

 \item{\dots}{further arguments (see \code{\link[lmtest]{bgtest}}).}
}

\value{
 An object of class \code{"htest"}.
}

\author{Giovanni Millo}

\details{
This Lagrange multiplier test uses the auxiliary model on
(quasi-)demeaned data taken from a model of class \code{plm} which may be a \code{pooling} (default for formula interface), \code{random} or \code{within} model. It performs a Breusch--Godfrey test (using \code{bgtest} from package \CRANpkg{lmtest}) on the residuals of the (quasi-)demeaned model, which should be serially uncorrelated under the null of no serial correlation in idiosyncratic
errors, as illustrated in Wooldridge (2002/2010). The function takes the demeaned data, estimates the model and calls \code{bgtest}.  

Unlike most other tests for serial correlation in panels, this one allows to choose the order of correlation to test for.
}

\note{
The argument \code{order} defaults to the minimum number of observations over the time dimension, while for \code{lmtest::bgtest} it defaults to \code{1}.
}


\references{
 Breusch, T.S. (1978), \dQuote{Testing for autocorrelation in dynamic linear models}, \emph{Australian Economic Papers}, \bold{17}(31), pp. 334--355.

 Godfrey, L.G. (1978), \dQuote{Testing against general autoregressive and moving average error models when the regressors include lagged dependent variables}, \emph{Econometrica}, \bold{46}(6), pp. 1293--1301.
  
 Wooldridge, J.M. (2002) \emph{Econometric Analysis of Cross-Section and Panel Data}, MIT Press, pp. 288--291.
 
 Wooldridge, J.M. (2010) \emph{Econometric analysis of cross-section and Panel Data}, 2nd ed., MIT Press, pp. 328--334.
 
 Wooldridge, J.M. (2013) \emph{Introductory Econometrics: A Modern Approach}, 5th ed., South-Western (Cengage Learning), Sec. 12.2, pp. 421--422.
}

\seealso{
For the original test in package \CRANpkg{lmtest} see \code{\link[lmtest]{bgtest}}. 
See \code{\link{pdwtest}} for the analogous panel Durbin--Watson test. 
See \code{\link{pbltest}}, \code{\link{pbsytest}}, \code{\link{pwartest}} and 
\code{\link{pwfdtest}} for other serial correlation tests for panel models.
}

\examples{
data("Grunfeld", package = "plm")
g <- plm(inv ~ value + capital, data = Grunfeld, model = "random")

# panelmodel interface
pbgtest(g)
pbgtest(g, order = 4)

# formula interface
pbgtest(inv ~ value + capital, data = Grunfeld, model = "random")

# F test statistic (instead of default type="Chisq")
pbgtest(g, type="F")
pbgtest(inv ~ value + capital, data = Grunfeld, model = "random", type = "F")

# same output as lmtest::bgtest [see Note]
pbgtest(g, order = 1)
lmtest::bgtest(g, order = 1)
}

\keyword{htest}
