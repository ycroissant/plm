\name{pdwtest}
\alias{pdwtest}
\alias{pdwtest.panelmodel}
\alias{pdwtest.formula}

\title{Durbin--Watson Test for Panel Models}

\description{
Test of serial correlation for (the idiosyncratic component of) the errors in 
panel models.
}

\usage{
pdwtest(x, \dots)
\method{pdwtest}{panelmodel}(x, \dots)
\method{pdwtest}{formula}(x, data, \dots)
}

\arguments{
 \item{x}{an object of class \code{"panelmodel"} or of class
   \code{"formula"},}
 \item{data}{a \code{data.frame},}
 \item{\dots}{further arguments to be passed on to \code{dwtest}, e.g. 
 \code{alternative}, see \code{\link[lmtest]{dwtest}} for further details.}
}

\details{
This Durbin--Watson test uses the auxiliary model on (quasi-)demeaned data taken 
from a model of class \code{plm} which may be a \code{pooling} (the default), 
\code{random} or \code{within} model. It performs a Durbin--Watson test 
(using \code{dwtest} from package \CRANpkg{lmtest}) on the residuals of the 
(quasi-)demeaned model, which should be serially uncorrelated under the null of 
no serial correlation in idiosyncratic errors. The function takes the demeaned 
data, estimates the model and calls \code{dwtest}. Thus, this test does not take 
the panel structure of the residuals into consideration; it shall not be confused
with the generalized Durbin-Watson test for panels in \code{pbnftest}.
}

\value{
An object of class \code{"htest"}.
}

\author{Giovanni Millo}

\references{
 Durbin, J. and Watson, G.S. (1950), Testing for Serial Correlation in Least Squares Regression. I, \emph{Biometrika}, \bold{37}(3/4), pp. 409--428.

 Durbin, J. and Watson, G.S. (1951), Testing for Serial Correlation in Least Squares Regression. II, \emph{Biometrika}, \bold{38}(1/2), pp. 159--177.

 Durbin, J. and Watson, G.S. (1971), Testing for Serial Correlation in Least Squares Regression. III, \emph{Biometrika}, \bold{58}(1), pp. 1--19.
 
 Wooldridge, J.M. (2002) \emph{Econometric Analysis of Cross Section and Panel Data}, MIT Press, p. 288.
 
 Wooldridge, J.M. (2010) \emph{Econometric Analysis of Cross Section and Panel Data}, 2nd ed., MIT Press, p. 328.
}

\seealso{
\code{\link[lmtest]{dwtest}} for the Durbin--Watson test in \CRANpkg{lmtest},
\code{\link{pbgtest}} for the analogous Breusch--Godfrey 
test for panel models, \code{\link[lmtest]{bgtest}} for the Breusch--Godfrey test for 
serial correlation in the linear model. \code{\link{pbltest}}, 
\code{\link{pbsytest}}, \code{\link{pwartest}} and \code{\link{pwfdtest}} for 
other serial correlation tests for panel models.

For the Durbin-Watson test generalized to panel data models see \code{\link{pbnftest}}.
}

\examples{
data("Grunfeld", package = "plm")
g <- plm(inv ~ value + capital, data = Grunfeld, model="random")
pdwtest(g)
pdwtest(g, alternative="two.sided")
## formula interface
pdwtest(inv ~ value + capital, data=Grunfeld, model="random")
}

\keyword{htest}
