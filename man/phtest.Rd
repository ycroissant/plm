\name{phtest}
\alias{phtest}
\alias{phtest.formula}
\alias{phtest.panelmodel}
\title{Hausman Test for Panel Models}

\description{
Specification test for panel models.
}

\usage{
phtest(x, \dots)
\method{phtest}{panelmodel}(x, x2, \dots)
\method{phtest}{formula}(x, data, model = c("within", "random"),
                method = c("chisq", "aux"),
                index = NULL, vcov = NULL, \dots)
}

\arguments{
 \item{x}{an object of class  \code{"panelmodel"} or \code{"formula"},}
 \item{x2}{an object of class \code{"panelmodel"},}
 \item{model}{a character vector containing the names of two models (length(model) must be 2),}
 \item{data}{a \code{data.frame},}
 \item{method}{one of \code{"chisq"} or \code{"aux"},}
 \item{index}{an optional vector of index variables,}
 \item{vcov}{an optional covariance function,}
 \item{\dots}{further arguments to be passed on. For the formula method, 
 place argument \code{effect} here to compare e.g. twoway models (\code{effect = "twoways"})
 Note: Argument \code{effect} is not respected in the panelmodel method.}
}

\value{
An object of class \code{"htest"}.
}

\details{
The Hausman test (sometimes also called Durbin--Wu--Hausman test) is
based on the difference of the vectors of coefficients of two different
models. The \code{panelmodel} method computes the original version of
the test based on a quadratic form (Hausman (1978)). The \code{formula} method, if
\code{method="chisq"} (default), computes
the original version of the test based on a quadratic form;
if \code{method="aux"} then the auxiliary-regression-based version in
Wooldridge (2010, Sec. 10.7.3.) is computed instead. Only the latter can
be robustified by specifying a robust covariance estimator as a function
through the argument \code{vcov} (see \bold{Examples}).

The equivalent tests in the \bold{one-way} case using a between model (either 
"within vs. between" or "random  vs. between") (see Hausman/Taylor (1981) or Baltagi 
(2013), Sec. 4.3) can also be performed by \code{phtest}, but only for 
\code{test = "chisq"}, not for the regression-based test.
NB: These equivalent tests using the between model do not extend to the two-ways case. 
(There are, however, some other equivalent tests, see Kang (1985) or Baltagi (2013), 
Sec. 4.3.7), but those are unsupported by \code{phtest}.)
}

\references{
Hausman, J.A. (1978), Specification tests in econometrics,
\emph{Econometrica}, \bold{46}(6), pp. 1251--1271.

Hausman, J.A./Taylor, W.E. (1981), Panel data and unobservable individual effects, 
\emph{Econometrica}, \bold{49}(6), pp. 1377--1398.

Kang, Suk (1985), A note on the equivalence of specification tests in the 
two-factor multivariate variance components model, \emph{Journal of Econometrics}, 
\bold{28}(2), pp. 193--203.

Wooldridge, Jeffrey M. (2010), \emph{Econometric Analysis of
  Cross Section and Panel Data}, 2nd ed., MIT Press, Sec. 10.7.3., pp. 328--334.
  
Baltagi, Badi H. (2013), \emph{Econometric Analysis of Panel Data}, 5th ed., 
John Wiley and Sons., Sec. 4.3.
}

\author{Yves Croissant, Giovanni Millo}
  
\examples{
data("Gasoline", package = "plm")
form <- lgaspcar ~ lincomep + lrpmg + lcarpcap
wi <- plm(form, data = Gasoline, model = "within")
re <- plm(form, data = Gasoline, model = "random")
phtest(wi, re)
phtest(form, data = Gasoline)
phtest(form, data = Gasoline, method = "aux")

# robust Hausman test (regression-based)
phtest(form, data = Gasoline, method = "aux", vcov = vcovHC)

# robust Hausman test with vcov supplied as a
# function and additional parameters
phtest(form, data = Gasoline, method = "aux",
  vcov = function(x) vcovHC(x, method="white2", type="HC3"))
}

\keyword{htest}
