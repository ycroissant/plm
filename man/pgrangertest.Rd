\name{pgrangertest}
\alias{pgrangertest}

\title{Panel Granger Causality Test (Dumitrescu/Hurlin (2012))}

\description{
Test for Granger causality in panel data.
}

\usage{
pgrangertest(formula, data, test = c("Ztilde", "Zbar", "Wbar"),
             order = 1L, index = NULL)
}

\arguments{
 \item{formula}{a \code{formula} object to describe the direction of the 
               hypothesized Granger causation,}
 \item{data}{a \code{pdata.frame} or a \code{data.frame},}
 \item{test}{a character to request the statistic to be returned, either 
             \code{"Ztilde"} (default), or \code{"Zbar"},}
 \item{order}{integer(s) giving the number of lags to include in the test's
              auxiliary regressions, the length of order must be either 1 (same
              lag order for all individuals) or equal to the number of individuals
              (to specify a lag order per individual),}
 \item{index}{only relevant if \code{data} is \code{data.frame} and not a 
             \code{pdata.frame}; if \code{NULL}, the first two columns of the 
             data.frame are assumed to be the index variables, for further 
             details see \code{\link{pdata.frame}}.}
% \item{\dots}{further arguments to be passed on to some function}. % not needed 
}

\details{
The panel Granger causality test is a combination of Granger tests 
(Granger (1969)) performed per individual. The test is developed by 
Dumitrescu/Hurlin (2012), a shorter exposition is given in Lopez/Weber (2017).

The formula \code{formula} describes the direction of the (panel) Granger 
causation where \code{y ~ x} means "x (panel) Granger causes y".

By setting argument \code{test} to either \code{"Ztilde"} (default) or \code{"Zbar"}, 
two different statistics can be requested. \code{"Ztilde"} gives the standardised 
statistic recommended by Dumitrescu/Hurlin (2012) for fixed T samples. If set to
\code{"Wbar"}, the intermediate Wbar statistic (average of individual Granger 
chi-square statistics) is given which is used to derive the other two. 

The Zbar statistic is not suitable for unbalanced panels. For the Wbar statistic, 
no p-value is available.

The implementation uses \code{\link[lmtest]{grangertest}} from package 
\CRANpkg{lmtest} to perform the individual Granger tests.
}

\value{
An object of class \code{c("pgrangertest", "htest")}. Besides the usual 
elements of a \code{htest} object, it contains element \code{indgranger} which
carries the Granger test statistics along their p-values, degrees of freedom and
the specified lag order per individual as a data frame.
}

\author{Kevin Tappe}

\references{

 Dumitrescu, E.-I./Hurlin, C. (2012), Testing for Granger non--causality in 
 heterogeneous panels, \emph{Economic Modelling}, \bold{29}(4), pp. 1450--460.
% supplements (test data, MATLAB code): http://www.runmycode.org/companion/view/42

 Granger, C. W. J. (1969), Investigating Causal Relations by Econometric Models and 
 Cross--spectral Methods, \emph{Econometrica}, \bold{37}(3), pp. 424--438.

 Lopez, L./Weber, S. (2017), Testing for Granger causality in panel data,
 \emph{IRENE Working paper 17-03}, September 11, 2017.
% supplements (xtgcause for Stata) https://ideas.repec.org/c/boc/bocode/s458308.html
}

\seealso{
\code{\link[lmtest]{grangertest}} for the Granger causality test in \CRANpkg{lmtest},
}

\examples{
## not meaningful, just to demonstrate usage
## H0: 'value' Granger causes 'inv' for all invididuals

data("Grunfeld", package = "plm")
pgrangertest(inv ~ value, data = Grunfeld)
pgrangertest(inv ~ value, data = Grunfeld, order = 2L)
pgrangertest(inv ~ value, data = Grunfeld, order = 2L, test = "Zbar")

# varying lag order (last individual lag order 3, others lag order 2)
pgrangertest(inv ~ value, data = Grunfeld, order = c(rep(2L, 9), 3L))

}

\keyword{htest}
