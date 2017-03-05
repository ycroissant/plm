\name{pbsytest}
\alias{pbsytest}
\alias{pbsytest.formula}
\alias{pbsytest.panelmodel}

\title{Bera, Sosa-Escudero and Yoon Locally--Robust Lagrange Multiplier Tests for Panel Models 
and Joint Test by Baltagi and Li}

\description{
 Test for residual serial correlation (or individual random effects) locally robust vs. individual random effects (serial correlation) for panel models and joint test by Baltagi and Li.
}

\usage{
pbsytest(x, \dots)
\method{pbsytest}{panelmodel}(x, test = c("ar", "re", "j"), 
            re.normal = if (test == "re") TRUE else NULL, \dots)
\method{pbsytest}{formula}(x, data, \dots, test = c("ar", "re", "j"),
            re.normal = if (test == "re") TRUE else NULL)
}

\arguments{
 \item{x}{an object of class \code{formula} or of class
  \code{panelmodel},}
 \item{data}{a \code{data.frame},}
 \item{test}{a character string indicating which test to perform:
  first--order serial correlation (\code{"ar"}), random effects
  (\code{"re"}) or joint test for either of them (\code{"j"}),}
 \item{re.normal}{logical, only relevant for \code{test = "re"}: 
 \code{TRUE} (default) computes the one-sided \code{"re"} test, 
 \code{FALSE} the two-sided test (see also Details);
 not relevant for other values of \code{test} and, thus, should be \code{NULL},}
 \item{\dots}{further arguments.}
}

\details{
 These Lagrange multiplier tests are robust vs. local misspecification of the alternative hypothesis, 
 i.e. they test the null of serially uncorrelated residuals against AR(1) residuals in a pooling model, 
 allowing for local departures from the assumption of no random effects; or they test the null of no 
 random effects allowing for local departures from the assumption of no serial correlation in residuals.
 They use only the residuals of the pooled OLS model and correct for local misspecification as outlined 
 in Bera \emph{et al.} (2001).
 
 For \code{test = "re"}, the default (\code{re.normal = TRUE}) is to compute a one-sided test which is
 expected to lead to a more powerful test (asymptotically N(0,1) distributed).
 Setting \code{re.normal = FALSE} gives the two-sided test (asymptotically chi-squared(2) distributed). Argument
 \code{re.normal} is irrelevant for all other values of \code{test}.

 The joint test (\code{test = "j"}) is due to Baltagi and Li (1991) (also mentioned in
 Baltagi and Li (1995), pp. 135--136) and is added for convenience under this same function.

 The unbalanced version of all tests are derived in Sosa-Escudero and Bera (2008). The functions implemented 
 are suitable for balanced as well as unbalanced panel data sets.
 
 A concise treatment of the statistics for only balanced panels is given in Baltagi (2013), p. 108.

 Here is an overview of how the various values of the \code{test} argument relate to the literature:

\itemize{
\item \code{test = "ar"}:
\itemize{
\item \eqn{RS*_{\rho}} in Bera et al. (2001), p. 9 (balanced)
\item \eqn{LM*_{\rho}} in Baltagi (2013), p. 108 (balanced)
\item \eqn{RS*_{\lambda}} in Sosa-Escudero/Bera (2008), p. 73 (unbalanced)
}

\item \code{test = "re", re.normal = TRUE} (default) (one-sided test, asymptotically N(0,1) distributed):
\itemize{
\item \eqn{RSO*_{\mu}} in Bera et al. (2001), p. 11 (balanced)
\item \eqn{RSO*_{\mu}} in Sosa-Escudero/Bera (2008), p. 75 (unbalanced)
}

\item \code{test = "re", re.normal = FALSE} (two-sided test, asymptotically chi-squared(2) distributed):
\itemize{
\item \eqn{RS*_{\mu}} in Bera et al. (2001), p. 7 (balanced)
\item \eqn{LM*_{\mu}} in Baltagi (2013), p. 108 (balanced)
\item \eqn{RS*_{\mu}} in Sosa-Escudero/Bera (2008), p. 73 (unbalanced)
}

\item \code{test = "j"}:
\itemize{
\item \eqn{RS_{\mu\rho}} in Bera et al. (2001), p. 10 (balanced)
\item \eqn{LM} in Baltagi/Li (2001), p. 279 (balanced)
\item \eqn{LM_{1}} in Baltagi and Li (1995), pp. 135--136 (balanced)
\item \eqn{LM1} in Baltagi (2013), p. 108 (balanced)
\item \eqn{RS_{\lambda\rho}} in Sosa-Escudero/Bera (2008), p. 74 (unbalanced)
}
}

}

\value{
 An object of class \code{"htest"}.
}

\author{Giovanni Millo (initial implementation) & Kevin Tappe (extension to unbalanced panels)}

\references{
 Bera, A.K., Sosa-Escudero, W. and  Yoon, M. (2001), Tests for the error component model in the presence of local misspecification, \emph{Journal of Econometrics}, \bold{101}(1), pp. 1--23.
 
 Baltagi, B. H. (2013) \emph{Econometric Analysis of Panel Data}, 5th edition, Wiley.
 
 Baltagi, B. H. and Li, Q. (1991), A joint test for serial correlation and random individual effects, \emph{Statistics and Probability Letters}, \bold{11}(3), pp. 277--280.
 
 Baltagi, B. H. and Li, Q. (1995), Testing AR(1) against MA(1) disturbances in an error component model, \emph{Journal of Econometrics}, \bold{68}(1), pp. 133--151.
 
 Sosa-Escudero, W. and Bera, A.K. (2008), Tests for unbalanced error--components models under local misspecification, \emph{The Stata Journal}, \bold{8}(1), pp. 68--78.
}

\seealso{
  \code{\link{plmtest}} for individual and/or time random effects tests based on a correctly specified model; \code{\link{pbltest}},
  \code{\link{pbgtest}} and \code{\link{pdwtest}} for serial correlation tests in random effects models.
}

\examples{
## Bera et al. (2001), p. 13, table 1 use a subset of the original Grunfeld
## data which contains three errors -> construct this subset:
data(Grunfeld, package = "plm")
Grunsubset <- rbind(Grunfeld[1:80, ], Grunfeld[141:160, ])
Grunsubset[Grunsubset$firm == 2 & Grunsubset$year \%in\% c(1940, 1952), ][["inv"]] <- c(261.6, 645.2)
Grunsubset[Grunsubset$firm == 2 & Grunsubset$year == 1946, ][["capital"]] <- 232.6

## default is AR testing (formula interface)
pbsytest(inv ~ value + capital, data = Grunsubset, index = c("firm", "year"))
pbsytest(inv ~ value + capital, data = Grunsubset, index = c("firm", "year"), test = "re")
pbsytest(inv ~ value + capital, data = Grunsubset, index = c("firm", "year"), 
  test = "re", re.normal = FALSE)
pbsytest(inv ~ value + capital, data = Grunsubset, index = c("firm", "year"), test = "j")

## plm interface
mod <- plm(inv ~ value + capital, data = Grunsubset, model = "pooling")
pbsytest(mod)
}

\keyword{htest}
