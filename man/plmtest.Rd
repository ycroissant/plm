\name{plmtest}
\alias{plmtest}
\alias{plmtest.plm}
\alias{plmtest.formula}

\title{Lagrange Multiplier Tests for Panel Models}

\description{
Test of individual and/or time effects for panel models.
}

\usage{
plmtest(x, ...)
\method{plmtest}{plm}(x, effect = c("individual", "time", "twoways"),
           type = c("honda", "bp", "ghm", "kw"), ...)
\method{plmtest}{formula}(x, data, ..., effect = c("individual", "time", "twoways"),
                      type = c("honda", "bp", "ghm", "kw"))
}

\arguments{
 \item{x}{an object of class \code{"plm"},}
 \item{data}{a \code{data.frame},}
 \item{effect}{a character string indicating which effects are tested: individual effects (\code{"individual"}), time effects (\code{"time"}) or both (\code{"twoways"}),}
 \item{type}{a character string indicating the test to be
  computed: \code{"bp"} for Breush--Pagan (1980), \code{"ghm"} for Gourieroux, Holly and Monfort (1982), \code{"honda"} for Honda (1985) or \code{"kw"} for King and Wu (1997),}
 \item{...}{further arguments passed to \code{plm}.}
}

\details{
These Lagrange multiplier tests use only the residuals of the pooling model. The main argument of this function may be either a pooling model of class \code{plm} or an object of class \code{formula}.
}

\value{
An object of class \code{"htest"}.
}

\author{Yves Croissant}

\references{
Breusch, T.S. and Pagan, A.R. (1980) The Lagrange multiplier test and its applications to model specification in econometrics, \emph{Review of Economic Studies}, \bold{47}, pp. 239--253.

Gourieroux, C., Holly, A. and Monfort, A. (1982) Likelihood ratio test, Wald test, and Kuhn--Tucker test in linear models with inequality constraints on the regression parameters, \emph{Econometrica}, \bold{50}, pp. 63--80.

Honda, Y. (1985) Testing the error components model with non--normal disturbances, \emph{Review of Economic Studies}, \bold{52}, pp. 681--690.

King, M.L. and Wu, P.X. (1997) Locally optimal one--sided tests for multiparameter hypotheses, \emph{Econometric Reviews}, \bold{33}, pp. 523--529.
}

\seealso{
\code{\link{pFtest}} for individual and/or time effects tests based on the within model.
}

\examples{
data("Grunfeld", package = "plm")
g <- plm(inv ~ value + capital, data = Grunfeld, model = "pooling")
plmtest(g)
plmtest(g, effect = "time")
plmtest(inv ~ value + capital, data = Grunfeld, type = "honda")
plmtest(inv ~ value + capital, data = Grunfeld, type = "ghm", effect="twoways")
plmtest(inv ~ value + capital, data = Grunfeld, type = "kw",  effect="twoways")
}

\keyword{htest}
