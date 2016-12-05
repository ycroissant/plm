\name{plmtest}
\alias{plmtest}
\alias{plmtest.plm}
\alias{plmtest.formula}

\title{Lagrange Multiplier Tests for Panel Models}

\description{
Test of individual and/or time effects for panel models.
}

\usage{
plmtest(x, \dots)
\method{plmtest}{plm}(x, effect = c("individual", "time", "twoways"),
           type = c("honda", "bp", "ghm", "kw"), \dots)
\method{plmtest}{formula}(x, data, \dots, effect = c("individual", "time", "twoways"),
                      type = c("honda", "bp", "ghm", "kw"))
}

\arguments{
 \item{x}{an object of class \code{"plm"} or a formula of class \code{"formula"},}
 \item{data}{a \code{data.frame},}
 \item{effect}{a character string indicating which effects are tested: individual effects (\code{"individual"}),
 time effects (\code{"time"}) or both (\code{"twoways"}),}
 \item{type}{a character string indicating the test to be
  performed: \itemize{
  \item \code{"honda"} (default) for Honda (1985),
  \item \code{"bp"} for Breusch/Pagan (1980),
  \item \code{"kw"} for King/Wu (1997), or
  \item \code{"ghm"} for Gourieroux/Holly/Monfort (1982);
  } For unbalanced panel data sets, the respective unbalanced version of the tests are computed,}
 \item{\dots}{further arguments passed to \code{plmtest}.}
}

\details{
These Lagrange multiplier tests use only the residuals of the pooling model. The first argument of this
function may be either a pooling model of class \code{plm} or an object of class \code{formula} describing
the model. For inputted within (fixed effects) or random effects models, the corresponding pooling model is
calculated internally first as the tests are based on the residuals of the pooling model.

The \code{"bp"} test for unbalanced panels was derived in Baltagi/Li (1990), the \code{"kw"} test for
unbalanced panels in Baltagi/Chang/Li (1998). The \code{"ghm"} test and the \code{"kw"} test were
extended to two-way effects in Baltagi/Chang/Li (1992).\cr
For a concise overview of all these statistics see Baltagi (2013), Sec. 4.2, pp.
68--76 (for balanced panels) and Sec. 9.5, pp. 200--203 (for unbalanced panels).
}

\value{
An object of class \code{"htest"}.
}

\note{
For the King-Wu statistics (\code{"kw"}), the oneway statistics (\code{"individual"} and \code{"time"}) coincide
with the respective Honda statistics (\code{"honda"}); twoway statistics of \code{"kw"} and \code{"honda"} differ.
}


\author{Yves Croissant (initial implementation), Kevin Tappe (generalization to unbalanced panels)}

\references{

Baltagi, B. H. (2013) \emph{Econometric Analysis of Panel Data}, 5th edition, Sec. 4.2, pp. 68--76 and Sec. 9.5, pp. 200--203.

Baltagi, B. H./Li, Q. (1990) A lagrange multiplier test for the error components model with incomplete panels, \emph{Econometric Reviews}, \bold{9}(1), pp. 103--107.

Baltagi B. H./Chang, Y./Li, Q. (1992) Monte Carlo results on several new and existing tests for the error component model, \emph{Journal of Econometrics, \bold{54}(1-3), pp. 95--120.}

Baltagi B. H./Chang, Y./Li, Q. (1998) Testing for random effects using unbalanced panel data, \emph{Advances in Econometrics, \bold{13}, pp. 1--20.}

Breusch, T. S./Pagan, A. R. (1980) The Lagrange multiplier test and its applications to model specification in econometrics, \emph{Review of Economic Studies}, \bold{47}(1), pp. 239--253.

Gourieroux, C./Holly, A./Monfort, A. (1982) Likelihood ratio test, Wald test, and Kuhn--Tucker test in linear models with inequality constraints on the regression parameters, \emph{Econometrica}, \bold{50}(1), pp. 63--80.

Honda, Y. (1985) Testing the error components model with non--normal disturbances, \emph{Review of Economic Studies}, \bold{52}(4), pp. 681--690.

King, M. L./Wu, P. X. (1997) Locally optimal one--sided tests for multiparameter hypotheses, \emph{Econometric Reviews}, \bold{16}(2), pp. 131--156.
}

\seealso{
\code{\link{pFtest}} for individual and/or time effects tests based on the within model.
}

\examples{
data("Grunfeld", package = "plm")
g <- plm(inv ~ value + capital, data = Grunfeld, model = "pooling")
plmtest(g)
plmtest(g, effect="time")
plmtest(inv ~ value + capital, data = Grunfeld, type = "honda")
plmtest(inv ~ value + capital, data = Grunfeld, type = "bp")
plmtest(inv ~ value + capital, data = Grunfeld, type = "bp",  effect = "twoways")
plmtest(inv ~ value + capital, data = Grunfeld, type = "ghm", effect = "twoways")
plmtest(inv ~ value + capital, data = Grunfeld, type = "kw",  effect = "twoways")

Grunfeld_unbal <- Grunfeld[1:(nrow(Grunfeld)-1), ] # create an unbalanced panel data set
g_unbal <- plm(inv ~ value + capital, data = Grunfeld_unbal, model = "pooling")
plmtest(g_unbal) # unbalanced version of test is indicated in output
}

\keyword{htest}