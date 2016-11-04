\name{cipstest}
\alias{cipstest}
\alias{cipstest.panelmodel}
\alias{cipstest.formula}

\title{Cross-sectionally Augmented IPS Test for Unit Roots in Panel Models}

\description{
Cross-sectionally augmented Im, Pesaran and Shin (IPS) test for unit roots in panel models.
}

\usage{
cipstest(x, lags = 2, type = c("trend", "drift", "none"),
      model = c("cmg", "mg", "dmg"), truncated = FALSE, \dots)
}

\arguments{
  \item{x}{an object of class \code{"pseries"},}
  \item{lags}{lag order for Dickey-Fuller augmentation,}
  \item{type}{one of \code{"trend"}, \code{"drift"}, \code{"none"},}
  \item{model}{one of \code{"cmg"}, \code{"mg"}, \code{"dmg"},}
  \item{truncated}{logical specifying whether to calculate the truncated
    version of the test,}
  \item{\dots}{further arguments passed to \code{critvals}.} 
% TODO NB: critvals (helper function) not mentioned before
% TODO NB: doc-code mismatch: currently, critval is not called with '...' in the code,
%          but mmean(tstats, ...) ist
}

\details{
This cross-sectionally augmented version of the IPS unit root test (H0:
the \code{pseries} has a unit root) is a so-called second-generation
panel unit root test: it is in fact robust against cross-sectional
dependence, provided that the default \code{type="cmg"} is
calculated. Else one can obtain the standard (\code{model="mg"}) or
cross-sectionally demeaned (\code{model="dmg"}) versions of the IPS
test.

% TODO: maybe be more verbose here? write about type arg which corresponds to
%       cases III, II, I in Pesaran (2007) etc.

}

\value{
An object of class \code{"htest"}.
}

\author{Giovanni Millo}

\references{
  Pesaran, M.H. (2007) A simple panel unit root test in the presence of
  cross-section dependence, \emph{Journal of Applied Econometrics},
  \bold{22(2)}, pp. 265--312.
}

\seealso{
 \code{\link{purtest}}
}

\examples{
data("Produc", package = "plm")
Produc <- pdata.frame(Produc, index=c("state", "year"))
## check whether the gross state product (gsp) is trend-stationary
cipstest(Produc$gsp, type = "trend")
}

\keyword{htest}
