\name{ranef.plm}
\alias{ranef.plm}
\alias{ranef}
\title{Extract the Random Effects}

\description{
  Function to calculate the random effects from a \code{plm} object (random effects model).
}

\usage{
\method{ranef}{plm}(object, \dots)
}

\arguments{
  \item{object}{an object of class \code{"plm"}, needs to be a fitted random effects model,}
  \item{\dots}{further arguments (currently not used).}
}

\value{
  A numeric with the random effects per dimension (individual or time).
}

\details{
Function \code{ranef} calculates the random effects of a fitted random effects model.
The calculated effects (individual or time) are those of the fitted model object.
Not all random effect model types are supported (yet?).
}

\seealso{
  \code{\link{fixef}} to extract the fixed effects from a fixed effects model (within model). 
}

\author{Kevin Tappe}

\examples{
data("Grunfeld", package = "plm")
m1 <- plm(inv ~ value + capital, data = Grunfeld, model = "random")
ranef(m1) # individual random effects

# compare to random effects by ML estimation by lmer from package lme4
\dontrun{
  library(lme4)
  m2 <- lmer(inv ~ value + capital + (1 | firm), data = Grunfeld) 
  cbind("plm" = ranef(m1), "lmer" = unname(ranef(m2)$firm))
}
}


%\references{
%}

\keyword{regression}