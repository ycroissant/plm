\name{pldv}
\alias{pldv}

\title{Panel estimators for limited dependent variables}

\description{
  Fixed and random effects estimators for truncated or censored limited
  dependent variable
}

\usage{
pldv(formula, data, subset, weights, na.action,
                 model = c("fd", "random", "pooling"), index = NULL,
                 R = 20, start = NULL, lower = 0, upper = + Inf,
                 objfun = c("lsq", "lad"), sample = c("cens", "trunc"), \dots)
}

\arguments{
  \item{formula}{a symbolic description for the model to be estimated,}
  \item{data}{a \code{data.frame},}
  \item{subset}{see \code{lm},}
  \item{weights}{see \code{lm},}
  \item{na.action}{see \code{lm},}
  \item{model}{one of \code{"fd"}, \code{"random"} or \code{"pooling"},}
  \item{index}{the indexes, see \code{\link{pdata.frame}},}
  \item{R}{the number of points for the gaussian quadrature,}
  \item{start}{a vector of starting values,}
  \item{lower}{the lower bound for the censored/truncated dependent
     variable,}
  \item{upper}{the upper bound for the censored/truncated dependent
     variable,}
  \item{objfun}{the objective function for the fixed effect model, one
      of \code{"lsq"} for least squares and \code{"lad"} for least absolute
      deviations,}

  \item{sample}{\code{"cens"} for a censored (tobit-like) sample, \code{"trunc"} for a
        truncated sample,}
  \item{\dots}{further arguments.}
}

\details{
\code{pldv} computes two kinds of models : maximum likelihood estimator
with an assumed normal distribution for the individual effects and a
LSQ/LAD estimator for the first-difference model.
}

\value{
An object of class \code{c("plm","panelmodel")}.

}

\author{Yves Croissant}

\references{
Honor\'e, Bo (1992). Trimmed LAD and least squares estimation of
truncated and censored regression models with fixed effects,
\emph{Econometrica}, \bold{60}(3), pp. 533--565.

}

\keyword{regression}
