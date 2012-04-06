\name{pccep}
\alias{pccep}
\alias{summary.pccep}
\alias{print.summary.pccep}

\title{Common Correlated Effects Pooled estimator}

\description{
Common Corrleated Effects Pooled (CCEP) estimator for panel data with
common factors (balanced or unbalanced)
}

\usage{
pccep(formula, data, subset, na.action,
 residuals = c("standard", "cce","ccemg"),
 index = NULL, trend = FALSE, ...)
\method{summary}{pccep}(object, ...)
\method{print}{summary.pccep}(x,digits = max(3, getOption("digits") -
2), width = getOption("width"),...)
}

\arguments{
 \item{formula}{a symbolic description of the model to be estimated,}
 \item{object, x}{an object of class \code{pccep},}
 \item{data}{a \code{data.frame},}
 \item{subset}{see \code{lm},}
 \item{na.action}{see \code{lm},}
 \item{residuals}{one of \code{c("standard", "cce","ccemg")}, allows for
 returning different inds of residuals for diagnostic purposes,}
 \item{index}{the indexes, see \code{\link{plm.data}},}
 \item{trend}{logical specifying whether an individual-specific trend has to be included,}
 \item{digits}{digits,}
 \item{width}{the maximum length of the lines in the print output,}
 \item{\dots}{further arguments.}
}

\value{
An object of class \code{c("pccep","panelmodel")} containing:
 \item{coefficients}{the vector of coefficients,}
 \item{residuals}{the vector of residuals,}
 \item{fitted.values}{the vector of fitted.values,}
 \item{vcov}{the covariance matrix of the coefficients,}
 \item{df.residual}{degrees of freedom of the residuals,}
 \item{model}{a data.frame containing the variables used for the
    estimation,}
 \item{call}{the call,}
 \item{sigma}{always \code{NULL}, \code{sigma} is here only for
   conmpatibility reasons (to allow using the same \code{summary} and
   \code{print} methods as \code{pggls}),}
 \item{indcoef}{the matrix of individual coefficients from separate time
   series regressions.}
}
  
\details{
\code{pccep} is a function for the estimation of linear panel models by the
Common Correlated Effects Pooled estimator, consistent under the
hypothesis of unobserved common factors and idiosyncratic factor
loadings; the CCEP estimator works by augmenting the model by
cross-sectional averages of the dependent variable and regressors in
order to account for the common factors, and adding individual
intercepts and possibly trends.
}

\author{Giovanni Millo}

\references{
G. Kapetanios, M. Hashem Pesaran, T. Yamagata (2011), Panels with
non-stationary multifactor error structures, \emph{Journal of
  Econometrics}, \bold{160(2)}, 326--348. 
}

\examples{
data("Produc", package = "plm")
ccepmod <- pccep(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc)
summary(ccepmod)
}

\keyword{regression}
