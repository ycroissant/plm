\name{pggls}
\alias{pggls}
\alias{summary.pggls}
\alias{print.summary.pggls}

\title{General FGLS Estimators}

\description{
General FGLS estimators for panel data (balanced or unbalanced)
}

\usage{
pggls(formula, data, subset, na.action, effect = c("individual", "time"),
      model = c("within","random","pooling","fd"),
      index = NULL, ...)
\method{summary}{pggls}(object, ...)
\method{print}{summary.pggls}(x,digits = max(3, getOption("digits") - 2),
                              width = getOption("width"),...)
}

\arguments{
 \item{formula}{a symbolic description of the model to be estimated,}
 \item{object, x}{an object of class \code{pggls},}
 \item{data}{a \code{data.frame},}
 \item{subset}{see \code{lm},}
 \item{na.action}{see \code{lm},}
 \item{effect}{the effects introduced in the model, one of \code{"individual"} or \code{"time"},}
 \item{model}{one of \code{"within"}, \code{"pooling"}, \code{"random"}
 or \code{"fd"},}
 \item{index}{the indexes, see \code{\link{pdata.frame}},}
 \item{digits}{digits,}
 \item{width}{the maximum length of the lines in the print output,}
 \item{\dots}{further arguments.}
}

\value{
An object of class \code{c("pggls","panelmodel")} containing:
 \item{coefficients}{the vector of coefficients,}
 \item{residuals}{the vector of residuals,}
 \item{fitted.values}{the vector of fitted.values,}
 \item{vcov}{the covariance matrix of the coefficients,}
 \item{df.residual}{degrees of freedom of the residuals,}
 \item{model}{a data.frame containing the variables used for the
    estimation,}
 \item{call}{the call,}
 \item{sigma}{the estimated intragroup (or cross-sectional, if \code{effect="time"}) covariance of errors,}
}
  
\details{

  \code{pggls} is a function for the estimation of linear panel models
by general feasible generalized least squares, either with or without
fixed effects. General FGLS is based on a two-step estimation process:
first a model is estimated by OLS (\code{pooling}), fixed effects
(\code{within}) or first differences (\code{fd}), then its residuals are
used to estimate an error covariance matrix for use in a feasible-GLS
analysis.
This framework allows the error covariance structure inside every group (if
\code{effect="individual"}, else symmetric) of observations to be fully
unrestricted and is therefore robust against any type of intragroup
heteroskedasticity and serial correlation. Conversely, this structure is
assumed identical across groups and thus general FGLS estimation is
inefficient under groupwise heteroskedasticity. Note also that this
method requires estimation of \eqn{T(T+1)/2} variance parameters, thus
efficiency requires N > > T (if \code{effect="individual"}, else the
opposite).  The \code{model="random"} and \code{model="pooling"}
arguments both produce an unrestricted FGLS model as in Wooldridge,
Ch. 10, although the former is deprecated and included only for
retrocompatibility reasons.
If \code{model="within"} (the default) then a FEGLS (fixed
effects GLS, see ibid.) is estimated; if \code{model="fd"} a FDGLS
(first-difference GLS).  }

\author{Giovanni Millo}

\references{
Kiefer, N. M. (1980) Estimation of Fixed Effects Models for Time Series
of Cross-Sections with Arbitrary Intertemporal Covariance, \emph{Journal
  of Econometrics}, \bold{14}, pp. 195--202. 


Im, K. S. and Ahn, S. C. and Schmidt, P. and Wooldridge, J. M. (1999)
Efficient Estimation of Panel Data Models with Strictly Exogenous
Explanatory Variables, \emph{Journal of Econometrics}, \bold{93},
177-201.

Wooldridge, J. M. (2002) \emph{Econometric Analysis of Cross Section and Panel Data}, MIT Press.

Wooldridge, J. M. (2010) \emph{Econometric analysis of cross-section and Panel Data}, 2nd ed., MIT Press.
}

\examples{
data("Produc", package = "plm")
zz <- pggls(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model = "pooling")
summary(zz)
}

\keyword{regression}
