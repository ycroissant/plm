\name{pmg}
\alias{pmg}
\alias{summary.pmg}
\alias{print.summary.pmg}

\title{Mean Groups (MG), Demeaned MG and CCE MG estimators}

\description{
Mean Groups (MG), Demeaned MG (DMG) and Common Correlated Effects MG
(CCEMG) estimators for heterogeneous panel models, possibly with
common factors (CCEMG)
}

\usage{
pmg(formula, data, subset, na.action,
 model = c("mg", "cmg", "dmg"),
 index = NULL, trend = FALSE, ...)
\method{summary}{pmg}(object, ...)
\method{print}{summary.pmg}(x,digits = max(3, getOption("digits") -
2), width = getOption("width"),...)
}

\arguments{
 \item{formula}{a symbolic description of the model to be estimated,}
 \item{object, x}{an object of class \code{pmg},}
 \item{data}{a \code{data.frame},}
 \item{subset}{see \code{\link{lm}},}
 \item{na.action}{see \code{\link{lm}},}
 \item{model}{one of \code{c("mg", "cmg", "dmg")},}
 \item{index}{the indexes, see \code{\link{pdata.frame}},}
 \item{trend}{logical specifying whether an individual-specific trend has to be included,}
 \item{digits}{digits,}
 \item{width}{the maximum length of the lines in the print output,}
 \item{\dots}{further arguments.}
}

\value{
An object of class \code{c("pmg", "panelmodel")} containing:
 \item{coefficients}{the vector of coefficients,}
 \item{residuals}{the vector of residuals,}
 \item{fitted.values}{the vector of fitted.values,}
 \item{vcov}{the covariance matrix of the coefficients,}
 \item{df.residual}{degrees of freedom of the residuals,}
 \item{model}{a data.frame containing the variables used for the
    estimation,}
 \item{call}{the call,}
 \item{sigma}{always \code{NULL}, \code{sigma} is here only for
   compatibility reasons (to allow using the same \code{summary} and
   \code{print} methods as \code{pggls}),}
 \item{indcoef}{the matrix of individual coefficients from separate time
   series regressions.}
}
  
\details{
\code{pmg} is a function for the estimation of linear panel models with
heterogeneous coefficients by the Mean Groups
estimator. \code{model="mg"} specifies the standard Mean Groups
estimator, based on the average of individual time series
regressions. If \code{model="dmg"} the data are demeaned
cross-sectionally, which is believed to reduce the influence of common
factors (and is akin to what is done in homogeneous panels when
\code{model="within"} and \code{effect="time"}. Lastly, if
\code{model="cmg"} then the CCEMG estimator is employed: this latter is
consistent under the hypothesis of unobserved common factors and
idiosyncratic factor loadings; it works by augmenting the model by
cross-sectional averages of the dependent variable and regressors in
order to account for the common factors, and adding individual
intercepts and possibly trends.
}

\author{Giovanni Millo}

\references{
M. Hashem Pesaran (2006), Estimation and Inference in Large Heterogeneous Panels with a Multifactor Error Structure, \emph{Econometrica}, \bold{74(4)}, pp. 967--1012. 
}

\examples{
data("Produc", package = "plm")
## Mean Groups estimator
mgmod <- pmg(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc)
summary(mgmod)
## demeaned Mean Groups
dmgmod <- pmg(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data =
Produc, model="dmg")
summary(dmgmod)
## Common Correlated Effects Mean Groups
ccemgmod <- pmg(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data
= Produc, model="cmg")
summary(ccemgmod)
}

\keyword{regression}
