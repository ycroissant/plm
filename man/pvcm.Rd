\name{pvcm}
\alias{pvcm}
\alias{summary.pvcm}
\alias{print.summary.pvcm}

\title{Variable Coefficients Models for Panel Data}

\description{
Estimators for random and fixed effects models with variable coefficients.
}

\usage{
pvcm(formula, data, subset, na.action, effect = c("individual","time"),
     model = c("within","random"), index = NULL, \dots)
\method{summary}{pvcm}(object, \dots)
\method{print}{summary.pvcm}(x, digits = max(3, getOption("digits") -2),
    width = getOption("width"), \dots)
}

\arguments{
  \item{formula}{a symbolic description for the model to be estimated,}
  \item{object, x}{an object of class \code{"pvcm"},}
  \item{data}{a \code{data.frame},}
  \item{subset}{see \code{lm},}
  \item{na.action}{see \code{lm},}
  \item{effect}{the effects introduced in the model: one of
    \code{"individual"}, \code{"time"},}
  \item{model}{one of \code{"within"}, \code{"random"},}
  \item{index}{the indexes, see \code{\link{pdata.frame}},}
  \item{digits}{digits,}
  \item{width}{the maximum length of the lines in the print output,}
  \item{\dots}{further arguments.}
}

\details{
\code{pvcm} estimates variable coefficients models. Time or individual effects are introduced, respectively, if \code{effect="time"} or \code{effect="individual"} (the default value). 
  
Coefficients are assumed to be fixed if \code{model="within"} and
random if \code{model="random"}. In the first case, a different
model is estimated for each individual (or time period). In the second case, the Swamy (1970) model is estimated. It is a
generalized least squares model which uses the results of the previous model.  
}

\value{
An object of class \code{c("pvcm","panelmodel")}, which has the following elements :

  \item{coefficients}{the vector (or the list for fixed effects) of coefficients,}
  \item{residuals}{the vector of residuals,}
  \item{fitted.values}{the vector of fitted.values,}
  \item{vcov}{the covariance matrix of the coefficients,}
  \item{df.residual}{degrees of freedom of the residuals,}
  \item{model}{a \code{data.frame} containing the variables used for the estimation,}
  \item{call}{the call,}
  \item{Delta}{the estimation of the covariance matrix of the
   coefficients (random effect models only),}
  \item{std.error}{the standard errors for all the coefficients for each individual (within models only),}
  \code{pvcm} objects have \code{print}, \code{summary} and \code{print.summary} methods.
}

\author{Yves Croissant}

\references{
Swamy, P.A.V.B. (1970). Efficient Inference in a Random Coefficient Regression Model, \emph{Econometrica}, \bold{38}(2), pp.311--323.
}

\examples{
data("Produc", package = "plm")
zw <- pvcm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model = "within")
zr <- pvcm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model = "random")
}

\keyword{regression}
