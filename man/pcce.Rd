\name{pcce}
\alias{pcce}
\alias{summary.pcce}
\alias{print.summary.pcce}

\title{Common Correlated Effects estimators}

\description{
Common Correlated Effects Mean Groups (CCEMG) and Pooled (CCEP) estimators for panel data with
common factors (balanced or unbalanced)
}

\usage{
pcce(formula, data, subset, na.action,
     model=c("mg", "p"),
     index = NULL, trend = FALSE, \dots)
\method{summary}{pcce}(object, \dots)
\method{print}{summary.pcce}(x, digits = max(3, getOption("digits") - 2),
      width = getOption("width"), \dots)
}

\arguments{
 \item{formula}{a symbolic description of the model to be estimated,}
 \item{object, x}{an object of class \code{"pcce"},}
 \item{data}{a \code{data.frame},}
 \item{subset}{see \code{lm},}
 \item{na.action}{see \code{lm},}
 \item{model}{one of \code{"mg"}, \code{"p"}, selects Mean Groups vs. Pooled CCE model,}
 \item{index}{the indexes, see \code{\link{pdata.frame}},}
 \item{trend}{logical specifying whether an individual-specific trend has to be included,}
 \item{digits}{digits,}
 \item{width}{the maximum length of the lines in the print output,}
 \item{\dots}{further arguments.}
}

\value{
An object of class \code{c("pcce", "panelmodel")} containing:
 \item{coefficients}{the vector of coefficients,}
 \item{residuals}{the vector of (defactored) residuals,}
 \item{stdres}{the vector of (raw) residuals,}
 \item{tr.model}{the transformed data after projection on H,}
 \item{fitted.values}{the vector of fitted values,}
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
 
 %% about pooled vs. random - email from Giovanni:
 %% TODO: one day, incorporate this information in the visible man page
 % The "random" vs. "pooling" issue is rooted in the approach of Wooldridge,
 % where the FGLS is consistent under "standard" RE, and therefore it is
 % presented among RE methods. But then, if you see it in the
 % demeaning/transforming framework, then FGLS is G(eneral)GLS applied to
 % untransformed ("pooled") data, FEGLS is GGLS on within transformed data and 
 % FDGLS is GGLS on first differenced. So "pooled" GGLS would be more
 % appropriate; but this is in turn less telling -- nobody calls it like this.

 % call the random/pooling version "General FGLS". The reason is that the 
 % standard RE estimator is also an FGLS, only relying on stricter assumptions,
 % and is so much better known: so "FGLS" alone would create confusion. On the
 % contrary, FEGLS and FDGLS -- and their extended forms you propose -- are
 % unambiguous, their "standard" counterparts being OLS methods.

 
 
\details{
\code{pcce} is a function for the estimation of linear panel models by the
Common Correlated Effects Mean Groups or Pooled estimator, consistent under the
hypothesis of unobserved common factors and idiosyncratic factor
loadings. The CCE estimator works by augmenting the model by
cross-sectional averages of the dependent variable and regressors in
order to account for the common factors, and adding individual
intercepts and possibly trends.
}

\author{Giovanni Millo}

\references{
G. Kapetanios, M. Hashem Pesaran, T. Yamagata (2011), Panels with
non-stationary multifactor error structures, \emph{Journal of
 Econometrics}, \bold{160}(2), pp. 326--348. 
}

\examples{
data("Produc", package = "plm")
ccepmod <- pcce(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model="p")
summary(ccepmod)

ccemgmod <- pcce(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model="mg")
summary(ccemgmod)
}

\keyword{regression}
