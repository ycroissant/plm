\name{ercomp}
\alias{ercomp}
\alias{ercomp.formula}
\alias{ercomp.plm}
\alias{print.ercomp}

\title{Estimation of the error components}

\description{
  This function enables the estimation of the variance components of a panel model.
}

\usage{
ercomp(object, \dots)
\method{ercomp}{formula}(object, data,
                         effect = c("individual", "time","twoways", "nested"), 
                         method = NULL, models = NULL,
                         dfcor = NULL, index = NULL, \dots)
\method{ercomp}{plm}(object, \dots)
\method{print}{ercomp}(x, digits= max(3, getOption("digits") - 3), \dots)
}

\arguments{
  \item{object}{a \code{formula} or a \code{plm} object,}
  \item{data}{a \code{data.frame},}
  \item{effect}{the effects introduced in the model, see
    \code{\link{plm}} for details,}
  \item{method}{method of estimation for the variance components,
    see \code{\link{plm}} for details,}
  \item{models}{the models used to estimate the variance components (an
  alternative to the previous argument),}
  \item{dfcor}{a numeric vector of length 2 indicating which degree of
  freedom should be used,}
  \item{index}{the indexes,}
  \item{x}{an \code{ercomp} object,}
  \item{digits}{digits,}
  \item{\dots}{further arguments.}
}

\value{  
  An object of class \code{"ercomp"}: a list containing
\itemize{
  \item \code{sigma2} a named numeric with estimates of the variance components,
  \item \code{theta} contains the parameter(s) used for the transformation
  of the variables: For a one-way model, a numeric corresponding to the selected
  effect (individual or time); for a two-ways model a list of length 3 with the 
  parameters. In case of a balanced model, the numeric has length 1 while for an
  unbalanced model, the numerics' length equal the number of observations.
}
}

%\details{
%}

\references{

  Amemiya, T. (1971) The estimation of the variances in a
  variance--components model, \emph{International Economic Review}, \bold{12}(1), pp. 1--13.

  Nerlove, M. (1971) Further evidence on the estimation of dynamic
  economic relations from a time series of cross sections,
  \emph{Econometrica}, \bold{39}(2), pp. 359--382.

  Swamy, P.A.V.B. and Arora, S.S. (1972) The exact finite sample
  properties of the estimators of coefficients in the error components
  regression models, \emph{Econometrica}, \bold{40}(2), pp. 261--275.

  Wallace, T.D. and Hussain, A. (1969) The use of error components
  models in combining cross section with time series data,
  \emph{Econometrica}, \bold{37}(1), pp. 55--72.

}
\seealso{
  \code{\link{plm}} where the estimates of the variance components are used if a random effects model is estimated
}

\examples{
data("Produc", package = "plm")
# an example of the formula method
ercomp(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc,
       method = "walhus", effect = "time")
# same with the plm method
z <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
         data = Produc, random.method = "walhus",
         effect = "time", model = "random")
ercomp(z)
# a two-ways model
ercomp(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc,
       method = "amemiya", effect = "twoways")
}
\author{Yves Croissant}
\keyword{regression}
