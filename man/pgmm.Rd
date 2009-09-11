\name{pgmm}
\alias{pgmm}
\alias{summary.pgmm}
\alias{print.summary.pgmm}
\title{General Method of Moments Estimator for Panel Data}
\description{
  General method of moments estimator for static or dynamic models with panel data.
}
\usage{
pgmm(formula, data, effect = c("individual","twoways", "none"),
     model = c("onestep", "twosteps"),
     instruments = NULL, gmm.inst, lag.gmm, lag.form,
     transformation = c("d", "ld"),
     fsm = NULL, ...)
\method{summary}{pgmm}(object,robust, ...)
\method{print}{summary.pgmm}(x, digits = max(3, getOption("digits") - 2),
    width = getOption("width"),...)
}

\arguments{
  \item{formula}{a symbolic description for the model to be
    estimated. Should be an object of class \code{"dynformula"} or of
    class \code{"formula"}. In the later case, a \code{lag.form}
    argument should be provided,}
  \item{object,x}{an object of class \code{"pgmm"},}
  \item{data}{a \code{data.frame},}
  \item{effect}{the effects introduced in the model, one of
    \code{"individual"} or \code{"twoways"},}
  \item{model}{one of \code{"onestep"} or \code{"twosteps"},}
  \item{instruments}{a one side formula containing the normal
    instruments,}
  \item{gmm.inst}{a one side formula containing the GMM instruments,}
  \item{lag.gmm}{a list containing the lags to use for each GMM
    instrument, or a list of two named lists (\code{"d"} for difference
    and \code{"l"} for level) in case of ``system GMM'',}
  \item{lag.form}{a list containing the lags to use for each variable of
  the formula. Only relevant if \code{formula} is a standard formula and
  not a \code{dynformula},}
  \item{transformation}{the kind of transformation to apply to the model
    : either \code{"d"} (the default value) for the ``difference GMM''
    model or \code{"ld"} for the ``system GMM'',}
  \item{fsm}{the matrix for the one step estimator : one of \code{"I"}
    (identity matrix) or \code{"G"} (\eqn{=D'D} where \eqn{D} is the first
    difference operator) if \code{transformation="d"}, one of
    \code{"GI"} or \code{"full"} if \code{transformation="ld"},}
  \item{digits}{digits,}
  \item{width}{the maximum length of the lines in the print
    output,}
  \item{robust}{if \code{TRUE}, robust inference is performed in the summary,}
  \item{\dots}{further arguments.}
}

\value{
  an object of class \code{c("pgmm","panelmodel")}, which has the following elements :
  \item{coefficients}{the vector (or the list for fixed effects) of coefficients,}
  \item{residuals}{the vector of residuals,}
  \item{fitted.values}{the vector of fitted.values,}
  \item{vcov}{the covariance matrix of the coefficients,}
  \item{df.residual}{degrees of freedom of the residuals,}
  \item{model}{a list containing  the variables used for the
    estimation for each individual,}
  \item{W}{a list containing the instruments for each individual (two
    lists in case of ``sys--GMM''),}
  \item{K}{a list containing \code{K} the number of explanatory
    variables, \code{Ky} the number of lags of the dependent variable
    and \code{Kt} the number of time dummies,}
  \item{A1}{the weighting matrix for the one--step estimator,}
  \item{A2}{the weighting matrix for the two--steps estimator,}
  \item{call}{the call.}
  It has \code{print}, \code{summary} and \code{print.summary} methods.
}
\details{
  \code{pgmm} estimates a model for panel data with the general method of
  moments estimator. The description of the model to estimate is
  provided with a \code{\link{dynformula}}. The GMM instruments are
  provided by filling the \code{pgmm.inst} argument with a one side
  formula. By default, all the variables of the model which are not used
  as GMM instruments are used as normal instruments with the same lag
  structure as the one specified in the \code{dynformula}, but the user
  may also specified one side formula to indicate expicitly the
  variables to use as normal instruments. The lags used for the GMM
  instruments are indicated with the \code{lag.gmm} argument. For each
  instrument, it is either a vector of length 2 (\code{c(99,3)} indicates to use
  all the lags available up to t-3), or a scalar (2 indicates to use the
  lags 0, 1 and 2). If a single vector (or scalar) is written, then this
  lag structure is replicated for all GMM instruments. Otherwise,
  \code{lag.gmm} should be a list of length equal to the number of GMM
  instruments.
  \code{transformation} indicates how the model should be transformed
  for the estimation. \code{"d"} gives the ``difference GMM'' model (see
  Arellano and Bond (1991)), \code{"ld"} the ``system GMM'' model (see
  Blundell and Bond (1998)).
  
  \code{pgmm} is an attempt to adapt GMM estimators available within the
  DPD library for GAUSS (see Arellano and Bond 1998) and OX (see
  Doornik, Arellano and Bond 2006) and with the xtabound2 library for
  STATA (see Roodman 2006).
}  
\references{
    
  Arellano, M. and Bond, S. (1991) Some Tests of Specification
  for Panel Data: Monte Carlo Evidence and an Application to Employment
  Equations, \emph{The Review of Economic Studies}, vol. \bold{58}(2), april
  1991, pp.227--297.
  
  Arellano, M. and  Bond, S. (1998) Dynamic Panel Data Estimation
  Using DPD89 for GAUSS : a Guide for Users.

  Blundell, R. and  Bond, S. (1998) Initial Conditions and
  Moment Restrictions in Dynamic Panel Data Models, \emph{Journal of
    Econometrics}, vol. \bold{87}, pp.115--143.

  Doornik, J.,  Arellano, M. and  Bond, S. (2006) Panel Data
  Estimation using DPD for Ox.
  

  Roodman, D. (2006) How to do xtabound2 : an Introduction to
  ``difference'' and ``system'' GMM in Stata, working paper no 103,
  Center for Global Development.
  
}
\seealso{
  \code{\link{dynformula}} for dynamic formulas, \code{\link{sargan}}
  for Sargan tests and \code{\link{mtest}} for Arellano--Bond 's tests
  of mulicolinearity
}

\author{Yves Croissant}

\examples{

data("EmplUK", package="plm")

## Arellano Bond 91 table 4b 
z1 <- pgmm(log(emp)~log(wage)+log(capital)+log(output),
    lag.form = list(2,1,0,1),
    data=EmplUK, effect="twoways", model="twosteps",
    gmm.inst=~log(emp),lag.gmm=list(c(2,99)))


## Blundell and Bond tab4 (cf DPD for OX p.12 col.4)

z2 <- pgmm(dynformula(log(emp)~log(wage)+log(capital),list(1,1,1)),
    data=EmplUK, effect="twoways", model="onestep",
    gmm.inst=~log(emp)+log(wage)+log(capital),lag.gmm=c(2,99),
    transformation="ld")
summary(z2,robust=TRUE)

}
\keyword{regression}
