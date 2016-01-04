\name{pgmm}
\alias{pgmm}
\alias{summary.pgmm}
\alias{print.summary.pgmm}
\title{Generalized Method of Moments (GMM) Estimation for Panel Data}

\description{
Generalized method of moments estimation for static or dynamic models with panel data.
}

\usage{
pgmm(formula, data, subset, na.action,
     effect = c("twoways", "individual"),
     model = c("onestep", "twosteps"),
     collapse = FALSE,
     lost.ts = NULL,
     transformation = c("d", "ld"),
     fsm = NULL, index = NULL, ...) 
\method{summary}{pgmm}(object, robust, time.dummies = FALSE, ...)
\method{print}{summary.pgmm}(x, digits = max(3, getOption("digits") - 2),
    width = getOption("width"),...)
}

\arguments{
  
  \item{formula}{a symbolic description for the model to be
    estimated. The preferred interface is now to indicate a multi--part
    formula, the first two parts describing the covariates and the gmm
    instruments and, if any, the third part the 'normal' instruments,}
  \item{object,x}{an object of class \code{"pgmm"},}
  \item{data}{a \code{data.frame},}
  \item{subset}{see \code{\link{lm}},}
  \item{na.action}{see \code{\link{lm}},}
  \item{effect}{the effects introduced in the model, one of
    \code{"twoways"} (the default) or \code{"individual"},}
  \item{model}{one of \code{"onestep"} (the default) or
    \code{"twosteps"},}
  \item{collapse}{if \code{TRUE}, the gmm instruments are collapsed,}
  \item{lost.ts}{the number of lost time series : if \code{NULL}, this
    is automatically computed. Otherwise, it can be defined by the user
    as a numeric vector of length 1 or 2. The first element is the
    number of lost time series in the model in difference, the second
    one in the model in level. If the second element is missing, it is
    set to the first one minus one,}
  \item{transformation}{the kind of transformation to apply to the
  model: either \code{"d"} (the default value) for the ``difference GMM''
  model or \code{"ld"} for the ``system GMM'',}
  \item{fsm}{the matrix for the one step estimator : one of \code{"I"}
  (identity matrix) or \code{"G"} (\eqn{=D'D} where \eqn{D} is the
  first--difference operator) if \code{transformation="d"}, one of
  \code{"GI"} or \code{"full"} if \code{transformation="ld"},}
  \item{index}{the indexes,}
  \item{digits}{digits,}
  \item{width}{the maximum length of the lines in the print
    output,}
  \item{robust}{if \code{TRUE}, robust inference is performed in the
    summary,}
  \item{time.dummies}{if \code{TRUE}, the estimated coefficients of time
    dummies are present in the table of coefficients,}
  \item{\dots}{further arguments.}
}

\value{
An object of class \code{c("pgmm","panelmodel")}, which has the following elements:
 
\item{coefficients}{the vector (or the list for fixed effects) of coefficients,}
\item{residuals}{the vector of residuals,}
\item{fitted.values}{the vector of fitted.values,}
\item{vcov}{the covariance matrix of the coefficients,}
\item{df.residual}{degrees of freedom of the residuals,}
\item{model}{a list containing the variables used for the
  estimation for each individual,}

\item{W}{a list containing the instruments for each individual (two
lists in case of ``sys--GMM''),}

\item{A1}{the weighting matrix for the one--step estimator,}

\item{A2}{the weighting matrix for the two--steps estimator,}

\item{call}{the call.}

It has \code{print}, \code{summary} and \code{print.summary} methods.
}

\details{
  
\code{pgmm} estimates a model for panel data with a generalized method
of moments (GMM) estimator. The description of the model to estimate is
provided with a multi--part formula which is (or which is coerced to) a
\code{Formula} object. The first right--hand side part describes the
covariates. The second one, which is mandatory, describes the gmm
instruments. The third one, which is optional, describes the 'normal'
instruments. By default, all the variables of the model which are not
used as GMM instruments are used as normal instruments with the same lag
structure as the one specified in the model.

\code{y~lag(y, 1:2)+lag(x1, 0:1)+lag(x2, 0:2) | lag(y, 2:99)} is similar
to \code{y~lag(y, 1:2) +
  lag(x1, 0:1) + lag(x2, 0:2) | lag(y, 2:99) |
lag(x1, 0:1)+lag(x2, 0:2)} and indicates that all lags from 2 of
\code{y} is used as gmm instruments.

 \code{transformation} indicates how the model should be transformed for
the estimation. \code{"d"} gives the ``difference GMM'' model (see
Arellano and Bond (1991)), \code{"ld"} the ``system GMM'' model (see
Blundell and Bond (1998)).
  
\code{pgmm} is an attempt to adapt GMM estimators available within the
DPD library for GAUSS (see Arellano and Bond 1998) and Ox (see Doornik,
Arellano and Bond 2006) and with the xtabond2 library for STATA (see
Roodman 2009).}

\references{
Arellano, M. and Bond, S. (1991) Some Tests of Specification for Panel
Data: Monte Carlo Evidence and an Application to Employment Equations,
\emph{The Review of Economic Studies}, vol. \bold{58}(2), 1991,
pp.227--297.
  
Arellano, M. and Bond, S. (1998) Dynamic Panel Data Estimation
Using DPD98 for GAUSS: A Guide for Users.

Blundell, R. and Bond, S. (1998) Initial Conditions and Moment
Restrictions in Dynamic Panel Data Models, \emph{Journal of
Econometrics}, vol. \bold{87}, pp.115--143.

Doornik, J., Arellano, M. and Bond, S. (2006) Panel Data Estimation
using DPD for Ox. \url{http://www.doornik.com/download/oxmetrics7/Ox_Packages/dpd.pdf}
  
Roodman, D. (2009) How to do xtabond2: An Introduction to ``difference''
and ``system'' GMM in Stata, \emph{Stata Journal}, vol. \bold{9}(1),
pp. 86--136. Working Paper version at
\url{http://ideas.repec.org/p/cgd/wpaper/103.html}

}

\seealso{
  
  \code{\link{dynformula}} for dynamic formulas, \code{\link{sargan}}
  for Sargan tests and \code{\link{mtest}} for Arellano--Bond's tests of
  multicollinearity.
  
}

\author{Yves Croissant}

\examples{
data("EmplUK", package = "plm")

## Arellano and Bond (1991), table 4b 
z1 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
           + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
            data = EmplUK, effect = "twoways", model = "twosteps")
summary(z1)

## Blundell and Bond (1998) table 4 (cf DPD for OX p.12 col.4)
z2 <- pgmm(log(emp) ~ lag(log(emp), 1)+ lag(log(wage), 0:1) +
           lag(log(capital), 0:1) | lag(log(emp), 2:99) +
           lag(log(wage), 2:99) + lag(log(capital), 2:99),        
           data = EmplUK, effect = "twoways", model = "onestep", 
           transformation = "ld")
summary(z2, robust = TRUE)

\dontrun{
## Same with the old formula or dynformula interface
## Arellano and Bond (1991), table 4b 
z1 <- pgmm(log(emp) ~ log(wage) + log(capital) + log(output),
            lag.form = list(2,1,0,1), data = EmplUK, 
            effect = "twoways", model = "twosteps",
            gmm.inst = ~log(emp), lag.gmm = list(c(2,99)))
summary(z1)

## Blundell and Bond (1998) table 4 (cf DPD for OX p.12 col.4)
z2 <- pgmm(dynformula(log(emp) ~ log(wage) + log(capital), list(1,1,1)), 
            data = EmplUK, effect = "twoways", model = "onestep", 
            gmm.inst = ~log(emp) + log(wage) + log(capital), 
            lag.gmm = c(2,99), transformation = "ld")
summary(z2, robust = TRUE)
}
}

\keyword{regression}
