\name{plm}
\alias{plm}
\alias{has.intercept}
\alias{print.panelmodel}
\alias{plot.plm}

\title{Panel Data Estimators}

\description{
Linear models for panel data estimated using the \code{lm} function on transformed data.
}

\usage{
plm(formula, data, subset, weights, na.action,
    effect = c("individual", "time", "twoways", "nested"),
    model = c("within", "random", "ht", "between", "pooling", "fd"),
    random.method = NULL,
    random.models = NULL,
    random.dfcor = NULL,
    inst.method = c("bvk", "baltagi", "am", "bms"),
    restrict.matrix = NULL,
    restrict.rhs = NULL,
    index = NULL, \dots)
\method{print}{panelmodel}(x, digits = max(3, getOption("digits") - 2), 
      width = getOption("width"), \dots)
\method{plot}{plm}(x, dx = 0.2, N = NULL, seed = 1,
     within = TRUE, pooling = TRUE, between = FALSE, random = FALSE, \dots)
}

\arguments{
  \item{formula}{a symbolic description for the model to be estimated,}
  \item{x}{an object of class \code{"plm"},}
  \item{data}{a \code{data.frame},}
  \item{subset}{see \code{\link{lm}},}
  \item{weights}{see \code{\link{lm}},}
  \item{na.action}{see \code{\link{lm}}; currently, not fully supported,}
  \item{effect}{the effects introduced in the model, one of
  \code{"individual"}, \code{"time"}, \code{"twoways"}, or \code{"nested"},}
  \item{model}{one of \code{"pooling"}, \code{"within"},
    \code{"between"}, \code{"random",} \code{"fd"}, or \code{"ht"},}
  \item{random.method}{method of estimation for the variance components
    in the random effects model, one of \code{"swar"} (default),
    \code{"amemiya"}, \code{"walhus"}, or \code{"nerlove"},}
  \item{random.models}{an alternative to the previous argument, the
  models used to compute the variance components estimations are indicated,}
  \item{random.dfcor}{a numeric vector of length 2 indicating which degree of
    freedom should be used,}
  \item{inst.method}{the instrumental variable transformation: one of
    \code{"bvk"}, \code{"baltagi"}, \code{"am"}, or \code{"bms"} (see also 
    Details),}
  \item{index}{the indexes,}
  \item{restrict.matrix}{a matrix which defines linear restrictions on
    the coefficients,}
  \item{restrict.rhs}{the right hand side vector of the linear
    restrictions on the coefficients,}
  \item{digits}{number of digits for printed output,}
  \item{width}{the maximum length of the lines in the printed output,}
  \item{dx}{the half--length of the individual lines for the plot
    method (relative to x range),}
  \item{N}{the number of individual to plot,}
  \item{seed}{the seed which will lead to individual selection,}
  \item{within}{if \code{TRUE}, the within model is plotted,}
  \item{pooling}{if \code{TRUE}, the pooling model is plotted,}
  \item{between}{if \code{TRUE}, the between model is plotted,}
  \item{random}{if \code{TRUE}, the random effect model is plotted,}
  \item{\dots}{further arguments.}
}

\details{
\code{plm} is a general function for the estimation of linear panel
models. It supports the following estimation methods: pooled OLS
(\code{model = "pooling"}), fixed effects (\code{"within"}), random
effects (\code{"random"}), first--differences (\code{"fd"}), and between
(\code{"between"}). It supports unbalanced panels and two--way effects
(although not with all methods).

For random effects models, four estimators of the transformation
parameter are available by setting \code{random.method} to one of
\code{"swar"} (Swamy and Arora (1972)) (default), \code{"amemiya"}
(Amemiya (1971)), \code{"walhus"} (Wallace and Hussain (1969)), or
\code{"nerlove"} (Nerlove (1971)).

For first--difference models, the intercept is maintained (which from a
specification viewpoint amounts to allowing for a trend in the levels
model). The user can exclude it from the estimated specification the
usual way by adding \code{"-1"} to the model formula.

Instrumental variables estimation is obtained using two--part formulas,
the second part indicating the instrumental variables used. This can be
a complete list of instrumental variables or an update of the first
part. If, for example, the model is \code{y ~ x1 + x2 + x3}, with
\code{x1} and \code{x2} endogenous and \code{z1} and \code{z2} external
instruments, the model can be estimated with:

\itemize{
  \item \code{formula = y~x1+x2+x3 | x3+z1+z2},
  \item \code{formula = y~x1+x2+x3 | . -x1-x2+z1+z2}.
}

If an instrument variable estimation is requested, argument \code{inst.method}
selects the instrument variable transformation method:
\itemize{
  \item \code{"bvk"} (default) for Balestra and Varadharajan-Krishnakumar (1987),
  \item \code{"baltagi"} for Baltagi (1981),
  \item \code{"am"} for Amemiya and MaCurdy (1986),
  \item \code{"bms"} for Breusch, Mizon, and Schmidt (1989).
}

The Hausman--Taylor estimator (Hausman and Taylor (1981)) is computed with arguments
\code{random.method = "ht", model = "random", inst.method = "baltagi"} (the other
way with only \code{model = "ht"} is deprecated).
}

\value{
An object of class \code{c("plm", "panelmodel")}.

A \code{"plm"} object has the following elements :
  
\item{coefficients}{the vector of coefficients,}
\item{vcov}{the variance--covariance matrix of the coefficients,}
\item{residuals}{the vector of residuals (these are the residuals of the (quasi-)demeaned model),}
\item{weights}{(only for weighted estimations) weights as specified,}
\item{df.residual}{degrees of freedom of the residuals,}
\item{formula}{an object of class \code{"pFormula"} describing the
  model,}
\item{model}{the model frame as a \code{"pdata.frame"} containing the
  variables used for estimation: the response is in first column followed
  by the other variables, the individual and time indexes are in the 'index'
  attribute of \code{model},}
\item{ercomp}{an object of class \code{"ercomp"} providing the
 estimation of the components of the errors (for random effects models only),}
\item{aliased}{named logical vector indicating any aliased coefficients which 
are silently dropped by \code{plm} due to linearly dependent terms (see also 
\code{\link{detect.lindep}}),}
\item{call}{the call.}

% some elements not listed here...: "assign", "contrast", etc...

% \item{na.action}{if relevant, information about handling of NAs by the 
% model.frame function,}
% NB: na.action is currently not included as it is not supported

It has \code{print}, \code{summary} and \code{print.summary} methods.
The \code{summary} method creates an object of class \code{"summary.plm"} that 
extends the object it is run on with information about (inter alia) F statistic 
and (adjusted) R-squared of model, standard errors, t--values, and p--values of 
coefficients, (if supplied) the furnished vcov, see \code{\link{summary.plm}}
for further details.
}

\seealso{
\code{\link{summary.plm}} for further details about the associated
summary method and the "summary.plm" object both of which provide some
model tests and tests of coefficients.  \code{\link{fixef}} to
compute the fixed effects for "within" models (=fixed effects models).
}

\author{Yves Croissant}

\references{
  
Amemiya, T. (1971) The estimation of the variances in a
variance--components model, \emph{International Economic Review},
\bold{12}(1), pp. 1--13.

Amemiya, T. and MaCurdy, T.E. (1986) Instrumental--variable estimation
of an error components model, \emph{Econometrica}, \bold{54}(4), pp. 869--880.

Balestra, P. and Varadharajan-Krishnakumar, J. (1987) Full information
estimations of a system of simultaneous equations with error components
structure, \emph{Econometric Theory}, \bold{3}(2), pp. 223--246.
  
Baltagi, B.H. (1981) Simultaneous equations with error components,
\emph{Journal of Econometrics}, \bold{17}(2), pp. 189--200.

Baltagi, B.H.; Song, S.H.; Jung, B.C. (2001), \dQuote{The unbalanced nested error
component regression model}, \emph{Journal of Econometrics}, \bold{101}(2), pp. 357--381.

Baltagi, B.H. (2013) \emph{Econometric Analysis of Panel Data}, 5th
ed., John Wiley and Sons.

Breusch, T.S., Mizon, G.E. and Schmidt, P. (1989) Efficient estimation
using panel data, \emph{Econometrica}, \bold{57}(3), pp. 695--700.

Hausman, J.A. and Taylor, W.E. (1981) Panel data and unobservable
individual effects, \emph{Econometrica}, \bold{49}(6), pp. 1377--1398.
 
Nerlove, M. (1971) Further evidence on the estimation of dynamic
economic relations from a time series of cross sections,
\emph{Econometrica}, \bold{39}(2), pp. 359--382.

Swamy, P.A.V.B. and Arora, S.S. (1972) The exact finite sample
properties of the estimators of coefficients in the error components
regression models, \emph{Econometrica}, \bold{40}(2), pp. 261--275.

Wallace, T.D. and Hussain, A. (1969) The use of error components models
in combining cross section with time series data, \emph{Econometrica},
\bold{37}(1), pp. 55--72.

}

\examples{
data("Produc", package = "plm")
zz <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
          data = Produc, index = c("state","year"))
summary(zz)

# replicates some results from Baltagi (2013), table 3.1
data("Grunfeld", package = "plm")
p <- plm(inv ~ value + capital,
         data = Grunfeld, model = "pooling")

wi <- plm(inv ~ value + capital,
          data = Grunfeld, model = "within", effect = "twoways")

swar <- plm(inv ~ value + capital,
            data = Grunfeld, model = "random", effect = "twoways")

amemiya <- plm(inv ~ value + capital,
               data = Grunfeld, model = "random", random.method = "amemiya",
               effect = "twoways")

walhus <- plm(inv ~ value + capital,
              data = Grunfeld, model = "random", random.method = "walhus",
              effect = "twoways")

# summary and summary with a funished vcov (passed as matrix, 
# as function, and as function with additional argument)
summary(wi)
summary(wi, vcov = vcovHC(wi))
summary(wi, vcov = vcovHC)
summary(wi, vcov = function(x) vcovHC(x, method = "white2"))


# nested random effect model
# replicate Baltagi/Song/Jung (2001), p. 378 (table 6), columns SA, WH
# == Baltagi (2013), pp. 204-205
data("Produc", package = "plm")
pProduc <- pdata.frame(Produc, index = c("state", "year", "region"))
form <- log(gsp) ~ log(pc) + log(emp) + log(hwy) + log(water) + log(util) + unemp
summary(plm(form, data = pProduc, model = "random", effect = "nested"))
summary(plm(form, data = pProduc, model = "random", effect = "nested",
            random.method = "walhus"))

## Hausman-Taylor estimator and Amemiya-MaCurdy estimator
## replicate Baltagi (2005, 2013), table 7.4
data("Wages", package = "plm")
ht <- plm(lwage ~ wks + south + smsa + married + exp + I(exp ^ 2) + 
              bluecol + ind + union + sex + black + ed |
              bluecol + south + smsa + ind + sex + black |
              wks + married + union + exp + I(exp ^ 2), 
          data = Wages, index = 595,
          random.method = "ht", model = "random", inst.method = "baltagi")
summary(ht)

am <- plm(lwage ~ wks + south + smsa + married + exp + I(exp ^ 2) + 
              bluecol + ind + union + sex + black + ed |
              bluecol + south + smsa + ind + sex + black |
              wks + married + union + exp + I(exp ^ 2), 
          data = Wages, index = 595,
          random.method = "ht", model = "random", inst.method = "am")
summary(am)
}

\keyword{regression}
