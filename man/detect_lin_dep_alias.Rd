\name{detect_lin_dep}
\alias{detect_lin_dep}
\alias{detect_lin_dep.matrix}
\alias{detect_lin_dep.data.frame}
\alias{detect_lin_dep.plm}
\alias{alias}
\alias{alias.plm}
\alias{alias.pdata.frame}

\title{Functions to detect linear dependence}

\description{
Little helper functions to aid users to detect linear dependent columns in a two-dimensional 
data structure, especially in a (transformed) model matrix - typically useful in 
interactive mode during model building phase.
}

\usage{
detect_lin_dep(object, \dots)
\method{detect_lin_dep}{matrix}(object, suppressPrint = FALSE, \dots)
\method{detect_lin_dep}{data.frame}(object, suppressPrint = FALSE, \dots)
\method{detect_lin_dep}{plm}(object, suppressPrint = FALSE, \dots)

\method{alias}{plm}(object, \dots)
\method{alias}{pdata.frame}(object,  
      model = c("pooling", "within", "Between", "between", "mean", "random", "fd"),
      effect = c("individual", "time", "twoways"), \dots)
}


\arguments{
  \item{object}{for \code{detect_lin_dep}: an object which should be checked for linear dependence
  (of class \code{"matrix"}, \code{"data.frame"}, or \code{"plm"}); for \code{alias}:
  either an estimated model of class \code{"plm"} or a \code{"pdata.frame"}. Usually, one wants to
  input a model matrix here or check an already estimated plm model,} 
  \item{suppressPrint}{for \code{detect_lin_dep} only: logical indicating whether a message shall be printed;
  defaults to printing the message, i. e. to \code{suppressPrint = FALSE},}
  \item{model, effect}{for \code{alias.pdata.frame} only: model and effect to specify the data 
  transformation, see \code{\link[plm]{plm}},}
  \item{\dots}{further arguments.}
}

\details{

  Linear dependence of columns/variables is (usually) readily avoided
 when building one's model.  However, linear dependence is sometimes not
 obvious and harder to detect for less experienced applied
 statisticians. The so called "dummy variable trap" is a common and
 probably the best--known fallacy of this kind (see e. g. Wooldridge
 (2016), sec. 7-2.). When building linear models with \code{lm} or
 \code{plm}'s \code{pooling} model, linear dependence in one's model is
 easily detected, at times post hoc.

However, linear dependence might also occur after some transformations
of the data, albeit it is not present in the untransformed data. The
within transformation (also called fixed effect transformation) used in
the \code{"within"} model can result in such linear dependence and this
is harder to come to mind when building a model. See \bold{Examples} for
two examples of linear dependent columns after the within
transformation: ex. 1) the transformed variables have the opposite sign
of one another; ex. 2) the transformed variables are identical.

During \code{plm}'s model estimation, linear dependent columns and their
corresponding coefficients in the resulting object are silently dropped,
while the corresponding model frame and model matrix still contain the
affected columns.  The plm object contains an element \code{aliased}
which indicates any such aliased coefficients by a named logical.

Both functions, \code{detect_lin_dep} and \code{alias}, help to detect
linear dependence and accomplish almost the same: \code{detect_lin_dep}
is a stand alone implementation while \code{alias} is a wrapper around
\code{\link[stats]{alias.lm}}, extending the \code{alias} generic to
classes \code{"plm"} and \code{"pdata.frame"}.  \code{alias} hinges on the
availability of the package \CRANpkg{MASS} on the system. Not all
arguments of \code{alias.lm} are supported.  Output of \code{alias} is
more informative as it gives the linear combination of dependent columns
(after data transformations, i. e. after (quasi)-demeaning) while
\code{detect_lin_dep} only gives columns involved in the linear
dependence in a simple format (thus being more suited for automatic
post--processing of the information).  }

\value{ For \code{detect_lin_dep}: A named numeric vector containing
column numbers of the linear dependent columns in the object after data
transformation, if any are present. \code{NULL} if no linear dependent
columns are detected.

For \code{alias}: return value of \code{\link[stats]{alias.lm}} run on
the (quasi-)demeaned model, i. e. the information outputted applies to
the transformed model matrix, not the original data.  }

\references{
Wooldridge, J.M. (2016) \emph{Introductory Econometrics: A Modern Approach}, 6th ed., Cengage Learning,
Boston, sec. 7-2, pp. 206--211.
}

\seealso{
  \code{\link[stats]{alias}}, 
  \code{\link[stats]{model.matrix}} and especially \code{plm}'s \code{\link[plm]{model.matrix}} for
  (transformed) model matrices, \code{\link[plm]{model.frame}}.
}

\examples{
### Example 1 ###
# prepare the data
data("Cigar" , package = "plm")
Cigar[ , "fact1"] <- c(0,1)
Cigar[ , "fact2"] <- c(1,0)
Cigar.p <- pdata.frame(Cigar)

# setup a pFormula and a model frame
form <- price ~ 0 + cpi + fact1 + fact2
#pform <- pFormula(price ~ 0 + cpi + fact1 + fact2)
#mf <- model.frame(pform, data = Cigar.p)
mf <- model.frame(Cigar.p, form)
# no linear dependence in the pooling model's model matrix
# (with intercept in the formula, there would be linear depedence)
#MM detect_lin_dep(model.matrix(pform, data = mf, model = "pooling"))
detect_lin_dep(model.matrix(mf, model = "pooling"))
# linear dependence present in the FE transformed model matrix
#MM modmat_FE <- model.matrix(pform, data = mf, model = "within")
modmat_FE <- model.matrix(mf, model = "within")
detect_lin_dep(modmat_FE)
#MM mod_FE <- plm(pform, data = Cigar.p, model = "within")
mod_FE <- plm(form, data = Cigar.p, model = "within")
detect_lin_dep(mod_FE) 
alias(mod_FE) # => fact1 == -1*fact2
#plm(pform, data = mf, model = "within")$aliased # "fact2" indicated as aliased
plm(form, data = mf, model = "within")$aliased # "fact2" indicated as aliased

# look at the data: after FE transformation fact1 == -1*fact2
head(modmat_FE)
all.equal(modmat_FE[ , "fact1"], -1*modmat_FE[ , "fact2"])

### Example 2 ###
# Setup the data:
# Assume CEOs stay with the firms of the Grunfeld data
# for the firm's entire lifetime and assume some fictional
# data about CEO tenure and age in year 1935 (first observation
# in the data set) to be at 1 to 10 years and 38 to 55 years, respectively.
# => CEO tenure and CEO age increase by same value (+1 year per year).
data("Grunfeld", package = "plm")
set.seed(42)
# add fictional data
Grunfeld$CEOtenure <- c(replicate(10, seq(from=s<-sample(1:10,  1), to=s+19, by=1)))
Grunfeld$CEOage    <- c(replicate(10, seq(from=s<-sample(38:65, 1), to=s+19, by=1)))

# look at the data
head(Grunfeld, 50)

form <- inv ~ value + capital + CEOtenure + CEOage
#pform <- pFormula(inv ~ value + capital + CEOtenure + CEOage)
#mf <- model.frame(pform, data=pdata.frame(Grunfeld))
mf <- model.frame(pdata.frame(Grunfeld), form)
# no linear dependent columns in original data/pooling model
#MM modmat_pool <- model.matrix(pform, data = mf, model="pooling")
modmat_pool <- model.matrix(mf, model="pooling")
detect_lin_dep(modmat_pool)
#MM mod_pool <- plm(pform, data = Grunfeld, model = "pooling")
mod_pool <- plm(form, data = Grunfeld, model = "pooling")
alias(mod_pool)

# CEOtenure and CEOage are linear dependent after FE transformation
# (demeaning per individual)
#MM modmat_FE <- model.matrix(pform, data = mf, model="within")
modmat_FE <- model.matrix(mf, model="within")
detect_lin_dep(modmat_FE)
#MM mod_FE <- plm(pform, data = Grunfeld, model = "within")
mod_FE <- plm(form, data = Grunfeld, model = "within")
detect_lin_dep(mod_FE)
alias(mod_FE)

# look at the transformed data: after FE transformation CEOtenure == 1*CEOage
head(modmat_FE, 50)
all.equal(modmat_FE[ , "CEOtenure"], modmat_FE[ , "CEOage"])
}

\author{Kevin Tappe}

\keyword{manip}
\keyword{array}
