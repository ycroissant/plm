\name{detect_lin_dep}
\alias{detect_lin_dep}
\alias{detect_lin_dep.matrix}
\alias{detect_lin_dep.data.frame}

\title{A function to detect linear dependence}

\description{
Little helper function to aid users to detect linear dependent columns in a data structure,
especially in a (transformed) model matrix - typically useful in interactive mode during model
building phase.
}

\usage{
detect_lin_dep(object, ...)
\method{detect_lin_dep}{matrix}(object, suppressPrint = FALSE, ...)
\method{detect_lin_dep}{data.frame}(object, suppressPrint = FALSE, ...)

} 


\arguments{
  \item{object}{an object which should be checked for linear dependence
  (of class \code{"matrix"} or \code{"data.frame"}). Usually, one wants to
  input a model matrix here,} 
  \item{suppressPrint}{logical indicating whether a message shall be printed;
  defaults to printing the message, i. e. to \code{suppressPrint = FALSE},}
  \item{\dots}{further arguments.}
}

\details{
Linear dependence of columns/variables is (usually) readily avoided when building one's model.
 However, linear dependence sometimes comes in a disguised fashion and is thus harder to detect for
less experienced applied statisticians. The so called "dummy variable trap" is a common and probably
the best--known fallacy of this kind (see e. g. Wooldridge (2016), sec. 7-2.). When building linear
models with \code{lm} or the \code{pooling} model in \code{plm}, linear dependence in one's model is
easily detected, at times post hoc.

However, linear dependence might also occur after some transformations of the data, albeit it
is not present in the untransformed data. The within transformation (also called fixed effect
transformation) used in the \code{"within"} model can result in such linear dependence and this
is harder to come to mind when building a model. See \bold{Examples} for two examples of linear
dependent columns after the within transformation: ex. 1) the transformed variables have the
opposite sign of one another; ex. 2) the transformed variables are identical.
}

\value{
A named numeric vector containing column numbers of the linear dependent columns in the object,
if any are present. \code{NULL} if no linear dependent columns are detected.
}

\references{
Wooldridge, J.M. (2016) \emph{Introductory Econometrics: A Modern Approach}, 6th ed., Cengage Learning,
Boston, sec. 7-2, pp. 206--211.
}

\seealso{
  \code{\link[stats]{model.matrix}} and especially \code{plm}'s \code{\link[plm]{model.matrix}} for
  (transformed) model matrices.
}

\examples{
### Example 1 ###
# prepare the data
data(Cigar)
Cigar[ , "fact1"] <- c(0,1)
Cigar[ , "fact2"] <- c(1,0)
Cigar.p <- pdata.frame(Cigar)

# setup a pFormula and a model frame
pform <- pFormula(price ~ 0 + cpi + fact1 + fact2)
mf <- model.frame(pform, data = Cigar.p)
# no linear dependence in the pooling model's model matrix
# (with intercept in the formula, there would be linear depedence)
detect_lin_dep(model.matrix(pform, data = mf, model = "pooling"))

# linear dependence present in the FE transformed model matrix
modmat_FE <- model.matrix(pform, data = mf, model = "within")
detect_lin_dep(modmat_FE)

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
data(Grunfeld, package = "plm")
set.seed(42)
# add fictional data
Grunfeld$CEOtenure <- c(replicate(10, seq(from=s<-sample(1:10,  1), to=s+19, by=1)))
Grunfeld$CEOage    <- c(replicate(10, seq(from=s<-sample(38:65, 1), to=s+19, by=1)))

# look at the data
head(Grunfeld, 50)

pform <- pFormula(inv ~ value + capital + CEOtenure + CEOage)
mf <- model.frame(pform, data=pdata.frame(Grunfeld))

# no linear dependent columns in original data/pooling model
modmat_pool <- model.matrix(pform, data = mf, model="pooling")
detect_lin_dep(modmat_pool)

# CEOtenure and CEOage are linear dependent after FE transformation
# (demeaning per individual)
modmat_FE <- model.matrix(pform, data = mf, model="within")
detect_lin_dep(modmat_FE)

# look at the data: after FE transformation CEOtenure == CEOage
head(modmat_FE, 50)
all.equal(modmat_FE[ , "CEOtenure"], modmat_FE[ , "CEOage"])
}

\author{Kevin Tappe}

\keyword{manip}
\keyword{array}