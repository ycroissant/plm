% model.frame and model.matrix documented separate file

\name{pFormula}
\alias{pFormula}
\alias{as.Formula.pFormula}



\title{pFormula: An extended Formula interface for panel data}

\description{pFormula is a Formula object used in the plm package.
}

\usage{
pFormula(object)
\S3method{as.Formula}{pFormula}(x, ...)

}

\arguments{
 \item{object}{an object of class \code{"formula"}, the formula to be coerced to
 class \code{"pFormula"},}
 \item{x}{an object of class \code{"pFormula"}, to be coerced to \code{class{"Formula"}},}
 \item{\dots}{further arguments.}
}

\value{
For \code{pFormula}, the return value is an object of class
\code{c("pFormula", "Formula", "formula")}.\cr
For \code{as.Formula}, the return value is an object of class
\code{c("Formula", "formula")}.\cr
}

% \details{
% }

% \references{
% }
\seealso{
  \code{plm}'s \code{\link[plm]{model.frame}} and \code{\link[plm]{model.matrix}} to create a
  model frame for panel data and a model.matrix with data transformations applied, respectively.\cr
  \code{plm}'s \code{\link{pmodel.response}} for (transformed) response variable.\cr
  \code{\link[Formula]{Formula}} from package \code{Formula}.
}

\examples{
# First, make a pdata.frame
data(Grunfeld)
pGrunfeld <- pdata.frame(Grunfeld)

# then make a model frame from a pFormula and a pdata.frame
pform <- pFormula(inv ~ value + capital)
mf <- model.frame(pform, data = pGrunfeld)

# then construct the (transformed) model matrix (design matrix)
# from formula and model frame
modmat <- model.matrix(pform, data = mf, model = "within")
}

\author{Yves Croissant}

\keyword{classes}
