\name{pmodel.response}
\alias{pmodel.response}
\alias{pmodel.response.data.frame}
\alias{pmodel.response.formula}
\alias{pmodel.response.plm}

\title{A function to extract the model.response}

\description{
pmodel.response has several methods to conveniently extract the
response of several objects.
}

\usage{
pmodel.response(object, \dots)
\method{pmodel.response}{data.frame}(object, \dots)
\method{pmodel.response}{formula}(object, data, \dots)
\method{pmodel.response}{plm}(object, \dots)
}

\arguments{
  \item{object}{an object of class \code{"plm"}, or a formula of class
    \code{"pFormula"},}
  \item{data}{a \code{data.frame}}
  \item{\dots}{further arguments.}
}

\value{
A pseries except if model responses' of a \code{"between"} or {"fd"} model as
these models "compress" the data (the number of observations used in estimation is smaller
than the original data due to the specific transformation). A numeric is returned for the 
"between" and "fd" model.
}

\details{
The model response is extracted from a \code{pdata.frame} (where the response
must reside in the first column; this is the case for a model frame), a \code{pFormula} + \code{data} or a
\code{plm} object, and the transformation specified by \code{effect} and \code{model} is
applied to it.\cr
Constructing the model frame first ensures proper NA handling and the response being
placed in the first column, see also \bold{Examples} for usage.
}

% \references{
% }

\seealso{
  \code{plm}'s \code{\link[plm]{model.matrix}} for (transformed) model matrix and the
  corresponding \code{\link[plm]{model.frame}} method to construct a model frame.
}

\examples{
# First, make a pdata.frame
data(Grunfeld)
pGrunfeld <- pdata.frame(Grunfeld)

# then make a model frame from a pFormula and a pdata.frame
pform <- pFormula(inv ~ value + capital)
mf <- model.frame(pform, data = pGrunfeld)

# construct (transformed) response of the within model
resp <- pmodel.response(pform, data = mf, model = "within", effect = "individual")

# retrieve (transformed) response directly from model frame
resp_mf <- pmodel.response(mf, model = "within", effect = "individual")

# retrieve (transformed) response from a plm object, i.e. an estimated model
fe_model <- plm(pform, data = pGrunfeld, model = "within")
pmodel.response(fe_model)

# same as constructed before
all.equal(resp, pmodel.response(fe_model), check.attributes = FALSE) # TRUE
}

\author{Yves Croissant}

\keyword{manip}
