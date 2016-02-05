\name{pFormula}
\alias{pFormula}
\alias{as.Formula.pFormula}
\alias{model.frame.pFormula}
\alias{model.matrix.pFormula}

\title{pFormula: An extended Formula interface for panel data}

\description{pFormula is a Formula object, with methods useful for panel data.
}

\usage{
pFormula(object)
\S3method{as.Formula}{pFormula}(x, ...)
\S3method{model.frame}{pFormula}(formula, data, ...,
                                 lhs = NULL, rhs = NULL)
\S3method{model.matrix}{pFormula}(object, data,
                                  model = c("pooling","within","Between",
  "between","mean","random","fd"), 
                                  effect = c("individual","time","twoways"),
                                  rhs = 1,
                                  theta = NULL, ...)
}

\arguments{
 \item{object, formula, x}{an object of class \code{"pFormula"},}
 \item{data}{a \code{data.frame}, see Details,}
 \item{effect}{the effects introduced in the model, one of
  \code{"individual"}, \code{"time"} or \code{"twoways"},}
 \item{model}{one of \code{"pooling"}, \code{"within"},
    \code{"between"}, \code{"random",} \code{"fd"} and \code{"ht"},}
 \item{theta}{the parameter for the transformation if \code{model = "random"},}
 \item{lhs}{,}
 \item{rhs}{,}
 \item{\dots}{further arguments.}
}

\value{
An object of class \code{c("pFormula", "Formula")}.
}

\details{
The \code{lhs} and \code{rhs} arguments are inherited from
\code{Formula}. The \code{model.frame} method returns a
\code{pdata.frame} object. The \code{model.matrix} method builds a model matrix
with transformations performed as specified by the \code{model} and \code{effect}
arguments, in this case the supplied \code{data} argument should be
a model frame (if not, it is tried to contruct the model frame from the data).
}

% \references{
% }
% \seealso{
% }
% \examples{
% }

\author{Yves Croissant}

\keyword{classes}
