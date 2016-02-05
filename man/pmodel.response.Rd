\name{pmodel.response}
\alias{pmodel.response}
\alias{pmodel.response.data.frame}
\alias{pmodel.response.pFormula}
\alias{pmodel.response.plm}

\title{A function to extract the model.response}

\description{
pmodel.response has several methods to conveniently extract the
response of several objects.
}

\usage{
pmodel.response(object, ...)
\method{pmodel.response}{data.frame}(object,
                                     model = c("pooling","within","Between",
                                               "between","mean","random","fd"),
                                    effect = c("individual","time","twoways"),
                                    lhs = NULL,
                                    theta = NULL, ...)
\method{pmodel.response}{pFormula}(object, data,
                                   model = c("pooling","within","Between",
                                             "between","mean","random","fd"),
                                   effect = c("individual","time","twoways"),
                                   lhs = NULL,
                                   theta = NULL, ...)
\method{pmodel.response}{plm}(object, ...)
}

\arguments{
  \item{object}{an object of class \code{"plm"}, or a formula of class \code{"pFormula"},}
  \item{data}{a \code{data.frame}, which is a model frame (if not, it is tried to contruct
  the model frame from the data)}
  \item{effect}{the effects introduced in the model, one of
  \code{"individual"}, \code{"time"} or \code{"twoways"},}
  \item{model}{one of \code{"pooling"}, \code{"within"},
    \code{"between"}, \code{"random",} \code{"fd"} and \code{"ht"},}
  \item{theta}{the parameter for the transformation if \code{model = "random"},}
  \item{lhs}{,}
  \item{\dots}{further arguments.}
}

\value{
A numeric vector.
}

\details{
The model response is extracted from a \code{data.frame}, a \code{pFormula} + \code{data} or a
\code{plm} object, and the transformation
specified by \code{effect} and \code{model} is applied to it. 
}

% \references{
% }

% \seealso{
% }

% \examples{
% }

\author{Yves Croissant}

\keyword{manip}
