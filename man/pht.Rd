\name{pht}
\alias{pht}
\alias{summary.pht}
\alias{print.summary.pht}
\title{Hausman-Taylor Estimator for Panel Data}

\description{
The Hausman-Taylor estimator is an instrumental variable estimator without external instruments.
}

\usage{
pht(formula, data, subset, na.action, index = NULL, ...)
\method{summary}{pht}(object, ...)
\method{print}{summary.pht}(x, digits = max(3, getOption("digits") - 2),
     width = getOption("width"), subset = NULL, ...)
}

\arguments{
 \item{formula}{a symbolic description for the model to be estimated,}
 \item{object,x}{an object of class \code{"plm"},}
 \item{data}{a \code{data.frame},}
 \item{subset}{see \code{\link{lm}} for \code{"plm"}, a character or
   numeric vector indicaing asubset of the table of coefficient to be
   printed for \code{"print.summary.plm"},}
 \item{na.action}{see \code{\link{lm}},}
 \item{index}{the indexes,}
 \item{digits}{digits,}
 \item{width}{the maximum length of the lines in the print output,}
 \item{\dots}{further arguments.}
}

\details{ \code{pht} estimates panels models using the Hausman-Taylor estimator. The model is specified a two-part formula, the second part containing the exogenous variables.
}

\value{
An object of class \code{c("pht", "plm", "panelmodel")}.

A \code{"pht"} object contains the same element as \code{plm}, with a further argument called \code{varlist} which describes the typology of the variables.  It has \code{summary} and \code{print.summary} methods.
}

\author{Yves Croissant}

\references{
Hausman, J.A. and  Taylor W.E. (1981) Panel data and unobservable
individual effects, \emph{Econometrica}, \bold{49}, pp.1377--1398.
}

\examples{
data("Wages", package = "plm")
ht <- plm(lwage~wks+south+smsa+married+exp+I(exp^2)+
          bluecol+ind+union+sex+black+ed | 
          sex+black+bluecol+south+smsa+ind,
          data=Wages,model="ht",index=595)
summary(ht)
}

\keyword{regression}
