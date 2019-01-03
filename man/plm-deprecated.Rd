\name{plm-deprecated}
\alias{plm-deprecated}
\alias{dynformula}
\alias{print.dynformula}
\alias{formula.dynformula}
\alias{plm.data}


\title{Deprecated functions of plm}

\description{
  \code{dynformula}, \code{pht},  \code{plm.data}, and \code{pvcovHC} are deprecated
  functions which could be removed from \pkg{plm} in a near future.

  \code{dynformula} was used to construct a dynamic formula which was
  the first argument of \code{pgmm}. \code{pgmm} uses now multi-part
  formulas.

  \code{pht} estimates the Hausman-Taylor model, which can now be
  estimated using the more general \code{plm} function.

  \code{plm.data} is replaced by \code{pdata.frame}.
  
  \code{pvcovHV} is replaced by \code{vcovHC}.

}

\usage{
dynformula(formula, lag.form = NULL, diff.form = NULL, log.form = NULL)
pht(formula, data, subset, na.action, model = c("ht", "am", "bms"), index = NULL, \dots)
\method{summary}{pht}(object, \dots)
\method{print}{summary.pht}(x, digits = max(3, getOption("digits") - 2),
     width = getOption("width"), subset = NULL, \dots)
plm.data(x, indexes = NULL)
}

\arguments{
  \item{formula}{a formula,}
  \item{lag.form}{a list containing the lag structure of each variable in the formula,}
  \item{diff.form}{a vector (or a list) of logical values indicating whether variables should be differenced,}
  \item{log.form}{a vector (or a list) of logical values indicating
    whether variables should be in logarithms.}
 \item{object, x}{an object of class \code{"plm"},}
 \item{data}{a \code{data.frame},}
 \item{subset}{see \code{\link{lm}} for \code{"plm"}, a character or
   numeric vector indicating a subset of the table of coefficient to be
   printed for \code{"print.summary.plm"},}
 \item{na.action}{see \code{\link{lm}},}
 \item{model}{one of \code{"ht"} for Hausman--Taylor, \code{"am"} for Amemiya--MaCurdy
 and \code{"bms"} for Breusch--Mizon--Schmidt,}
 \item{index}{the indexes,}
 \item{digits}{digits,}
 \item{width}{the maximum length of the lines in the print output,}
 \item{\dots}{further arguments.}
 \item{indexes}{a vector (of length one or two) indicating the
  (individual and time) indexes (see Details).}  
}
