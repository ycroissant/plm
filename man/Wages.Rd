\name{Wages}
\docType{data}
\alias{Wages}

\title{Panel Data of Individual Wages}

\description{
A panel of 595 individuals from 1976 to 1982, taken from the Panel Study of Income Dynamics (PSID).\cr\cr
The data are organized as a stacked time series/balanced panel, see \bold{Examples} on how to convert to a \code{pdata.frame}.

 \emph{total number of observations} :  4165

 \emph{observation} :  individuals

 \emph{country} :  United States
}

\usage{data(Wages)}

\format{A data frame containing:

 \describe{
  \item{exp}{years of full-time work experience.}
  \item{wks}{weeks worked.}
  \item{bluecol}{blue collar?}
  \item{ind}{works in a manufacturing industry?}
  \item{south}{resides in the south?}
  \item{smsa}{resides in a standard metropolitan statistical area?}
  \item{married}{married?}
  \item{sex}{a factor with levels \code{"male"} and \code{"female"}}
  \item{union}{individual's wage set by a union contract?}
  \item{ed}{years of education.}
  \item{black}{is the individual black?}
  \item{lwage}{logarithm of wage.}
 }
}

\source{
Online complements to Baltagi (2001):

 \url{http://www.wiley.com/legacy/wileychi/baltagi/}
 
Online complements to Baltagi (2013):

  \url{http://bcs.wiley.com/he-bcs/Books?action=resource&bcsId=4338&itemId=1118672321&resourceId=13452}
}

\references{
Baltagi, Badi H. (2001) \emph{Econometric Analysis of Panel Data}, 2nd ed., John Wiley and Sons.

Baltagi, Badi H. (2013) \emph{Econometric Analysis of Panel Data}, 5th ed., John Wiley and Sons.

Cornwell, C. and P. Rupert (1988) \dQuote{Efficient estimation with
panel data: an empirical comparison of instrumental variables
estimators}, \emph{Journal of Applied Econometrics}, \bold{3}, pp. 149--155.
}

\examples{
# data set 'Wages' is organized as a stacked time series/balanced panel
data("Wages", package = "plm")
Wag <- pdata.frame(Wages, index=595)
}

\keyword{datasets}
