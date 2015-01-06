\name{Wages}
\docType{data}
\alias{Wages}

\title{Panel Data of Individual Wages}

\description{
A panel of 595 observations from 1976 to 1982, taken from the     Panel Study of Income Dynamics (PSID).    

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
  \item{sex}{a factor with levels \code{"male", "female"}}
  \item{union}{individual's wage set by a union contract?}
  \item{ed}{years of education.}
  \item{black}{is the individual black?}
  \item{lwage}{logarithm of wage.}
 }
}

\source{
Online complements to Baltagi (2001).

\url{http://www.wiley.com/legacy/wileychi/baltagi/}
}

\references{ Baltagi, Badi H. (2003) \emph{Econometric Analysis of Panel
Data}, 2nd ed., John Wiley and Sons.

Cornwell, C. and P. Rupert (1988) \dQuote{Efficient estimation with
panel data: an empirical comparison of instrumental variables
estimators}, \emph{Journal of Applied Econometrics}, \bold{3}, 149--155.
}

\keyword{datasets}
