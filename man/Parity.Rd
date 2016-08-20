\name{Parity}
\docType{data}
\alias{Parity}
\title{Purchasing Power Parity and other parity relationships}

\description{
 A panel of 104 quarterly observations from 1973Q1 to 1998Q4

 \emph{total number of observations} : 1768

 \emph{observation} :  country

 \emph{country} :  OECD

}

\usage{data(Parity)}

\format{A data frame containing :
  \describe{
    \item{country}{country codes: a factor with 17 levels}
    \item{time}{the quarter index, 1973Q1-1998Q4}
    \item{ls}{log spot exchange rate vs. USD}
    \item{lp}{log price level}
    \item{is}{short term interest rate}
    \item{il}{long term interest rate}
    \item{ld}{log price differential vs. USA}
    \item{uis}{U.S. short term interest rate}
    \item{uil}{U.S. long term interest rate}
    }
}
\source{
  Coakley, J., Fuertes, A. M., and Smith, R. (2006) \dQuote{Unobserved
  heterogeneity in panel time series models}, \emph{Computational
  Statistics & Data Analysis}, \bold{50}(9), 2361--2380.
}

\references{
  Coakley, J., Fuertes, A. M., and Smith, R. (2006) \dQuote{Unobserved
  heterogeneity in panel time series models}, \emph{Computational
  Statistics & Data Analysis}, \bold{50}(9), 2361--2380.

  Driscoll, J. C., and Kraay, A. C. (1998). \dQuote{Consistent
  covariance matrix estimation with spatially dependent panel data},
  \emph{Review of Economics and Statistics}, \bold{80}(4), 549--560.
}

\keyword{datasets}
