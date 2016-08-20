\name{SumHes}
\docType{data}
\alias{SumHes}

\title{The Penn World Table, v. 5}

\description{
A panel of 125 observations from 1960 to 1985

\emph{total number of observations} :  3250

\emph{observation} :  country

\emph{country} :  World
}

\usage{data(SumHes)}

\format{A data frame containing :
 \describe{
  \item{year}{the year}
  \item{country}{the country name (factor)}
  \item{opec}{OPEC member?}
  \item{com}{communist regime? }
  \item{pop}{country's population (in thousands)}
  \item{gdp}{real GDP per capita (in 1985 US dollars)}
  \item{sr}{saving rate (in percent)}
  }
}

\source{
Online supplements to Hayashi (2000).

\url{http://fhayashi.fc2web.com/datasets.htm}
}

\references{
Hayashi, F. (2000) \emph{Econometrics}, Princeton University Press, chapter 5, pp. 358--363.

Summers, R. and A. Heston (1991) \dQuote{The Penn World Table (Mark 5): An expanded set of international comparisons, 1950--1988}, \emph{Quarterly Journal of Economics}, \bold{106}(2), pp. 327-368.
}

\keyword{datasets}
