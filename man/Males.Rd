\name{Males}
\docType{data}
\alias{Males}
\title{Wages and Education of Young Males}

\description{
 A panel of 545 observations from 1980 to 1987

 \emph{total number of observations} :  4360

 \emph{observation} :  individuals

 \emph{country} :  United States

}

\usage{data(Males)}

\format{A data frame containing :
  \describe{
    \item{nr}{identifier}
    \item{year}{year}
    \item{school}{years of schooling}
    \item{exper}{years of experience (computed as \code{age-6-school})}
    \item{union}{wage set by collective bargaining ?}
    \item{ethn}{a factor with levels \code{black, hisp, other}}
    \item{married}{married?}
    \item{health}{health problem ?}
    \item{wage}{log of hourly wage}
    \item{industry}{a factor with 12 levels}
    \item{occupation}{a factor with 9 levels}
    \item{residence}{a factor with levels \code{rural area, north east, northern central, south}}
  }
}

\source{
Journal of Applied Econometrics data archive  
 \url{http://qed.econ.queensu.ca/jae/1998-v13.2/vella-verbeek/}.
}

\references{
 Vella, F. and  M. Verbeek (1998) \dQuote{Whose Wages Do Unions Raise? A Dynamic Model of Unionism and Wage Rate Determination for Young Men}, \emph{Journal of Applied Econometrics}, \bold{13}(2), 163--183.

 Verbeek, M. (2004) \emph{A Guide to Modern Econometrics}, John Wiley and Sons, \url{http://feb.kuleuven.be/GME/}, chapter 10.
}

\keyword{datasets}
