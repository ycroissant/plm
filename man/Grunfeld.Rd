\name{Grunfeld}
\docType{data}
\alias{Grunfeld}
\title{Grunfeld's Investment Data}

\description{
  A balanced panel of 10 observational units (firms) from 1935 to 1954

 \emph{total number of observations} :  200

 \emph{observation} :  production units

 \emph{country} :  United States

}

\usage{data(Grunfeld)}

\format{A data frame containing :
  \describe{
    \item{firm}{observation}
    \item{year}{date}
    \item{inv}{gross Investment}
    \item{value}{value of the firm}
    \item{capital}{stock of plant and equipment}
  }
}

\source{
Online complements to Baltagi (2001):

 \url{http://www.wiley.com/legacy/wileychi/baltagi/}
 \url{http://www.wiley.com/legacy/wileychi/baltagi/supp/Grunfeld.fil}
 
Online complements to Baltagi (2013):

  \url{http://bcs.wiley.com/he-bcs/Books?action=resource&bcsId=4338&itemId=1118672321&resourceId=13452}
}

\note{
  The Grunfeld data as provided in package \code{plm} is the same data as used in Baltagi (2001), see \bold{Examples} below.
  
  NB:\cr
  Various versions of the Grunfeld data circulate online. Also, various text books (and also varying among editions)
  and papers use different subsets of the original Grunfeld data, some of which contain errors in a few data points
  compared  to the original data used by Grunfeld (1958) in his PhD thesis. See Kleiber/Zeileis (2010) and its
  accompanying website for a comparision of various Grunfeld data sets in use.
}

\references{
  Baltagi, Badi H. (2001) \emph{Econometric Analysis of Panel Data}, 2nd ed., John Wiley and Sons;\cr
  accompanying website \url{http://www.wiley.com/legacy/wileychi/baltagi/}.
  
  Baltagi, Badi H. (2013) \emph{Econometric Analysis of Panel Data}, 5th ed., John Wiley and Sons;\cr
  accompanying website \url{http://bcs.wiley.com/he-bcs/Books?action=resource&bcsId=4338&itemId=1118672321&resourceId=13452}.
  
  Grunfeld, Yehuda (1958) \emph{The Determinants of Corporate Investment}, Ph.D. thesis, Department of Economics, University of Chicago.
  
  Kleiber, C./Zeileis, A. (2010) \dQuote{The Grunfeld Data at 50}, \emph{German Economic Review}, \bold{11}(4), pp. 404--417,
  \url{http://dx.doi.org/10.1111/j.1468-0475.2010.00513.x};\cr
  website accompanying the paper with various variants of the Grunfeld data: \url{http://statmath.wu-wien.ac.at/~zeileis/grunfeld/}.
}

\seealso{For the complete Grunfeld data (11 firms), see
  \code{\link[AER]{Grunfeld}}, in the \code{AER} package.
}

\examples{
\dontrun{
# Compare plm's Grunfeld data to Baltagi's (2001) Grunfeld data:
  data(Grunfeld, package="plm")
  Grunfeld_baltagi2001 <- read.csv("http://www.wiley.com/legacy/wileychi/
    baltagi/supp/Grunfeld.fil", sep="", header = FALSE)
  library(compare)
  compare::compare(Grunfeld, Grunfeld_baltagi2001, allowAll = T) # same data set
  }
}

\keyword{datasets}
