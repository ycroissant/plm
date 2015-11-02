\name{Crime}
\docType{data}
\alias{Crime}
\title{Crime in North Carolina}

\description{
  a panel of 90 observations (counties) from 1981 to 1987

 \emph{total number of observations} :  630

 \emph{observation} :  regional

 \emph{country} :  United States

}

\usage{data(Crime)}

\format{A data frame containing :
  \describe{
    \item{county}{county identifier}
    \item{year}{year from 1981 to 1987}
    \item{crmrte}{crimes committed per person}
    \item{prbarr}{'probability' of arrest}
    \item{prbconv}{'probability' of conviction}
    \item{prbpris}{'probability' of prison sentenc}
    \item{avgsen}{average sentence, days    }
    \item{polpc}{police per capita}
    \item{density}{people per square mile}
    \item{taxpc}{tax revenue per capita}
    \item{region}{factor. One of 'other', 'west' or 'central'.}
    \item{smsa}{factor. Does the individual reside in a SMSA (standard metropolitan statistical area)?}
    \item{pctmin}{percentage minority in 1980}
    \item{wcon}{weekly wage in construction}
    \item{wtuc}{weekly wage in trns, util, commun}
    \item{wtrd}{weekly wage in whole sales and retail trade}
    \item{wfir}{weekly wage in  finance, insurance and real estate}
    \item{wser}{weekly wage in service industry}
    \item{wmfg}{weekly wage in manufacturing}
    \item{wfed}{weekly wage of federal emplyees}
    \item{wsta}{weekly wage of state employees}
    \item{wloc}{weekly wage of local governments employees      }
    \item{mix}{offence mix: face-to-face/other}
    \item{pctymle}{percentage of young males}
  }
}

\source{
Online complements to Baltagi (2001):

 \url{http://www.wiley.com/legacy/wileychi/baltagi/}
 
Online complements to Baltagi (2013):

  \url{http://bcs.wiley.com/he-bcs/Books?action=resource&bcsId=4338&itemId=1118672321&resourceId=13452}

See also Journal of Applied Econometrics data archive entry for Baltagi (2006) at 
\url{http://qed.econ.queensu.ca/jae/2006-v21.4/baltagi/}.
}

\references{
 Cornwell, C. and  W.N. Trumbull (1994) \dQuote{Estimating the economic model of crime with panel data}, \emph{Review of Economics and Statistics}, \bold{76}, 360--366.

 Baltagi, B. H. (2006) \dQuote{Estimating an economic model of crime using panel data from North Carolina}, \emph{Journal of Applied Econometrics}, 21(4), 543--547.

 Baltagi, Badi H. (2001) \emph{Econometric Analysis of Panel Data}, 2nd ed., John Wiley and Sons.
 
 Baltagi, Badi H. (2013) \emph{Econometric Analysis of Panel Data}, 5th ed., John Wiley and Sons.
}

\keyword{datasets}
