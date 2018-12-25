\name{Crime}
\docType{data}
\alias{Crime}
\title{Crime in North Carolina}

\description{
  a panel of 90 observational units (counties) from 1981 to 1987

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
    \item{prbpris}{'probability' of prison sentence}
    \item{avgsen}{average sentence, days}
    \item{polpc}{police per capita}
    \item{density}{people per square mile}
    \item{taxpc}{tax revenue per capita}
    \item{region}{factor. One of 'other', 'west' or 'central'.}
    \item{smsa}{factor. (Also called "urban".) Does the individual reside in a
                SMSA (standard metropolitan statistical area)?}
    \item{pctmin}{percentage minority in 1980}
    \item{wcon}{weekly wage in construction}
    \item{wtuc}{weekly wage in transportation, utilities, communications}
    \item{wtrd}{weekly wage in wholesale and retail trade}
    \item{wfir}{weekly wage in finance, insurance and real estate}
    \item{wser}{weekly wage in service industry}
    \item{wmfg}{weekly wage in manufacturing}
    \item{wfed}{weekly wage in federal government}
    \item{wsta}{weekly wage in state government}
    \item{wloc}{weekly wage in local government}
    \item{mix}{offence mix: face-to-face/other}
    \item{pctymle}{percentage of young males (between ages 15 to 24)}
    
    \item{lcrmrte}{log of crimes committed per person}
    \item{lprbarr}{log of 'probability' of arrest}
    \item{lprbconv}{log of 'probability' of conviction}
    \item{lprbpris}{log of 'probability' of prison sentence}
    \item{lavgsen}{log of average sentence, days}
    \item{lpolpc}{log of police per capita}
    \item{ldensity}{log of people per square mile}
    \item{ltaxpc}{log of tax revenue per capita}
    \item{lpctmin}{log of percentage minority in 1980}
    \item{lwcon}{log of weekly wage in construction}
    \item{lwtuc}{log of weekly wage in transportation, utilities, communications}
    \item{lwtrd}{log of weekly wage in wholesale and retail trade}
    \item{lwfir}{log of weekly wage in finance, insurance and real estate}
    \item{lwser}{log of weekly wage in service industry}
    \item{lwmfg}{log of weekly wage in manufacturing}
    \item{lwfed}{log of weekly wage in federal government}
    \item{lwsta}{log of weekly wage in state government}
    \item{lwloc}{log of weekly wage in local government}
    \item{lmix}{log of offence mix: face-to-face/other}
    \item{lpctymle}{log of percentage of young males (between ages 15 to 24)}
  }
}

\details{
 The variables l* (lcrmrte, lprbarr, ...) contain the pre-computed logarithms
 of the base variables as found in the original data set. Note that these values
 slightly differ from what R's log() function yields for the base variables.
 In order to reproduce examples from the literature, the pre-computed logs
 need to be used, otherwise the results differ slightly.
}

\source{
Journal of Applied Econometrics Data Archive (complements Baltagi (2006)):

 \url{http://qed.econ.queensu.ca/jae/2006-v21.4/baltagi/}

Online complements to Baltagi (2001):

 \url{http://www.wiley.com/legacy/wileychi/baltagi/}
 
Online complements to Baltagi (2013):

  \url{http://bcs.wiley.com/he-bcs/Books?action=resource&bcsId=4338&itemId=1118672321&resourceId=13452}

See also Journal of Applied Econometrics data archive entry for Baltagi (2006) at 
\url{http://qed.econ.queensu.ca/jae/2006-v21.4/baltagi/}.
}

\references{
 Cornwell, C. and  W.N. Trumbull (1994) \dQuote{Estimating the economic model of 
 crime with panel data}, \emph{Review of Economics and Statistics}, \bold{76}(2), 
 pp. 360--366.

 Baltagi, B. H. (2006) \dQuote{Estimating an economic model of crime using panel 
 data from North Carolina}, \emph{Journal of Applied Econometrics}, \bold{21}(4), 
 pp. 543--547.

 Baltagi, Badi H. (2001) \emph{Econometric Analysis of Panel Data}, 2nd ed., 
 John Wiley and Sons.
 
 Baltagi, Badi H. (2013) \emph{Econometric Analysis of Panel Data}, 5th ed., 
 John Wiley and Sons.
}

\keyword{datasets}
