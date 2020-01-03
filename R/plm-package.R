#' Functions exported from other packages
#' 
#' These functions are imported from other packages and re-exported by
#' \pkg{plm} to enable smooth use within \pkg{plm}.  Please follow the
#' links to view the function's original documentation.
#' 
#' \itemize{
#' \item index
#' \item fixef
#' \item ranef
#' \item vcovHC
#' \item nobs
#' \item maxLik
#' }
#' 
#' 
#' 
#' @name re-export_functions
#' @keywords internal
NULL

#' @rdname re-export_functions
#' @name index
#' @importFrom zoo index
#' @export
NULL

#' @rdname re-export_functions
#' @name fixef
#' @importFrom nlme fixef
#' @export
NULL

#' @rdname re-export_functions
#' @name ranef
#' @importFrom nlme ranef
#' @export
NULL

#' @rdname re-export_functions
#' @name vcovHC
#' @importFrom sandwich vcovHC
#' @export
NULL

#' @rdname re-export_functions
#' @name lag
#' @importFrom stats lag
#' @export
NULL

#' @rdname re-export_functions
#' @name nobs
#' @importFrom stats nobs
#' @export
NULL

#' @rdname re-export_functions
#' @name maxLik
#' @importFrom maxLik maxLik
#' @export
NULL


#' plm package: linear models for panel data
#' 
#' plm is a package for R which intends to make the estimation of linear panel
#' models straightforward. plm provides functions to estimate a wide variety of
#' models and to make (robust) inference.
#' 
#' For a gentle and comprehensive introduction to the package, please see the
#' package's vignette.
#' 
#' The main functions to estimate models are:
#'
#' - `plm`: panel data estimators using `lm` on transformed data,
#' - `pgmm`: generalized method of moments (GMM) estimation for panel
#' data,
#' - `pvcm`: variable coefficients models
#' - `pmg`: mean groups (MG), demeaned MG and common correlated effects
#' (CCEMG) estimators.
#'
#' Next to the model estimation functions, the package offers several
#' functions for statistical tests related to panel data/models.
#' 
#' Multiple functions for (robust) variance--covariance matrices are
#' at hand as well.
#' 
#' The package also provides data sets to demonstrate functions and to
#' replicate some text book/paper results.  Use
#' `data(package="plm")` to view a list of available data sets in
#' the package.
#' 
#' @name plm-package
#' @docType package
#' @keywords package
#' @examples
#' 
#' data("Produc", package = "plm")
#' zz <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#'           data = Produc, index = c("state","year"))
#' summary(zz)
#' 
#' # replicates some results from Baltagi (2013), table 3.1
#' data("Grunfeld", package = "plm")
#' p <- plm(inv ~ value + capital,
#'          data = Grunfeld, model="pooling")
#' 
#' wi <- plm(inv ~ value + capital,
#'           data = Grunfeld, model="within", effect = "twoways")
#' 
#' swar <- plm(inv ~ value + capital,
#'             data = Grunfeld, model="random", effect = "twoways")
#'           
#' amemiya <- plm(inv ~ value + capital,
#'                data = Grunfeld, model = "random", random.method = "amemiya",
#'                effect = "twoways")
#'                 
#' walhus <- plm(inv ~ value + capital,
#'               data = Grunfeld, model = "random", random.method = "walhus",
#'               effect = "twoways")
#' 
NULL


#' Cigarette Consumption
#' 
#' a panel of 46 observations from 1963 to 1992
#' 
#' *total number of observations* : 1380
#' 
#' *observation* : regional
#' 
#' *country* : United States
#' 
#' 
#' @name Cigar
#' @docType data
#' @format
#'
#' A data frame containing :
#' \describe{
#' \item{state}{state abbreviation}
#' \item{year}{the year}
#' \item{price}{price per pack of cigarettes}
#' \item{pop}{population}
#' \item{pop16}{population above the age of 16}
#' \item{cpi}{consumer price index (1983=100)}
#' \item{ndi}{per capita disposable income}
#' \item{sales}{cigarette sales in packs per capita}
#' \item{pimin}{minimum price in adjoining states per pack of cigarettes}
#' }
#'
#' @references
#'
#' \insertRef{BALT:01}{plm}
#' 
#' \insertRef{BALT:13}{plm}
#' 
#' \insertRef{BALT:LEVI:92}{plm}
#' 
#' \insertRef{BALT:GRIF:XION:00}{plm}
#'
#' @source
#'
#' Online complements to Baltagi (2001):
#' 
#' \url{http://www.wiley.com/legacy/wileychi/baltagi/}
#' 
#' Online complements to Baltagi (2013):
#' 
#' \url{http://bcs.wiley.com/he-bcs/Books?action=resource&bcsId=4338&itemId=1118672321&resourceId=13452}
#' @importFrom Rdpack reprompt
#' @keywords datasets
NULL

#' Crime in North Carolina
#' 
#' a panel of 90 observational units (counties) from 1981 to 1987
#' 
#' *total number of observations* : 630
#' 
#' *observation* : regional
#' 
#' *country* : United States
#' 
#' The variables l* (lcrmrte, lprbarr, ...) contain the pre-computed logarithms
#' of the base variables as found in the original data set. Note that these
#' values slightly differ from what R's log() function yields for the base
#' variables.  In order to reproduce examples from the literature, the
#' pre-computed logs need to be used, otherwise the results differ slightly.
#' 
#' @name Crime
#' @docType data
#' @format A data frame containing :
#' \describe{
#' \item{county}{county identifier}
#' \item{year}{year from 1981 to 1987}
#' \item{crmrte}{crimes committed per person}
#' \item{prbarr}{'probability' of arrest}
#' \item{prbconv}{'probability' of conviction}
#' \item{prbpris}{'probability' of prison sentence}
#' \item{avgsen}{average sentence, days}
#' \item{polpc}{police per capita}
#' \item{density}{people per square mile}
#' \item{taxpc}{tax revenue per capita}
#' \item{region}{factor. One of 'other', 'west' or 'central'.}
#' \item{smsa}{factor. (Also called "urban".) Does the individual reside in a SMSA (standard metropolitan statistical area)?}
#' \item{pctmin}{percentage minority in 1980}
#' \item{wcon}{weekly wage in construction}
#' \item{wtuc}{weekly wage in transportation, utilities, communications}
#' \item{wtrd}{weekly wage in wholesale and retail trade}
#' \item{wfir}{weekly wage in finance, insurance and real estate}
#' \item{wser}{weekly wage in service industry}
#' \item{wmfg}{weekly wage in manufacturing}
#' \item{wfed}{weekly wage in federal government}
#' \item{wsta}{weekly wage in state government}
#' \item{wloc}{weekly wage in local government}
#' \item{mix}{offence mix: face-to-face/other}
#' \item{pctymle}{percentage of young males (between ages 15 to 24)}
#' \item{lcrmrte}{log of crimes committed per person}
#' \item{lprbarr}{log of 'probability' of arrest}
#' \item{lprbconv}{log of 'probability' of conviction}
#' \item{lprbpris}{log of 'probability' of prison sentence}
#' \item{lavgsen}{log of average sentence, days}
#' \item{lpolpc}{log of police per capita}
#' \item{ldensity}{log of people per square mile}
#' \item{ltaxpc}{log of tax revenue per capita}
#' \item{lpctmin}{log of percentage minority in 1980}
#' \item{lwcon}{log of weekly wage in construction}
#' \item{lwtuc}{log of weekly wage in transportation, utilities, communications}
#' \item{lwtrd}{log of weekly wage in wholesale and retail trade}
#' \item{lwfir}{log of weekly wage in finance, insurance and real estate}
#' \item{lwser}{log of weekly wage in service industry}
#' \item{lwmfg}{log of weekly wage in manufacturing}
#' \item{lwfed}{log of weekly wage in federal government}
#' \item{lwsta}{log of weekly wage in state government}
#' \item{lwloc}{log of weekly wage in local government}
#' \item{lmix}{log of offence mix: face-to-face/other}
#' \item{lpctymle}{log of percentage of young males (between ages 15 to 24)}}
#' 
#' @references
#'
#' \insertRef{CORN:TRUM:94}{plm}
#'
#' \insertRef{BALT:06}{plm}
#'
#' \insertRef{BALT:01}{plm}
#'
#' \insertRef{BALT:13}{plm}
#' 
#' @source
#'
#' Journal of Applied Econometrics Data Archive (complements Baltagi
#' (2006)):
#' 
#' \url{http://qed.econ.queensu.ca/jae/2006-v21.4/baltagi/}
#' 
#' Online complements to Baltagi (2001):
#' 
#' \url{http://www.wiley.com/legacy/wileychi/baltagi/}
#' 
#' Online complements to Baltagi (2013):
#' 
#' \url{http://bcs.wiley.com/he-bcs/Books?action=resource&bcsId=4338&itemId=1118672321&resourceId=13452}
#' 
#' See also Journal of Applied Econometrics data archive entry for
#' Baltagi (2006) at
#' \url{http://qed.econ.queensu.ca/jae/2006-v21.4/baltagi/}.
#'
#' @keywords datasets
NULL

#' Employment and Wages in the United Kingdom
#' 
#' An unbalanced panel of 140 observations from 1976 to 1984
#' 
#' *total number of observations* : 1031
#' 
#' *observation* : firms
#' 
#' *country* : United Kingdom
#' 
#' 
#' @name EmplUK
#' @docType data
#' @format A data frame containing :
#' \describe{
#' \item{firm}{firm index}
#' \item{year}{year}
#' \item{sector}{the sector of activity}
#' \item{emp}{employment}
#' \item{wage}{wages}
#' \item{capital}{capital}
#' \item{output}{output}
#' }
#' @source
#' \insertRef{AREL:BOND:91}{plm}
#' @keywords datasets
NULL

#' Gasoline Consumption
#' 
#' A panel of 18 observations from 1960 to 1978
#' 
#' *total number of observations* : 342
#' 
#' *observation* : country
#' 
#' *country* : OECD
#' 
#' 
#' @name Gasoline
#' @docType data
#' @format A data frame containing :
#' \describe{
#' \item{country}{a factor with 18 levels}
#' \item{year}{the year}
#' \item{lgaspcar}{logarithm of motor gasoline consumption per car}
#' \item{lincomep}{logarithm of real per-capita income}
#' \item{lrpmg}{logarithm of real motor gasoline price}
#' \item{lcarpcap}{logarithm of the stock of cars per capita}
#' }
#' @references
#'
#' \insertRef{BALT:01}{plm}
#'
#' \insertRef{BALT:13}{plm}
#'
#' \insertRef{BALT:GRIF:83}{plm}
#'
#' @source
#'
#' Online complements to Baltagi (2001):
#' 
#' \url{http://www.wiley.com/legacy/wileychi/baltagi/}
#' 
#' Online complements to Baltagi (2013):
#' 
#' \url{http://bcs.wiley.com/he-bcs/Books?action=resource&bcsId=4338&itemId=1118672321&resourceId=13452}
#' @keywords datasets
NULL

#' Grunfeld's Investment Data
#' 
#' A balanced panel of 10 observational units (firms) from 1935 to 1954
#' 
#' *total number of observations* : 200
#' 
#' *observation* : production units
#' 
#' *country* : United States
#' 
#' 
#' @name Grunfeld
#' @docType data
#' @format A data frame containing :
#' \describe{
#' \item{firm}{observation}
#' \item{year}{date}
#' \item{inv}{gross Investment}
#' \item{value}{value of the firm}
#' \item{capital}{stock of plant and equipment} }
#' 
#' @note The Grunfeld data as provided in package `plm` is the
#'     same data as used in Baltagi (2001), see **Examples** below.
#' 
#' NB:\cr Various versions of the Grunfeld data circulate
#' online. Also, various text books (and also varying among editions)
#' and papers use different subsets of the original Grunfeld data,
#' some of which contain errors in a few data points compared to the
#' original data used by Grunfeld (1958) in his PhD thesis. See
#' Kleiber/Zeileis (2010) and its accompanying website for a
#' comparison of various Grunfeld data sets in use.
#'
#' @seealso For the complete Grunfeld data (11 firms), see
#' [AER::Grunfeld], in the `AER` package.
#'
#' @references
#'
#' \insertRef{BALT:01}{plm}
#'
#' \insertRef{BALT:13}{plm}
#'
#' \insertRef{GRUN:58}{plm}
#' 
#' \insertRef{KLEI:ZEIL:10}{plm}
#' 
#'  website accompanying the paper with various variants of the
#' Grunfeld data:
#' \url{https://eeecon.uibk.ac.at/~zeileis/grunfeld/}.
# \url{http://statmath.wu-wien.ac.at/~zeileis/grunfeld/}.
#' 
#' @source Online complements to Baltagi (2001):
#' 
#' \url{http://www.wiley.com/legacy/wileychi/baltagi/}
#'
#' \url{http://www.wiley.com/legacy/wileychi/baltagi/supp/Grunfeld.fil}
#' 
#' Online complements to Baltagi (2013):
#' 
#' \url{http://bcs.wiley.com/he-bcs/Books?action=resource&bcsId=4338&itemId=1118672321&resourceId=13452}
#' @keywords datasets
#' @examples
#' 
#' \dontrun{
#' # Compare plm's Grunfeld data to Baltagi's (2001) Grunfeld data:
#'   data("Grunfeld", package="plm")
#'   Grunfeld_baltagi2001 <- read.csv("http://www.wiley.com/legacy/wileychi/
#'     baltagi/supp/Grunfeld.fil", sep="", header = FALSE)
#'   library(compare)
#'   compare::compare(Grunfeld, Grunfeld_baltagi2001, allowAll = T) # same data set
#'   }
#' 
NULL


#' Hedonic Prices of Census Tracts in the Boston Area
#' 
#' A cross-section
#' 
#' *number of observations* : 506
#' 
#' *observation* : regional
#' 
#' *country* : United States
#' 
#' 
#' @name Hedonic
#' @docType data
#' @format A dataframe containing:
#' \describe{
#' \item{mv}{median value of owner--occupied homes}
#' \item{crim}{crime rate}
#' \item{zn}{proportion of 25,000 square feet residential lots}
#' \item{indus}{proportion of no--retail business acres}
#' \item{chas}{is the tract bounds the Charles River?}
#' \item{nox}{annual average nitrogen oxide concentration in parts per hundred million}
#' \item{rm}{average number of rooms}
#' \item{age}{proportion of owner units built prior to 1940}
#' \item{dis}{weighted distances to five employment centers in the Boston area}
#' \item{rad}{index of accessibility to radial highways}
#' \item{tax}{full value property tax rate ($/$10,000)}
#' \item{ptratio}{pupil/teacher ratio}
#' \item{blacks}{proportion of blacks in the population}
#' \item{lstat}{proportion of population that is lower status}
#' \item{townid}{town identifier} }
#' 
#' @references
#'
#' \insertRef{BALT:01}{plm}
#'
#' \insertRef{BALT:13}{plm}
#'
#' \insertRef{BESL:KUH:WELS:80}{plm}
#' 
#' \insertRef{HARR:RUBI:78}{plm}

#' @source Online complements to Baltagi (2001):
#' 
#' \url{http://www.wiley.com/legacy/wileychi/baltagi/}
#' 
#' Online complements to Baltagi (2013):
#' 
#' \url{http://bcs.wiley.com/he-bcs/Books?action=resource&bcsId=4338&itemId=1118672321&resourceId=13452}
#' @keywords datasets
NULL

#' Wages and Hours Worked
#' 
#' A panel of 532 observations from 1979 to 1988
#' 
#' *number of observations* : 5320
#' 
#' 
#' @name LaborSupply
#' @docType data
#' @format A data frame containing :
#' \describe{
#' \item{lnhr}{log of annual hours worked}
#' \item{lnwg}{log of hourly wage}
#' \item{kids}{number of children}
#' \item{age}{age}
#' \item{disab}{bad health}
#' \item{id}{id}
#' \item{year}{year}
#' }
#'
#' @references
#'
#' \insertRef{CAME:TRIV:05}{plm}
#'
#' \insertRef{ZILI:97}{plm}
#' 
#' @source Online complements to Ziliak (1997).
#' 
#' Journal of Business Economics and Statistics web site:
#' \url{http://amstat.tandfonline.com/loi/ubes20/}.
#'
#' @keywords datasets
NULL



#' Wages and Education of Young Males
#' 
#' A panel of 545 observations from 1980 to 1987
#' 
#' *total number of observations* : 4360
#' 
#' *observation* : individuals
#' 
#' *country* : United States
#' 
#' 
#' @name Males
#' @docType data
#' @format A data frame containing :
#' \describe{
#' \item{nr}{identifier}
#' \item{year}{year}
#' \item{school}{years of schooling}
#' \item{exper}{years of experience (computed as `age-6-school`)}
#' \item{union}{wage set by collective bargaining ?}
#' \item{ethn}{a factor with levels `black, hisp, other`}
#' \item{married}{married?}
#' \item{health}{health problem ?}
#' \item{wage}{log of hourly wage}
#' \item{industry}{a factor with 12 levels}
#' \item{occupation}{a factor with 9 levels}
#' \item{residence}{a factor with levels `rural_area, north_east, northern_central, south`}
#' }
#' 
#' @references
#'
#' \insertRef{VELL:VERB:98}{plm}
#'
#' \insertRef{VERB:04}{plm}
#' 
#' @source Journal of Applied Econometrics data archive
#' \url{http://qed.econ.queensu.ca/jae/1998-v13.2/vella-verbeek/}.
#'
#' @keywords datasets
NULL


#' Purchasing Power Parity and other parity relationships
#' 
#' A panel of 104 quarterly observations from 1973Q1 to 1998Q4
#' 
#' *total number of observations* : 1768
#' 
#' *observation* : country
#' 
#' *country* : OECD
#' 
#' 
#' @name Parity
#' @docType data
#' @format A data frame containing :
#' \describe{
#' \item{country}{country codes: a factor with 17 levels}
#' \item{time}{the quarter index, 1973Q1-1998Q4}
#' \item{ls}{log spot exchange rate vs. USD}
#' \item{lp}{log price level}
#' \item{is}{short term interest rate}
#' \item{il}{long term interest rate}
#' \item{ld}{log price differential vs. USA}
#' \item{uis}{U.S. short term interest rate}
#' \item{uil}{U.S. long term interest rate} }
#' 
#' @references
#'
#' \insertRef{COAK:FUER:SMIT:06}{plm}
#' 
#' \insertRef{DRIS:KRAA:98}{plm}
#' 
#' @source
#'
#' \insertRef{COAK:FUER:SMIT:06}{plm}

#' @keywords datasets
NULL


#' US States Production
#' 
#' A panel of 48 observations from 1970 to 1986
#' 
#' *total number of observations* : 816
#' 
#' *observation* : regional
#' 
#' *country* : United States
#' 
#' 
#' @name Produc
#' @docType data
#' @format A data frame containing :
#' \describe{
#' \item{state}{the state}
#' \item{year}{the year}
#' \item{region}{the region}
#' \item{pcap}{public capital stock}
#' \item{hwy}{highway and streets}
#' \item{water}{water and sewer facilities}
#' \item{util}{other public buildings and structures}
#' \item{pc}{private capital stock}
#' \item{gsp}{gross state product}
#' \item{emp}{labor input measured by the employment in non--agricultural payrolls}
#' \item{unemp}{state unemployment rate} }
#' 
#' @references
#'
#' \insertRef{BALT:01}{plm}
#'
#' \insertRef{BALT:13}{plm}
#'
#' \insertRef{BALT:PINN:95}{plm}
#'
#' \insertRef{MUNN:90}{plm}
#' 
#' @source Online complements to Baltagi (2001):
#' 
#' \url{http://www.wiley.com/legacy/wileychi/baltagi/}
#' 
#' Online complements to Baltagi (2013):
#' 
#' \url{http://bcs.wiley.com/he-bcs/Books?action=resource&bcsId=4338&itemId=1118672321&resourceId=13452}
#' @keywords datasets
NULL



#' Production of Rice in Indonesia
#' 
#' a panel of 171 observations
#' 
#' *number of observations* : 1026
#' 
#' *observation* : farms
#' 
#' *country* : Indonesia
#' 
#' 
#' @name RiceFarms
#' @docType data
#' @format A dataframe containing :
#' \describe{
#' \item{id}{the farm identifier}
#' 
#' \item{size}{the total area cultivated with rice, measured in hectares}
#' 
#' \item{status}{land status, on of `'owner'` (non sharecroppers,
#' owner operators or leaseholders or both), `'share'`
#' (sharecroppers), `'mixed'` (mixed of the two previous status)}
#' 
#' \item{varieties}{one of `'trad'` (traditional varieties),
#' `'high'` (high yielding varieties) and `'mixed'` (mixed
#' varieties)}
#'
#' \item{bimas}{bIMAS is an intensification program; one of
#' `'no'` (non-bimas farmer), `'yes'` (bimas farmer) or
#' `'mixed'` (part but not all of farmer's land was registered to
#' be in the bimas program)}
#'
#' \item{seed}{seed in kilogram}
#' 
#' \item{urea}{urea in kilogram}
#'
#' \item{phosphate}{phosphate in kilogram}
#' 
#' \item{pesticide}{pesticide cost in Rupiah}
#'
#' \item{pseed}{price of seed in Rupiah per kg}
#'
#' \item{purea}{price of urea in Rupiah per kg}
#' 
#' \item{pphosph}{price of phosphate in Rupiah per kg}
#'
#' \item{hiredlabor}{hired labor in hours}
#' 
#' \item{famlabor}{family labor in hours}
#' 
#' \item{totlabor}{total labor (excluding harvest labor)}
#'
#' \item{wage}{labor wage in Rupiah per hour}
#' 
#' \item{goutput}{gross output of rice in kg}
#'
#' \item{noutput}{net output, gross output minus harvesting cost (paid
#' in terms of rice)}
#' 
#' \item{price}{price of rough rice in Rupiah per kg}
#'
#' \item{region}{one of `'wargabinangun'`, `'langan'`,
#' `'gunungwangi'`, `'malausma'`, `'sukaambit'`,
#' `'ciwangi'`}
#'
#' }
#' 
#' @source
#'
#' \insertRef{FENG:HORR:12}{plm}

#' @keywords datasets
NULL

#' Employment and Wages in Spain
#' 
#' A panel of 738 observations from 1983 to 1990
#' 
#' *total number of observations*: 5904
#' 
#' *observation*: firms
#' 
#' *country*: Spain
#' 
#' 
#' @name Snmesp
#' @docType data
#' @format A data frame containing:
#' 
#' \describe{
#' \item{firm}{firm index}
#' \item{year}{year}
#' \item{n}{log of employment}
#' \item{w}{log of wages}
#' \item{y}{log of real output}
#' \item{i}{log of intermediate inputs}
#' \item{k}{log of real capital stock}
#' \item{f}{real cash flow} }
#' 
#' @references
#'
#' \insertRef{ALON:AREL:99}{plm}

#' @source Journal of Business Economics and Statistics data archive:
#' 
#' \url{http://amstat.tandfonline.com/loi/ubes20/}.
#'
#' @keywords datasets
NULL

#' The Penn World Table, v. 5
#' 
#' A panel of 125 observations from 1960 to 1985
#' 
#' *total number of observations* : 3250
#' 
#' *observation* : country
#' 
#' *country* : World
#' 
#' 
#' @name SumHes
#' @docType data
#' @format A data frame containing :
#' \describe{
#' \item{year}{the year}
#' \item{country}{the country name (factor)}
#' \item{opec}{OPEC member?}
#' \item{com}{communist regime? }
#' \item{pop}{country's population (in thousands)}
#' \item{gdp}{real GDP per capita (in 1985 US dollars)}
#' \item{sr}{saving rate (in percent)}}
#' 
#' @references
#'
#' \insertRef{HAYA:00}{plm}
#'
#' \insertRef{SUMM:HEST:91}{plm}
#'
#' @source Online supplements to Hayashi (2000).
#' 
#' \url{http://fhayashi.fc2web.com/datasets.htm}
#'
#' @keywords datasets
NULL

#' Panel Data of Individual Wages
#' 
#' A panel of 595 individuals from 1976 to 1982, taken from the Panel Study of
#' Income Dynamics (PSID).\cr\cr The data are organized as a stacked time
#' series/balanced panel, see **Examples** on how to convert to a
#' `pdata.frame`.
#' 
#' *total number of observations* : 4165
#' 
#' *observation* : individuals
#' 
#' *country* : United States
#' 
#' 
#' @name Wages
#' @docType data
#' @format A data frame containing:
#' \describe{
#' \item{exp}{years of full-time work experience.}
#' \item{wks}{weeks  worked.}
#' \item{bluecol}{blue collar?}
#' \item{ind}{works in a manufacturing industry?}
#' \item{south}{resides in the south?}
#' \item{smsa}{resides in a standard metropolitan statistical area?}
#' \item{married}{married?}
#' \item{sex}{a factor with levels `"male"` and `"female"`}
#' \item{union}{individual's wage set by a union contract?}
#' \item{ed}{years of education.}
#' \item{black}{is the individual black?}
#' \item{lwage}{logarithm of wage.} }
#'
#' @references
#'
#'\insertRef{BALT:01}{plm}
#'
#' \insertRef{BALT:13}{plm}
#'
#' \insertRef{CORN:RUPE:88}{plm}
#' 
#' @source Online complements to Baltagi (2001):
#' 
#' \url{http://www.wiley.com/legacy/wileychi/baltagi/}
#' 
#' Online complements to Baltagi (2013):
#' 
#' \url{http://bcs.wiley.com/he-bcs/Books?action=resource&bcsId=4338&itemId=1118672321&resourceId=13452}
#' @keywords datasets
#' @examples
#' 
#' # data set 'Wages' is organized as a stacked time series/balanced panel
#' data("Wages", package = "plm")
#' Wag <- pdata.frame(Wages, index=595)
#' 
NULL



