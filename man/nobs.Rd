\name{nobs}
\alias{nobs}
\alias{nobs.panelmodel}


\title{Extract Total Number of Observations Used in Estimated Panelmodel}

\description{This function extracts the total number of 'observations' from a fitted panel model.}

\usage{
\method{nobs}{panelmodel}(object, \dots)
}

\arguments{
 \item{object}{a \code{panelmodel} object for which the number of total observations is to be extracted,}
 \item{\dots}{further arguments.}
}

\value{
 A single number, normally an integer.
}

\details{
  The number of observations is usually the length of the residuals vector. Thus, \code{nobs} gives
  the number of observations actually used by the estimation procedure. It is not necessarily the number
  of observations of the model frame (number of rows in the model frame), because sometimes the model frame
  is further reduced by the estimation procedure. This is e.g. the case for first--difference models 
  estimated by \code{plm(\dots, model = "fd")} where the model frame does not yet contain the differences (see
  also \bold{Examples}).
}


\seealso{\code{\link{pdim}}}

\examples{

# estimate a panelmodel
data("Produc", package = "plm")
z <- plm(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp,data=Produc,
         model="random", subset = gsp > 5000)
         
nobs(z)       # total observations used in estimation
pdim(z)$nT$N  # same information
pdim(z)       # more information about the dimensions (no. of individuals and time periods)

# illustrate difference between nobs and pdim for first-difference model
data("Grunfeld", package = "plm")
fdmod <- plm(inv ~ value + capital, data = Grunfeld, model = "fd")
nobs(fdmod)      # 190
pdim(fdmod)$nT$N # 200
}

\keyword{attribute}
