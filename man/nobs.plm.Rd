\name{nobs}
\alias{nobs}
\alias{nobs.plm}

\title{Extract Total Number of Observations Used in Estimated Panelmodel}

\description{This function extracts the total number of 'observations' from a fitted panel model.}

\usage{
nobs(x)
\method{nobs}{panelmodel}(x)
}

\arguments{
 \item{x}{a \code{panelmodel} object for which the number of total observations is to be extracted.}
}

\value{
 A single number, normally an integer.
}

\details{
\code{nobs} is just a wrapper for convenience, it calls \code{pdim(x)$nT$N} and is similiar to \code{lm()}'s \code{nobs}.
}

\author{Yves Croissant (\code{pdim}) and Kevin Tappe (wrapper \code{nobs})}

\seealso{\code{\link{pdim}}}

\examples{

# estimate a panelmodel
data("Produc", package = "plm")
z <- plm(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp,data=Produc,
         model="random", subset = gsp > 5000)
         
nobs(z)       # total observations used in estimation
pdim(z)$nT$N  # same information
pdim(z)       # more information about the dimensions (no. of individuals and time periods)

}

\keyword{attribute}
