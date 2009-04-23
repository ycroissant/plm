\name{plm.data}
\alias{plm.data}

\title{Data Frame Special Format for Panel Data}
\description{
  This function transforms a data frame in a format suitable for using
  with the estimation functions of \code{plm}.
}
\usage{
 x <- plm.data(x, index = NULL)
}
\arguments{
  \item{x}{a \code{data.frame},}
  \item{index}{a vector (of length one or two) indicating the
    (individual and time) indexes.}
}

\value{
  A \code{data.frame}.
}

\details{
  \code{indexes} can be :
  \itemize{
  \item a character string which is the name of the individual index
  variable, in this case a new variable called ``time'' which contains the
  time index is added,
  \item an integer,  the number of individuals in case of
  balanced panel, in this case two new variables ``time'' and ``id'' which
  contain the individual and the time indexes are added,
  \item a vector of two character strings which contains the names of the individual and of
  the time indexes.
}

  

}

\author{Yves Croissant}

\examples{

# There are 595 individuals

data("Wages", package="Ecdat")
Wages <- plm.data(Wages, 595)

# Gasoline contains two variables which are individual and time indexes
# The pdata.frame is called gas
data("Gasoline", package="Ecdat")
Gasoline <- plm.data(Gasoline, c("country","year"))
summary(Gasoline)

# Hedonic is an unbalanced panel, townid is the individual index
data("Hedonic", package="Ecdat")
Hedonic <- plm.data(Hedonic, "townid")

}
\keyword{attribute}
