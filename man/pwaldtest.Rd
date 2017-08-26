\name{pwaldtest}
\alias{pwaldtest}
\alias{pwaldtest.plm} 
% \alias{pwaldtest.panelmodel} - needed?
% \alias{pwaldtest.formula} % not implemented (yet? - do we need it?)

\title{Wald-style Chi-square Test and F Test}

\description{
Wald-style chi-square test and F test of slope coefficients being zero jointly, 
including robust versions of the tests.
}

\usage{
pwaldtest(x, ...)
\method{pwaldtest}{plm}(x, test = c("Chisq", "F"), vcov = NULL, 
          df2adj = (test == "F" && !is.null(vcov) && missing(.df2)),
          .df1, .df2, \dots)
% \method{pwaldtest}{formula}(x, \dots)  % not implemented (yet? - do we need it?)
}

\arguments{
\item{x}{an estimated model of which the coefficients should be tested (usually of class \code{"plm"}),} % maybe also class "panelmodel"?

\item{test}{a character, indicating the test to be performed, may be either \code{"Chisq"} or
\code{"F"} for the Wald-style Chi-square test or F test, respectively, }

\item{vcov}{\code{NULL} by default; a \code{matrix} giving a variance--covariance 
matrix or a function which computes such; if supplied (non \code{NULL}), 
the test is carried out using the variance--covariance matrix indicated resulting 
in a robust test,}

\item{df2adj}{logical, only relevant for \code{test = "F"}, indicating whether
the adjustment for clustered standard errors for the second degrees of freedom 
parameter should be performed (see \bold{Details}, also for further requirements
regarding the variance--covariance matrix in \code{vcov} for the adjustment to be 
performed),}

\item{.df1}{a numeric, used if one wants to overwrite the first degrees
of freedom parameter in the performed test (usually not used),}

\item{.df2}{a numeric, used if one wants to overwrite the second degrees
of freedom parameter for the F test (usually not used),}

\item{\dots}{further arguments (currently none).}
}

\details{


\code{pwaldtest} can be used stand--alone with a plm object. It is also used in 
\code{\link{summary.plm}} to produce the F statistic. % TODO: or Chi-square statistic, once "activated" in summary.plm and if Chi-sq is used for RE models.

\code{pwaldtest} performs the test if the slope coefficients of a panel regression are jointly zero.
It does not perform general purpose Wald-style tests (for those, see \code{\link[lmtest]{waldtest}} (from package \CRANpkg{lmtest}) or \code{linearHypothesis} (from \CRANpkg{car})).

If a user specified variance-covariance matrix/function is given in 
argument \code{vcov}, the robust version of the tests are carried out. 
In that case, if the F test is requested (\code{test = "F"}) and no
overwriting of the second degrees of freedom parameter is given (by 
supplying argument (\code{.df2})), the adjustment of the second degrees
of freedom parameter is performed by default. The second degrees of 
freedom parameter is adjusted to be the number of unique elements of the 
cluster variable - 1, e. g. the number of individuals - 1. For the degrees of 
freedom adjustment of the F test in general, see e. g. Cameron/Miller (2015), 
section VII;  Andress/Golsch/Schmidt (2013), pp. 126, footnote 4.

The degrees of freedom adjustment requires the vcov object supplied or created by a supplied function
to carry an attribute called "cluster" with a known clustering described as a 
character (for now this could be either \code{"group"} or \code{"time"}).
The vcovXX functions of the package \pkg{plm} provide such an attribute for their returned 
variance--covariance matrices. No adjustment is done for unknown descriptions given
in the attribute "cluster" or when the attribute "cluster" is not present.
Robust vcov objects/functions from package \CRANpkg{clubSandwich} work as inputs to 
\code{pwaldtest}'s F test because a they are translated internally to match the 
needs described above.
}

\value{
An object of class \code{"htest"}.
}

\seealso{

\code{\link[plm]{vcovHC}} for an example of the vcovXX functions, a robust 
estimation for the variance--covariance matrix;
\code{\link[plm]{summary.plm}}
}

\author{Yves Croissant (initial implementation) and Kevin Tappe (extensions: vcov argument and F test's df2 adjustment)}

\references{
Wooldridge, J.M. (2010) \emph{Econometric Analysis of Cross Section and Panel Data}, 2nd ed., MIT Press, Sec. 4.2.3, pp. 60--62.

Andress, H.-J./Golsch, K./Schmidt, A. (2013), Applied Panel Data Analysis for Economic and Social Surveys, Springer, Heidelberg et al.

Cameron, A. C./Miller, D. L. (2015), "A Practitioner's Guide to Cluster-Robust Inference", \emph{Journal of Human Resources}, 2015, Vol. 50, No. 2, pp. 317--373; see also the supplements under \url{http://cameron.econ.ucdavis.edu/research/papers.html}.
}


\examples{
data("Grunfeld", package = "plm")
mod_fe <- plm(inv ~ value + capital, data = Grunfeld, model = "within")
mod_re <- plm(inv ~ value + capital, data = Grunfeld, model = "random")
pwaldtest(mod_fe, test = "F")
pwaldtest(mod_re, test = "Chisq")

# with robust vcov (matrix, function)
pwaldtest(mod_fe, vcov = vcovHC(mod_fe))
pwaldtest(mod_fe, vcov = function(x) vcovHC(x, type = "HC3"))

pwaldtest(mod_fe, vcov = vcovHC(mod_fe), df2adj = FALSE) # w/o df2 adjustment

# example without attribute "cluster" in the vcov
vcov_mat <- vcovHC(mod_fe)
attr(vcov_mat, "cluster") <- NULL  # remove attribute
pwaldtest(mod_fe, vcov = vcov_mat) # no df2 adjustment performed

}

\keyword{htest}
