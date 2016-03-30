\name{summary.plm}
\alias{summary.plm}
\alias{print.summary.plm}

\title{Summary for plm objects}

\description{
The summary method for plm objects generates some more information about estimated plm models.
}

\usage{
\method{summary}{plm}(object, .vcov = NULL, ...)
\method{print}{summary.plm}(x, digits = max(3, getOption("digits") - 2),
    width = getOption("width"), subset = NULL, ...)
}

\arguments{
  \item{object}{an object of class \code{"plm"},}
  \item{x}{an object of class \code{"summary.plm"},}
  \item{subset}{a character or numeric vector indicating a subset of the table of coefficients to be
    printed for \code{"print.summary.plm"},}
  \item{.vcov}{a variance--covariance matrix furnished by the user or a function to calculate one (see \bold{Examples}),}
  \item{digits}{number of digits for printed output,}
  \item{width}{the maximum length of the lines in the printed output,}
  \item{\dots}{further arguments.}
}

\details{
The \code{summary} method for plm objects (\code{summary.plm}) creates an object of class \code{c("summary.plm", "plm", "panelmodel")} that 
extends the plm object it is run on with various information about the estimated model like
(inferential) statistics, see \bold{Value}. It has an associated print method (\code{print.summary.plm}).
}

\value{
An object of class \code{c("summary.plm", "plm", "panelmodel")}. Some of its elements are
carried over from the associated plm object and described there (\code{\link[plm]{plm}}).
The following elements are new or changed relative to the elements of a plm object:

\item{fstatistic}{F statistic for joint test of significance of coefficients (class "htest"),}
% TODO for item fstatistic: robust statistic if arg '.vcov' != NULL once "activated" in summary.plm; mention small sample adjustment; Chisq for RE models?
\item{coefficients}{a matrix with the estimated coefficients, standard errors, t--values, and p--values, if argument '.vcov' was set to non-\code{NULL} the standard errors (and t-- and p--values) in their respective robust variant,}
\item{vcov}{the "regular" variance--covariance matrix of the coefficients (class "matrix"),}
\item{rvcov}{only present if argument \code{.vcov} was set to non-\code{NULL}: the furnished 
            variance--covariance matrix of the coefficients (class "matrix"),}
\item{r.squared}{a named numeric containing the R-squared ("rsq") and the adjusted R-squared ("adjrsq") of the model.}
}


\seealso{
% TODO: Ftest (once Ftest it is exported and has its own doc)
\code{\link[plm]{plm}} for estimation of various models; 
\code{\link[plm]{vcovHC}} for an example of a robust estimation of variance--covariance matrix; 
\code{\link[stats]{print.htest}} for some information about class "htest";
\code{\link[plm]{fixef}} to compute the fixed effects for "within" (=fixed effects) models and 
\code{\link[plm]{within_intercept}} for an "overall intercept"" for such models.

}

\author{Yves Croissant}

% \references{}

\examples{
data("Produc", package = "plm")
zz <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
          data = Produc, index = c("state","year"))
summary(zz)

# summary with a funished vcov, passed as matrix, as function, and
# as function with additional argument
data("Grunfeld", package = "plm")
wi <- plm(inv ~ value + capital,
          data = Grunfeld, model="within", effect = "individual")
summary(wi, .vcov = vcovHC(wi))
summary(wi, .vcov = vcovHC)
summary(wi, .vcov = function(x) vcovHC(x, method = "white2"))

# extract F statistic
wi_summary <- summary(wi)
Fstat <- wi_summary[["fstatistic"]]

# extract estimates and p-values
est <- wi_summary[["coefficients"]][ , "Estimate"]
pval <- wi_summary[["coefficients"]][ , "Pr(>|t|)"]

# print summary only for coefficent "value"
print(wi_summary, subset = "value")
}

\keyword{regression}
