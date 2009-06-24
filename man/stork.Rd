\name{stork}
\Rdversion{1.1}
\alias{stork}
\docType{data}
\title{
Neyman's Stork data
}
\description{
Data invented by Neyman to look at spurious correlations and adjusting
for lurking variables by looking at the relationship between storks and biths.
}
\usage{data(stork)}
\format{
  A data frame with 54 observations on the following 6 variables.
  \describe{
    \item{\code{County}}{ID of county}
    \item{\code{Women}}{Number of Women (*10,000)}
    \item{\code{No.storks}}{Number of Storks sighted}
    \item{\code{No.babies}}{Number of Babies Born}
    \item{\code{Stork.rate}}{Storks per 10,000 women (=No.storks/Women)}
    \item{\code{Birth.rate}}{Babies per 10,000 women (=No.babies/Women)}
  }
}
\details{
This is an entertaining example to show a relationship that is due to a
third possibly lurking variable.  The source paper shows how completely
different relationships can be found by mis-analyzing the data.
}
\source{
  Kronmal, Richard A. (1993) Spurious Cerrolation and the Fallacy of the
  Ratio Standard Revisited. Journal of the Royal Statistical
  Society. Series A, Vol. 156, No. 3, 379-392.
}
\references{
Neyman, J. (1952) Lectures and Conferences on Mathematical Statistics
and Probability, 2nd edn, pp. 143-154. Washington DC: US Department of Agriculture.
}
\examples{
data(stork)
pairs(stork[,-1], panel=panel.smooth)
## maybe str(stork) ; plot(stork) ...
}
\keyword{datasets}
