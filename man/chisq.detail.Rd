\name{chisq.detail}
\alias{chisq.detail}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Print details of a chi-squared test}
\description{
  Prints out the details of the computations involved in a chi-squared
  test on a table.  Includes the expected values and the chi-squared
  contribution of each cell.
}
\usage{
chisq.detail(tab)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab}{Matrix or table to be analyzed}
}
\details{
  This function prints out the input table along with the expected value
  for each cell under the null hypothesis.  It also prints out the
  chi-squared contribution of each cell in the same pattern as the
  table.  This shows the computations involved and one rule of thumb is
  to look for these values that are greater than 4 as a post-hoc analysis.
}
\value{

  This function is used primarily for its side effect of printing the
  results, but does return invisibly a list with the following components:

  \item{obs}{A matrix of the observed values, same as tab.}
  \item{expected}{A matrix of the expected values under the null hypothesis.}
  \item{chi.table}{A matrix of the chi-squared contributions of each
    cell.}
  \item{chi2}{The chi-squared test statistic.}
}
\references{ ~put references to the literature/web site here ~ Moore, bps }
\author{Greg Snow, \email{greg.snow@imail.org}}
%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{chisq.test}},\code{\link{loglin}},
  \code{\link{xtabs}}, \code{\link{table}}, \code{\link{prop.table}},
  \code{CrossTable} from the gmodels package.}
\examples{
chisq.detail(HairEyeColor[,,1])
chisq.detail(HairEyeColor[,,2])
}
\keyword{htest}% at least one, from doc/KEYWORDS

