\name{SensSpec.demo}
\alias{SensSpec.demo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Demonstrate Sensitivity, Specificity, PPV, and NPV }
\description{
  This function demonstrates how to get PPV and NPV from Sensitivity,
  Specificity, and Prevalence by using a virtual population rather than
  a direct application of Bayes Rule.  This approach is more intuitive
  to mathphobes.
}
\usage{
SensSpec.demo(sens, spec, prev, n = 100000, step = 11)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sens}{ Sensitivity (between 0 and 1) }
  \item{spec}{ Specificity (between 0 and 1) }
  \item{prev}{ Prevalence (between 0 and 1) }
  \item{n}{ Size of the virtual population (large round number) }
  \item{step}{ which step of the process to display }
}
\details{
  The common way to compute Positive Predictive Value (probability of
  disease given a positive test (PPV)) and Negative Predictive Value
  (probability of no disease given negative test (NPV)) is to use Bayes' rule
  with the Sensitivity, Specificity, and Prevalence.

  This approach can be overwhelming to non-math types, so this
  demonstration goes through the steps of assuming a virtual population, then
  filling in a 2x2 table based on the population and given values of
  Sensitivity, Specificity, and Prevalence.  PPV and NPV are then
  computed from this table.  This approach is more intuitive to many
  people.

  The function can be run multiple times with different values of
  \code{step} to show the steps in building the table, then rerun with
  different values to show how changes in the inputs affect the results.
}
\value{
  An invisible matrix with the 2x2 table
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow, \email{greg.snow@imail.org} }
%\note{ ~~further notes~~
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{roc.demo}}, \code{\link{fagan.plot}},
  the various Epi packages, \code{\link{tkexamp}}}
\examples{
for(i in seq(1,11,2)) {
  SensSpec.demo(sens=0.95, spec=0.99, prev=0.01, step=i)
  if( interactive() ) {
    readline("Press Enter to continue")
  }
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }

