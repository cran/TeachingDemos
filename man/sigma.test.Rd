\name{sigma.test}
\alias{sigma.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ One sample Chi-square test for a population variance }
\description{
  Compute the test of hypothesis and compute a confidence interval on the
  variance of a population.
}
\usage{
sigma.test(x, sigma = 1, sigmasq = sigma^2, alternative = c("two.sided", "less", "greater"), conf.level = 0.95, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Vector of data values. }
  \item{sigma}{ Hypothesized standard deviation of the population. }
  \item{sigmasq}{ Hypothesized variance of the population. }
  \item{alternative}{ Direction of the alternative hypothesis. }
  \item{conf.level}{ Confidence level for the interval computation. }
  \item{\dots}{ Additional arguments are silently ignored. }
}
\details{
  Many introductory statistical texts discuss inference on a single population variance and introduce the chi-square test for a population variance as another example of a hypothesis test that can be easily derived.  Most statistical packages do not include the chi-square test, perhaps because it is not used in practice very often, or because the test is known to be highly sensitive to nonnormal data. For the two-sample problem, see \code{var.test}.
}
\value{
  An object of class \code{htest} containing the results
}

\author{ G. Jay Kerns \email{gkerns@ysu.edu} }
\note{ This test is highly sensitive to nonnormality. }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{var.test}}, \code{\link{print.htest}} }
\examples{
x <- rnorm(20, mean = 15, sd = 7)
sigma.test(x, sigma = 6)

}
\keyword{ htest }% at least one, from doc/KEYWORDS

