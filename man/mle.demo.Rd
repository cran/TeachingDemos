\name{mle.demo}
\alias{mle.demo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Demonstrate the basic concept of Maximum Likelihood Estimation }
\description{
  This function graphically shows log likelihoods for a set of data and
  the normal distribution and allows you to interactively change the
  parameter estimates to see the effect on the log likelihood.
}
\usage{
mle.demo(x = rnorm(10, 10, 2), start.mean = mean(x) - start.sd, start.sd = 1.2 * sqrt(var(x)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A vector of data (presumably from a normal distribution). }
  \item{start.mean}{ The initial value for estimating the mean. }
  \item{start.sd}{ The initial value for estimating the standard deviation. }
}
\details{
  The function creates a plot with 3 panels:  the top panel shows a
  normal curve based on the current values of the mean and standard
  deviation along with a vertical line for each point in \code{x} (the
  product of the heights of these lines is the likelihood, the sum of
  the logs of their heights is the log likelihood).

  The lower 2 plots show the profiles of the mean and standard
  deviation. The y-axis is the likelihoods of the parameters tried so
  far, and the x-axes are the mean and standard deviation tried.  The
  point corresponding to the current parameter estimates will be solid
  red.

  A Tk slider box is also created that allows you to change the current
  estimates of the mean and standard deviation to show the effect on the
  log likelihood and find the maximum likelihood estimate.
}
\value{
  This function is run for its side effects and returns NULL.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow \email{greg.snow@imail.org} }
%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{fitdistr} in package MASS, \code{mle} in
  package stats4, \code{\link{slider}} }
\examples{

if(interactive()){
mle.demo()

m <- runif(1, 50,100)
s <- runif(1, 1, 10)
x <- rnorm(15, m, s)

mm <- mean(x)
ss <- sqrt(var(x))
ss2 <- sqrt(var(x)*11/12)
mle.demo(x)
# now find the mle from the graph and compare it to mm, ss, ss2, m, and s
}
}
\keyword{ iplot }% at least one, from doc/KEYWORDS
\keyword{ dynamic }% __ONLY ONE__ keyword per line
