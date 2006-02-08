\name{clt.examp}
\alias{clt.examp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot Examples of the Central Limit Theorem}
\description{
  Takes samples of size \code{n} from 4 different distributions and
  plots histograms of the means along with a normal curve with matching
  mean and standard deviation.  Creating the plots for different values
  of \code{n} demonstrates the Central Limit Theorem.
}
\usage{
clt.examp(n = 1, reps = 10000, nclass = 16)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{size of the individual samples}
  \item{reps}{number of samples to take from each distribution}
  \item{nclass}{number of bars in the histograms}
}
\details{
  The 4 distributions sampled from are a Normal with mean 0 and standard
  deviation 1, an exponential with lambda 1/3 (mean = 3), a uniform
  distribution from 0 to 1, and a beta distribution with alpha 0.35 and
  beta 0.25 (U shaped left skewed).

  Running the function with \code{n}=1 will show the populations.  Run
  the function again with \code{n} at higher values to show that the
  sampling distribution of the uniform quickly becomes normal and the
  exponential and beta distributions eventually become normal (but much
  slower than the uniform).
}
\value{
  This function is run for its side effect of creating plots.  It
  returns NULL invisibly.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Greg Snow \email{greg.snow@ihc.com}}
%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{rnorm}}, \code{\link{rexp}}, \code{\link{runif}},
  \code{\link{rbeta}} }
\examples{
clt.examp()
clt.examp(5)
clt.examp(30)
clt.examp(50)

}
\keyword{ hplot }% at least one, from doc/KEYWORDS
\keyword{ distribution }% __ONLY ONE__ keyword per line
\keyword{ univar }