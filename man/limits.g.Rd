\name{limits.g}
\alias{limits.g}
\alias{sd.g}
\alias{stats.g}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Functions to plot Shewhart g chart }
\description{
  These functions are used to compute statistics required by the g chart
  (geometric distribution) for use with the qcc package.
}
\usage{
stats.g(data, sizes)
sd.g(data, sizes)
limits.g(center, std.dev, sizes, conf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ the observed data values }
  \item{center}{ sample center statistic }
  \item{sizes}{ sample sizes (not used) }
  \item{std.dev}{ standard deviation of geometric distribution }
  \item{conf}{ a numeric value used to compute control limits,
    specifying the number of standard deviations (if 'conf' > 1) or the
    confidence level (if 0 < 'conf' < 1).}
}
\value{
  The function 'stats.g' returns a list with components 'statistics' and
  'center'.

  The function 'sd.g' returns 'std.dev' the standard deviation
  (sqrt(1-p)/p).

  The function 'limits.g' returns a matrix with lower and upper control limits.
}
\details{

  These functions work with the qcc package and may someday be moved
  into the qcc package.
  
  The g chart plots the number of non-events between events.  np charts
  do not work well when the probability of an event is rare (see example
  below).  Instead of plotting the number of events, the g chart plots
  the number of non-events between events.
}

%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow \email{greg.snow@ihc.com} }
\note{ The geometric distribution is quite skewed so using 'conf' > 1
  does not give good limits, use a value of 'conf' between 0 and 1. }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{qcc}}
\examples{
if(require(qcc)){
  tmp <- rbinom(1000, 1, 0.01)
  tmp2 <- diff(which(c(1,tmp)==1))-1
  qcc(tmp,'np',1)
  qcc(tmp2, "g")
}
}
\keyword{ hplot }% __ONLY ONE__ keyword per line
