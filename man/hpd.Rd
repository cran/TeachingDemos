\name{hpd}
\alias{hpd}
\alias{emp.hpd}

\title{Compute Highest Posterior Density Intervals}
\description{
  Compute the Highest Posterior Density Interval (HPD) from an inverse
  density function (hpd) or a vector of realizations of the distribution
  (emp.hpd).
}

\usage{
hpd(posterior.icdf, conf=0.95, tol=0.00000001,...)

emp.hpd(x, conf=0.95)
}
\arguments{
  \item{posterior.icdf}{ Function, the inverse cdf of the posterior
    distribution (usually a function whose name starts with 'q').}
  \item{x}{ A vector of realizations from the posterior distribution.}
  \item{conf}{ Scalar, the confidence level desired. }
  \item{tol}{ Scalar, the tolerance for \code{optimize}.}
  \item{\dots}{Additional arguments to \code{posterior.icdf}.}
}
\details{
  These functions compute the highest posterior density intervals
  (sometimes called minimum length confidence intervals) for a Bayesian
  posterior distribution.  The \code{hpd} function is used when you have
  a function representing the inverse cdf (the common case with
  conjugate families).  The \code{emp.hpd} function is used when you
  have realizations of the posterior (when you have results from an MCMC
  run).
}
\value{
  A vector of length 2 with the lower and upper limits of the interval.
}
\author{ Greg Snow \email{greg.snow@imail.org }}
\note{These functions assume that the posterior distribution is
  unimodal, they compute only 1 interval, not the set of intervals that
  are appropriate for multimodal distributions.}
\seealso{\code{hdr} in the hdrcde package.}
\examples{

hpd(qbeta, shape1=50, shape2=250)

tmp <- rbeta(10000, 50, 250)
emp.hpd(tmp)

}

\keyword{univar}
