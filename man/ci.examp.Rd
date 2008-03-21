\name{ci.examp}
\alias{ci.examp}
\alias{run.ci.examp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plot examples of Confidence Intervals }
\description{
  Generate \code{reps} samples from a normal distribution then compute
  and plot confidence intervals for each sample along with information
  about the population to demonstrate confidence intervals.  Optionally
  change the confidence level using a Tk slider.
}
\usage{
ci.examp(mean.sim = 100, sd = 10, n = 25, reps = 50, conf.level = 0.95, method = "z", lower.conf = (1 - conf.level)/2, upper.conf = 1 - (1 - conf.level)/2)
run.ci.examp(reps = 100, seed, method="z", n=25)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mean.sim}{ The mean of the population. }
  \item{sd}{ The standard deviation of the population. }
  \item{n}{ The sample size for each sample. }
  \item{reps}{ The number of samples/intervals to create. }
  \item{conf.level}{ The confidence level of the intervals.  }
  \item{method}{ 'z', 't', or 'both', should the intervals be based on
    the normal, the t, or both distributions. }
  \item{lower.conf}{ Quantile for lower confidence bound. }
  \item{upper.conf}{ Quantile for upper confidence bound. }
  \item{seed}{ The seed to use for the random number generation. }
}
\details{
  These functions demonstrate the concept of confidence intervals by
  taking multiple samples from a known normal distribution and
  calculating a confidence interval for each sample and plotting the
  interval relative to the true mean.  Intervals that contain the true
  mean will be plotted in black and those that do not include the true
  mean will be plotted in different colors.

  The \code{method} argument determines the type of interval: 'z' will
  use the normal distribution and the known population standard
  deviation, 't' will use the t distribution and the sample standard
  deviations, 'both' will compute both for each sample for easy
  comparison (it is best to reduce \code{reps} to about 25 when using
  'both').

  The optional arguments \code{lower.conf} and \code{upper.conf} can be
  used to plot non-symmetric or 1 sided confidence intervals.

  The function \code{run.ci.examp} also creates a Tk slider that will
  allow you to interactively change the confidence level and replot the
  intervals to show how the interval widths change with the confidence
  level.
}
\value{
  These functions are run solely for the side effect of plotting the
  intervals, there is no meaningfull return value.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Greg Snow \email{greg.snow@imail.org} }
%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{z.test}}, \code{\link{t.test}} }
\examples{
ci.examp()

if(interactive()) {
  run.ci.examp()
}

# 1 sided confidence intervals
ci.examp(lower.conf=0, upper.conf=0.95)

# non-symmetric intervals
ci.examp(lower.conf=0.02, upper.conf=0.97)
}
\keyword{hplot}% at least one, from doc/KEYWORDS
\keyword{dynamic}% __ONLY ONE__ keyword per line
\keyword{univar}
