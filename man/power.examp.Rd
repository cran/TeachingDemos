\name{power.examp}
\alias{power.examp}
\alias{run.power.examp}
\alias{run.power.examp.old}
\alias{power.refresh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Graphically illustrate the concept of power. }
\description{
  Create graphs of a normal test statistic under the null and
  alternative hypotheses to graphically show the idea of power.
}
\usage{
power.examp(n = 1, stdev = 1, diff = 1, alpha = 0.05, xmin = -2, xmax = 4)
run.power.examp(hscale=1.5, vscale=1.5, wait=FALSE)
run.power.examp.old()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ The sample size for the test statistic. }
  \item{stdev}{ The standard deviation of the population. }
  \item{diff}{ The true difference in means (alternate hypothesis). }
  \item{alpha}{ The type I error rate to use for the test. }
  \item{xmin}{ The minimum x value to show on the graph. }
  \item{xmax}{ The maximum x value to show on the graph. }
  \item{hscale}{Controls width of plot, passed to \code{tkrplot}.}
  \item{vscale}{Controls height of plot, passed to \code{tkrplot}.}
  \item{wait}{Should R wait for the window to close.}
}
\details{
  This function will draw 2 graphs representing an upper-tailed test of
  hypothesis.

  The upper panel represents the test statistic under the
  null hypothesis that the true mean (or mean difference) is 0.  It then
  also shows the upper tail area equal to \code{alpha} and the rejection
  region for the test statistic.

  The lower panel shows the normal distribution for the test statistic
  under the alternative hypothesis where the true mean (or mean
  difference) is \code{diff}.  Using the rejection region from the upper
  panel it shades the upper tail area that corresponds to the power of
  the test.

  Both curves are affected by the specified \code{stdev} and sample size
  \code{n}.

  The function \code{run.power.examp} will in addition create a Tk
  slider box that will allow you to interactively change the values of
  \code{stdev}, \code{diff}, \code{alpha}, and \code{n} to dynamically
  see the effects of the change on the graphs and on the power of the
  test.

  This can be used to demonstrate the concept of power, show the effect
  of sample size on power, show the inverse relationship between the
  type I and type II error rates, and show how power is dependent on the
  true mean (or difference) and the population standard deviation.
}
\value{
  \code{power.examp} invisibly returns the power computed.

  \code{run.power.examp} returns a list with the parameter settings and
  the power if \code{wait} is TRUE.

  \code{run.power.examp.old} does not return anything meaningful.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow \email{538280@gmail.com} }
%\note{ ~~further notes~~ }

 %~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{power.t.test}} }
\examples{
power.examp()
power.examp(n=25)
power.examp(alpha=0.1)
}
\keyword{ hplot }% at least one, from doc/KEYWORDS
\keyword{ dynamic }% __ONLY ONE__ keyword per line
\keyword{ univar }
\keyword{ htest }
