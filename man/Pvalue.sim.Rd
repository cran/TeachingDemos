\name{Pvalue.norm.sim}
\alias{Pvalue.norm.sim}
\alias{Pvalue.binom.sim}
\alias{run.Pvalue.norm.sim}
\alias{run.Pvalue.binom.sim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Simulate P-values }
\description{
  Simulate and plot p-values from a normal or binomial based test under
  various conditions.  When all the assumptions are true, the p-values
  should follow an approximate uniform distribution.  These functions
  show that along with how violating the assumptions changes the
  distribution of the p-values.
}
\usage{
Pvalue.norm.sim(n = 50, mu = 0, mu0 = 0, sigma = 1, sigma0 = sigma,
     test= c("z", "t"), alternative = c("two.sided", "less", "greater", "<>",
           "!=", "<", ">"), alpha = 0.05, B = 10000)
Pvalue.binom.sim(n=100, p=0.5, p0=0.5, test=c('exact','approx'),
                            alternative=c('two.sided', 'less', 'greater',
                            '<>','!=','<','>'),
                            alpha=0.05, B=1000)
run.Pvalue.norm.sim()
run.Pvalue.binom.sim()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{Sample Size for each simulated dataset}
  \item{mu}{Simulation mean for samples}
  \item{mu0}{Hypothesized mean for tests}
  \item{sigma}{Simulation SD for samples}
  \item{sigma0}{Hypothesized SD for tests, if blank or missing, use the
    sample SD in the tests}
  \item{p}{Simulation proportion for samples}
  \item{p0}{Hypothesized proportion for tests}
  \item{test}{Which test to use, "z" or "t" tests for normal, "exact"
    (binomial) or "approx" (normal approximation) for binomial }
  \item{alternative}{Direction for alternative hypothesis }
  \item{alpha}{alpha level for test (optional) }
  \item{B}{Number of simulated datasets }
}
\details{
  These functions generate \code{B} samples from either a normal or
  binomial distribution, then compute the P-values for the test of
  significance on each sample and plot the P-values.

  The \code{run.Pvalue.norm.sim} and \code{run.Pvalue.binom.sim}
  functions are GUI wrappers for the other 2 functions allowing you to
  change the parameters and click on "refresh" to run a new set of
  simulations.

  Using \code{NA} for \code{sigma0} will result in the sample standard
  deviations being used (leave blank in the GUI).

  When the simulation conditions and the hypothesized values match, the
  distributions of the p-values will be approximately uniform.
  Changing the parameter of interest will show the idea of power.
  Changing the other parameters can show the effects of assumptions not
  being met.
}
\value{
  The P-values are invisibly returned.
}
\references{ Murdock, D, Tsai, Y, and Adcock, J (2008) _P-Values are
  Random Variables_. The American Statistician. (62) 242-245.}
\author{Greg Snow, \email{greg.snow@imail.org}}
\note{
  Note: the 2-sided p-values for the binomial may not match the results
  from binom.test and prop.test.  The method used here is an
  approximation for speed.
}
\seealso{ \code{\link{t.test}}, \code{\link{z.test}},
  \code{\link{binom.test}}, \code{\link{prop.test}}, \code{\link{tkexamp}} }
\examples{
 if(interactive()) {
  run.Pvalue.norm.sim()
  run.Pvalue.binom.sim()
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
\keyword{dynamic}% __ONLY ONE__ keyword per line
\keyword{datagen}
\keyword{distribution}
\keyword{htest}