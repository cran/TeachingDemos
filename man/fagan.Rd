\name{fagan.plot}
\alias{fagan.plot}
\alias{plotFagan}
\alias{plotFagan2}
\alias{plotFagan.old}
\alias{plotFagan2.old}
\title{Create a Fagan plot to demonstrate Bayes Theorem and screening
  tests}
\description{
  These functions create a plot showing the relationship between the
  prior probability, the LR (combination of sensitivity and
  specificity), and the posterior probability.
}
\usage{
fagan.plot(probs.pre.test, LR, test.result="+")
plotFagan(hscale=1.5, vscale=1.5, wait=FALSE)
plotFagan2(hscale=1.5, vscale=1.5, wait=FALSE)
plotFagan.old()
plotFagan2.old()
}
\arguments{
  \item{probs.pre.test}{ The prior probability }
  \item{LR}{ the likelihood ratio (sensitivity/(1-specificity))}
  \item{test.result}{either '+' or '-' indicating whether you want the
    probability of the event or of not seeing the event}
  \item{hscale}{Horizontal scale, passed to \code{tkrplot}}
  \item{vscale}{Vertical scale, passed to \code{tkrplot}}
  \item{wait}{Should the R session wait for the window to close}
}
\details{
  When Bayes theorem is expressed in terms of log-odds it turns out that
  the posterior log-odds are a linear function of the prior log-odds
  and the log likelihood ratio.  These functions plot an axis on the left
  with the prior log-odds, an axis in the middle representing the log
  likelihood ratio and an axis on the right representing the posterior
  log-odds.  A line is then drawn from the prior probability on the left
  through the LR in the center and extended to the posterior probability
  on the right.  The \code{fagan.plot} creates the plot based on input
  to the function.  The \code{plotFagan} and \code{plotFagan2}
  functions set up Tk windows with sliders representing the possible
  inputs and show how the plot and the posterior probability changes
  when you adjust the inputs.  The \code{plotFagan} function creates
  sliders for the prior probability and the LR, while the
  \code{plotFagan2} function replaces the LR slider with 2 sliders for
  the sensitivity and specificity.

  More detail on the plots and the math behind them can be found at the
  websites below.
}
\value{ The old functions are run for their side effects and do not return
  a meaningful value.  If \code{wait} is FALSE then NULL is returned, if
\code{wait} is TRUE, then a list with the current values is returned.}
\references{
  Fagan TJ. Nomogram for Bayes theorem. N Engl J Med 1975;293(5):257-61.
\url{http://ebm.bmjjournals.com/cgi/content/full/6/6/164}
}

\author{ Guazzetti Stefano and Greg Snow
  \email{538280@gmail.com}}
\seealso{ \code{slider}}
\examples{
 fagan.plot(0.8, 2)

 fagan.plot(0.8, 0.95/(1-0.90) )

if(interactive()) {
 plotFagan()

 plotFagan2()
}
}
\keyword{hplot}
\keyword{dynamic}
