\name{vis.binom}
\alias{vis.binom}
\alias{vis.gamma}
\alias{vis.normal}
\alias{vis.t}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plot various distributions then interactivly adjust the parameters. }
\description{
  Plot a curve of a distribution, then using a Tk slider window adjust
  the parameters and see how the distribution changes.  Optionally also
  plots reference distributions.
}
\usage{
vis.binom()
vis.gamma()
vis.normal()
vis.t()
}
%- maybe also 'usage' for other objects documented here.
\details{
  These functions plot a distribution, then create a Tk slider box that
  allows you to adjust the parameters of the distribution to see how the
  curve changes.

  Check boxes are available in some cases to also show reference
  distributions (normal and poisson for the binomial, exponential and
  chi-squared for gamma, and normal for t).  The exponential and
  chi-squared distributions are those with the same mean as the plotted
  gamma. 

  If you change the plotting
  ranges then you need to click on the 'refresh' button to update the plot.
}
\value{
  These functions are run for their side effects and do not return
  anything meaningful.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow \email{538280@gmail.com} }
%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{dnorm}}, \code{\link{dgamma}}, etc. }
\examples{
if(interactive()){
vis.binom()
vis.normal()
vis.gamma()
vis.t()
}
}
\keyword{ distribution }% at least one, from doc/KEYWORDS
\keyword{ hplot }% __ONLY ONE__ keyword per line
\keyword{ dynamic }