\name{dots}
\alias{dots}
\alias{dots2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create a quick dotchart (histogram)}
\description{
  Create a quick dotchart of 1 or 2 datasets.  These dotcharts are a poor
  man's histogram, not the trellis dotplot.
}
\usage{
dots(x,...)
dots2(x, y, colx = "green", coly = "blue", lab1 =
deparse(substitute(x)), lab2 = deparse(substitute(y)),...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Vector, data to be plotted (should be rounded). }
  \item{y}{ Vector, second dataset to be plotted. }
  \item{colx}{ Color of points for \code{x}. }
  \item{coly}{ Color of points for \code{y}. }
  \item{lab1}{ Label for \code{x}.}
  \item{lab2}{ Label for \code{y}.}
  \item{\ldots}{ Additional arguments passed to plotting functions. }
}
\details{
  These functions create basic dotcharts that are quick "back of the
  envelope" approximations to histograms.  Mainly intended for demonstration.
}
\value{
  No meaninful value.  These functions are run for the side effect of
  creating a plot.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Greg Snow \email{greg.snow@imail.org } }
%\note{ ~~further notes~~ }



\seealso{ \code{\link{dotplot}} in the lattice package, \code{\link{hist}} }
\examples{

dots( round( rnorm(50, 10,3) ) )
dots2( round( rnorm(20, 10,3) ), round(rnorm(20,12,2)) )

}
\keyword{ hplot }% at least one, from doc/KEYWORDS

