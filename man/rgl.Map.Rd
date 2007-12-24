\name{rgl.Map}
\alias{rgl.Map}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plot a map in an rgl window }
\description{
  Plots a map (from a Map object from package maptools) on a unit sphere
  in an rgl window that can then be interactively rotated.
}
\usage{
rgl.Map(Map, which, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Map}{ A \code{Map} object }
  \item{which}{ Vector indicating the subset of polygons to plot. }
  \item{\dots}{ Additional arguments passed on to \code{rgl.lines}. }
}
\details{
  This assumes that the map is cordinates in degrees and plots the map
  on a unit sphere in an rgl window making a globe.  You can then rotate
  the globe by clicking and dragging in the window.
}
\value{
  There is no return value, this function is run for its side effect.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow \email{greg.snow@imail.org} }
\note{ This function is still beta level software (some extra lines show
up).}

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{rgl} in package rgl, \code{plot.Map} in
  package maptools }
\examples{
if(interactive()){
# assumes that the time zone shape files have been downloaded
# from: http://openmap.bbn.com/data/shape/timezone/

tz <- read.shape('WRLDTZA')
rgl.Map(tz)
rgl.spheres(0,0,0,.999, col='darkblue')
}
}
\keyword{ hplot }% at least one, from doc/KEYWORDS
\keyword{ dynamic }% __ONLY ONE__ keyword per line
