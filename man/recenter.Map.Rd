\name{recenter.Map}
\alias{recenter.Map}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Recenter a Map object from the maptools package. }
\description{
  This function is deprecated, you should be able to do the same thing
  using the \code{project} function from the \code{rgdal} package.
  
  Most maps are centered at a longitude of 0 with points of east
  longitude plotted on the right side of the map and points with west
  longitude plotted to the left.  This function transforms the points to
  have a new center moving polygons from the right to left or left to right.
}
\usage{
recenter.Map(Map, center = 0, range = c(-180, 180))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Map}{ A \code{Map} object from the maptools package. }
  \item{center}{ The new longitude to use as the center. }
  \item{range}{ The range of possible longitudes on the original scale. }
}
\details{
  Plotting a map of the United States can place some of the Alaskan
  islands to the far right of the map (they have east longitude).  This
  function shifts the longitudes to bring the seperate pieces together.

  Polygons will only be shifted if all points can move together, it will
  not split a polygon between sides of the map.
}
\value{
  An object of class \code{Map} (from the maptools package) that is a
  copy of the input Map but with the longitudes shifted.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow \email{greg.snow@imail.org} }
\note{ This is beta level software and will probably be superceded by
  the \code{spproj} or \code{sp} packages. The function \code{plot.Map} has
  been depricated, use \code{plot.polylist} instead (use \code{Map2poly}
to convert).}

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{plot.polylist} }
\examples{
\dontrun{
## this assumes that the time zone shape files have been downloaded
## from: http://openmap.bbn.com/data/shape/timezone/
## and the US states map (st99_d00) files have been downloaded from
## http://www.census.gov/geo/www/maps/

us <- read.shape('st99_d00')
tz <- read.shape('WRLDTZA')
plot(Map2poly(us))
plot(Map2poly(recenter.Map(us,-100)))
plot(Map2poly(recenter.Map(tz,-165)))
}
}
\keyword{ manip }% at least one, from doc/KEYWORDS
\keyword{ spatial }% __ONLY ONE__ keyword per line
