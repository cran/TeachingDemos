\name{identify.Map}
\alias{identify.Map}
\alias{identify.polylist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Identify polygons within a plotted map from the maptools packages. }
\description{
  These functions are depricated because the object types are no longer
  used in the maptools and sp packages and the general functionality is
  available using \code{overlay} in the \code{sp} package.

  This function allows you to click in a polygon in a map plotted using
  the maptools package and labels the polygon.
  
  
}
\usage{
identify.Map(x, labels = as.character(Map$att.data$NAME), n = Inf, ...)

identify.polylist(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The map object plotted using \code{plot.Map}. }
  \item{labels}{ Vector of labels used to label the polygons. }
  \item{n}{ Maximum number of polygons to label. }
  \item{\dots}{ Additional arguments passed to \code{identify.Map} or \code{text}. }
}
\details{
  The function \code{identify.polylist} is just a wrapper function for
  \code{identify.Map} which handles both types of data.

  The point you click on is checked against every polygon in the map, so
  this function may be slow on very complicated maps.

  Right click on the map to stop before \code{n}.
}
\value{
  A vector of integers corresponding to the polygons clicked on.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow \email{greg.snow@imail.org} }
\note{ These functions are beta level at best and will probably soon be
  seperceeded by functions in \code{maptools}, \code{sp}, or
  \code{spproj}. The plot.Map function has been depricated so use
  plot.polylist instead (convert Map objects to polylist objects before
  plotting), identify.Map works on polylist objects as well.}

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{plot.Map} in the \code{maptools} package, \code{\link{identify}} }
\examples{
\dontrun{
library(maptools)
plot(state.vbm)
identify(state.vbm) # now click on the map a few times.

plot(state.vbm)
identify(state.vbm, state.abb, n=5)
}
}
\keyword{ aplot }% at least one, from doc/KEYWORDS
\keyword{ iplot }% __ONLY ONE__ keyword per line
