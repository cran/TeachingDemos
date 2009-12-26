\name{project.Map}
\alias{project.Map}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Apply projections to Map objects (from maptools package) }
\description{
  This function is depricated, use \code{project} from the \code{rgdal}
  package instead.

  Apply the \code{mapproject} function from the \code{mapproj} library
  to Map objects from the \code{maptools} library.
}
\usage{
project.Map(Map, projection = "", parameters = NULL, orientation = c(90, 0, 0))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Map}{ A \code{Map} object. }
  \item{projection}{ See \code{mapproject} }
  \item{parameters}{ See \code{mapproject} }
  \item{orientation}{ See \code{mapproject}  }
}
\details{
  This function takes a \code{Map} object and applies \code{mapproject}
  from the \code{mapproject} library to it.
}
\value{
  An object of class \code{Map} with the coordinates transformed.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow \email{greg.snow@imail.org} }
\note{ This is beta level software, there are now better alternatives in
the sp or related packages.}

\note{This function works on Map objects which are not really used
  anymore, the spatial data frames from the sp package are a better
  option and the \code{project} function from package \code{rgdal} is a
  better way to do this now.}

\seealso{ \code{mapproject} from the mapproj library. }
\examples{
\dontrun{

library(mapproj)
# assumes that the time zone shape files have been downloaded
# from: http://openmap.bbn.com/data/shape/timezone/

tz <- maptools:::read.shape('WRLDTZA')
plot(Map2poly(project.Map(tz,'bonne',param=45)))

plot(Map2poly(project.Map(tz,'bonne',param=45)),
  col=rainbow(48)[tz$att.data$OFFSET])

plot(Map2poly(project.Map(tz,'albers',param=c(30,40))))
}
}
\keyword{ manip }% at least one, from doc/KEYWORDS
\keyword{ spatial }% __ONLY ONE__ keyword per line
