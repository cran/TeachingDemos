\name{towork}
\alias{towork}
\alias{h2h}
\alias{ccc}
\docType{data}
\title{ Sample data downloaded and converted from a GPS unit}
\description{
  These are GPS information from 3 trips.
}
\usage{}
\format{
  Data frames with the following variables.
  \describe{
    \item{\code{Index}}{Measurement number}
    \item{\code{Time}}{a POSIXt, Time of measurement}
    \item{\code{Elevation}}{a numeric vector, Elevation in Feet}
    \item{\code{Leg.Dist}}{a character/numeric vector, The distance
      traveled in that leg (in feet for \code{ccc})}
    \item{\code{Leg.Time}}{a difftime, the time of that leg}
    \item{\code{Speed}}{a numeric vector, Speed in mph}
    \item{\code{Direction}}{a numeric vector, Direction in Degrees, 0 is
    North, 90 is East, 180 is South, 270 is West}
    \item{\code{LatLon}}{a character vector, Latitude and Longitude as
      characters}
    \item{\code{Leg.Dist.f}}{a numeric vector, Length of that leg in feet}
    \item{\code{Leg.Dist.m}}{a numeric vector, Length of that leg in miles}
    \item{\code{Lat}}{a numeric vector, Numeric latitude}
    \item{\code{Lon}}{a numeric vector, Numeric longitude (negative for west)}
    \item{\code{Distance}}{a numeric vector, Distance from start in feet}
    \item{\code{Distance.f}}{a numeric vector, Distance from start in feet}
    \item{\code{Distance.m}}{a numeric vector, Distance from start in miles}
    \item{\code{Time2}}{a difftime, Time from start}
    \item{\code{Time3}}{a difftime, cumsum of \code{Leg.Time}}
  }
}
\details{
  The data frame \code{ccc} came from when I was walking back to my
  office from a meeting and decided to take the scenic route and started
  the GPS.

  The data frame \code{h2h} is a trip from my office to another for a
  meeting.  The first part is traveling by car, the last part by foot
  from the parking lot to the building.  Speed is a mixture of
  distributions.

  The data frame \code{towork} came from driving to work one morning
  (the first point is where the GPS got it's first lock, not my house).
  The overall trip was mostly NorthWest but with enough North and
  NorthEast that a simple average of direction shows SouthEast, good
  example for circular stats.
}
\source{
  My GPS device
}
%\references{
%  ~~ possibly secondary sources and usages ~~
%}
\examples{
if( interactive() ){
  with(ccc, TkApprox(Distance, Elevation))
}
}
\keyword{datasets}
