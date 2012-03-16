\name{ldsgrowth}
\alias{ldsgrowth}
\docType{data}
\title{
Growth of The Church of Jesus Christ of Latter-day Saints.
}
\description{
Data on the Growth of The Church of Jesus Christ of Latter-day Saints
(commonly known as the Mormon church (\url{http://www.mormon.org})).
}
\usage{data(ldsgrowth)}
\format{
  A data frame with 179 observations on the following 6 variables.
  \describe{
    \item{\code{Year}}{Year from 1830 to 2008}
    \item{\code{Members}}{Total number of Members}
    \item{\code{Wards}}{Number of Wards and Branches (individual congregations)}
    \item{\code{Stakes}}{Number of Stakes (a group of wards/branches)}
    \item{\code{Missions}}{Number of Missions}
    \item{\code{Missionaries}}{Number of Missionaries called}
  }
}
\details{
The data comes from the church records and are as of December 31st of
each year.

The church was officially organized on 6 April 1830 (hence the starting
year of 1830).

The \code{Missionaries} column represents the number of missionaries
called each year.  Missionaries generally serve for about 2 years.
}
\source{
Deseret News 2010 Church News Almanac
}
\examples{
data(ldsgrowth)
with(ldsgrowth, plot(Year, log(Members)))
}
\keyword{datasets}
