\name{cal}
\alias{cal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot a month or year calendar
}
\description{
  Plot a calendar of the specified year or month.  Monthly calendars can
  have additional information (text/plots) added to the individual cells.
}
\usage{
cal(month, year)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{month}{
    The month for the calendar, if ommitted will do a yearly calendar,
    can either be a number from 1 to 12 or a character string that will
    be matched (using \code{pmatch}) against \code{month.name}.
  }
  \item{year}{
    The year for the calendar.  If ommitted and \code{month} is an
    integer less than or equal to 12 then \code{month} will be used as
    the year.
  }
}
\details{
  This function plots on the current (or default) graphics device a
  yearly or monthly calendar.  It tries to guess what you want, if both
  \code{year} and \code{month} are ommitted then it will plot the
    current month.  If \code{month} is an integer greater than 12 and no
    \code{year} is specified then
    that value will be used as the year for a yearly calendar.  The
    \code{month} can be either an integer from 1 to 12 or a character
    string that will be matched against \code{month.name} using
    \code{pmatch}.

    Each day of the monthly calendar is a plotting frame that can be
    added to using stardard low level functions, the coordinates of the
    plotting region (the entire box) are from 0 to 1 in both
    dimensions.  The \code{\link{updateusr}} function can be used to
    change the coordinates.  The return from the function (when creating
    a monthly calendar) can be used to select the day.
}
\value{
  Nothing is returned when a whole year calendar is created.  When the
    month calendar is created a function is returned invisibly that if
    passed an integer corresponding to a day of the month will set the
    graphics parameters so the corresponding day in the calendar becomes
    the current plotting figure.  See the examples below.
}

\author{Greg Snow, \email{greg.snow@imail.org}}



\seealso{
 \code{\link{Sys.time}}, \code{\link{as.POSIXlt}}, \code{\link{par}}, \code{\link{updateusr}}
}
\examples{
cal(2011)

cal('May')

setday <- cal(11, 2011)

setday(3)
text(0.5,0.5, 'Some\nCentered\nText')

setday(8)
text(1,1,'Top Right',adj=c(1,1))

setday(18)
text(0,0,'Bottom Left', adj=c(0,0) )

setday(21)
tmp.x <- runif(25)
tmp.y <- rnorm(25, tmp.x, .1)
mrgn.x <- 0.04*diff(range(tmp.x))
mrgn.y <- 0.04*diff(range(tmp.y))
updateusr( 0:1, 0:1, range(tmp.x)+c(-1,1)*mrgn.x, range(tmp.y)+c(-1,1)*mrgn.y)
points(tmp.x, tmp.y)

setday(30)
tmp <- hist(rnorm(100), plot=FALSE)
updateusr( 0:1, 0:1, range(tmp$breaks), range(tmp$counts*1.1,0) )
lines(tmp)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
\keyword{chron}% __ONLY ONE__ keyword per line
\keyword{ts}
