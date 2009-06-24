\name{squishplot}
\alias{squishplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Squish the plotting area to a specified aspect ratio }
\description{
  Adjusts the plotting area to a specific aspect ratio.  This is
  different from using the \code{asp} argument in that it puts the extra
  space in the margins rather than inside the plotting region.
}
\usage{
squishplot(xlim, ylim, asp = 1, newplot=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xlim}{ The x limits of the plot, or the entire x vector.}
  \item{ylim}{ The y limits of the plot, or the entire y vector.}
  \item{asp}{ The y/x aspect ratio.}
  \item{newplot}{ Should plot.new() be called before making the
    calculations.}
}
\details{
  This function sets the plot area of the current graph device so that
  the following plot command will plot with the specified aspect ratio.

  This is different from using the \code{asp} argument to
  \code{plot.default} in where the created white space goes (see the
  example).  Using \code{plot.default} will place the whitespace within
  the plotting region and can result in the axes and annotations being
  quite far from the actual data.  This command sets up the plotting
  region so that the extra whitespace is in the margin areas and moves
  the axes and annotations close to the data.

  Any other desired parameter settings or resizing of the graphics
  device should be set before calling
  \code{squishplot}, especially settings dealing with multiple figures
  or margin areas.

  After plotting, the parameters need to be reset or later plots may
  come out wrong.
}
\value{
  Invisible list containing the '\code{plt}' values from \code{par} that
  were in place before the call to \code{squishplot} that can be used to
  reset the graphical parameters after plotting is finished.
}
\author{ Greg Snow \email{greg.snow@imail.org} }
\note{
  Remember to set other graphical parameters, then call
  \code{squishplot}, then call the plotting function(s), then reset the
  parameters.
}
\seealso{ \code{\link{plot.default}}, \code{\link{plot.window}},
  \code{\link{par}} }
\examples{
x <- rnorm(25, 10, 2 )
y <- 5 + 1.5*x + rnorm(25,0,2)

par(mfrow=c(1,3))
plot(x,y)

op <- squishplot(x,y,1)
plot(x,y)
par(op)

plot(x,y, asp=1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }

