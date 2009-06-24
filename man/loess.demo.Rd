\name{loess.demo}
\alias{loess.demo}

\title{ Demonstrate the internals of loess curve fits }
\description{
  Creates a scatterplot with a loess fit, then interactively shows the
  window and case weights used to create the curve at the selected value
  of \code{x}.
}
\usage{
loess.demo(x, y, span = 2/3, degree = 1, nearest = F, xlim = numeric(0), ylim = numeric(0), verbose = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The \code{x} coordinates to be plotted. }
  \item{y}{ The \code{y} coordinates to be plotted. }
  \item{span}{ The relative width of the window, passed on to \code{loess}. }
  \item{degree}{ Degree of polynomial to use (0, 1, or 2), passed on to \code{loess}. }
  \item{nearest}{ Logical, should predictions be made at the point where
  you clicked (FALSE), or at the nearest x value of the data to where
  you clicked (TRUE).}
  \item{xlim}{ Limits of the Horizonal axis. }
  \item{ylim}{ Limits of the Vertical axis. }
  \item{verbose}{ If true then print the x coordinate being predicted. }
}
\details{
  This function demonstrates the underlying calculations of loess
  curves.

  Given \code{x} and \code{y} vectors it will create a scatterplot and
  add 2 loess fit lines (one using straight loess smooth with linear
  interpolation and one that does a spline interpolation of the loess
  fit).

  The function then waits for the user to click on the plot.  The
  function then shows the window of points (centered at the \code{x}
  value clicked on) used in the weighting for predicting that point and
  shows a circle around each point in the window where the area of the
  circle is proportional to the weight of that point in the linear fit.
  The function also shows the linear (or quadratic) fit used to predict
  at the selected point.

  The basic steps of the loess algorithm (as demonstrated by the
  function)  is that to predict the y-value for a given x-value the
  computer:

  1. Find all the points within a window around the x-value (the width
     of the window is based on the parameter \code{span}).
  2. Weight the points in the window with points nearest the x-value
     having the highest weight.
  3. Fit a weighted linear (quadratic) line to the points in the
     window.
  4. Use the y-value of the fitted line (curve) at the x-value to give
     loess prediction at that x-value.

     Clicking on another point in the graph will replot with the new
     situation.

     Right click and select 'stop' to end the demonstration.
}
\value{
  This function does not return anything, it is run purely for its side
  effects.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow \email{greg.snow@imail.org} }
%\note{ ~~further notes~~ }



\seealso{ \code{\link{loess}}, \code{\link{locator}} }
\examples{

if(interactive()){
data(ethanol, package='lattice')
attach(ethanol)
loess.demo(E, NOx)
# now click a few places, right click to end
loess.demo(E, NOx, span=1.5)
loess.demo(E, NOx, span=0.25)
loess.demo(E, NOx, degree=0)
loess.demo(E, NOx, degree=2)
detach()
}
}
\keyword{ hplot }% at least one, from doc/KEYWORDS
\keyword{ dynamic }% __ONLY ONE__ keyword per line
\keyword{ iplot }