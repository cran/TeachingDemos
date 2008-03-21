\name{TkApprox}
\alias{TkApprox}
\title{Plot a set of data in a Tk window and interactively move lines to
see predicted y-values corresponding to selected x-values.}
\description{
  This function plots a dataset in a Tk window then places 3 lines on
  the plot which show a predicted y value for the given x value.  The
  lines can be clicked on and dragged to new x-values with the predicted
  y-values automatically updating.  A table at the bottom of the graph
  shows the differences between the pairs of x-values and y-values.
}
\usage{
TkApprox(x, y, type = "b", snap.to.x = FALSE, digits = 4, cols = c("red", "#009900", "blue"), xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), hscale = 1.5, vscale = 1.5, wait = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The x-values of the data, should be sorted }
  \item{y}{ The corresponding y-values of the data }
  \item{type}{ Type of plot (lines, points, both) passed to \code{plot} }
  \item{snap.to.x}{If True then the lines will snap to x-values (can be
    changed with a checkbox in the Tk window) }
  \item{digits}{Number of significant digits to display (passed to
    \code{format}) }
  \item{cols}{Vector of 3 colors, used for the reference lines }
  \item{xlab}{ Label for x-axis }
  \item{ylab}{ Label for y-axis }
  \item{hscale}{ Horizontal Scale of the plot, passed to \code{tkrplot} }
  \item{vscale}{ Vertical Scale of the plot, passed to \code{tkrplot} }
  \item{wait}{ Should R wait for the window to be closed }
  \item{\dots}{ Additional parameters passed to \code{plot}}
}
\details{
  This provides an interactive way to explore predictions from a set of
  x and y values.  Internally the function \code{approxfun} is used to
  make the predictions.

  The x-value of the 3 reference lines can be changed by clicking and
  dragging the line to a new position.  The x and y values are shown in
  the margins of the graph.  Below the graph is a table with the
  differences (absolute value) between the pairs of points.

  This can be used to find peaks/valleys in trends and to see how they
  differ from starting points, other peaks/valleys, etc..
}
\value{
  If \code{wait} is FALSE then an invisible NULL is returned, if
  \code{wait} is TRUE then an invisible list with the x and y values of
  the 3 reference lines is returned.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Greg Snow \email{greg.snow@imail.org}}
%\note{ ~~further notes~~
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{approxfun}}, \code{\link{TkSpline}} }
\examples{
if(interactive()) {
  with(ccc, TkApprox(Time2,Elevation))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot }
\keyword{dynamic }% __ONLY ONE__ keyword per line
