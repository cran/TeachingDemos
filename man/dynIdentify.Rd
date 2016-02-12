\name{dynIdentify}
\alias{dynIdentify}
\alias{TkIdentify}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Interacively place labels for points in a plot }
\description{
  These functions create a scatterplot of your points and place labels
  for the points on them.  You can then use the mouse to click and drag
  the labels to new positions with a line stretching between the point
  and label.
}
\usage{
dynIdentify(x, y, labels = seq_along(x),
  corners = cbind(c(-1, 0, 1, -1, 1, -1, 0, 1),
    c(1, 1, 1, 0, 0, -1, -1, -1)), ...)
TkIdentify(x, y, labels=seq_along(x), hscale=1.75, vscale=1.75,
  corners = cbind( c(-1,0,1,-1,1,-1,0,1), c(1,1,1,0,0,-1,-1,-1) ),...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{x-values to plot }
  \item{y}{y-values to plot }
  \item{labels}{Labels for the points, defaults to a sequence of integers }
  \item{corners}{ 2 column matrix of locations where the line can attach
    to the label, see below}
  \item{hscale,vscale}{Scaling passed to tkrplot}
  \item{\dots}{Additional parameters passed to \code{plot}}
}
\details{
  These functions create a scatterplot of the x and y points with the
  labels (from the argument above) plotted on top.  You can then use the
  mouse to click and drag the labels to new locations.  The Tk version
  shows the labels being dragged, \code{dynIdentify} does not show the
  labels being dragged, but the label will jump to the new location as
  soon as you release the mouse button.

  The \code{corners} argument is a 2 column matrix that gives the
  allowable points at which the line from the point can attach to the
  label (so the line does not cover thelabel).  The first column
  represents the x-coordinates and the 2nd column the y-coordinates.  A
  1 represents the right/top of the label, A -1 is the left/bottom and a
  0 is the center.  The default values allow attachments at the 4
  corners and the centers of the 4 sides of the rectangle bounding the label.
}
\value{
  A list of lists with the coordinates of the final positions of the
  labels and the line ends.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow, \email{538280@gmail.com} }
\seealso{\code{\link{identify}} }
\note{
  The \code{dynIdentify} function only works on windows,
  \code{TkIdentify} should work on any platform with tcltk.
}
\examples{
if(interactive()) {
  tmp <- TkIdentify(state.x77[,'Frost'], state.x77[,'Murder'],
  state.abb)
     ### now move the labels

     ### recreate the graph on the current device
  plot( state.x77[,'Frost'], state.x77[,'Murder'],
        xlab='Frost', ylab='Frost')
  text( tmp$labels$x, tmp$labels$y, state.abb )
  segments( state.x77[,'Frost'], state.x77[,'Murder'],
            tmp$lineends$x, tmp$lineends$y )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dynamic }
\keyword{ aplot }% __ONLY ONE__ keyword per line
