\name{TkSpline}
\alias{TkSpline}
\title{Plot a set of data in a Tk window and interactively move a line to
see predicted y-values from a spline fit corresponding to selected x-values.}
\description{
  This function plots a dataset in a Tk window then draws the spline fit
  through the points.  It places a line to show the predicted y from the
  given x value.  The line can be clicked on and dragged to new x-values
  with the predicted y-values automatically updating.  A table at the
  bottem of the graph shows the values and the 3 derivatives.
}
\usage{
TkSpline(x, y, method='natural', snap.to.x=FALSE, digits=4,
                     col=c('blue','#009900','red','black'),
                     xlab=deparse(substitute(x)), ylab=deparse(substitute(y)),
                     hscale=1.5, vscale=1.5, wait=TRUE,
                     ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The x-values of the data, should be sorted }
  \item{y}{ The corresponding y-values of the data }
  \item{method}{Spline Method, passed to \code{splinefun}}
  \item{snap.to.x}{Logical, if TRUE then the line will only take on the
    values of \code{x}}
  \item{digits}{Number of digits to print, passed to \code{format}}
  \item{col}{Colors of the prediction and other lines}
  \item{xlab}{Label for the x-axis, passed to \code{plot}}
  \item{ylab}{Label for the y-axis, passed to \code{plot}}
  \item{hscale}{Horizontal scaling, passed to \code{tkrplot}}
  \item{vscale}{Vertical scaling, passed to \code{tkrplot}}
  \item{wait}{Should R wait for the window to close}
  \item{\dots}{ Additional parameters passed to \code{plot}}
}
\details{
  This provides an interactive way to explore predictions from a set of
  x and y values.  Internally the function \code{splinefun} is used to
  make the predictions.

  The x-value of the reference line can be changed by clicking and
  dragging the line to a new position.  The x and y values are shown in
  the margins of the graph.  Below the graph is a table with the
  y-value and derivatives.
}
\value{
  If \code{wait} is FALSE then an invisible NULL is returned, if
  \code{wait} is TRUE then an invisible list with the x and y values and
  derivatives is returned.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Greg Snow \email{greg.snow@imail.org}}
%\note{ ~~further notes~~
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{splinefun}}, \code{\link{TkApprox}} }
\examples{
if(interactive()) {
  x <- 1:10
  y <- sin(x)
  TkSpline(x,y, xlim=c(0,11))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot }
\keyword{dynamic }% __ONLY ONE__ keyword per line
