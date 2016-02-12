\name{zoomplot}
\alias{oldzoomplot}
\alias{zoomplot}
\title{Zoom or unzoom an existing plot in the plot window}
\description{This function allows you to change the x and y ranges of
  the plot that is currently in the plot window.  This has the effect of
  zooming into a section of the plot, or zooming out (unzooming) to show
  a larger region than is currently shown.}
\usage{
zoomplot(xlim, ylim=NULL )
oldzoomplot(xlim, ylim=NULL )
}
\arguments{
  \item{xlim, ylim}{The new x and y limits of the plot.  These can be
  passed in in any form understood by \code{xy.coords}.  The range of
  xlim and ylim are actually used so you can pass in more than 2 points.}
}
\details{
  This function recreates the current plot in the graphics window but
  with different xlim, ylim arguments.  This gives the effect of zooming
  or unzooming the plot.

  This only works with traditional graphics (not lattice/trellis).

  This function is a quick hack that should only be used for quick
  exploring of data.  For any serious work you should create a script
  with the plotting commands and adjust the xlim and ylim parameters to
  give the plot that you want.

  Only the x and y ranges are changed, the size of the plotting
  characters and text will stay the same.

  The \code{oldzoomplot} function is the version that worked for 2.15
  and earlier, \code{zoomplot} should be used for R 3.0.
}
\value{
  This function is run for its side effects and does not return anything
  meaningful.
}
\author{Greg Snow \email{538280@gmail.com}}
\note{ For any serious projects it is best to put your code into a
  script to begin with and edit the original script rather than using
  this function.

  This function works with the standard \code{plot} function and some
  others, but may not work for more complicated plots.

  This function depends on the \code{recordPlot} function which can
  change in any version.  Therefore this function should not be
  considered stable.
}
\seealso{\code{\link{plot.default}}, \code{\link{par}},
  \code{\link{matplot}}, \code{\link{plot2script}},
  \code{\link{source}}}
\examples{
if(interactive()){

with(iris, plot(Sepal.Length, Petal.Width,
    col=c('red','green','blue')[Species]))
text( 6.5, 1.5, 'test' )
zoomplot( locator(2) ) # now click on 2 points in the plot to zoom in

plot( 1:10, rnorm(10) )
tmp <- rnorm(10,1,3)
lines( (1:10) + 0.5, tmp, col='red' )
zoomplot( c(0,11), range(tmp) )
}
}
\keyword{dplot}
\keyword{iplot}

