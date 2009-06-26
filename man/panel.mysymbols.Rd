\name{panel.my.symbols}
\alias{panel.my.symbols}
\title{Draw Symbols (User Defined) on a Lattice Plot}
\description{This function draws symbols on a lattice plot.  It is similar to
  the builtin \code{symbols} function with the difference that it plots
  symbols defined by the user rather than a prespecified set of
  symbols.}
\usage{
panel.my.symbols(x, y, symb, inches=1, polygon=FALSE,
                       ..., symb.plots=FALSE, subscripts, MoreArgs)
}
\arguments{
  \item{x, y}{The \code{x} and \code{y} coordinates for the position of
  the symbols to be plotted.  These can be specified in any way which is
  accepted by \code{xy.coords}.}
  \item{symb}{Either a matrix, list, or function defining the symbol to
  be plotted.  If it is a matrix or list it needs to be formatted that
  it can be passed directly to the \code{llines} function.  It then
  defines the shape of the symbol on on a range/domain of -1 to 1.  If
  this is a function it can either return a matrix or list as above
  (points on the range/domain of -1 to 1).}
  \item{inches}{The size of the square containing the symbol in inches
    (note: unlike \code{symbols} this cannot be \code{FALSE}).}
  \item{polygon}{If TRUE, use \code{lpolygon} function to plot rather than the
    \code{llines} function.}
  \item{symb.plots}{Currently not implemented.}
  \item{...}{Additional arguments will be replicated to the same length
  as \code{x} then passed to \code{symb} (if \code{symb} is a function)
  and/or the \code{lines} function (one value per symbol drawn).}
  \item{subscripts}{subscripts for the current panel}
  \item{MoreArgs}{A list with any additional arguments to be passed to
  the \code{symb} function (as is, without being replicated/split).}
}
\details{
  The \code{symb} argument can be a 2 column matrix or a list with
  components 'x' and 'y' that defines points on the interval [-1,1] that
  will be connected with lines to draw the symbol.  If you want a closed
  polygon then be sure to replicate the 1st point as the last point or
  use the \code{polygon} option.

  If any point contains an NA then the line will not be drawn to or from
  that point.  This can be used to create a symbol with disjoint parts
  that should not be connected.

  If \code{symb} is a function then any unmatched
  arguments that end up in the '...' argument will be replicated to the
  same length as 'x' (using the \code{rep} function) then the values
  will be passed one at a time to the \code{symb} function.  If
  \code{MoreArgs} is specified, the elements of it will also be passed
  to \code{symb} without modification.  The \code{symb} function can
  either return a matrix or list with the points that will then be
  passed to the \code{llines} function (see above).
}
\value{
  This function is run for its side effect of plotting, it returns an
  invisible NULL.
}
\author{Greg Snow \email{greg.snow@imail.org}}
\note{
  Plotting coordinates and sizes are based on the size of the device at
  the time the function is called.  If you resize the device after
  plotting, all bets are off.
}
\seealso{\code{\link{symbols}}, \code{\link{my.symbols}}, \code{\link{subplot}},
  \code{\link{mapply}}, \code{\link{ms.polygram}}, \code{\link{lines}}}
\examples{
if(require(lattice)) {
  tmpdf <- data.frame( x=1:10, y=1:10, g=rep( c("A","B"), each=5 ),
	z=c(1:5,5:1) )

  xyplot( y ~ x, tmpdf, panel=panel.my.symbols, symb=ms.female, inches=0.3 )

  xyplot( y ~ x | g, tmpdf, panel=panel.my.symbols, symb=ms.male, inches=0.3)

  xyplot( y ~ x, tmpdf, panel=panel.superpose, groups=g,
	panel.groups= function(group.number, ...) {
		if(group.number==1) {
			panel.my.symbols(..., symb=ms.male)
		} else {
			panel.my.symbols(..., symb=ms.female)
		} },
		inches=0.3
	)

  xyplot( y ~ x, tmpdf, panel=panel.my.symbols, symb=ms.polygram, n=tmpdf$z,
		inches=0.3)

  xyplot( y ~ x | g, tmpdf, panel=panel.my.symbols, symb=ms.polygram,
		n=tmpdf$z, inches=0.3)

  xyplot( y ~ x, tmpdf, panel=panel.superpose, groups=g,
	panel.groups = panel.my.symbols,
		inches=0.3, symb=ms.polygon, n=tmpdf$z, polygon=TRUE,
		adj=rep(c(0,pi/4),5)
	)
}
}
\keyword{aplot}
\keyword{dplot}
\keyword{hplot}
