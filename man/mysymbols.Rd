\name{my.symbols}
\alias{my.symbols}
\title{Draw Symbols (User Defined) on a Plot}
\description{This function draws symbols on a plot.  It is similar to
  the builtin \code{symbols} function with the difference that it plots
  symbols defined by the user rather than a prespecified set of
  symbols.}
\usage{
my.symbols(x, y=NULL, symb, inches=1, xsize, ysize,
                       add=TRUE,
                       vadj=0.5, hadj=0.5,
                       symb.plots=FALSE,
                       xlab=deparse(substitute(x)),
                       ylab=deparse(substitute(y)), main=NULL,
                       xlim=NULL, ylim=NULL, linesfun=lines,
                       ..., MoreArgs)
}
\arguments{
  \item{x, y}{The \code{x} and \code{y} coordinates for the position of
  the symbols to be plotted.  These can be specified in any way which is
  accepted by \code{xy.coords}.}
  \item{symb}{Either a matrix, list, or function defining the symbol to
  be plotted.  If it is a matrix or list it needs to be formatted that
  it can be passed directly to the \code{lines} function.  It then
  defines the shape of the symbol on on a range/domain of -1 to 1.  If
  this is a function it can either return a matrix or list as above
  (points on the range/domain of -1 to 1), or it can do the plotting
  itself.}
  \item{inches}{The size of the square containing the symbol in inches
  (note: unlike \code{symbols} this cannot be \code{FALSE}).  This is
  ignored if \code{xsize} or \code{ysize} is specified.}
  \item{xsize}{The width of the bounding box(s) of the symbols in the same
    units as the \code{x} variable.  Computed from \code{ysize} or
    \code{inches} if not specified.  Can be a single value or a vector.}
  \item{ysize}{The height of the bounding box(s) of the symbols in the
    same units as the \code{y} variable.  Computed from \code{xsize} or
    \code{inches} if not specified.  Can be a single value or a vector.}
  \item{add}{if 'add' is 'TRUE' then the symbols are added to the
  existing plot, otherwise a new plot is created.}
  \item{vadj,hadj}{Numbers between 0 and 1 indicating how 'x' and 'y'
  specify the location of the symbol.  The defaults center the symbol at
  x,y; 0 means put the bottom/left at x,y; and 1 means put
  the top/right of the symbol at x,y.}
  \item{symb.plots}{If \code{symb} is a function that does its own
  plotting, set this to TRUE, otherwise it should be FALSE.}
  \item{xlab, ylab, main, xlim, ylim}{If 'add' is 'FALSE' these are
  passed to the \code{plot} function when setting up the plot.}
  \item{linesfun}{The function to draw the lines if the function does not do its own drawing.  The default is \code{lines} but could be replaced with \code{polygon} to draw filled polygons}
  \item{...}{Additional arguments will be replicated to the same length
  as \code{x} then passed to \code{symb} (if \code{symb} is a function)
  and/or the \code{lines} function (one value per symbol drawn).}
  \item{MoreArgs}{A list with any additional arguments to be passed to
  the \code{symb} function (as is, without being replicated/split).}
}
\details{
  The \code{symb} argument can be a 2 column matrix or a list with
  components 'x' and 'y' that defines points on the interval [-1,1] that
  will be connected with lines to draw the symbol.  If you want a closed
  polygon then be sure to replicate the 1st point as the last point.

  If any point contains an NA then the line will not be drawn to or from
  that point.  This can be used to create a symbol with disjoint parts
  that should not be connected.

  If \code{symb} is a function then it should include a '...' argument
  along with any arguments to define the symbol.  Any unmatched
  arguments that end up in the '...' argument will be replicated to the
  same length as 'x' (using the \code{rep} function) then the values
  will be passed one at a time to the \code{symb} function.  If
  \code{MoreArgs} is specified, the elements of it will also be passed
  to \code{symb} without modification.  The \code{symb} function can
  either return a matrix or list with the points that will then be
  passed to the \code{lines} function (see above).  Or the function can
  call the plotting functions itself (set \code{symb.plots} to TRUE).
  High level plotting can be done (\code{plot}, \code{hist}, and other
  functions), or low level plotting functions (\code{lines},
  \code{points}, etc) can be used; in this case they should add things
  to a plot with 'x' and 'y' limits of -1 to 1.

  The size of the symbols can be specified by using \code{inches} in
  which case the symbol will be set inside of squares whose sizes are
  \code{inches} size based on the plotting device.  The size can also be
  set using \code{xsize} and/or \code{ysize} which use the same units as
  the \code{x} and/or \code{y} variables.  If only one is specified then
  the box will be square.  If both are specified and they do not match
  the aspect ratio of the plot then the bounding box will not be square
  and the symbol will be distorted.
}
\value{
  This function is run for its side effect of plotting, it returns an
  invisible NULL.
}
\author{Greg Snow \email{greg.snow@imail.org}}
\note{Since the '...' argument is passed to both \code{lines} and
  \code{symb}, the \code{symb} function should have a '...' argument so
  that it will ignore any additional arguments.

  Arguments such as 'type' can be passed through the '...' argument if
  you want the symbol made of something other than lines.

  Plotting coordinates and sizes are based on the size of the device at
  the time the function is called.  If you resize the device after
  plotting, all bets are off.

  Currently missing values in \code{x} or \code{y} are not handled
  well.  It is best if remove all missing values first.
}
\seealso{\code{\link{symbols}}, \code{\link{subplot}},
  \code{\link{mapply}}, \code{\link{ms.polygram}}, \code{\link{lines}}}
\examples{

# symb is matrix

my.symbols( 1:10, runif(10), ms.male, add=FALSE, xlab='x',
  ylab='y', inches=0.3, col=c('blue','green'), xlim=c(0,11), ylim=c(-0.1,1.1))
my.symbols( (1:10)+0.5, runif(10), ms.female, add=TRUE, inches=0.3,
  col=c('red','green') )

# symb is function returning matrix

plot(1:10, 1:10)
my.symbols( 1:10, 1:10, ms.polygram, n=1:10, inches=0.3 )


# symb is plotting function
# create a variation on monthplot

fit <- lm( log(co2) ~ time(co2) )
fit.r <- resid(fit)

x <- 1:12
y <- tapply(fit.r, cycle(co2), mean)

tmp.r <- split( fit.r, cycle(co2) )
tmp.r <- lapply( tmp.r, function(x) x-mean(x) )

yl <- do.call('range',tmp.r)

tmpfun <- function(w,data,ylim,...){
  tmp <- data[[w]]
  plot(seq(along=tmp),tmp, type='l', xlab='',ylab='',
       axes=FALSE, ylim=ylim)
  lines(par('usr')[1:2], c(0,0), col='grey')
}

my.symbols(x,y, symb=tmpfun, inches=0.4, add=FALSE, symb.plots=TRUE,
  xlab='Month',ylab='Adjusted CO2', xlim=c(0.5,12.5),
  ylim=c(-0.012,0.012),
  w=1:12, MoreArgs=list(data=tmp.r,ylim=yl) )

# using xsize and ysize
plot( 1:10, (1:10)*100, type='n', xlab='', ylab='' )
my.symbols( 5, 500, ms.polygon, n=250, inches=1.5 )
my.symbols( 5, 500, ms.polygon, n=250, xsize=2, col='blue' )
my.symbols( 5, 500, ms.polygon, n=250, ysize=200, col='green' )
my.symbols( 5, 500, ms.polygon, n=250, xsize=2, ysize=200, col='red' )
abline( v=c(4,6), col='grey' )
abline( h=c(400, 600), col='grey' )


# hand crafted hexagonal grid

x1 <- seq(0, by=2*sqrt(3), length.out=10)
y1 <- seq(0, by=3, length.out=10)

mypoints <- expand.grid(x=x1, y=y1)
mypoints[,1] <- mypoints[,1] + rep( c(0,sqrt(3)), each=10, length.out=100 )

plot(mypoints, asp=1, xlim=c(-2,35))
my.symbols(mypoints, symb=ms.filled.polygon, n=6,
  inches=par('pin')[1]/(diff(par('usr')[1:2]))*4,
  bg=paste('gray',1:100,sep=''), fg='green' )

}
\keyword{aplot}
\keyword{dplot}
\keyword{hplot}
