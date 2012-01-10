\name{gp.open}
\alias{gp.open}
\alias{gp.close}
\alias{gp.send}
\alias{gp.plot}
\alias{gp.splot}
\title{Alpha version functions to send plotting commands to GnuPlot}
\description{These functions allow you to open a connection to a gnuplot
  process, send data and possibly other information to gnuplot for it to
  plot, then close gnuplot and clean up temporary files and variables.
  These functions are alpha level at best, use at your own risk.}
\usage{
gp.open(where='c:/progra~1/GnuPlot/bin/pgnuplot.exe')
gp.close(pipe=gpenv$gp)
gp.send(cmd='replot',pipe=gpenv$gp)
gp.plot(x,y,type='p',add=FALSE, title=deparse(substitute(y)),pipe=gpenv$gp)
gp.splot(x,y,z, add=FALSE, title=deparse(substitute(z)), pipe=gpenv$gp, datafile=tempfile())
}
\arguments{
  \item{where}{Path to GnuPlot Executable}
  \item{pipe}{The pipe object connected to GnuPlot (returned from
    \code{gp.open}), warning: changing this from the default will
    probably break things}
  \item{cmd}{Text string, the command to be sent verbatim to the GnuPlot
    process}
  \item{x}{The \code{x} coordinates to plot}
  \item{y}{the \code{y} coordinates to plot}
  \item{z}{the \code{z} coordinates to splot}
  \item{type}{Either 'p' or 'l' for plotting points or lines}
  \item{add}{Logical, should the data be added to the existing plot or
    start a new plot}
  \item{title}{The title or legend entry}
  \item{datafile}{The file to store the data in for transfer to gnuplot}
}
\details{
  These functions provide a basic interface to the GnuPlot program (you
  must have GnuPlot installed (separate install)), \code{gp.open} runs
  GnuPlot and establishes a pipe connection, \code{gp.close} sends a
  quite command to gnuplot and cleans up temporary variables and files,
  \code{gp.send} sends a command to the GnuPlot process verbatim, and
  \code{gp.plot} sends data and commands to the process to create a
  standard scatterplot or line plot.
}
\value{
  \code{gp.open} returns and invisible copy of the pipe connection
  object (to pass to other functions, but don't do this because it
  doesn't work right yet).

  The other 3 functions don't return anything meaningful.  All functions
  are run for their side effects.
}
\references{
  \url{http://www.gnuplot.info/}
}
\author{ Greg Snow \email{greg.snow@imail.org} }
\note{
  These functions create some temporary files and 2 temporary global
  variables (.gp and .gp.tempfiles), running \code{gp.close} will clean
  these up (so use it).

  These functions are still alpha level.
}
\seealso{\code{\link{plot}} }
\examples{
 \dontrun{

x <- 1:10
y <- 3-2*x+x*x+rnorm(10)

gp.open()
gp.plot(x,y)
gp.send('replot 3-2*x+x**2')

tmp <- expand.grid(x=1:10, y=1:10)
tmp <- transform(tmp, z=(x-5)*(y-3))
gp.splot(tmp$x, tmp$y, tmp$z)

gp.close()
 }
}
\keyword{hplot}



