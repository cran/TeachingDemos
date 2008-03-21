\name{plot2script}
\alias{plot2script}
\title{Create a script from the current plot}
\description{This function attempts to create a script that will
  recreate the current plot (in the graphics window).  You can then edit
  any parts of the script that you want changed and rerun to get the
  modified plot.}
\usage{
plot2script(file='clipboard')
}
\arguments{
  \item{file}{The filename (the clipboard by default) for the script to
  create or append to.}
}
\details{
  This function works with the graphics window and mainly traditional
  graphics (it may work with lattice or other graphics, but has not
  really been tested with those).

  This function creates a script file (or puts it on the clipboard so
  that you can past into a script window or text editor) that will
  recreate the current graph in the current graph window.  The script
  consists of very low level functions (calls to \code{plot.window} and
  \code{axis} rather than letting \code{plot} handle all this).

  If you want the higher level functions that were actually used, then
  use the \code{history} or \code{savehistory} commands (this will
  probably be the better method for most cases).

  Some of the low level plotting functions use different arguments to
  the internal version than the user callable version (\code{box} for
  example), the arguments to these functions may need to be editted
  before the full script will run correctly.

  The lengths of command lines between the creation of the script and
  what can be run in R do not always match, you may need to manually
  wrap long lines in the script before it will run properly.
}
\value{
  This function is run for its side effects and does not return anything
  meaningful.
}
\author{Greg Snow \email{greg.snow@imail.org}}
\note{ For any serious projects it is best to put your code into a
  script to begin with and edit the original script rather than using
  this function.

  This function depends on the \code{recordPlot} function which can
  change in any version.  Therefore this function should not be
  considered stable.
}
\seealso{\code{\link{history}}, \code{\link{savehistory}},
  \code{\link{recordPlot}}, \code{\link{source}} }
\examples{
if(interactive()){

# create a plot
plot(runif(10),rnorm(10))
lines( seq(0,1,length=10), rnorm(10,1,3) )

# create the script
plot2script()

# now paste the script into a script window or text processor.
# edit the ranges in plot.window() and change some colors or
# other options.  Then run the script.
 }
}
\keyword{iplot}
\keyword{dplot}