\name{run.cor.examp}
\alias{run.cor.examp}
\alias{run.cor2.examp}
\alias{run.old.cor.examp}
\alias{run.old.cor2.examp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Interactively demonstrate correlations }
\description{
  Make a scatterplot and a Tk slider window that allows you to
  interactively set the correlation and/or R\^2.
}
\usage{
run.cor.examp(n=100, seed, vscale=1.5, hscale=1.5, wait=FALSE)
run.cor2.examp(n=100, seed, vscale=1.5, hscale=1.5, wait=FALSE)
run.old.cor.examp(n = 100, seed)
run.old.cor2.examp(n = 100, seed)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ Number of points to plot. }
  \item{seed}{ What seed to use. }
  \item{vscale}{ Vertical scale passed to tkrplot. }
  \item{hscale}{ Horizontal scale passed to tkrplot. }
  \item{wait}{ Should R wait for the tk window to close. }
}
\details{
  The function \code{run.cor.examp} draws a scatterplot and allows you
  to set the correlation using a Tk slider window.

  The function \code{run.cor2.examp} does the same, but has a slider for
  R\^2 as well as the correlation, when either slider is moved the other
  one will update to match.

  The 2 "old" versions use the default graphics device with a seperate
  window with the sliders, the versions without "old" in the name
  include the plot and sliders together in a single tk window.

  The size of the plot can be changed by changing the values in the
  hscale and vscale boxes and clicking on the "Refresh" button.
}
\value{
  If \code{wait} is TRUE, then the return value is a list with the x and
  y values of the final plot.

  If \code{wait} is FALSE (and in the "old" versions) an invisible NULL
  is returned.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow \email{538280@gmail.com} }
\note{ If \code{wait} is TRUE then R will wait until you click on the
  "Exit" button before you can use your R session again.  If \code{wait}
  is FALSE then the tk window will appear, but R will regain control so
  that you can continue to use R as well as interact with the
  demonstration window.}

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{cor}}, \code{\link{tkexamp}} }
\examples{
if(interactive()) {
  run.cor2.examp()
}
}
\keyword{ dynamic }% at least one, from doc/KEYWORDS

