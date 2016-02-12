\name{sliderv}
\alias{sliderv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create a Tk slider window }
\description{
  Create a Tk slider window with the sliders positioned vertically
  instead of horizontally.
}
\usage{
sliderv(refresh.code, names, minima, maxima, resolutions, starts,
  title = "control", no = 0, set.no.value = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{refresh.code}{ Function to be called when sliders are moved. }
  \item{names}{ Labels for the sliders. }
  \item{minima}{ Vector of minimum values for the sliders. }
  \item{maxima}{ Vector of maximum values for the sliders. }
  \item{resolutions}{ Vector of resolutions for the sliders. }
  \item{starts}{ Vector of starting values for the sliders. }
  \item{title}{ Title to put at the top of the Tk box. }
  \item{no}{ The number of the slider whose value you want. }
  \item{set.no.value}{ Vector of length 2 with the number of slider to
    set and the new value. }
}
\details{
  This is a variation on the \code{slider} function with vertical sliders
  arranged in a row rather than horizontal sliders arranged in a column.

  This is based on an early version of \code{slider} and therefore does
  not have as many bells and whistles (but sometimes fits the screen better).
}
\value{
  Returns the value of a given slider when used as: \code{slider(no=i)}.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow \email{538280@gmail.com} }
\note{
  You can move the slider in 3 different ways:  You can left click
  and drag the slider itself, you can left click in the trough to either
  side of the slider and the slider will move 1 unit in the direction
  you clicked, or you can right click in the trough and the slider will
  jump to the location you clicked at.

  This function may not stay in this package (consider it
  semi-depricated).  See the \code{\link{tkexamp}} function for another
  approach to do the same thing.
}

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{tkexamp}}, \code{\link{slider}} }
\examples{
if(interactive()){
face.refresh <- function(...){
	vals <- sapply(1:15, function(x) slider(no=x))
	faces( rbind(0, vals, 1), scale=F)
}

sliderv( face.refresh, as.character(1:15), rep(0,15), rep(1,15),
	rep(0.05, 15), rep(0.5,15), title='Face Demo')
}
}
\keyword{ dynamic }% at least one, from doc/KEYWORDS
\keyword{ iplot}
