\name{lattice.demo}
\alias{lattice.demo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Interactively explore the conditioned panels in lattice plots. }
\description{
  Plot 1 panel from an xyplot, and optionally a 3d graph highligting the
  shown points, then allow you to interactively set the conditioning set
  of data to see the effects and help you better understand how xyplot works.
}
\usage{
lattice.demo(x, y, z, show3d = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The x variable to plot (numeric). }
  \item{y}{ The y variable to plot (numeric). }
  \item{z}{ The variable to condition on (numeric). }
  \item{show3d}{ Logical, should a 3D cloud be shown as well.}
}
\details{
  This function is intended to for demonstration purposes to help
  understand what is happening in an \code{xyplot} (lattice).  When you
  run the demo it will create a single panel from a conditioned
  \code{xyplot} and optionally a 3D cloud with the points included in
  the panel highlighted.  The function then opens a tcl/tk dialog box
  that allows you to choose which points are included in the panel
  (based on the conditioning variable).  You can choose the center and
  width of the shingle displayed and the graph will update to show the
  new selection.

  The intent for this function is for a teacher to show a class how
  lattice graphics take slices of a 3d plot and show each slice
  seperately.  Students could then work through some examples on their
  own to better understand what functions like \code{xyplot} are doing
  automatically. 
}
\value{
  No meaningful return value, this function is run for the side effects.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow \email{greg.snow@imail.org} }
%\note{ ~~further notes~~ }



\seealso{\code{xyplot} in lattice package}
\examples{

if(interactive()){
require(stats)
lattice.demo(quakes$long, quakes$lat, quakes$depth)
}

}
\keyword{ hplot }% at least one, from doc/KEYWORDS
\keyword{ dynamic }% __ONLY ONE__ keyword per line
