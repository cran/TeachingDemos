\name{TkBuildDist}
\alias{TkBuildDist}
\alias{TkBuildDist2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interactively create a probability distribution.
}
\description{
Build a probability distribution (one option for creating a prior
distribution) by clicking or dragging a plot.
}
\usage{
TkBuildDist(x = seq(min + (max - min)/nbin/2, max - (max - min)/nbin/2,
  length.out = nbin), min = 0, max = 10, nbin = 10, logspline = TRUE,
  intervals = FALSE)

TkBuildDist2( min=0, max=1, nbin=10, logspline=TRUE)
}
\arguments{
  \item{x}{
    A starting set of data points, will default to a sequence of uniform values.
}
  \item{min}{
    The minimum value for the histogram
}
  \item{max}{
    The maximum value for the histogram
}
  \item{nbin}{
    The number of bins for the histogram
}
  \item{logspline}{
    Logical, whether to include a logspline curve on the plot and in the output.
}
  \item{intervals}{
    Logical, should the logspline fit be based on the interval counts
    rather than the clicked data points, also should the interval
    summary be returned.
  }

}
\details{
  Bothe of these functions will open a Tk window to interact with.  The
  window will show a histogram (the defaults will show a uniform
  distribution), optionally a logspline fit line will be included as
  well.  Including the logspline will slow things down a bit, so you may
  want to skip it on slow computers.

  If you use the \code{TkBuildDist} function then a left click on the
  histogram will add an additional point to the histogram bar clicked on
  (the actual x-value where clicked will be saved, returned, and used in
  the optional logspline unless \code{intervals} is TRUE).  Right
  clicking on the histogram will remove the point closest to where
  clicked (based only on x), which will usually have the effect of
  decreasing the clicked bar by 1, but could affect the neigboring bar
  if you click near the edge or click on a bar that is 0.

  If you use the \code{TkBuildDist2} function then the individual bars
  can be adjusted by clicking at the top of a bar and dragging up or
  down, or clicking at what you want the new height of the bar to be.
  As the current bar is adjusted the other bars will adjust in the
  oposite direction proportional to their current heights.

  The logspline fit assumes the basis for the distribution is the real
  line, the \code{min} and \code{max} arguments only control the
  histogram and where values can be changed.
}
\value{
  Both functions return a list with the breaks that were used the
  logspline fit (if \code{logspline} is TRUE), the x-values clicked on
  (for \code{TkBuildDist}), and the proportion of the distribution
  within each interval (for \code{TkBuildDist2} or if \code{intervals}
  is TRUE).
}
\author{Greg Snow \email{538280@gmail.com}}
\seealso{
 The logspline package
}
\examples{

if(interactive()){
tmp1 <- TkBuildDist()
tmp2 <- TkBuildDist2()
}

}
% R documentation directory.
\keyword{ iplot }
\keyword{ dynamic }% __ONLY ONE__ keyword per line
