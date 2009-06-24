\name{spread.labs}
\alias{spread.labs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Spread out close points for labeling in plots }
\description{
This function takes as set of coordinates and spreads out the close
values so that they can be used in labeling plots without overlapping.
}
\usage{
spread.labs(x, mindiff, maxiter = 1000, stepsize = 1/10, min = -Inf, max = Inf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The coordinate values (x or y, not both) to spread out. }
  \item{mindiff}{ The minimum distance between return values }
  \item{maxiter}{ The maximum number of iterations }
  \item{stepsize}{ How far to move values in each iteration }
  \item{min}{ Minimum bound for returned values }
  \item{max}{ Maximum bound for returned values }
}
\details{
  Sometimes the desired locations for labels in plots results in the
  labels overlapping.  This function takes the coordinate values (x or
  y, not both) and finds those points that are less than \code{mindiff}
  (usually a function of \code{strheight} or \code{strwidth}) apart and
  increases the space between them (by \code{stepsize} *
  \code{mindiff}).  This may or may not be enough and moving some points
  away from their nearest neighbor may move them too close to another
  neighbor, so the process is iterated until either \code{maxiter} steps
  have been tried, or all the values are at least \code{mindiff} apart.

  The \code{min} and \code{max} arguments prevent the values from going
  outside that range (they should be specified such that the original
  values are all inside the range).

  The values do not need to be presorted.
}
\value{
  A vector of coordinates (order corresponding to the original \code{x})
  that can be used as a replacement for \code{x} in placing labels.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow, \email{greg.snow@imail.org} }
%\note{ ~~further notes~~
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{text}}, the \code{spread.labels} function in the
  \code{plotrix} package. }
\examples{
# overlapping labels
plot(as.integer(state.region), state.x77[,1], ylab='Population',
	xlab='Region',xlim=c(1,4.75), xaxt='n')
axis(1, at=1:4, lab=levels(state.region) )

text( as.integer(state.region)+.5, state.x77[,1], state.abb )
segments( as.integer(state.region)+0.025, state.x77[,1],
		as.integer(state.region)+.375, state.x77[,1] )

# now lets redo the plot without overlap

tmp.y <- state.x77[,1]
for(i in levels(state.region) ) {
  tmp <- state.region == i
  tmp.y[ tmp ] <- spread.labs( tmp.y[ tmp ], 1.2*strheight('A'),
	maxiter=1000, min=0 )
}

plot(as.integer(state.region), state.x77[,1], ylab='Population',
	xlab='Region', xlim=c(1,4.75), xaxt='n')
axis(1, at=1:4, lab=levels(state.region) )

text( as.integer(state.region)+0.5, tmp.y, state.abb )
segments( as.integer(state.region)+0.025, state.x77[,1],
		as.integer(state.region)+0.375, tmp.y )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }

