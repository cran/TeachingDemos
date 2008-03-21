\name{strip.shingle}
\alias{strip.shingle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ strip function for trellis plots }
\description{
  The \code{strip.shingle} function is a modification of
  \code{strip.default} that shows the range of each shingle in the
  panel strips as well as the name of the shingle variable.

  This function is now depricated, you can do the same thing with
  \code{strip.custom} now, see the example below.
}
\usage{
strip.shingle(which.given, which.panel, var.name, factor.levels, shingle.intervals, strip.names = c(FALSE, TRUE), style = 1, bg = trellis.par.get("strip.background")$col[which.given], fg = trellis.par.get("strip.shingle")$col[which.given], par.strip.text = trellis.par.get("add.text"),...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{which.given}{ See \code{strip.default}. }
  \item{which.panel}{ See \code{strip.default}. }
  \item{var.name}{ See \code{strip.default}. }
  \item{factor.levels}{See \code{strip.default}.  }
  \item{shingle.intervals}{ See \code{strip.default}. }
  \item{strip.names}{ See \code{strip.default}. }
  \item{style}{ See \code{strip.default}. }
  \item{bg}{ See \code{strip.default}. }
  \item{fg}{ See \code{strip.default}. }
  \item{par.strip.text}{ See \code{strip.default}. }
  \item{...}{Arguments passed through}
}
\details{
  Does the same as \code{strip.default} with the exception that by
  default when a shingle is used as the conditioning variable, the strip
  at the top of the panel will inclued the name of the conditioning
  variable and the range of the shingle for that plot.

  This function is now depricated, see the example below for the way to
  do the same thing using core lattice code.
}
\value{
  This function is only usefull for its side effects.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow \email{greg.snow@imail.org} }
%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{strip.default}, \code{strip.custom} }
\examples{
library(lattice)
Depth <- equal.count(quakes$depth, number=8, overlap=.1)

\dontrun{
# this is the old way, don't do this anymore use the example below
xyplot(lat ~ long | Depth, data = quakes, strip=strip.shingle)
}

if(require(lattice)) {
xyplot(lat ~ long | Depth, data=quakes,
  strip=strip.custom(strip.names=TRUE, strip.levels=TRUE),
  par.strip.text=list(cex=0.75))
}

}
\keyword{ aplot }% at least one, from doc/KEYWORDS
\keyword{ hplot }% __ONLY ONE__ keyword per line
\keyword{ dplot }