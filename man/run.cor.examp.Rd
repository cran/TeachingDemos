\name{run.cor.examp}
\alias{run.cor.examp}
\alias{run.cor2.examp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Interactively demonstrate correlations }
\description{
  Make a scatterplot and a Tk slider window that allows you to
  interactively set the correlation and/or R\^2.
}
\usage{
run.cor.examp(n = 100, seed)
run.cor2.examp(n = 100, seed)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ Number of points to plot. }
  \item{seed}{ What seed to ues. }
}
\details{
  The function \code{run.cor.examp} draws a scatterplot and allows you
  to set the correlation using a Tk slider window.

  The function \code{run.cor2.examp} does the same, but has a slider for
  R\^2 as well as the correlation, when either slider is moved the other
  one will update to match.
}
\value{
  These functions are run for their side effect, nothing meaningful is
  returned.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow \email{greg.snow@ihc.com} }
%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{cor}}, \code{\link{slider}} }
\examples{
\dontrun{
run.cor.examp()
run.cor2.examp()
}
}
\keyword{ dynamic }% at least one, from doc/KEYWORDS

