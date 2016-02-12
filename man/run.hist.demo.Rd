\name{run.hist.demo}
\alias{run.hist.demo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create a histogram and interactively change the number of bars. }
\description{
  Create a histogram then use a Tk slider window to change the number of
  bars, the minimum, and the maximum.
}
\usage{
run.hist.demo(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Data to plot. }
}
\details{
  Draws a histogram and creates a Tk slider window that allows you to
  explore how changing the parameters affects the appearance of the plot.
}
\value{
  No meaninful value is returned.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow \email{538280@gmail.com} }
%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{hist}}, \code{\link{slider}}  }
\examples{
if(interactive()){
run.hist.demo( rnorm(250, 100, 5) )
}
}
\keyword{ dynamic }% at least one, from doc/KEYWORDS
\keyword{ hplot }% __ONLY ONE__ keyword per line
