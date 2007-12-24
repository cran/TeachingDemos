\name{roc.demo}
\alias{roc.demo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Demonstrate ROC curves by interactively building one }
\description{
  This demonstration allows you to interactively build a Receiver
  Operator Curve to better understand what goes into creating them.
}
\usage{
roc.demo(x = rnorm(25, 10, 1), y = rnorm(25, 11, 1.5))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Data values for group 1 (controls). }
  \item{y}{ Data values for group 2 (cases). }
}
\details{
  Density plots for the 2 groups will be created in the lower panel of
  the plot (colored red (group 1) and blue (group 2)) along with rug
  plots of the actual datapoints.  There is also a
  green vertical line that represents a decision rule cutoff, any points
  higher than the cutoff are predicted to be in group 2 and points less
  than the cuttoff are predicted to be in group 1.  The sensitivity and
  specificity for the current cuttoff value are printed below the plot.

  A Tk slider box is also created that allows you to move the cuttoff
  value and update the plots.  As the cutoff value changes, the
  different combinations of sensitivity and specificity are added to the
  ROC curve in the top panel (the point corresponding to the current
  cuttoff value is highlighted in red).  A line is also drawn from the
  point representing sensitivity and specificity both equal to 1 to the
  point closest to it.
}
\value{
  No meaninful value is returned, this function is run solely for the
  side effects.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow \email{greg.snow@imail.org} }
%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{slider}}, \code{ROC} function in package
  Epi, \code{auROC} in package limma, package ROC }
\examples{

if(interactive()){
roc.demo()
with(CO2,
  roc.demo(uptake[Type=='Mississippi'],
           uptake[Type=='Quebec']       )
)
}
}
\keyword{ dynamic }% at least one, from doc/KEYWORDS
\keyword{ classif }% __ONLY ONE__ keyword per line
