\name{tree.demo}
\alias{tree.demo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Interactively demonstrate regression trees }
\description{
  Interactively recursively partition a dataset to demonstrate
  regression trees.
}
\usage{
tree.demo(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The predictor variable. }
  \item{y}{ The response variable. }
}
\details{
  This function first creates a scatterplot of \code{x} and \code{y} and
  shows the residual sum of squares from fitting a horizontal line to
  the y-values.

  Clicking anywhere on the graph will show an updated graph where the
  data is partitioned into 2 groups based on the x-value where you
  clicked with a horizontal line fit to each group (including showing
  the updated residual sum of squares).  Clicking again will move the
  partitioning value based on the new click.

  When you have found a good partitioning (reduces the RSS), right click
  and choose 'stop' and that partition will become fixed.  Now you can
  click to do a second set of partions (breaking the data into 3
  groups).

  To finish the demo, right click and choose 'stop', then right click
  again and choose 'stop' again.
}
\value{
  A vector with the x-values of the cut points that you selected (sorted).
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow \email{greg.snow@imail.org} }
%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ The rpart and tree packages }
\examples{
if(interactive()){
data('ethanol', package='lattice')
print(with(ethanol, tree.demo(E,NOx)))
}
}
\keyword{ dynamic }% at least one, from doc/KEYWORDS
\keyword{ models }% __ONLY ONE__ keyword per line
