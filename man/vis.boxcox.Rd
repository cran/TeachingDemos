\name{vis.boxcox}
\alias{vis.boxcox}
\alias{vis.boxcox.old}
\alias{vis.boxcoxu}
\alias{vis.boxcoxu.old}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Interactively visualize Box-Cox transformations }
\description{
  Explore the Box-Cox family of distributions by plotting data
  transformed and untransformed and interactively choose values for lambda.
}
\usage{
vis.boxcox(lambda = sample(c(-1,-0.5,0,1/3,1/2,1,2), 1),
           hscale=1.5, vscale=1.5, wait=FALSE)

vis.boxcoxu(lambda = sample( c(-1,-0.5,0,1/3,1/2,1,2), 1),
                        y, xlab=deparse(substitute(y)), 
                        hscale=1.5, vscale=1.5, wait=FALSE)

vis.boxcox.old(lambda = sample(c(-1, -0.5, 0, 1/3, 1/2, 1, 2), 1))

vis.boxcoxu.old(lambda = sample(c(-1, -0.5, 0, 1/3, 1/2, 1, 2), 1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lambda}{ The true value of lambda to use. }
  \item{y}{ Optional data to use in the transform. }
  \item{xlab}{ Label for x-axis.}
  \item{hscale}{ The horizontal scale, passed to \code{tkrplot}. }
  \item{vscale}{ The vertical scale, passed to \code{tkrplot}. }
  \item{wait}{ Should R wait for the demo window to close. }
}
\details{
  These functions will generate a sample of data and plot the
  untrasformed data (left panels) and the transformed data (right
  panels).  Initially the value of \code{lambda} is 1 and the 2 sets of
  plots will be identical.

  You then adjust the transformation parameter \code{lambda} to see how
  the right panels change.

  The function \code{vis.boxcox} shows the effect of transforming the
  y-variable in a simple linear regression.

  The function \code{vis.boxcoxu} shows a single variable compared to
  the normal distribution.
}
\value{
  The old versions have no useful return value.  If \code{wait} is FALSE
  then they will return an invisible NULL, if \code{wait} is TRUE then
  the return value will be a list with the final value of \code{lamda},
  the original data, and the transformed y (at the final \code{lamda} value).
}
\references{ GEP Box; DR Cox. An Analysis of Transformations.  Journal
  of the Royal Statitical Society. Series B, Vol. 26, No. 2 (1964) 211-252 }
\author{ Greg Snow \email{greg.snow@imail.org} }
%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{bct}}, \code{boxcox} in package MASS }
\examples{
if(interactive()) {
vis.boxcoxu()
vis.boxcox()
}
}
\keyword{ dynamic }% at least one, from doc/KEYWORDS
\keyword{ univar }% __ONLY ONE__ keyword per line
\keyword{ regression }