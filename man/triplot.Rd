\name{triplot}
\alias{triplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create or add to a Trilinear Plot }
\description{
  Create (or add to) a trilinear plot of 3 proportions that sum to 1.
}
\usage{
triplot(x, y = NULL, z = NULL, labels = dimnames(x)[[2]],
  txt = dimnames(x)[[1]], legend = NULL, legend.split = NULL,
  inner = TRUE, inner.col = c("lightblue", "pink"), inner.lty = c(2, 3),
  add = FALSE, main = "", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Vector or matrix of up to 3 columns. }
  \item{y}{ Vector (if \code{x} is a vector). }
  \item{z}{ Vector (if \code{x} is a vector). }
  \item{labels}{ Labels for the 3 components (printed at corners). }
  \item{txt}{ Vector of text strings to be plotted instead of points. }
  \item{legend}{ Labels for the data points }
  \item{legend.split}{ What proportion of the labels will go on the left. }
  \item{inner}{ Logical, should the inner reference lines be plotted. }
  \item{inner.col}{ Colors for the 2 inner triangles. }
  \item{inner.lty}{ Line types for the 2 inner triangles. }
  \item{add}{ Add points to existing plot (TRUE), or create a new plot (FALSE). }
  \item{main}{ Main title for the plot. }
  \item{\dots}{ Additional arguments passed on to \code{points} or \code{text}. }
}
\details{
  Trilinear plots are useful for visualizing membership in 3 groups by
  plotting sets of 3 proportions that sum to 1 within each set.

  The data can be passed to the function as a matrix with either 2 or 3
  columns, or as seperate vectors to \code{x}, \code{y}, and optionaly
  \code{z}.  If 2 columns are passed in, then they must be between 0 and
  1 and the 3rd column will be created by subtracting both from 1.  If 3
  columns of data are given to the function then each will be divided by
  the sum of the 3 columns (they don't need to sum to 1 before being
  passed in).
}
\value{
  An invisible matrix with 2 columns and the same number of rows as \code{x}
  corresponding to the points plotted (after transforming to 2
  dimensions).

  The return matrix can be passed to \code{identify} for labeling of
  individual points.

  Using \code{type='n'} and \code{add=FALSE} will return the transformed
  points without doing any plotting.
}
\references{ Allen, Terry. Using and Interpreting the Trilinear Plot.
  Chance. 15 (Summer 2002). }
\author{ Greg Snow \email{538280@gmail.com} }
%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{triangle.plot} in package ade4,
  \code{ternaryplot} in package vcd, \code{tri} in package cwhtool,
  \code{soil.texture} and \code{triax.plot} in package plotrix. }
\examples{
triplot(USArrests[c(1,4,2)])
tmp <- triplot(USArrests[c(1,4,2)],txt=NULL)
if(interactive()){ identify(tmp, lab=rownames(USArrests) ) }

tmp <- rbind( HairEyeColor[,,'Male'], HairEyeColor[,,'Female'])
tmp[,3] <- tmp[,3] + tmp[,4]
tmp <- tmp[,1:3]
triplot(tmp, legend=rep(c('Male','Femal'),each=4),
 col=rep(c('black','brown','red','yellow'),2))
}
\keyword{ hplot }% at least one, from doc/KEYWORDS
\keyword{ aplot }% __ONLY ONE__ keyword per line
