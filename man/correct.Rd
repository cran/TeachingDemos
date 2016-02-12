\name{cor.rect.plot}
\alias{cor.rect.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot a visualization of the correlation using colored rectangles
}
\description{
  This function creates a scatterplot of the data, then adds colored
  rectangles between the points and the mean of x and y to represent the
  idea of the correlation coefficient.
}
\usage{
cor.rect.plot(x, y, corr = TRUE, xlab = deparse(substitute(x)),
  ylab = deparse(substitute(y)), col = c("#ff000055", "#0000ff55"),
  ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  The \code{x} value or any object that can be interpreted by \code{xy.coords}
}
  \item{y}{
  The \code{y} value
}
  \item{corr}{
  Should the standardized axes (right and top) show the values divided
  by the standard deviation (TRUE, which shows correlation ideas) or not
  (FALSE, shows covariance idea)
}
  \item{xlab}{
The label for the \code{x} axis
}
  \item{ylab}{
The label for the \code{y} axis
}
  \item{col}{
A vector of length 2 with the colors to use for the fill of the
rectangles, the 1st value will be used for "positive" rectangles and the 2nd value will be used for the "negative" rectangles.
}
  \item{\dots}{
Possible further arguments, currently ignored
}
}
\details{
  This will create a scatterplot of the data, draw refrence lines at
  the mean of \code{x} and the mean of \code{y}, then draw rectangles
  from the mean point to the data points.  The right and top axes will
  show the centered (and possibly scaled if \code{corr=TRUE}) values.

  The idea is that the correlation/covariance is based on summing the
  area of the "positive" rectangles and subtracting the sum of the areas
  of the "negative" rectangles (then dividing by n-1).  If the positive
  and negative areas are about the same then the correlation/covariance
  is near 0, if there is more area in the positive rectangles then the
  correlation/covariance will be positive.
}
\value{
This function returns an invisible NULL, it is run for its side effects.
}

\author{Greg Snow, \email{538280@gmail.com}}

\seealso{
 \code{\link{cor}}
}
\examples{
## low correlation
x <- rnorm(25)
y <- rnorm(25)
cor(x,y)
cor.rect.plot(x,y)

## Positive correlation
x <- rnorm(25)
y <- x + rnorm(25,3, .5)
cor(x,y)
cor.rect.plot(x,y)

## negative correlation
x <- rnorm(25)
y <- rnorm(25,10,1.5) - x
cor(x,y)
cor.rect.plot(x,y)

## zero correlation but a definite relationship
x <- -5:5
y <- x^2
cor(x,y)
cor.rect.plot(x,y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }

