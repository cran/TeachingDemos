\name{bct}
\alias{bct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Box-Cox Transforms}
\description{
  Computes the Box-Cox transform of the data for a given value of
  lambda.  Includes the scaling factor.
}
\usage{
bct(y, lambda)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{Vector of data to be transformed.}
  \item{lambda}{Scalar exponent for transform (1 is linear, 0 is log).}
}
\details{
  \code{bct} computes the Box-Cox family of transforms:  y = (y\^lambda -
  1)/(lambda*gm\^(lambda-1)), where gm is the geometric mean of the y's.
  returns log(y)*gm when lambda equals 0.
}
\value{
 A vector of the same length as y with the corresponding transformed values.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Greg Snow \email{greg.snow@imail.org}}
%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{vis.boxcox}}, \code{\link{vis.boxcoxu}},
  \code{\link[MASS]{boxcox}} in package MASS, other implementations in various
  packages}
\examples{
y <- rlnorm(500, 3, 2)
par(mfrow=c(2,2))
qqnorm(y)
qqnorm(bct(y,1/2))
qqnorm(bct(y,0))
hist(bct(y,0))



}
\keyword{manip}% at least one, from doc/KEYWORDS
\keyword{datagen}
\keyword{regression}
