\name{SnowsCorrectlySizedButOtherwiseUselessTestOfAnything}
\alias{SnowsCorrectlySizedButOtherwiseUselessTestOfAnything}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Snow's Correctly Sized But Otherwise Useless Test of Anything
}
\description{
This is a hypothesis test designed to be correctly sized in that the
probability of rejecting the null when it is true will be equal to your
alpha level.  Other than that it is a pretty useless test mainly
intended for when people say something like "I just need a p-value".
}
\usage{
SnowsCorrectlySizedButOtherwiseUselessTestOfAnything(x,
  data.name = deparse(substitute(x)),
  alternative = "You Are Lucky", ..., seed)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The data, or nothing, or something equally irrelevant}
  \item{data.name}{ The name of  the data for the output}
  \item{alternative}{The phrase for the alternate hypothesis in the output}
  \item{\dots}{ Additional arguments that will be silently ignored (like
  \code{x}), future versions may mockingly ignore these instead}
  \item{seed}{ A seed (numeric or character) used to seed the random
    number generator.  Use this or manually set the seed if you want
    reproducible (but still meaningless) results}
}
\details{
  Some of the advantages/disadvantages of this test include:
  \itemize{
    \item{The probability of a Type I error is alpha}
    \item{Power can be easily computed (it is alpha)}
    \item{Power is independent of the sample size}
    \item{Power is independent of the hypotheses}
    \item{This test is not affected by missing data (present data
      either)}
    \item{This test does not depend on any distributional or
      independence assumptions}
    }
}
\value{
  An object of class htest with the following elements:

  \item{p.value}{The p-value}
  \item{statistic}{The test statistic (identical to the p-value)}
  \item{data.name}{The name of the data (if any)}
  \item{method}{The name of the test}
  \item{alternative}{a phrase representing the alternative hypothesis}
  \item{seed}{optionally the seed that was used}
}
\references{
The author is unlikely to be willing to publish in any "journal" that
  would be willing to publish this test.
}
\author{
Greg Snow \email{greg.snow@imail.org}
}
\note{
If someone has suggested that you consider this test, they most likely
  do not intend for you to actually use the test, rather to reconsider
  your question or the assumptions that you are making or trying to
  avoid.  This test should only be used to illustrate a point and
  decisions (other than maybe who should pay for lunch) should never be
  made based on the results of this test.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{runif}}
}
\examples{
  SnowsCorrectlySizedButOtherwiseUselessTestOfAnything(log(rnorm(100)))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }

