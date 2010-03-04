\name{SnowsPenultimateNormalityTest}
\alias{SnowsPenultimateNormalityTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Test the uninteresting question of whether the data represents an
  exact normal distribution.
}
\description{
  This function tests the null hypothesis that the data comes from an
  exact normal population.  This is a much less interesting/useful null
  than what people usually want, which is to know if the data come from
  a distribution that is similar enough to the normal to use normal
  theory inference.
}
\usage{
SnowsPenultimateNormalityTest(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The data }
}
\details{
  The theory for this test is based on the probability of getting a
  rational number from a truly continuous distribution defined on the
  reals.

  The main goal of this test is to quickly give a p-value for those that
  feel it necessary to test the uninteresting and uninformative null
  hypothesis that the data represents an exact normal, and allows the
  user to then move on to much more important questions, like "is the
  data close enough to the normal to use normal theory inference?".

  After running this test (or better instead of running this and any
  other test of normality) you should ask yourself what it means to test
  for normality and why you would want to do so.  Then plot the data and
  explore the interesting/useful questions.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
An object of class "htest" with components:

\item{p.value}{The p-value}
\item{alternative}{a string representing the alternative hypothesis}
\item{method}{a string describing the method}
\item{data.name}{a string describing the name of the data}
}
\references{
\code{fortune(234)}
}
\author{
Greg Snow \email{greg.snow@imail.org}
}
\note{
Note: if you just use this function and report the p-value then the
function has failed in its purpose.  If this function helps you to think
about your analysis and what question(s) you are really interested in,
create meaningful plots, and focus on the more meaningful parts of
research, then it has succeeded.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{qqnorm}}, \code{\link{vis.test}}
}
\examples{
SnowsPenultimateNormalityTest(rt(100,25))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ distribution }
\keyword{ htest }% __ONLY ONE__ keyword per line
