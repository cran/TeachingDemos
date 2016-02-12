\name{char2seed}
\alias{char2seed}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Convert a character string into a random seed }
\description{
  This function creates a seed for the random number generator from a
  character string.  Character strings can be based on student names so
  that every student has a different random sample, but the teacher can
  generate the same datasets.
}
\usage{
char2seed(x, set = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A character string }
  \item{set}{ Logical, should the seed be set or just returned }
  \item{\dots}{ Additional parameters passed on to \code{set.seed} }
}
\details{
  Simulations or other situations call for the need to have repeatable
  random numbers, it is easier to remember a word or string than a
  number, so this function converts words or character strings to an
  integer and optionally sets the seed based on this.

  Teachers can assign students to generate a random dataset using their
  name to seed the rng, this way each student will have a different
  dataset, but the teacher can generate the same set of data to check
  values.

  Any characters other than letters (a-zA-Z) or digits (0-9) will be
  silently removed.  This function is not case sensitive, so "ABC" and
  "abc" will generate the same seed.

  This is a many to one function, so it is possible to find different
  words that generate the same seed, but this is unlikely by chance alone.
}
\value{
  This returns an integer (but mode numeric) to use as a seed for the
  RNG.  If \code{set} is true then it is returned invisibly.
}
\author{ Greg Snow \email{538280@gmail.com} }
\seealso{ \code{\link{set.seed}} }
\examples{
char2seed('Snow')
x <- rnorm(100)
rnorm(10)
tmp <- char2seed('Snow',set=FALSE)
set.seed(tmp)
y <- rnorm(100)

all.equal(x,y) # should be true

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ datagen }

