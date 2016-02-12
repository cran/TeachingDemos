\name{digits}
\alias{digits}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Return the digits that make up an integer
}
\description{
  Takes an integer or vector of integers and returns a vector, list, or
  matrix of the individual digits (decimal) that make up that number.
}
\usage{
digits(x, n = NULL, simplify = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    An integer or vector of integers (if not integer, the fractional
    part will be ignored)
}
  \item{n}{
    The number of digits to be returned, this can be used to place 0's
    in front of smaller numbers.  If this is less than the number of
    digits then the last \code{n} digits are returned.
}
  \item{simplify}{
    Should \code{sapply} simplify the list into a matrix
}
}
\details{
  This function transforms an integer (or real ignoring the fractional
  part) into the decimal digits that make of the decimal representation
  of the number using modular mathematics rather than converting to
  character, splitting the string, and converting back to numeric.
}
\value{
  If \code{x} is of length 1 then a vector of the digits is returned.

  If \code{x} is a vector and \code{simplify} is \code{FALSE} then a list
  of vectors is returned, one element for each element of \code{x}.

  If \code{x} is a vector and \code{simplify} is \code{TRUE} then a
  matrix with 1 column for each element of \code{x}.
}
\author{
  Greg Snow \email{538280@gmail.com}
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{\%\%}}, \code{\link{\%/\%}}, \code{\link{strsplit}}
}
\examples{

digits( 12345 )
digits( 567, n=5 )

x <- c(1, 23, 456, 7890)

digits(x)
digits(x, simplify=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ arith }% __ONLY ONE__ keyword per line
