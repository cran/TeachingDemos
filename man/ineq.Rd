\name{\%<\%}
\Rdversion{1.1}
\alias{\%<\%}
\alias{\%<=\%}
  %- Also NEED an '\alias' for EACH other topic documented here.
  \title{
Less than or Less than and equal operators that can be chained together.
}
\description{Comparison operators that can be chained together into
  something like 0 \%<\% x \%<\% 1 instead of 0 < x \&\& x < 1. }
\usage{
x \%<\% y
x \%<=\% y
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x,y}{Values to compare
}
}
\details{
These functions/operators allow chained inequalities.  To specify that
you want the values between two values (say 0 and 1) you can use \code{0
  \%<\% x \%<\% 1 } rather than \code{ 0 < x \&\& x < 1 }.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...

A logical vector is returned that can be used for subsetting like
\code{<}, but the original values are included as attributes to be used
in additional comparisons.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Greg Snow, \email{greg.snow@imail.org}
}
\note{
  %%  ~~further notes~~
This operator is not fully associative, so be careful with parentheses.
See the examples.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{

x <- -3:3

 -2 \%<\% x \%<\% 2
c( -2 \%<\% x \%<\% 2 )
x[ -2 \%<\% x \%<\% 2 ]
x[ -2 \%<=\% x \%<=\% 2 ]


x <- rnorm(100)
y <- rnorm(100)

x[ -1 \%<\% x \%<\% 1 ]
range( x[ -1 \%<\% x \%<\% 1 ] )


cbind(x,y)[ -1 \%<\% x \%<\% y \%<\% 1, ]
cbind(x,y)[ (-1 \%<\% x) \%<\% (y \%<\% 1), ]
cbind(x,y)[ ((-1 \%<\% x) \%<\% y) \%<\% 1, ]
cbind(x,y)[ -1 \%<\% (x \%<\% (y \%<\% 1)), ]
cbind(x,y)[ -1 \%<\% (x \%<\% y) \%<\% 1, ] # oops


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ logic }% __ONLY ONE__ keyword per line
