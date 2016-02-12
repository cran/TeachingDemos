\name{HWidentify}
\Rdversion{1.1}
\alias{HWidentify}
\alias{HTKidentify}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Show label for point being Hovered over. }
\description{
  These functions create a scatterplot then you Hover the mouse pointer
  over a point in the plot and it will show an id label for that point.
}
\usage{
HWidentify(x, y, label = seq_along(x), lab.col="darkgreen",
pt.col="red", adj=c(0,0), clean=TRUE, xlab = deparse(substitute(x)),
ylab = deparse(substitute(y)), ...)
HTKidentify(x, y, label = seq_along(x), lab.col="darkgreen",
pt.col="red", adj=c(0,0), xlab = deparse(substitute(x)),
ylab = deparse(substitute(y)), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ x-coordinates to plot }
  \item{y}{ y-coordinates to plot}
  \item{label}{ Labels to show for each point }
  \item{lab.col}{The color to plot the labels}
  \item{pt.col}{The color of the highlighting point}
  \item{adj}{The adjustment of the labels relative to the cursor point.
    The default places the label so that its bottom left corner is at
    the curser, values below 0 or greater than 1 will move the label to
    not touch the cursor.}
  \item{clean}{Logical value, should any labels on the plot be removed
    at the end of the plotting.}
  \item{xlab}{ Label for x-axis }
  \item{ylab}{ Label for y-axis}
  \item{\dots}{additional arguments passed through to plot}
}
\details{
  This is an alternative to the \code{identify} function.  The label
  only shows up for the point currently closest to the mouse pointer.
  When the mouse pointer moves closer to a different point, the label
  changes to the one for the new point.  The currently labeled point is
  also highlighted.  HWidentify only works on windows, HTKidentify
  requires the tkrplot package.
}
\value{
These functions are run for their side effects, nothing meaningful is returned.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Greg Snow, \email{538280@gmail.com}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{identify}}
}
\examples{
if( interactive() ){
  tmpx <- runif(25)
  tmpy <- rnorm(25)
  HTKidentify(tmpx,tmpy, LETTERS[1:25], pch=letters)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dynamic }
\keyword{ iplot }
