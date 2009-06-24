\name{shadowtext}
\Rdversion{1.1}
\alias{shadowtext}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Add text to a plot with a contrasting background.}
\description{
  This is similar to the text function, but it also puts a background
  shadow (outline) behind the text to make it stand out from the
  background better.
}
\usage{
shadowtext(x, y = NULL, labels, col = "white", bg = "black", theta = seq(pi/4, 2 * pi, length.out = 8), r = 0.1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{x-coordinates for the text}
  \item{y}{y-coordinates for the text}
  \item{labels}{The text labels to plot}
  \item{col}{Color of the text}
  \item{bg}{Color of the background shadow}
  \item{theta}{Angels for plotting the background}
  \item{r}{Thickness of the shadow relative to plotting size}
  \item{\dots}{Additional arguments passed on to \code{text}}
}
\details{
  When adding text to a plot it is possible that the color of the text
  may make it difficult to see relative to its background.  If the text
  spans different backgrounds then it may not be possible to find a
  single color to give proper contrast.

  This function creates a contrasting shadow for the text by first
  plotting several copies of the text at angles \code{theta} and
  distance \code{r} in the background color, then plotting the text on
  top.

  This gives a shadowing or outlining effect to the text making it
  easier to read on any background.
}
\value{
 This function is run for its side effects, returns NULL.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Greg Snow, \email{greg.snow@imail.org}}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{text}}
}
\examples{
 plot(1:10, 1:10, bg='aliceblue')
 rect(3,3,5,8, col='navy')
 text(5,6, 'Test 1', col='lightsteelblue')
 shadowtext(5,4, 'Test 2', col='lightsteelblue')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }

