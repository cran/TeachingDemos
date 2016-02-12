\name{rgl.coin}
\alias{rgl.coin}
\alias{rgl.die}
\alias{flip.rgl.coin}
\alias{roll.rgl.die}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Animated die roll or coin flip }
\description{
  Open an rgl window, plot either a representation of a coin or a die
  then animate the flipping/rolling.
}
\usage{
rgl.coin(x, col = "black", heads = x[[1]], tails = x[[2]], ...)

rgl.die(x=1:6, col.cube = "white", col.pip = "black", sides = x, ...)

flip.rgl.coin(side = sample(2, 1), steps = 150)

roll.rgl.die(side = sample(6, 1), steps = 250)
}
\arguments{
  \item{x}{ for \code{rgl.coin} a list with information for drawing
    the faces of the coin, defaults to \code{coin.faces}.  For
    \code{rgl.die} a vector with the number of pips to put on the
    sides of the die (alternative way of specifying \code{sides}). }
  \item{col}{ Color of lines on the coin faces. }
  \item{heads}{ Design to use as "heads" side of coin. }
  \item{tails}{ Design to use as "tails" side of coin. }
  \item{col.cube}{ Color of the cube for the die. }
  \item{col.pip}{ Color of the pips (spots) on the die }
  \item{sides}{ Vector of length 6 indicating which numbers to show on
    the die.  }
  \item{side}{ Which side of the coin (1 or 2) or die (1 through 6)
    should end up face up. }
  \item{steps}{ The number of steps in each part of the animation,
    higher values will be smoother and slower, lower values will be
    faster but more jumpy. }
  \item{...}{ Currently any additional options are silently ignored. }
}
\details{
  You must use the plot function first to create the coin or die, then
  use the flip or roll function to start the animation.  You can animate
  multiple times for a single use of the plotting function.

  You can manually rotate the image as well, see the \code{rgl} package
  for details.

  The defaults plot a regular coin and die, but arguments are available
  to create special casses (2 headed coin, die with 2 6's and no 1,
  ...).

  The data list \code{coin.faces} contains information on designs for
  the faces of the coins in case you want to choose a different design.

  The default rolling and flipping options ranomly choose which side
  will be face up following a uniform distribution.  You can specify the
  side yourself, or use the \code{sample} function to do a biased random
  flip/roll.
}
\value{
  Which side ended up face up (1 or 2 for coin, 1 through 6 for die).
  This is the internal numbering and does not match a change in the
  \code{sides} argument.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow \email{538280@gmail.com}}
\note{ The current algorithm for animating the die roll shows all the
  sides, but I am not satisfied with it.  Please suggest improvements. }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{dice}}, \code{\link{plot.dice}},
  \code{\link{coin.faces}}, \code{\link{sample}} }
\examples{
if(interactive()){
rgl.coin()
flip.rgl.coin()
flip.rgl.coin(1)
flip.rgl.coin(2)

rgl.clear()

# two-headed coin
rgl.coin(tails=coin.faces$qh)

rgl.clear()

# letters instead of pictures
rgl.coin(heads=coin.faces$H, tails=coin.faces$T)

# biased flip
flip.rgl.coin( sample(2,1, prob=c(0.65, 0.35) ) )

rgl.clear()

rgl.die()
roll.rgl.die()
roll.rgl.die(6)

# biased roll
roll.rgl.die( sample(6,1, prob=c(1,2,3,3,2,1) ) )
}
}
\keyword{ dynamic }% at least one, from doc/KEYWORDS
\keyword{ datagen }% __ONLY ONE__ keyword per line
\keyword{distribution}