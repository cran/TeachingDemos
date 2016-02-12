\name{rotate.cloud}
\alias{rotate.cloud}
\alias{rotate.persp}
\alias{rotate.wireframe}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Interactively rotate 3D plots }
\description{
  Interactively rotate common 3d plots: cloud, persp, and wireframe.
}
\usage{
rotate.cloud(x, ...)
rotate.persp(x, y, z)
rotate.wireframe(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ \code{x}, see \code{persp}, or formula/matrix to pass to
    cloud or wireframe }
  \item{y}{ \code{y}, see \code{persp}  }
  \item{z}{ \code{z}, see \code{persp}  }  
  \item{\dots}{ additional arguments passed on to \code{cloud} or \code{persp}}
}
\details{
  Use these functions just like \code{cloud}, \code{persp}, and
  \code{wireframe}.  In addition to the default plot a Tk slider window
  will be created that will allow you to rotate the plot.

  The rotations parameters are passed the \code{screen} argument of
  \code{cloud} and \code{wireframe} and the \code{theta}, \code{phi},
  \code{r}, \code{d}, \code{ltheta}, \code{lphi}, and \code{shade}
  arguments of \code{persp}.

  For \code{cloud} and \code{wireframe} plots the order of the \code{x},
  \code{y}, and \code{z} argumets can be rearanged, just type the
  appropriate letters in the boxes on the left, then press the "refresh"
  button (changing the order changes the plot for these 2 plots).
}
\value{
  These functions are run for the side effects of the plots and Tk
  windows, nothing meaninful is returned.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow \email{538280@gmail.com}}
%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{cloud} it the lattice package,
  \code{\link{persp}}, \code{wireframe} in the lattice package }
\examples{

if(interactive()){
rotate.cloud(Sepal.Length ~ Petal.Length*Petal.Width, data=iris)

rotate.wireframe(volcano)

z <- 2 * volcano        # Exaggerate the relief
x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)
rotate.persp(x,y,z)
}
}
\keyword{ dynamic }% at least one, from doc/KEYWORDS
\keyword{ hplot }% __ONLY ONE__ keyword per line
