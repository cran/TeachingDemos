\name{clipplot}
\alias{clipplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Clip plotting to a rectangular region }
\description{
  Clip plotting to a rectangular region that is a subset of the plotting area
}
\usage{
clipplot(fun, xlim = par("usr")[1:2], ylim = par("usr")[3:4])
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fun}{ The function or expression to do the plotting. }
  \item{xlim}{ A vector of length 2 representing the x-limits to clip
    plotting to, defaults to the entire width of the plotting region. }
  \item{ylim}{ A vector of length 2 representing the y-limits to clip
    the plot to, defaults to the entire height of the plotting region. }
}
\details{
  This function resets the active region for plotting to a rectangle
  within the plotting area and turns on clipping so that any points,
  lines, etc. that are outside the rectange are not plotted.

  A side effect of this function is a call to the \code{box()} command,
  it is called with a fully transparent color so if your graphics device
  honors transparency then you will probably see no effect.
}
\value{
  Nothing meaningful is returned
}

\author{ Greg Snow \email{538280@gmail.com} }
\note{
  This function abuses some of the intent of what par(plt=...) is
  supposed to mean.  In R2.7.0 and beyond there is a new funcntion
  \code{clip} with the intended purpose of doing this in a more proper
  manner (however as of my last test it is not working perfectly either,
  so \code{clipplot} will remain undepricated for now).

  It uses some hacks to make sure that the clipping region is set, but
  it does this by plotting some tranparent boxes, therefore you should
  not use this on devices where tranparency is not supported (or you may
  see extra boxes).
}
\seealso{ \code{\link{par}}, \code{\link{lines}}, \code{clip} in R2.7.0
  and later }
\examples{
x <- seq(1,100)
y <- rnorm(100)
plot(x,y, type='b', col='blue')
clipplot( lines(x,y, type='b', col='red'), ylim=c(par('usr')[3],0))


attach(iris)

tmp <- c('red','green','blue')
names(tmp) <- levels(Species)
plot(Petal.Width,Petal.Length, col=tmp[Species])
for(s in levels(Species)){
  clipplot( abline(
    lm(Petal.Length~Petal.Width, data=iris, subset=Species==s),
    col=tmp[s]),
    xlim=range(Petal.Width[Species==s]))
}

detach(iris)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }
\keyword{ dplot }% __ONLY ONE__ keyword per line
