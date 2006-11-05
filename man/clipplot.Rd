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
  most of the time this will have no visible effect unless you
  suppressed the drawing of the box initially.
}
\value{
  Nothing meaningful is returned
}

\author{ Greg Snow \email{greg.snow@intermountainmail.org} }
\seealso{ \code{\link{par}}, \code{\link{lines}} }
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
