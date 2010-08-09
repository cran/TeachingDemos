\name{pairs2}
\alias{pairs2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create part of a scatterplot matrix}
\description{
  This function is similar to the \code{pairs} function, but instead of
  doing all pairwise plots, it takes 2 matricies or data frames and does
  all combinations of the first on the x-axis with the 2nd on the
  y-axis.  Used with pairs and subsets can spread a scatterplot matrix
  accross several pages.
}
\usage{
pairs2(x, y, xlabels, ylabels, panel = points, ..., row1attop = TRUE, gap = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Matrix or data frame of variables to be used as the x-axes. }
  \item{y}{ Matrix or data frame of variables to be used as the y-axes. }
  \item{xlabels}{ Labels for x variables (defaults to colnames of \code{x}). }
  \item{ylabels}{ Labels for y variables (defaults to colnames of \code{y}). }
  \item{panel}{ Function to do the plotting (see \code{pairs}). }
  \item{\dots}{ additional arguments passed to graphics functions}
  \item{row1attop}{ Logical, should the 1st row be the top.}
  \item{gap}{ Distance between plots. }
}
\details{
  This functios is similar to the \code{pairs} function, but by giving
  it 2 sets of data it only does the combinations between them.  Think
  of it as giving the upper right or lower left set of plots from
  \code{pairs}.  If a regular scatterplot matrix is too small on the
  page/device then use \code{pairs} on subsets of the data to get the
  diagonal blocks of a scatterplot matrix and this function to get the
  off diagonal blocks.
}
\value{
  This function is run for the side effect of the plot.  It does not
  return anything useful.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow, \email{greg.snow@imail.org} }
\note{
  Large amounts of the code for this function were blatently
  borrowed/stolen from the \code{pairs} function, the credit for the
  useful parts should go to the original authors, blame for any problems
  should go to me.  This function is also released under GPL since much
  of it comes from GPL code.
}
\seealso{\code{\link{pairs}}, \code{splom} in the lattice package}
\examples{
pairs2(iris[,1:2], iris[,3:4], col=c('red','green','blue')[iris$Species])

# compare the following plot:
pairs(state.x77, panel=panel.smooth)

# to the following 4 plots

pairs(state.x77[,1:4], panel=panel.smooth)
pairs(state.x77[,5:8], panel=panel.smooth)
pairs2( state.x77[,1:4], state.x77[,5:8], panel=panel.smooth)
pairs2( state.x77[,5:8], state.x77[,1:4], panel=panel.smooth)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}

