\name{tkBrush}
\alias{tkBrush}
\title{Change the Color and Styles of points interactively}
\description{Creates a Tk window with a scatterplot matrix, then allows
  you to "brush" the points to change their color and/or style.}
\usage{
tkBrush(mat,hscale=1.75,vscale=1.75,wait=TRUE,...)
}
\arguments{
  \item{mat}{A matrix of the data to plot, columns are variables, rows
    are observations, same as \code{pairs}}
  \item{hscale}{Passed to \code{tkrplot}}
  \item{vscale}{Passed to \code{tkrplot}}
  \item{wait}{Should the function wait for you to finish, see below}
  \item{...}{Additional arguments passed to the panel functions}
}
\details{
  This function creates a Tk window with a pairs plot of \code{mat},
  then allows you to interactively move a rectangle (the brush) over the
  points to change their color and plotting character.

  The arrow keys can be used to change the size and shape of the brush.
  The left arrow makes the rectangle wider, the right makes it
  narrower.  The up arrow key makes it taller, the right makes it
  shorter.

  When the mouse button is not pressed the points inside the brush will
  change while in the brush, but return to their previous state when the
  brush moves off them.  If the mouse button is pressed then the points
  inside the brush will be changed and the change will remain until a
  different set of conditions is brushed on them.

  The style of the brushed points is determined by the values of the 2
  entry boxes on the right side of the plot.  You can specify the
  plotting character in the \code{pch} box, this can be anything that
  you would regularly pass to the \code{pch} argument of \code{points},
  e.g. an integer or single character.  You can specify the color of the
  brushed points using the \code{color} entry box, specify the name of
  any color recognized by R (see \code{colors}), if this box does not
  contain a legal color name then black will be used.

  If \code{wait} is FALSE then the Tk window will exist independently of
  R and you can continue to do other things in the R window, in this
  case the function returns NULL.  If \code{wait} is TRUE then R waits
  for you to close the Tk window (using the quit button) then returns a
  list with the colors and plotting characters resulting from your
  brushing, this information can be used to recreate the plot using
  \code{pairs} on a new graphics device (for printing or saving).

}
\value{
  Either NULL (if Wait=FALSE) or a list with components \code{col} and
  \code{pch} corresponding to the state of the points.
}
\author{ Greg Snow \email{538280@gmail.com}}
\seealso{\code{\link{pairs}},\code{colors},\code{\link{points}},
  the \code{iplots} package}
\examples{
if(interactive()){

# Iris dataset

out1 <- tkBrush(iris)

#  Now brush the points

pairs(iris, col=out1$col, pch=out1$pch)

# or

colhist <- function(x,...){
    tmp <- hist(x,plot=F)
    br <- tmp$breaks
    w <- as.numeric(cut(x,br,include.lowest=TRUE))
    sy <- unlist(lapply(tmp$counts,function(x)seq(length=x)))
    my <- max(sy)
    sy <- sy/my
    my <- 1/my
    sy <- sy[order(order(x))]
    tmp.usr <- par('usr'); on.exit(par(usr=tmp.usr))
    par(usr=c(tmp.usr[1:2],0,1.5))
    rect(br[w], sy-my, br[w+1], sy, 
       col=out1$col, # note out1$col is hardcoded here.
       border=NA)
    rect(br[-length(br)], 0, br[-1], tmp$counts*my)
}
pairs(iris, col=out1$col, pch=out1$pch, diag.panel=colhist)


# some spheres

s1 <- matrix(nrow=0,ncol=3)

while( nrow(s1) < 1000 ){
	tmp <- rnorm(3)
	if( sum(tmp^2) <= 1 ){
		s1 <- rbind(s1,tmp)
	}
}

s2 <- matrix(rnorm(3000), ncol=3)
s2 <- s2/apply(s2,1,function(x) sqrt(sum(x^2)))

tkBrush(s1, wait=FALSE)
tkBrush(s2, wait=FALSE)

# now paint values where var 2 is close to 0 in both plots 
# and compare the var 1 and var 3 relationship

}
}

\keyword{ hplot }
\keyword{ iplot }
\keyword{ dynamic }