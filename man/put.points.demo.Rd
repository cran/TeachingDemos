\name{put.points.demo}
\alias{put.points.demo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Demonstrate Correlation and Regression by placing and moving
  data points }
\description{
  Place data points on a graph to demonstrate concepts related to
  correlation and regression.
}
\usage{
put.points.demo(x = NULL, y = NULL, lsline = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ x-coordinates for initial points. }
  \item{y}{ y-coordinates for initial points. }
  \item{lsline}{ Logical, should the ls regresion line be included. }
}
\details{
  The plot area is divided into 2 sections, the left section shows a
  scatterplot of your points, the right panel controls what happens when
  you click in the left panel.

  The top of the right panel has an "end" button that you click on to
  end the demonstration.

  The middle right panel toggles the least squares line and information.

  The bottom right panel has radio buttons that determine what clicking
  in the left panel will do, the options are to add a point, delete a
  point, or move a point.

  To move a point click on the point you want to move, it will become
  solid, then click in the place you want it to move to.

  When deleting or moving points, the closest point to where you click
  will be deleted or moved, even if you click in an empty area.

  Whenever you add, delete, or move a point the correlation, r^2, and
  regression line will be updated.  You can start with a set of points
  then demonstrate what happens to the correlation and regression line
  when outliers are added or important points are moved or deleted.
}
\value{
  This function does not return anything.
}
\author{ Greg Snow \email{538280@gmail.com} }

\seealso{ \code{\link{plot}}, \code{\link{cor}} }
\examples{

if(interactive()){
put.points.demo()

x <- rnorm(25, 5, 1)
y <- x + rnorm(25)
put.points.demo(x,y)
}

}
\keyword{ dynamic }% at least one, from doc/KEYWORDS
\keyword{ iplot }% __ONLY ONE__ keyword per line
\keyword{regression}