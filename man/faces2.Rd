\name{faces2}
\alias{faces2}
\alias{face2.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Chernoff Faces }
\description{
  Plot Chernoff Faces of the dataset, rows represent
  subjects/observations, columns represent variables.
}
\usage{
faces2(mat, which = 1:ncol(mat), labels = rownames(mat),
  nrows = ceiling(nrow(mat)/ncols), ncols = ceiling(sqrt(nrow(mat))),
  byrow = TRUE, scale = c("columns", "all", "center", "none"),
  fill = c(0.5, 0.5, 1, 0.5, 0.5, 0.3, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
           0.5, 0.5, 0.5, 0.5, 1, 0.5), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{ Matrix containing the data to plot. }
  \item{which}{ Which columns correspond to which features (see details). }
  \item{labels}{ Labels for the individual faces }
  \item{nrows}{ Number of rows in the graphical layout }
  \item{ncols}{ Number of columns in the graphical layout }
  \item{byrow}{ Logical, should the faces be drawn rowwise or columnwise.
  }
  \item{scale}{ Character, how should the data be scaled. }
  \item{fill}{ What value to use for features not assocaiated with a
    column of data. }
  \item{\dots}{ Additional arguments passed on to plotting functions. }
}
\details{
  The features are:
   1  Width of center
   2  Top vs. Bottom width (height of split)
   3  Height of Face
   4  Width of top half of face
   5  Width of bottom half of face
   6  Length of Nose
   7  Height of Mouth
   8  Curvature of Mouth (abs < 9)
   9  Width of Mouth
   10 Height of Eyes
   11 Distance between Eyes (.5-.9)
   12 Angle of Eyes/Eyebrows
   13 Circle/Ellipse of Eyes
   14 Size of Eyes
   15 Position Left/Right of Eyeballs/Eyebrows
   16 Height of Eyebrows
   17 Angle of Eyebrows
   18 Width of Eyebrows

   The face plotting routine needs the data values to be between 0 and 1
   (inclusive).  The \code{scale} option controls how scaling will be
   done on \code{mat}:  "columns" scales each column to range from 0 to
   1, "all" scales the entire dataset to vary from 0 to 1, "center"
   scales each column so that the mean of the column becomes 0.5 and all
   other values are between 0 and 1, and "none" does no scaling assuming
   that the data has already been scaled.
}
\value{
  This function is run for its side effect of plotting and does not
  return anything.
}
\references{ Chernoff, H. (1973): The use of faces to represent statistiscal assoziation,
JASA, 68, pp 361--368. }
\author{ Original code by  ; current implementation by Greg Snow \email{greg.snow@imail.org} }
\note{ If you choose to not scale the data and any data values are
  outside of the 0 to 1 range, then strange things may happen.

  This function is based on code available at \url{}, the good things
  come from there, any problems are likely due to my (Greg's) tweaking.
}


\seealso{\code{\link{faces}}}
\examples{
  faces2(matrix( runif(18*10), nrow=10), main='Random Faces')
}
\keyword{ hplot }% at least one, from doc/KEYWORDS

