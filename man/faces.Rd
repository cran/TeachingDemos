\name{faces}
\alias{faces}
\title{    Chernoff Faces    }
\description{
     faces represent the rows of a data matrix by faces
}
\usage{
faces(xy, which.row, fill = FALSE, nrow, ncol, scale = TRUE, byrow = FALSE, main, labels)
}
\arguments{
  \item{xy}{   \code{xy} data matrix, rows represent individuals and columns attributes  }
  \item{which.row}{   defines a permutation of the rows of the input matrix   }
  \item{fill}{   \code{if(fill==TRUE)}, only the first \code{nc} attributes of the faces are
                  transformed, \code{nc} is the number of columns of \code{xy}   }
  \item{nrow}{   number of columns of faces on graphics device   }
  \item{ncol}{   number of rows of faces   }
  \item{scale}{   \code{if(scale==TRUE)}, attributes will be normalized   }
  \item{byrow}{   \code{if(byrow==TRUE)}, \code{xy} will be transposed   }
  \item{main}{   title   }
  \item{labels}{   character strings to use as names for the faces   }
}
\details{
    The features paramters of this implementation are:
1-height of face,
2-width of face,
3-shape of face,
4-height of mouth,
5-width of mouth,
6-curve of smile,
7-height of eyes,
8-width of eyes,
9-height of hair,
10-width of hair,
11-styling of hair,
12-height of nose,
13-width of nose,
14-width of ears,
15-height of ears. For details look at the literate program of \code{faces}
}
\value{
  a plot of faces is created on the graphics device,
  no numerical results
}
\references{  Chernoff, H. (1973): The use of faces to represent statistiscal assoziation,
JASA, 68, pp 361--368.
The smooth curves are computed by an algorithm found in
Ralston, A. and Rabinowitz, P. (1985):
A first course in numerical analysis, McGraw-Hill, pp 76ff.
\url{http://www.wiwi.uni-bielefeld.de/~wolf/} : S/R - functions : faces
   }
\author{   H. P. Wolf    }
\note{ version 12/2003   }

\seealso{   ---    }
\examples{
faces(rbind(1:3,5:3,3:5,5:7))

data(longley)
faces(longley[1:9,])

set.seed(17)
faces(matrix(sample(1:1000,128,),16,8),main="random faces")
}
%\keyword{  Chernoff faces}% at least one, from doc/KEYWORDS
%\keyword{  Flury faces }% __ONLY ONE__ keyword per line
%\keyword{  faces }% __ONLY ONE__ keyword per line
\keyword{ hplot }