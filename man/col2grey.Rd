\name{col2grey}
\alias{col2grey}
\alias{col2gray}
\title{Convert colors to grey/grayscale}
\description{
  Convert colors to grey/grayscale so that you can see how your plot
  will look after photocopying or printing to a non-color printer.
}
\usage{
col2grey(cols)
col2gray(cols)
}
\arguments{
  \item{cols}{ Colors to convert.}
}
\details{
  converts colors to greyscale using the formula grey=0.3*red +
  0.59*green + 0.11*blue.  This allows you to see how your color plot
  will approximately look when printed on a non-color printer or photocopied.
}
\value{
  A vector of colors (greys) corresponding to the input colors.
}
\author{ Greg Snow \email{greg.snow@imail.org} }
\seealso{ \code{\link{grey}}, \code{\link{col2rgb}}, dichromat package }
\examples{
par(mfcol=c(2,2))
tmp <- 1:3
names(tmp) <- c('red','green','blue')

barplot( tmp, col=c('red','green','blue') )
barplot( tmp, col=col2gray( c('red','green','blue') ) )

barplot( tmp, col=c('red','#008100','#3636ff') )
barplot( tmp, col=col2grey( c('red','#008100','#3636ff') ) )
}
\keyword{dplot}
\keyword{color}


