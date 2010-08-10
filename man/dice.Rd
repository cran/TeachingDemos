\name{dice}
\alias{dice}
\alias{plot.dice}
\alias{panel.dice}
\alias{prepanel.dice}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulate rolling dice }
\description{
  Simulate and optionally plot rolls of dice.
}
\usage{
dice(rolls = 1, ndice = 2, sides = 6, plot.it = FALSE, load = rep(1, sides))
plot.dice(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rolls}{ Scalar, the number of times to roll the dice. }
  \item{ndice}{ Scalar, the number of dice to roll each time. }
  \item{sides}{ Scalar, the number of sides per die. }
  \item{plot.it}{ Logical, Should the results be plotted. }
  \item{load}{ Vector of length \code{sides}, how the dice should be
    loaded.}
  \item{x}{ Data frame, return value from \code{dice}. }
  \item{\dots}{ Additional arguments passed to lattice plotting function. }
}
\details{
  Simulates the rolling of dice.  By default it will roll 2 dice 1 time
  and the dice will be fair.  Internally the \code{sample} function is
  used and the load option is passed to sample.  \code{load} is not
  required to sum to 1, but the elements will be divided by the sum of
  all the values.
}
\value{
  A data frame with \code{rolls} rows and \code{ndice} columns
  representing the results from rolling the dice.

  If only 1 die is rolled, then the return value will be a vector.

  If \code{plot.it} is TRUE, then the return value will be invisible.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow \email{greg.snow@imail.org }}
\note{ If the plot function is used or if \code{plot.it} is TRUE, then a
plot will be created on the current graphics device.}



\seealso{ \code{\link{sample}} }
\examples{

# 10 rolls of 4 fair dice
dice(10,4, plot.it=TRUE)

# or

plot(dice(10,4))

# or

tmp <- dice(10,4)
plot(tmp)

# a loaded die
table(tmp <- dice(100,1,plot.it=TRUE, load=6:1 ) )
mean(tmp)

# Efron's dice
ed <- list( rep( c(4,0), c(4,2) ),
  rep(3,6), rep( c(6,2), c(2,4) ),
  rep( c(5,1), c(3,3) ) )

tmp <- dice( 10000, ndice=4 )
ed.out <- sapply(1:4, function(i) ed[[i]][ tmp[[i]] ] )

mean(ed.out[,1] > ed.out[,2])
mean(ed.out[,2] > ed.out[,3])
mean(ed.out[,3] > ed.out[,4])
mean(ed.out[,4] > ed.out[,1])


## redo De Mere's question

demere1 <- dice(10000,4)
demere2 <- dice(10000,24,sides=36)

mean(apply( demere1, 1, function(x) 6 \%in\% x ))

mean(apply( demere2, 1, function(x) 36 \%in\% x))

plot(demere1[1:10,])

## plot all possible combinations of 2 dice

plot.dice( expand.grid(1:6,1:6), layout=c(6,6) )
}
\keyword{ distribution }% at least one, from doc/KEYWORDS
\keyword{ hplot }% __ONLY ONE__ keyword per line
\keyword{ datagen }