\name{outliers}
\alias{outliers}
\docType{data}
\title{
Outliers data
}
\description{
This dataset is approximately bell shaped, but with some outliers.  It
is meant to be used for demonstration purposes.  If students are tempted
to throw out all outliers, then have them work with this data (or use a
scaled/centered/shuffled version as errors in a regression problem) and
see how many throw away 3/4 of the data before rethinking their strategy.
}
\usage{data(outliers)}
\format{
  The format is:
 num [1:100] -1.548 0.172 -0.638 0.233 -0.228 ...
}
\details{
This is simulated data meant to demonstrate "outliers".
}
\source{
Simulated, see the examples section.
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(outliers)
qqnorm(outliers)
qqline(outliers)
hist(outliers)

o.chuck <- function(x) {  # function to throw away outliers
	qq <- quantile(x, c(1,3)/4, names=FALSE)
	r <- diff(qq) * 1.5
	tst <- x < qq[1] - r | x > qq[2] + r
	if(any(tst)) {
		cat('Removing ', paste(x[tst], collapse=', '), '\n')
		x <- x[!tst]
		out <- Recall(x)
	} else {
		out <- x
	}
	out
}

x <- o.chuck( outliers )
length(x)

if(require(MASS)) {
  char2seed('robust')
  x <- 1:100
  y <- 3 + 2*x + sample(scale(outliers))*10

  plot(x,y)
  fit <- lm(y~x)
  abline(fit, col='red')

  fit.r <- rlm(y~x)
  abline(fit.r, col='blue', lty='dashed')

  rbind(coef(fit), coef(fit.r))
  length(o.chuck(resid(fit)))
}



### The data was generated using code similar to:

char2seed('outlier')

outliers <- rnorm(25)

dir <- 1

while( length(outliers) < 100 ){
	qq <- quantile(c(outliers, dir*Inf), c(1,3)/4)
	outliers <- c(outliers,
		qq[ 1.5 + dir/2 ] + dir*1.55*diff(qq) + dir*abs(rnorm(1)) )
	dir <- -dir
}

}
\keyword{datasets}
