# These functions were written by Greg Snow (538280@gmail.com) 
# They are free to use, but come with no warrenty whatsoever
# use at your own risk (not that I can think of anything bad that
# they would do).


hpd <- function(posterior.icdf, conf=0.95, tol=0.00000001,...){
	conf <- min( conf, 1-conf )
	f <- function(x,posterior.icdf,conf,...){
		posterior.icdf(1-conf+x,...) - posterior.icdf(x,...)
	}
	out <- optimize(f, c(0,conf), posterior.icdf = posterior.icdf,
                        conf=conf, tol=tol, ...)
	return( c( posterior.icdf(out$minimum,...), 
	           posterior.icdf(1-conf+out$minimum,...) ) )
}

emp.hpd <- function(x, conf=0.95, lower, upper){
  if(!missing(lower)) {
    if(length(lower) != 1 || !is.numeric(lower)) stop("lower bound must be a single number")
    if(any(x < lower)){
      stop("At least one value is smaller than the lower bound")
    }
    x <- c(lower, x)
  }
  if(!missing(upper)) {
    if(length(upper) != 1 || !is.numeric(upper)) stop("upper bound must be a single number")
    if(any(x > upper)) {
      stop("At least one value is larger than the upper bound")
    }
    x <- c(x, upper)
  }
	conf <- min(conf, 1-conf)
	n <- length(x)
	nn <- floor( n*conf )
	if(nn < 1) {
	  warning("Sample size too small for given confidence/credible level, returning 100% range.")
	  return(range(x))
	}
	x <- sort(x)
	xx <- utils::tail(x, n) - utils::head(x, n)
	nnn <- which.min(xx)
	return( c( x[ nnn ], x[ n-nn+nnn ] ) )
}
