"identify.Map" <-
function(x, labels=as.character(Map$att.data$NAME), 
	n=Inf, ...){
        Map <- x # for consistancy with identify and readability
	if (class(Map) != 'polylist'){
		Pol <- maptools:::Map2poly(Map)
	} else {
		Pol <- Map
	}
	library(sgeostat)
        in.polygon <- get('in.polygon','package:sgeostat')
	idents <- numeric(0)
	i <- 0
	while( i < n ){
		i <- i+1
		tmp <- locator(1)
		if( length(tmp) != 2 ) break
		tmp2 <- which(sapply(Pol, function(x){ x <- na.exclude(x)
	     	in.polygon( tmp$x, tmp$y, x[,1], x[,2] ) } ) )
		if(length(tmp2) < 1) next
		text(tmp, labels=labels[tmp2], ...)
		idents <- c(idents,tmp2)
	}
	idents
}

