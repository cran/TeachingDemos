plot2script <- function(file='clipboard'){
	con <- file(file)
	open(con, open='a')
	tmp <- recordPlot()[[1]]
	for (i in seq(along=tmp)){
		fn <- tmp[[i]][[1]]
		args <- tmp[[i]][[2]]
		fns <- deparse(fn)
		m <- sub('^.*"(.*)".*$', '\\1', fns, perl=TRUE)
		c2 <- as.list(c(m,args))
		tmp2 <- do.call('call',c2)
		cat(deparse(match.call(get(m), call=tmp2)),"\n", file=con)
	}
	close(con)
}

zoomplot <- function( xlim, ylim=NULL ){
	xy <- xy.coords(xlim,ylim)
	xlim <- range(xy$x)
	ylim <- range(xy$y)

	tmp <- recordPlot()[[1]]
	for(i in seq(along=tmp)){
		fn <- tmp[[i]][[1]]
		alst <- as.list(tmp[[i]][[2]])
		tmp2 <- all.equal( .Primitive("locator"), fn)
		if(is.logical(tmp2) && tmp2){
			next
		}
		tmp2 <- all.equal( .Primitive("plot.window"), fn )
		if(is.logical(tmp2) && tmp2) {
			alst[[1]] <- xlim
			alst[[2]] <- ylim
		}
	
		do.call(fn, alst)
	}
}
 



