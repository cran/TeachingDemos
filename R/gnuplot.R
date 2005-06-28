gp.open <- function(where='c:/progra~1/GnuPlot/bin/pgnuplot.exe'){
	.gp <<- pipe(where,'w')
	.gp.tempfiles <<- character(0)
	invisible(.gp)
}


gp.close <- function(pipe=.gp){
	cat("quit\n",file=pipe)
	close(pipe)
	if(exists('.gp.tempfiles')){
		unlink(.gp.tempfiles)
		rm(.gp.tempfiles,pos=1)
	}
	rm(.gp,pos=1)
	invisible()
}

gp.send <- function(cmd='replot',pipe=.gp){
	cat(cmd, file=pipe)
	cat("\n",file=pipe)
	invisible()
}

gp.plot <- function(x,y,type='p',add=FALSE, title=deparse(substitute(y)), 
		pipe=.gp){
	tmp <- tempfile()
	.gp.tempfiles <<- c(.gp.tempfiles, tmp)

	write.table( cbind(x,y), tmp, row.names=FALSE, col.names=FALSE )
	w <- ifelse(type=='p', 'points', 'lines')
	r <- ifelse(add, 'replot', 'plot')

	cat( paste(r," '",tmp,"' with ",w," title '",title,"'\n",sep=''), 
		file=pipe)
	invisible()
}
