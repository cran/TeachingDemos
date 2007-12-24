.gp <- numeric(0)
.gp.tempfiles <- character(0)

gp.open <- function(where='c:/progra~1/GnuPlot/bin/pgnuplot.exe'){
  unlockBinding('.gp', env=environment(gp.open))
  unlockBinding('.gp.tempfiles', env=environment(gp.open))
	.gp <<- pipe(where,'w')
	.gp.tempfiles <<- character(0)
	invisible(.gp)
}


gp.close <- function(pipe=.gp){
	cat("quit\n",file=pipe)
	close(pipe)
	if(exists('.gp.tempfiles')){
		unlink(.gp.tempfiles)
		.gp.tempfiles <<- character(0)
	}
	.gp <<- numeric(0)
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
