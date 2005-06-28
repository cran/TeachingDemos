clipplot <- function(fun, xlim=par('usr')[1:2], 
	ylim=par('usr')[3:4] ){
  old.par <- par(c('plt','xpd'))


  if( length(xlim) < 2 ) stop('xlim must be a vector with at least 2 elements')
  if( length(ylim) < 2 ) stop('ylim must be a vector with at least 2 elements')

  if( !require(TeachingDemos) ) stop('TeachingDemos package needed')

  xl <- range(xlim)
  yl <- range(ylim)

  pc <- cnvrt.coords(xl,yl)$fig

  par(plt=c(pc$x,pc$y),xpd=FALSE)

  fun

  par(old.par)
  box() # need to plot something to reset

}  

