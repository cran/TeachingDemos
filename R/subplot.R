subplot <- function(fun, x, y=NULL, size=c(1,1), vadj=0.5, hadj=0.5,
                    inset=c(0,0), type=c('plt','fig'), pars=NULL){

  old.par <- par(no.readonly=TRUE)
  on.exit(par(old.par))

  type <- match.arg(type)

  if(missing(x)) x <- locator(2)

  if(is.character(x)) {
      if(length(inset) == 1) inset <- rep(inset,2)
      x.char <- x
      tmp <- par('usr')
      x <- (tmp[1]+tmp[2])/2
      y <- (tmp[3]+tmp[4])/2

      if( length(grep('left',x.char, ignore.case=TRUE))) {
          x <- tmp[1] + inset[1]*(tmp[2]-tmp[1])
          if(missing(hadj)) hadj <- 0
      }
      if( length(grep('right',x.char, ignore.case=TRUE))) {
          x <- tmp[2] - inset[1]*(tmp[2]-tmp[1])
          if(missing(hadj)) hadj <- 1
      }
      if( length(grep('top',x.char, ignore.case=TRUE))) {
          y <- tmp[4] - inset[2]*(tmp[4]-tmp[3])
          if(missing(vadj)) vadj <- 1
      }
      if( length(grep('bottom',x.char, ignore.case=TRUE))) {
          y <- tmp[3] + inset[2]*(tmp[4]-tmp[3])
          if(missing(vadj)) vadj <- 0
      }
  }

  xy <- xy.coords(x,y)

  if(length(xy$x) != 2){
    pin <- par('pin')
    tmp <- cnvrt.coords(xy$x[1],xy$y[1],'usr')$plt

    x <- c( tmp$x - hadj*size[1]/pin[1],
            tmp$x + (1-hadj)*size[1]/pin[1] )
    y <- c( tmp$y - vadj*size[2]/pin[2],
            tmp$y + (1-vadj)*size[2]/pin[2] )

    xy <- cnvrt.coords(x,y,'plt')$fig
  } else {
    xy <- cnvrt.coords(xy,,'usr')$fig
  }

  par(pars)
  if(type=='fig'){
      par(fig=c(xy$x,xy$y), new=TRUE)
  } else {
      par(plt=c(xy$x,xy$y), new=TRUE)
  }
  fun
  tmp.par <- par(no.readonly=TRUE)

  return(invisible(tmp.par))
}
