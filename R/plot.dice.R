"plot.dice" <-
function(x,...){
  if(!require(lattice)) stop('The lattice package is needed')

  old.trellis.par <- trellis.par.get()
  on.exit(trellis.par.set(old.trellis.par))
  trellis.par.set(theme=col.whitebg())
  df <- as.matrix(x)
  x <- c(df)
  y <- c(col(df)) - 1
  g <- factor(c(row(df)))

  xx <- ceiling(sqrt(dim(df)[2]))
  yy <- ceiling( dim(df)[2]/xx )

  invisible(print(xyplot( y~x|g, prepanel=prepanel.dice, panel=panel.dice,
                         scales=list(draw=FALSE), aspect=yy/xx, strip=FALSE,
                         as.table=TRUE,
                         xlab="", ylab="",...)))
}

