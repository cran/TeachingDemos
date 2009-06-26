"rgl.Map" <-
function(Map,which,...) {
  if (missing(which)) which <- T
  if(!require(rgl)) stop("This function depends on the 'rgl' package which is not available")

  lapply(Map$Shapes[which], function(shape) {
    long <- shape$verts[,1] * pi/180
    lat  <- pi/2 - shape$verts[,2] * pi/180

#    x <- cos(lat)*sin(long)
#    y <- -sin(lat)*sin(long)
#    z <- cos(lat)*cos(long)

    z <- cos(long)*sin(lat)
    y <- cos(lat)
    x <- sin(long)*sin(lat)


    tmp.i <- rep( seq(along=x), each=2)
    tmp.i <- c(tmp.i[-1],1)

    rgl.lines(x[tmp.i], y[tmp.i], z[tmp.i],...)
  })
  invisible()
}

