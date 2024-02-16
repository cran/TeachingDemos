"rgl.Map" <-
function(Map, which, ...) {
  if (missing(which)) which <- TRUE
  if(!requireNamespace('rgl', quietly = TRUE)) stop("This function depends on the 'rgl' package which is not available")

  n1 <- length(Map[which])
  for(i in seq_len(n1)) {
    n2 <- length(Map[which][[i]])
    for(j in seq_len(n2)) {
      tmp <- Map[which][[c(i,j,1)]]
      long <- tmp[,1] * pi/180
      lat <- pi/2 - tmp[,2] * pi/180
      
      z <- cos(long)*sin(lat)
      y <- cos(lat)
      x <- sin(long)*sin(lat)
      
    
      rgl::lines3d(x,y,z, ...)
    }
  }

  invisible()
}

