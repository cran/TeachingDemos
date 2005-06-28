"project.Map" <-
function(Map, projection='', parameters=NULL,
                        orientation=c(90,0,0)) {
  library(mapproj)
  tmp <- attributes(Map$Shapes)
  Map$Shapes <- lapply(Map$Shapes, function(x){
    tmp.x <- x$verts[,1]
    tmp.y <- x$verts[,2]
    out <- mapproject(tmp.x,tmp.y, projection=projection,
                      parameters=parameters,orientation=orientation)
    x$verts <- cbind(out$x,out$y)
    x$bbox <- c(min(out$x, na.rm=T), min(out$y, na.rm=T),
                max(out$x, na.rm=T), max(out$y, na.rm=T) )
    attr(x,'bbox') <- x$bbox
    x
  })

  attributes(Map$Shapes) <- tmp

  attr(Map$Shapes, 'minbb')[1] <- min( sapply(Map$Shapes, function(x){
    x$bbox[1] } ), na.rm=T )
  attr(Map$Shapes, 'minbb')[2] <- min( sapply(Map$Shapes, function(x){
    x$bbox[2] } ), na.rm=T )
  attr(Map$Shapes, 'maxbb')[1] <- max( sapply(Map$Shapes, function(x){
    x$bbox[3] } ), na.rm=T )
  attr(Map$Shapes, 'maxbb')[2] <- max( sapply(Map$Shapes, function(x){
    x$bbox[4] } ), na.rm=T )

  Map
}

