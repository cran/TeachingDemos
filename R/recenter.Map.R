"recenter.Map" <-
function(Map, center=0, range=c(-180,180)){
  tmp <- attributes(Map$Shapes)
  Map$Shapes <- lapply(Map$Shapes, function(x){
    if( all( x$bbox[c(1,3)] > center+range[2] ) ) { # shift left
      x$verts[,1] <- x$verts[,1] - diff(range)
      x$bbox[c(1,3)] <- x$bbox[c(1,3)] - diff(range)
      attr(x, "bbox") <- x$bbox
    } else if( all( x$bbox[c(1,3)] < center+range[1] ) ) { # shift right
      x$verts[,1] <- x$verts[,1] + diff(range)
      x$bbox[c(1,3)] <- x$bbox[c(1,3)] + diff(range)
      attr(x, "bbox") <- x$bbox
    }
    x
  } )

  attributes(Map$Shapes) <- tmp
  
  attr(Map$Shapes, 'minbb')[1] <- min( sapply(Map$Shapes, function(x){
    x$bbox[1] } ) )
  attr(Map$Shapes, 'maxbb')[1] <- max( sapply(Map$Shapes, function(x){
    x$bbox[3] } ) )

  Map
}

