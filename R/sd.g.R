"sd.g" <-
  function (data, sizes) 
{
  data <- as.vector(data)
  p <- 1/mean(data)
  std.dev <- sqrt( (1-p) )/p
  return(std.dev)
}
