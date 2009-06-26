"stats.g" <-
  function (data, sizes) 
{
  statistics <- as.vector(data)
  center <- mean(statistics)
  list(statistics = statistics, center = center)
}
