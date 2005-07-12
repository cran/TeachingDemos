\name{coin.faces}
\alias{coin.faces}
\docType{data}
\title{ Designs for coin faces for use with plot.rgl.coin}
\description{
  This is a list of matricies where each matrix represents a design for
  drawing lines on the face of a coin.
}
\usage{data(coin.faces)}
\format{
  The format is:
List of 4
 $ qh: num [1:57, 1:2] 0.387 0.443 0.515 0.606 0.666 ...
 $ qt: num [1:62, 1:2] 0.862 0.873 0.875 0.857 0.797 ...
 $ H : num [1:28, 1:2] 0.503 0.506 0.548 0.548 0.500 ...
 $ T : num [1:18, 1:2] 0.506 0.520 0.569 0.626 0.626 ...
}
\details{
  The current options are a capitol "H", a capitol "T", a design
  representing George Washingtons head traced from the heads of a US
  quarter, and a design representing an eagle traced from the tails of a
  US quarter.

  The tracings here have pretty much exhausted my artistic ability, if
  you can do better, please do, I will be happy to include it in future
  versions.  It would also be nice to include some designs representing
  faces of non-US coins, please submit your contributions (the design
  should fit within a circle inscribed within the unit square).
}
\examples{
\dontrun{
plot.rgl.coin(heads=coin.faces$H, tails=coin.faces$T)
}
}
\keyword{datasets}
