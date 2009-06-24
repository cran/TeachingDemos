\name{evap}
\Rdversion{1.1}
\alias{evap}
\docType{data}
\title{
Data on soil evaporation.
}
\description{
Data from 46 consecutive days on weather variables used to estimate
amount of evaporation from the soil. 
}
\usage{data(evap)}
\format{
  A data frame with 46 observations on the following 14 variables.
  \describe{
    \item{\code{Obs}}{Observation number}
    \item{\code{Month}}{Month (6-June, 7-July)}
    \item{\code{day}}{Day of the month}
    \item{\code{MaxST}}{Maximum Soil Temperature}
    \item{\code{MinST}}{Minimum Soil Temperature}
    \item{\code{AvST}}{Average (integrated) Soil Temperature}
    \item{\code{MaxAT}}{Maximum Air Temperature}
    \item{\code{MinAT}}{Minimum Air Temperature}
    \item{\code{AvAT}}{Average (integrated) Air Temperature}
    \item{\code{MaxH}}{Maximum Relative Humidity}
    \item{\code{MinH}}{Minimum Relative Humidity}
    \item{\code{AvH}}{Average (integrated) Relative Humidity}
    \item{\code{Wind}}{Total Wind}
    \item{\code{Evap}}{Total evoporation from the soil}
  }
}
\details{
The idea of the data is to predict the amount of evaporation given the
other variables.  Note that the "average" values are scaled differently
from the others, this is more an area under the curve measure
representing the total/average value.

This dataset was entered by hand from a low quality copy of the paper.
If you find any typos, please e-mail them to the package maintainer.
}
\source{
  Freund, R.J. (1979) Multicollinearity etc., Some "New" Examples.
  Proceedings of the Statistical Computing Section, *4*, 111-112.
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(evap)
pairs(evap[,-c(1,2,3)], panel=panel.smooth)
## maybe str(evap) ; plot(evap) ...
}
\keyword{datasets}
