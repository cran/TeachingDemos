\name{USCrimes}
\alias{USCrimes}
\docType{data}
\title{
 US Crime Statistics
}
\description{
  This is a 3 dimensional Array of the US crime statistics downloaded
  from the "Uniform Crime Reporting Statistics" of the US government.
  It comprises the years 1960 through 2010 for all 50 states, Washington
  DC, and a total for the country.
}
\usage{data(USCrimes)}
\format{
  The format is:
 num [1:52, 1:51, 1:19] 3266740 226167 1302161 1786272 15717204 ...
 - attr(*, "dimnames")=List of 3
  ..$ State: chr [1:52] "Alabama" "Alaska" "Arizona" "Arkansas" ...
  ..$      : chr [1:51] "1960" "1961" "1962" "1963" ...
  ..$      : chr [1:19] "Population" "ViolentCrimeRate" "MurderRate" "RapeRate" ...
}
\details{
The first dimension is the state, the dimnames match the variable
\code{state.name} with the exception of including "District of Columbia"
in the 9th position (alphabetically) and "United States-Total" in
position 45 (alphabetical).

The second dimension is the year, ranging from 1960 to 2010.  If indexing
by year, remember to put the year in quotes.

The third dimension is the variable:
\describe{
  \item{Population:}{Total number of residents}
  \item{ViolentCrimeRate:}{The total of the violent crimes (Murder,
    Rape, Robbery, Assault) per 100,000 population}
  \item{MurderRate:}{The number of Murders and Nonnegligent
    Manslaughters per 100,000 population}
  \item{RapeRate:}{Forcible Rapes per 100,000 population}
  \item{RobberyRate:}{Robberies per 100,000 population}
  \item{AssaultRate:}{Aggravated Assults per 100,000}
  \item{PropertyCrimeRate:}{The total of the property crimes (Burglary,
    Theft, Vehicle Theft) per 100,000 population}
  \item{BurglaryRate:}{Burglaries per 100,000 population}
  \item{TheftRate:}{Larceny-Thefts per 100,000 population}
  \item{VehicleTheftRate:}{Motor Vehicle Thefts per 100,000 population}
  \item{ViolentCrimeTotal:}{The total of the violent crimes (Murder,
    Rape, Robbery, Assault}
  \item{Murder:}{The number of Murders and Nonnegligent
    Manslaughters}
  \item{Rape:}{Forcible Rapes}
  \item{Robbery:}{Robberies}
  \item{Assault:}{Aggravated Assults}
  \item{PropertyCrimeTotal:}{The total of the property crimes (Burglary,
    Theft, Vehicle Theft)}
  \item{Burglary:}{Burglaries}
  \item{Theft:}{Larceny-Thefts}
  \item{VehicleTheft:}{Motor Vehicle Thefts}
  }
}
\source{
\url{https://ucrdatatool.gov/}
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(USCrimes)
## maybe str(USCrimes)

# plot time series/sparkline for each state
if(require(maptools)) {
	data(state.vbm)
	plot(state.vbm)

	tmp.x <- state.vbm$center_x
	tmp.x <- c( tmp.x[1:8], 147, tmp.x[9:43], 83, tmp.x[44:50] )
	tmp.y <- state.vbm$center_y
	tmp.y <- c( tmp.y[1:8], 45, tmp.y[9:43], -18, tmp.y[44:50] )
	tmp.r <- range( USCrimes[,,'ViolentCrimeRate'], na.rm=TRUE)
	for(i in 1:52) {
		subplot( plot(1960:2010, USCrimes[i,,'ViolentCrimeRate'],
					ann=FALSE, bty='n', type='l', axes=FALSE),
			tmp.x[i], tmp.y[i], size=c(0.2,0.2) )
	}
}


## Gapminder style animation over time
if( interactive() ) {
x.r <- range( USCrimes[-c(9,45),,'Population'], na.rm=TRUE )
y.r <- range( USCrimes[-c(9,45),,'PropertyCrimeRate'], na.rm=TRUE )

tmpfun <- function(Year=1960, ... ) {
	y <- as.character(Year)
	plot( USCrimes[-c(9,45),y,'Population'],
		 USCrimes[-c(9,45),y,'PropertyCrimeRate'],
		type='n', xlab='log Population',
		ylab='Property Crime Rate',
		main=y, xlim=x.r, ylim=y.r, log='x' )
	text( USCrimes[-c(9,45),y,'Population'],
		 USCrimes[-c(9,45),y,'PropertyCrimeRate'],
		state.abb, ... )
}

tmp.list <- list( Year=list('animate', from=1960, to=2010, delay=250) )

tmpcol <- c('blue','darkgreen','red','purple')[state.region]
tkexamp( tmpfun(col=tmpcol), tmp.list )
}
}
\keyword{datasets}
