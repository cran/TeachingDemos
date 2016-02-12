\name{steps}
\alias{steps}
\docType{data}
\title{
Steps data
}
\description{
This is the export from a pedometer worn for nearly about 11 months by
the package author.
}
\usage{data(steps)}
\format{
  A data frame with 331 observations on the following 79 variables.
  \describe{
    \item{\code{Date}}{The Date for the given data}
    \item{\code{Total.Steps}}{Total Steps recorded for the day}
    \item{\code{Aerobic.Steps}}{Total Aerobic Steps recorded for the day (see below)}
    \item{\code{Aerobic.Walking.Time}}{Time spent in aerobic walking for
    the day}
    \item{\code{Calories}}{Estimated calories burned for the day}
    \item{\code{Distance}}{Estimated distance walked for the day in miles}
    \item{\code{Fat.Burned}}{Estimated grams of fat burned by walking}
    \item{\code{Steps.12AM}}{Steps recorded between Midnight and 1 am}
    \item{\code{Steps.1AM}}{Steps recorded between 1 am and 2 am}
    \item{\code{Steps.2AM}}{Steps recorded between 2 am and 3 am}
    \item{\code{Steps.3AM}}{Steps recorded between 3 am and 4 am}
    \item{\code{Steps.4AM}}{Steps recorded between 4 am and 5 am}
    \item{\code{Steps.5AM}}{Steps recorded between 5 am and 6 am}
    \item{\code{Steps.6AM}}{Steps recorded between 6 am and 7 am}
    \item{\code{Steps.7AM}}{Steps recorded between 7 am and 8 am}
    \item{\code{Steps.8AM}}{Steps recorded between 8 am and 9 am}
    \item{\code{Steps.9AM}}{Steps recorded between 9 am and 10 am}
    \item{\code{Steps.10AM}}{Steps recorded between 10 am and 11 am}
    \item{\code{Steps.11AM}}{Steps recorded between 11 am and Noon}
    \item{\code{Steps.12PM}}{Steps recorded between Noon and 1 pm}
    \item{\code{Steps.1PM}}{Steps recorded between 1 pm and 2 pm}
    \item{\code{Steps.2PM}}{Steps recorded between 2 pm and 3 pm}
    \item{\code{Steps.3PM}}{Steps recorded between 3 pm and 4 pm}
    \item{\code{Steps.4PM}}{Steps recorded between 4 pm and 5 pm}
    \item{\code{Steps.5PM}}{Steps recorded between 5 pm and 6 pm}
    \item{\code{Steps.6PM}}{Steps recorded between 6 pm and 7 pm}
    \item{\code{Steps.7PM}}{Steps recorded between 7 pm and 8 pm}
    \item{\code{Steps.8PM}}{Steps recorded between 8 pm and 9 pm}
    \item{\code{Steps.9PM}}{Steps recorded between 9 pm and 10 pm}
    \item{\code{Steps.10PM}}{Steps recorded between 10 pm and 11 pm}
    \item{\code{Steps.11PM}}{Steps recorded between 11 pm and Midnight}
    \item{\code{Aerobic.Steps.12AM}}{Aerobic steps recorded between
      Midnight and 1 am}
    \item{\code{Aerobic.Steps.1AM}}{Aerobic steps recorded between 1 am
      and 2 am}
    \item{\code{Aerobic.Steps.2AM}}{Aerobic steps recorded between 2 am
      and 3 am}
    \item{\code{Aerobic.Steps.3AM}}{Aerobic steps recorded between 3 am
      and 4 am}
    \item{\code{Aerobic.Steps.4AM}}{Aerobic steps recorded between 4 am
      and 5 am}
    \item{\code{Aerobic.Steps.5AM}}{Aerobic steps recorded between 5 am
      and 6 am}
    \item{\code{Aerobic.Steps.6AM}}{Aerobic steps recorded between 6 am
      and 7 am}
    \item{\code{Aerobic.Steps.7AM}}{Aerobic steps recorded between 7 am
      and 8 am}
    \item{\code{Aerobic.Steps.8AM}}{Aerobic steps recorded between 8 am
      and 9 am}
    \item{\code{Aerobic.Steps.9AM}}{Aerobic steps recorded between 9 am
      and 10 am}
    \item{\code{Aerobic.Steps.10AM}}{Aerobic steps recorded between 10
      am and 11 am}
    \item{\code{Aerobic.Steps.11AM}}{Aerobic steps recorded between 11
      am and Noon}
    \item{\code{Aerobic.Steps.12PM}}{Aerobic steps recorded between Noon
      and 1 pm}
    \item{\code{Aerobic.Steps.1PM}}{Aerobic steps recorded between 1 pm
      and 2 pm}
    \item{\code{Aerobic.Steps.2PM}}{Aerobic steps recorded between 2 pm
      and 3 pm}
    \item{\code{Aerobic.Steps.3PM}}{Aerobic steps recorded between 3 pm
      and 4 pm}
    \item{\code{Aerobic.Steps.4PM}}{Aerobic steps recorded between 4 pm
      and 5 pm}
    \item{\code{Aerobic.Steps.5PM}}{Aerobic steps recorded between 5 pm
      and 6 pm}
    \item{\code{Aerobic.Steps.6PM}}{Aerobic steps recorded between 6 pm
      and 7 pm}
    \item{\code{Aerobic.Steps.7PM}}{Aerobic steps recorded between 7 pm
      and 8 pm}
    \item{\code{Aerobic.Steps.8PM}}{Aerobic steps recorded between 8 pm
      and 9 pm}
    \item{\code{Aerobic.Steps.9PM}}{Aerobic steps recorded between 9 pm
      and 10 pm}
    \item{\code{Aerobic.Steps.10PM}}{Aerobic steps recorded between 10
      pm and 11 pm}
    \item{\code{Aerobic.Steps.11PM}}{Aerobic steps recorded between 11
      pm and Midnight}
    \item{\code{Used.12AM}}{Any movement detected between Midnight and 1 am}
    \item{\code{Used.1AM}}{Any movement detected between 1 am and 2 am}
    \item{\code{Used.2AM}}{Any movement detected between 2 am and 3 am}
    \item{\code{Used.3AM}}{Any movement detected between 3 am and 4 am}
    \item{\code{Used.4AM}}{Any movement detected between 4 am and 5 am}
    \item{\code{Used.5AM}}{Any movement detected between 5 am and 6 am}
    \item{\code{Used.6AM}}{Any movement detected between 6 am and 7 am}
    \item{\code{Used.7AM}}{Any movement detected between 7 am and 8 am}
    \item{\code{Used.8AM}}{Any movement detected between 8 am and 9 am}
    \item{\code{Used.9AM}}{Any movement detected between 9 am and 10 am}
    \item{\code{Used.10AM}}{Any movement detected between 10 am and 11 am}
    \item{\code{Used.11AM}}{Any movement detected between 11 am and Noon}
    \item{\code{Used.12PM}}{Any movement detected between Noon and 1 pm}
    \item{\code{Used.1PM}}{Any movement detected between 1 pm and 2 pm}
    \item{\code{Used.2PM}}{Any movement detected between 2 pm and 3 pm}
    \item{\code{Used.3PM}}{Any movement detected between 3 pm and 4 pm}
    \item{\code{Used.4PM}}{Any movement detected between 4 pm and 5 pm}
    \item{\code{Used.5PM}}{Any movement detected between 5 pm and 6 pm}
    \item{\code{Used.6PM}}{Any movement detected between 6 pm and 7 pm}
    \item{\code{Used.7PM}}{Any movement detected between 7 pm and 8 pm}
    \item{\code{Used.8PM}}{Any movement detected between 8 pm and 9 pm}
    \item{\code{Used.9PM}}{Any movement detected between 9 pm and 10 pm}
    \item{\code{Used.10PM}}{Any movement detected between 10 pm and 11 pm}
    \item{\code{Used.11PM}}{Any movement detected between 11 pm and Midnight}
  }
}

\examples{
data(steps)
## maybe str(steps) ; plot(steps) ...
}
\keyword{datasets}
