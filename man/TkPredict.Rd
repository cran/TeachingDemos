\name{TkPredict}
\alias{TkPredict}
\alias{Predict.Plot}
%- Also NEED an '\alias' for EACH other topic documented here.
%-  cp slider2.Rd /home/wiwi/pwolf/work/work.rtrevive/install.dir/rwined/man/slider.Rd
\title{Plot predicted values from a model against one of the predictors
  for a given value of the othe predictors}
\description{
  These functions create a plot of predicted values vs. one of the
  predictors for given values of the other predictors.  TkPredict
  further creates a Tk gui to allow you to change the values of the
  other predictors.
}
\usage{
Predict.Plot(model, pred.var, ..., type='response', add=FALSE,
plot.args=list(), n.points=100, ref.val, ref.col='green', ref.lty=1,
data)
TkPredict(model, data, pred.var, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{A model of class 'lm' or 'glm' (or possibly others) from
  which to plot predictions.}
  \item{pred.var}{A character string indicating which predictor variable
  to put on the x-axis of the plot.}
  \item{...}{for \code{Predict.Plot} The predictor variables and their
  values for the predictions.  See below for detail.}
  \item{type}{The type value passed on to the predict function.}
  \item{add}{Whether to add a line to the existing plot or start a new
  plot.}
  \item{plot.args}{A list of additional options passed on to the
  plotting function.}
  \item{n.points}{The number of points to use in the approximation of
  the curve.}
  \item{ref.val}{A reference value for the \code{pred.var}, a reference
  line will be drawn at this value to the corresponding predicted value.}
  \item{ref.col, ref.lty}{The color and line type of the reference line
  if plotted.}
  \item{data}{The data frame or environment where the variables that the
  model was fit to are found.  If missing, the model will be examined
  for an attempt find the needed data.}
}
\details{
  These functions plot the predicted values from a regression model
  (\code{lm} or \code{glm}) against one of the predictor variables for
  given values of the other predictors.  The values of the other
  predictors are passed as the \code{...} argument to
  \code{Predict.Plot} or are set using gui controls in \code{TkPredict}
  (initial values are the medians).

  If the variable for the x axis (name put in \code{pred.var}) is not
  included with the \code{...} variables, then the range will be
  computed from the \code{data} argument or the data component of the
  \code{model} argument.

  If the variable passed as \code{pred.var} is also included in the
  \code{...} arguments and contains a single value, then this value will
  be used as the \code{ref.val} argument.

  If it contains 2 or more values, then the range of these values will
  be used as the x-limits for the predictions.

  When running \code{TkPredict} you can click on the "Print Call" button
  to print out the call of \code{Predict.Plot} that will recreate the
  same plot.  Doing this for different combinations of predictor values
  and editing the \code{plot.args} and \code{add} arguments will give
  you a script that will create a static version of the predictions.
}
\value{
  These functions are run for their side effects of creating plots and
  do not return anything.
}
\author{Greg Snow, \email{538280@gmail.com}}

\seealso{ \code{tkrplot}, \code{\link{tkexamp}}, \code{\link{predict}} }

\note{ The GUI currently allows you to select a factor as the
  x-variable.  If you do this it will generate some errors and you will
  not see the plot, just choose a different variable as the x-variable
  and the plot will return. }

\examples{
library(splines)

fit.lm1 <- lm( Sepal.Width ~ ns(Petal.Width,3)*ns(Petal.Length,3)+Species,
	data=iris)

Predict.Plot(fit.lm1, pred.var = "Petal.Width", Petal.Width = 1.22,
      Petal.Length = 4.3, Species = "versicolor",
 	plot.args = list(ylim=range(iris$Sepal.Width), col='blue'),
      type = "response")
Predict.Plot(fit.lm1, pred.var = "Petal.Width", Petal.Width = 1.22,
      Petal.Length = 4.3, Species = "virginica",
	plot.args = list(col='red'),
      type = "response", add=TRUE)
Predict.Plot(fit.lm1, pred.var = "Petal.Width", Petal.Width = 1.22,
      Petal.Length = 4.4, Species = "virginica",
	plot.args = list(col='purple'),
      type = "response", add=TRUE)

fit.glm1 <- glm( Species=='virginica' ~ Sepal.Width+Sepal.Length,
	data=iris, family=binomial)

Predict.Plot(fit.glm1, pred.var = "Sepal.Length", Sepal.Width = 1.99,
      Sepal.Length = 6.34, plot.args = list(ylim=c(0,1), col='blue'),
 	type = "response")
Predict.Plot(fit.glm1, pred.var = "Sepal.Length", Sepal.Width = 4.39,
      Sepal.Length = 6.34, plot.args = list(col='red'),
	type = "response", add=TRUE)




if(interactive()){
TkPredict(fit.lm1)

TkPredict(fit.glm1)
}

}
\keyword{dynamic}% at least one, from doc/KEYWORDS
\keyword{iplot}% __ONLY ONE__ keyword per line
\keyword{regression}
