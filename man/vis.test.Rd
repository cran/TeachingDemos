\name{vis.test}
\Rdversion{1.1}
\alias{vis.test}
\alias{vt.qqnorm}
\alias{vt.normhist}
\alias{vt.scatterpermute}
\alias{vt.tspermute}
\alias{vt.residpermute}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Do a Visual test of a null hypothesis by choosing the graph that does
not belong.
}
\description{
These functions help in creating a set of plots based on the real data
and some modification that makes the null hypothesis true.  The user
then tries to choose which graph represents the real data.
}
\usage{
vis.test(..., FUN, nrow=3, ncol=3, npage=3, data.name = "", alternative)
vt.qqnorm(x, orig=TRUE)
vt.normhist(x, ..., orig=TRUE)
vt.scatterpermute(x, y, ..., orig=TRUE)
vt.tspermute(x, type='l', ..., orig=TRUE)
vt.residpermute(model, ..., orig=TRUE)
}

\arguments{
  \item{\dots}{ data and arguments to be passed on to \code{FUN} or to
  plotting functions, see details below}
  \item{FUN}{ The function to create the plots on the original or null
    hypothesis data}
  \item{nrow}{ The number of rows of graphs per page }
  \item{ncol}{ The number of columns of graphs per page }
  \item{npage}{ The number of pages to use in the testing }
  \item{data.name}{Optional character string for the name of the data in
  the output}
  \item{alternative}{Optional character string for the alternative
    hypothesis in the output}
  \item{orig}{ Logical, should the original data be plotted, or data
  based on the null hypothesis }
  \item{x}{data or x-coordinates of the data}
  \item{y}{y-coordinates of the data}
  \item{type}{type of plot, passed on to plot function (use 'p' for
  points)}
  \item{model}{An \code{lm} object, or any model object for which
  \code{fitted} and \code{resid} return vectors}
}
\details{
  The \code{vis.test} function will create a \code{nrow} by \code{ncol}
  grid of plots, one
  of which is based on the real (original) data and the others which
  are based on a null hypothesis simulation (a statistical "lineup").
  The real plot is placed at
  random within the set.  The user then clicks on their best guess
  of which plot is the real one (the most different from the others).
  If the null hypothesis is true for the real data, then this will be a
  guess with a 1/(\code{nrow}*\code{ncol}) probability of success.  This
  process is then
  repeated for a total of \code{npage} times.   A p-value is then
  constructed based on the
  number of correct guesses and the null hypothesis that
  there is a 1/(\code{nrow}*\code{ncol}) chance of guessing correct each
  time (this will work
  best if the person doing the choosing has not already seen
  plots/summaries of the data).

  If the plotting function (\code{FUN}) is not passed as a named
  argument, then the first argument (in the \dots) that is a function
  will be used.  If no functions are passed then the function will stop
  with an error.

  The plotting function (\code{FUN}) can be an existing function or a
  user supplied function.  The function must have an argument named
  "orig" which indication whether to plot the original data or the null
  hypothesis data.  A new seed will be set before each call to
  \code{FUN} except when \code{orig} is \code{TRUE}.  Inside the
  function if \code{orig} is \code{TRUE} then the function should plot
  the original data.  When \code{orig} is \code{FALSE} then the function
  should do some form of simulation based on the data with the null
  hypothesis true and plot the simulated data (making sure to give no
  signs that it is different from the original plot).

  The return object includes a list with the seeds set before each of
  the plots (\code{NA} for the original data plot) and a vector of the
  plots selected by the user.  This information can be used to recreate
  the simulated plots by setting the seed then calling \code{FUN}.


  The \code{vt.qqnorm} function tests the null hypothesis that a vector
  of data comes from a normal distribution (or at least pretty close) by
  creating a \code{qqnorm} plot of the original data, or the same plot
  of random data from a normal distribution with the same mean and
  standard deviation as the original data.

  The \code{vt.normhist} function tests the null hypothesis that a
  vector of data comes from a normal distribution (or at least pretty
  close) by plotting a histogram with a reference line representing a
  normal distribution of either the original data or a set of random
  data from a normal distribution with the same mean and standard
  deviation as the original.

  The \code{vt.scatterpermute} function tests the null hypothesis of "no
  relationship" between 2 vectors of data.  When \code{orig} is \code{TRUE} the
  function creates a scatterplot of the 2 variables, when \code{orig} is
  \code{FALSE} the function first permutes the y variable randomly
  (making no relationship) then creates a scatter plot with the original
  x and permuted y variables.

  The \code{vt.tspermute} function creates a time series type plot of a
  single vector against its index.  When \code{orig} is false, the
  vector is permuted before plotting.

  The \code{vt.residpermute} function takes a regression object (class
  lm, or any model type object for which \code{fitted} and \code{resid}
  return vectors) and does a residual plot of the fitted values on the x
  axis and residuals on the y axis.  The loess smooth curve
  (\code{scatter.smooth} is the plotting function) and a reference line
  at 0 are included.  When \code{orig} is \code{FALSE} the residuals are
  randomly permuted before being plotted.


}
\value{
  The \code{vis.test} function returns an object of class \code{htest}
  with the following components:
  \item{method}{The string "Visual Test"}
  \item{data.name}{The name of the data passed to the function}
  \item{statistic}{The number of correct "guesses"}
  \item{p.value}{The p-value based on the number of correct "guesses"}
  \item{nrow}{The number of rows per page}
  \item{ncol}{The number of columns per page}
  \item{npage}{The number of pages}
  \item{seeds}{A list with 3 vectors containing the seeds set before
    calling \code{FUN}, the correct plot has an \code{NA}}
  \item{selected}{A vector of length \code{npage} indicating the number of the
    figure picked in each of the \code{npage} tries}

  The other functions are run for their side effects and do not return
  anything meaningful.
}
\references{
Buja, A., Cook, D. Hofmann, H., Lawrence, M. Lee, E.-K., Swayne, D.F and
  Wickham, H. (2009) Statistical Inference for exploratory data analysis
  and model diagnostics Phil. Trans. R. Soc. A 2009 367, 4361-4383 doi:
  10.1098/rsta.2009.0120
}
\author{Greg Snow \email{greg.snow@imail.org}}


%% ~Make other sections like Warning with \section{Warning }{....} ~
\section{Warning}{The p-value is based on the assumption that under the
  null hypothesis there is a 1/(\code{nrow}*\code{ncol}) chance of
  picking the correct plot
  and that the \code{npage} choices are independent of each other.  This
  may not be
  true if the user is familiar with the data or remembers details of the
  plot between picks.}

\seealso{\code{\link{set.seed}} }
\examples{

if(interactive()) {
  x <- rexp(25, 1/3)
  vis.test(x, vt.qqnorm)

  x <- rnorm(100, 50, 3)
  vis.test(x, vt.normhist)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
\keyword{ datagen }% __ONLY ONE__ keyword per line
\keyword{ htest }