\name{tkprogress}
\alias{tkprogress}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a progress bar and function to control it. }
\description{
  This function opens a new graphical window with a progress bar
  (originally set at 0) and returns a function to be used to control the
  progress bar.  This can be used inside of a loop to show your progress.
}
\usage{
tkprogress(numsteps = 100, title = "Progress")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{numsteps}{ The number of steps for the progress bar, generally
    the number of iterations of your loop }
  \item{title}{ A title for the window holding the progress bar }
}
\details{
  This function creates a window and a progress bar (using Tk) and
  returns a function that controls the progress bar.

  This function uses only packages with namespaces so can be called as
  \code{TeachingDemos::tkprogress} without having the package loaded.

  The return value is a function for controlling the progress bar.  If
  the returned function is called without any arguments then the
  progress bar is incremented 1, given a value it will increment by that
  much.  Called with \code{set=n} will jump the progress bar to n (an
  integer between 0 and \code{numsteps}).  Called with \code{quit=TRUE}
  will remove the progress bar and its window.
}
\value{
  A function to control the progress bar, the arguments for the function are:
  \item{inc }{The amount to increment the progress bar (overridden by
  other arguments)}
  \item{set }{The value to set the progress bar to (integer between 0
    and \code{nubsteps})}
  \item{quit }{ Should the progress bar quit and its window go away }
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow, \email{greg.snow@imail.org} }
\note{
  This function uses a real progress bar in R 2.7.0 and above, but fakes
  it using a slider in prior versions (the developement version works
  like 2.7.0).
}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
   pg <- tkprogress(10) # or TeachingDemos:::tkprogress(10)
   for(i in 1:10) {
     pg()
     Sys.sleep(1)
   }
   pg(set=3)
   pg(5)
   pg(quit=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ iteration }

