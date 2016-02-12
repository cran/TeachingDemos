\name{TkListView}
\alias{TkListView}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Interactivly view structures of list and list like objects. }
\description{
  This is somewhat like the \code{str} function, except that it creates
  a new Tk window and a tree object representing the list or object.
  You can then click on the '+' signs to expand branches of the list to
  see what they contain.
}
\usage{
TkListView(list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{list}{ The list or object to be viewed. }
}
\details{
  This function opens a Tk window with a tree view of the list in the
  leftmost pane.  Next to the tree is the result from the \code{str}
  function for each element of the list.  Clicking on the '+' symbol
  next to list elements will expand the tree branch to show what that
  list/sublist contains.  On the right is an output pane with 3 buttons
  below it.  These can be used by first selecting (clicking on) a list
  element in the left pane (this can be a whole list or single
  element), then clicking on one of the buttons.  The output from the
  button appears in the right pane (replacing anything that may have
  been there before).  The 'print' button just prints the element/sublist
  selected.  The 'str' button calls the \code{str} function on the
  selected element/list/sublist.  The 'Eval:' button will evaluate the
  code in the entry box next to it with the selected element of the list
  being the 'x' variable.  For example you could click on an element in
  the list that is a numeric vector, type 'hist(x)' in the entry box,
  and click on the 'Eval:' button to produce a histogram
  (current/default R graphics device) of the data in that element.

  any lists/objects with attributes will show the attributes as an
  additional branch in the tree with a label of "<<attributes>>".

  This function works on S3 objects that are stored as lists.  Since
  currently S4 objects are saved as attributes, wrapping them in a list
  will work with this function to view their structure, see the example below.
}
\value{
  This function is ran for its side effects, it does not return anything
  of use.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Greg Snow, \email{538280@gmail.com} }
%\note{ ~~further notes~~
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{str}} }
\examples{
if(interactive()) {
  tmp <- list( a=letters, b=list(1:10, 10:1), c=list( x=rnorm(100),
        z=data.frame(x=rnorm(10),y=rnorm(10))))
  TkListView(tmp)

  if(require(maptools)){
    data(state.vbm)
    TkListView(list(state.vbm))
  # change the eval box to: plot(x, type='l') and eval the main branches
  }

  fit <- lm(Petal.Width ~ ., data=iris)
  TkListView(fit)


  if(require(stats4)){
   # this example is copied almost verbatim from ?mle
         x <- 0:10
     y <- c(26, 17, 13, 12, 20, 5, 9, 8, 5, 4, 8)
     ll <- function(ymax=15, xhalf=6)
         -sum(stats::dpois(y, lambda=ymax/(1+x/xhalf), log=TRUE))
     (fit <- mle(ll))
     TkListView(list(fit))
  }
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ attribute }% __ONLY ONE__ keyword per line
\keyword{ list }
