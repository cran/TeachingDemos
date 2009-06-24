\name{tkexamp}
\alias{tkexamp}
%- Also NEED an '\alias' for EACH other topic documented here.
%-  cp slider2.Rd /home/wiwi/pwolf/work/work.rtrevive/install.dir/rwined/man/slider.Rd
\title{Create Tk dialog boxes with controls to show examples of changing
parameters on a graph.}
\description{
  This utility will create a Tk window with a graph and controls to
  change the parameters of the plotting function interactively.
}
\usage{
 tkexamp(FUN, param.list, vscale=1.5, hscale=1.5, wait=FALSE,
plotloc='top', print=FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FUN}{A function call to create the example plot}
  \item{param.list}{A list of lists with information on the parameters
    to control and the controls to use.  See Details Below}
  \item{vscale}{Vertical size of plot, passed to \code{tkrplot}}
  \item{hscale}{Horizontal size of plot, passed to \code{tkrplot}}
  \item{wait}{Should R wait for the demo to end}
  \item{plotloc}{Character with "top", "left", or "right" indicating
    where the plot should be placed relative to the controls}
  \item{print}{Automatically print the result (useful for ggplot2/lattice)}
  \item{...}{Extra arguments, currently ignored}
}
\details{
  This is a helper function to create interactive demonstrations of the
  effect of various function arguments on the resulting graph.

  The \code{FUN} argument should be a function call to create the basic
  plot (if run stand alone this should create the starting plot).  The
  arguments to be changed should not be included.

  The \code{param.list} is a nested list of lists that defines which
  controls to use for which function arguments.  Additional levels of
  nested lists creates groups of controls (see examples below) and if
  the list is named in the enclosing list, that name will be used to
  label the group.

  The
  lowest level of lists control a single function argument with the
  control to be used.  The name of the list in the enclosing list is the
  name of the function argument to be used, e.g. \code{"pch=list(...)"}
  will  create a control for the \code{pch} parameter.

  The first element of the innermost list is a character string
  specifying which control to use (from the list below), the rest of the
  elements must be named and specify parameters of the controls.  For
  details on all possible parameters see the tcltk documentation.  Any
  parameter can be set using this list, for example most controls have a
  \code{width} parameter that can be set with code like \code{width=5}.
  Most controls also have an \code{init} argument that specifies the
  initial value that the control will be set to (most have a default in
  case you don't specify the value).

  The following are the possible controls you can specify as the first
  element of the list along with the most common parameters to specify:

  "numentry", an entry box where a number can be typed in, this will be
  passed to \code{FUN} wrapped in \code{as.numeric()}.

  "entry", an entry box where a character string can be typed in (this
  will be passed to \code{FUN} as a character string, not converted).

  "slider", a slider (or scale) that can be dragged left and right to
  choose the different values.  The common parameters to specify are
  "from" (the lowest value), "to" (the largest value), and "resolution"
  (the increment size when sliding).

  "vslider", just like slider except that the slider is dragged up and
  down rather than left and right.

  "spinbox", an entry widget for a number with small arrows on the right
  side that can be used to increment/decrement the value, or you can
  type in a value.  The common parameters to set are "from" (smallest
  value), "to" (largest value), and "increment" (how much to change the
  value by when clicking on the arrows).  You can also set "values"
  which is a vector of values that can be used.  This will be passed to
  \code{FUN} as a number.

  "checkbox", a box that can be checked, passed to \code{FUN} as a
  logical (TRUE if checked, FALSE if not checked).

  "combobox", an entry widget with an arrow on the right side that will
  bring up a list of values to choose from.  This value is passed to
  \code{FUN} as a character string.  The important parameter to set is
  "values" which is a vector of character strings to choose between.
  This option will only work with tcl version 8.5 or later and will
  probably produce an error in earlier versions.

  "radiobuttons", a set of choices with check boxes next to each, when
  one is selected the previous selection is cleared.  The important
  parameter to set is "values" wich is a vector of character strings to
  choose between.

  Each nesting of lists will also change how the controls are placed
  (top to bottom vs. left to right).


  The Tk window will also have a default set of controls at the bottom.
  These include entry widgets for \code{vscale} and \code{hscale} for
  changing the size of the graph (initially set by arguments to
  \code{tkexamp}).  A "Refresh" button that will refresh the graph with
  the new parameter values (some controls like sliders will
  automatically refresh, but others like entries will not refresh on
  their own and you will need to click on this button to see the
  updates).  A "Print Call" button that when clicked will print a text
  string to the R terminal that represents the function call with the
  current argument settings (copying and pasting this to the command
  line should recreate the current plot on the current plotting
  device).  And an "Exit" button that will end the demo and close the window.
}
\value{
  If \code{wait} is FALSE then it returns an invisible NULL, if
  \code{wait} is TRUE then it returns a list with the argument values
  when the window was closed.
}
\author{Greg Snow, \email{greg.snow@imail.org}}

\note{You can move the sliders in 3 different ways:  You can left click
  and drag the slider itself, you can left click in the trough to either
  side of the slider and the slider will move 1 unit in the direction
  you clicked, or you can right click in the trough and the slider will
  jump to the location you clicked at.
    }

\seealso{ \code{tkrplot}, the fgui package, the playwith package, and
  the rpanel package }

\examples{
if(interactive()){

x <- sort( runif(25,1,10) )
y <- rnorm(25, x)

# some common plotting parameters

tke.test1 <- list(Parameters=list(
                 pch=list('spinbox',init=1,from=0,to=255,width=5),
                 cex=list('slider',init=1.5,from=0.1,to=5,resolution=0.1),
                 type=list('combobox',init='b',
                   values=c('p','l','b','o','c','h','s','S','n'),
                        width=5),
                 lwd=list('spinbox',init=1,from=0,to=5,increment=1,width=5),
                 lty=list('spinbox',init=1,from=0,to=6,increment=1,width=5)
                 ))

tkexamp( plot(x,y), tke.test1, plotloc='top' )

# different controls for the parameters

tke.test2 <- list(Parameters=list(
                 pch=list('spinbox',init=1,values=c(0:25,32:255),width=5),
                 cex=list('slider',init=1.5,from=0.1,to=5,resolution=0.1),
                 type=list('radiobuttons',init='b',
                   values=c('p','l','b','o','c','h','s','S','n'),
                        width=5),
                 lwd=list('spinbox',init=1,from=0,to=5,increment=1,width=5),
                 lty=list('spinbox',init=1,from=0,to=6,increment=1,width=5),
                 xpd=list('checkbox')
                 ))

tkexamp( plot(x,y), tke.test2, plotloc='left')

tmp <- tkexamp( plot(x,y), list(tke.test2), plotloc='right', wait=TRUE )

# now recreate the plot
tmp$x <- x
tmp$xlab <- 'x'
tmp$y <- y
tmp$ylab <- 'y'
do.call('plot', tmp)

# a non plotting example

tke.test3 <- list(
        sens=list('slider', init=0.95, from=0.9, to=1, resolution=0.005),
        spec=list('slider', init=0.9, from=0.8, to=1, resolution=0.005),
        prev=list('slider', init=0.01, from=0.0001, to=0.1, resolution=0.0001),
        step=list('spinbox', init=1, from=1, to=11, width=5),
        n=list('numentry',init=100000, width=7)
)

options(scipen=1)
tkexamp( SensSpec.demo(), tke.test3 )
# now increment step and watch the console

# Above example but converting it to plot

tempfun <- function(sens,spec,prev,step,n) {
	if(missing(sens) || missing(n)) return(invisible(NULL))
	tmp <- capture.output( SensSpec.demo(sens=sens,spec=spec,
				prev=prev, n=n, step=step) )
	par(cex=2.25)
	plot.new()
	tmp2 <- strheight(tmp)
	text(0, 1-cumsum(tmp2*1.5), tmp, family='mono', adj=0)
	title('Sensitivity and Specificity Example')
}

tkexamp( tempfun(), tke.test3, hscale=4, vscale=2 )

# an example using trellis graphics

tke.test4 <- list(
   alpha=list('slider', from=0,to=1,init=1,
                resolution=0.05),
   cex=list('spinbox',init=.8,from=.1,to=3,increment=.1,width=5),
   col=list('entry',init='#0080ff'),
   pch=list('spinbox',init=1, from=0,to=255,
                increment=1,width=5),
   fill=list('entry',init='transparent')
)

tempfun <- function(x,y,alpha,cex,col,pch,fill) {
	if(missing(alpha) || missing(cex)) {return()}
	trellis.par.set(plot.symbol=list(alpha=alpha, cex=cex, col=col,
					font=1,pch=pch,fill=fill))
	print(xyplot( y~x ))
}

require(lattice)

tkexamp( tempfun(x,y), list(tke.test4), plotloc='left')


# Two example using ggplot2

if( require(ggplot2) ) {

##  1
  tkexamp( qplot(cty,data=mpg, geom='histogram'),
      list(binwidth=list('slider',from=1,to=25)),
      print=TRUE)

##  2
  tmpfun <- function(bw=2){
	print(ggplot(mpg, aes(cty)) +
  		geom_histogram(binwidth = bw))
  }

  tkexamp( tmpfun, list(bw=list('slider',from=1, to=5)))


}

}
}
\keyword{dynamic}% at least one, from doc/KEYWORDS
\keyword{iplot}% __ONLY ONE__ keyword per line

