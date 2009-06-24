\name{TeachingDemos-package}
\alias{TeachingDemos-package}
\alias{TeachingDemos}
\docType{package}
\title{
  Various functions for demonstration and learning.
}
\description{
  This package provides various demonstrations that can be used in
  classes or by individuals to better learn statistical concepts and
  usage of R.  Various utility functions are also included}
\details{
  \tabular{ll}{
    Package: \tab TeachingDemos\cr
    Type: \tab Package\cr
    Version \tab 2.4\cr
    Date: \tab 2008-11-13\cr
    License: \tab Artistic-2.0\cr
  }
  Demonstration functions in this package include:

  \tabular{ll}{
    ci.examp, run.ci.examp \tab Confidence Interval Examples \cr
    clt.examp \tab Central Limit Theorem Example\cr
    dice, plot.dice \tab Roll and Plot dice (possibly loaded)\cr
    faces, faces2 \tab Chernoff face plots\cr
    fagan.plot \tab Fagan plot for screening designs\cr
    lattice.demo \tab The 3d slicing idea behind lattice/trellis
    graphics\cr
    loess.demo \tab Interactive demo to show ideas of loess smooths\cr
    mle.demo \tab Interactive demo of Maximum Likelihood Estimation\cr
    plot.rgl.coin, plot.rgl.die \tab Animate flipping a coin or rolling
    a die\cr
    power.examp \tab Demonstrate concepts of Power.\cr
    put.points.demo \tab Add/move points on a plot and see the effect on
    correlation and regression.\cr
    roc.demo \tab Interactive demo of ROC curves.\cr
    rotate.cloud \tab Interactively rotate 3d plots.\cr
    run.cor.examp \tab Show plots representing different
    correlations.\cr
    run.hist.demo \tab Interactively change parameters for
    histograms.\cr
    SensSpec.demo \tab Show relationship between Sensitivity,
    Specificity, Prevalence and PPV and NPV.\cr
    TkApprox \tab Interactive linear interpolations of data.\cr
    tkBrush \tab Brush points in a scatterplot matrix.\cr
    TkSpline \tab Interactive spline interpolations of data.\cr
    tree.demo \tab Interactively Recursive partition data (create
    trees).\cr
    vis.binom \tab Plot various probability distributions and
    interactively change parameters.\cr
    vis.boxcox \tab Interactively change lambda for Box Cox
    Transforms.\cr
    z.test \tab Z-test similar to t.test for students who have not
    learned t tests yet.\cr
    Pvalue.norm.sim \tab \cr
    Pvalue.binom.sim \tab Simulate P-values to see how they are
    distributed.\cr
    run.Pvalue.norm.sim \tab GUI for above. \cr
    run.Pvalue.binom.sim \tab \cr
  }

  Utility functions include:
  \tabular{ll}{
    bct \tab Box-Cox Transforms.\cr
    char2seed \tab set or create the random number seed using a
    character string\cr
    clipplot \tab clip a plot to a rectangular region within the plot\cr
    col2grey \tab convert colors to greyscale\cr
    cnvrt.coords \tab Convert between the different coordinate
    systems\cr
    dynIdentify \tab Scatterplot with point labels that can be dragged
    to a new position \cr
    TkIdentify \tab Scatterplot with lables that can be dragged to new
    positions \cr
    gp.plot gp.splot \tab send commonds to gnuplot\cr
    hpd \tab Highest Posterior Density intervals\cr
    my.symbols \tab Create plots using user defined symbols.\cr
    panel.my.symbols \tab Create lattice plots using user defined symbols.\cr
    plot2script \tab Create a script file that recreates the current
    plot.\cr
    squishplot \tab Set the margins so that a plot has a specific aspect
    ratio without large whitespace inside.\cr
    spread.labs \tab Spread out coordinates so that labels do not overlap.\cr
    subplot \tab create a plot inside of an existing plot.\cr
    tkexamp \tab create plots that can have parameters adjusted
    interactively.\cr
    triplot \tab Trilinear plot for 3 proportions.\cr
    txtStart/etxtStart \tab Save commands and output to a text file
  (possibly for post processing with enscript).\cr
    zoomplot \tab recreate the current plot with different x/y limits
    (zoom in out).\cr
    %<% %<=% \tab Transtitive inequalities.\cr
  }

}
\author{
  Greg Snow \email{greg.snow@imail.org}
}
\keyword{package}
\keyword{aplot}
\keyword{iplot}
\keyword{dynamic}
\seealso{ The tkrplot package }
\examples{
ci.examp()

clt.examp()
clt.examp(5)

plot.dice( expand.grid(1:6,1:6), layout=c(6,6) )

faces(rbind(1:3,5:3,3:5,5:7))

plot(1:10, 1:10)
my.symbols( 1:10, 1:10, ms.polygram, n=1:10, inches=0.3 )

x <- seq(1,100)
y <- rnorm(100)
plot(x,y, type='b', col='blue')
clipplot( lines(x,y, type='b', col='red'), ylim=c(par('usr')[3],0))

power.examp()
power.examp(n=25)
power.examp(alpha=0.1)


}
