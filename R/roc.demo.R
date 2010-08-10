"roc.demo" <-
function(x=rnorm(25,10,1), y=rnorm(25,11,1.5) ){

    if(!require(tcltk)){stop('The tcltk package is needed')}
    if(!exists('slider.env')) slider.env <<- new.env()

  range.min <- min(x,y) - 0.1 * diff(range(x,y))
  range.max <- max(x,y) + 0.1 * diff(range(x,y))

  cutoff <- range.max; assign('cutoff',tclVar(cutoff), env=slider.env)

  .sens <-c(0,1)
  .spec <-c(0,1)

  dx <- density(x)
  dy <- density(y)

  roc.refresh <- function(...){
    cutoff <- as.numeric(evalq(tclvalue(cutoff), env=slider.env))

    old.par <- par(no.readonly=T)
    on.exit(par(old.par))

    sens <- mean( y > cutoff )
    spec <- mean( x > cutoff )

    .sens <<- c(.sens, sens)
    .spec <<- c(.spec, spec)


    par(mar=c(5,4,0,1)+.1)
    layout( matrix(c(1,2), ncol=1), heights=c(2,1))

    op <- par(pty="s")
    plot(.spec,.sens, xlab="1-Specificity",ylab="Sensitivity",
         xlim=c(0,1),ylim=c(0,1))
    par(pty="m")

    tmp <- chull(c(1,.spec),c(0,.sens))
    lines(c(NA,.spec)[tmp],c(NA,.sens)[tmp])
    points(spec,sens, col='red',pch=16)

    specdiff <- diff( c(NA,.spec)[tmp] )
    specdiff <- specdiff[!is.na(specdiff)]
    sensmean <- (c(c(NA,.sens)[tmp][-1],NA) + c(NA,.sens)[tmp])/2
    sensmean <- sensmean[!is.na(sensmean)]
    auc <- sum( specdiff*sensmean )
    text(1,0.1, paste("Area Under Curve =", round(auc,3)), cex=1.7, adj=1)

    d <- (1-.sens)^2 + (.spec)^2
    dd <- which.min(d)
    lines(c(0,.spec[dd]),c(1,.sens[dd]), col='purple')


    plot( dx$x, dx$y, type='l', col='red', xlim=c(range.min,range.max),
         xlab=paste("Sensitivity = ",round(sens,3),", Specificity = ",round(1-spec,3),sep=''),
         ylab="Densities",ylim=c(0,max(dx$y,dy$y)))
    if(any(x <= cutoff))
      rug(x[x<=cutoff], col='red', ticksize=.3)
    if(any(x > cutoff))
      rug(x[x>cutoff], col='red', ticksize=.3, side=3)

    lines( dy$x, dy$y, col='blue')
    if(any(y<=cutoff))
      rug(y[y<=cutoff], col='blue',ticksize=.3)
    if(any(y>cutoff))
      rug(y[y>cutoff], col='blue',ticksize=.3, side=3)

    abline(v=cutoff, col='green')
  }

  m <- tktoplevel()
  tkwm.title(m,'ROC curve demo')
  tkwm.geometry(m, '+0+0')

  # cutoff
  tkpack(fr <- tkframe(m), side='top')
  tkpack(tklabel(fr, text='cutoff', width='10'), side='right')

  tkpack(sc <- tkscale(fr, command=roc.refresh, from=range.min,
                       to=range.max, orient='horiz',
                       resolution = (range.max-range.min)/100,
                       showvalue=T),
         side='left')

  assign('sc',sc, env=slider.env)
  evalq(tkconfigure(sc, variable=cutoff), env=slider.env)



  tkpack(tkbutton(m, text="Refresh", command=roc.refresh), side='left')
  tkpack(tkbutton(m, text="Exit", command=function()tkdestroy(m)),
         side='right')

}

