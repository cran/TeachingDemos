vis.boxcoxu.old <-
function(lambda = sample( c(-1, -0.5, 0, 1/3, 1/2, 1, 2), 1)) {

  if( !require(tkrplot) ) stop('This function depends on the tkrplot package being available')


  y <- rnorm(1000, 7, 2)
  if( min(y) <= 0 ) y <- y - min(y)+0.05
  if (lambda==0) {
    y <- exp(y)
  } else {
    y <- y^(1/lambda)
  }

    if(!exists('slider.env')) slider.env <<-new.env()
  #library(tcltk)

  lam <- 1 ; assign('lam',tclVar(lam), env=slider.env)

  bc.refresh <- function(...){
    lam <- as.numeric(evalq(tclvalue(lam), env=slider.env))

    old.par <- par(mfcol=c(2,2))
    on.exit(par(old.par))

    ty <- bct(y,lam)
    hist(y, prob=T, xlab='x', main='Histogram of x')
    xx <- seq(min(y),max(y), length=250)
    lines(xx, dnorm( xx, mean(y), sqrt(var(y)) ))
    qqnorm(y, xlab='x')
    qqline(y)

    hist(ty, prob=T, xlab='Transformed x', main = 'Histogram of Transformed x')
    xx <- seq(min(ty),max(ty), length=250)
    lines(xx,dnorm(xx, mean(ty), sqrt(var(ty)) ) )

    qqnorm(ty, xlab='Transformed x')
    qqline(ty)
  }

  m <- tktoplevel()
  tkwm.title(m, 'Box Cox Transform')
  tkwm.geometry(m,'+0+0')

  tkpack(fr <- tkframe(m), side='top')
  tkpack(tklabel(fr, text='lambda', width='10'), side='right')
  tkpack(sc <- tkscale(fr, command=bc.refresh, from=-2, to=3, orient='horiz',
                       resolution=0.1, showvalue=T),
         side='left')
  assign('sc',sc,env=slider.env)
  evalq(tkconfigure(sc, variable=lam), env=slider.env)

  tkpack(tkbutton(m, text="Refresh", command=bc.refresh), side='left')
  tkpack(tkbutton(m, text="Exit", command=function()tkdestroy(m)),
         side='right')

}




vis.boxcoxu <- function(lambda = sample( c(-1,-0.5,0,1/3,1/2,1,2), 1),
                        y, xlab=deparse(substitute(y)),
                        hscale=1.5, vscale=1.5, wait=FALSE) {

  if( missing(y) ) {
    if(missing(xlab)) xlab <- 'y'
    y <- rnorm(1000, 7, 2)
    if( min(y) <= 0 ) y <- y - min(y) + 0.05
    if(lambda==0) {
      y <- exp(y)
    } else {
      y <- y^(1/lambda)
    }
  }


  lam <- tclVar()
  tclvalue(lam) <- 1
  hsc <- tclVar()
  tclvalue(hsc) <- hscale
  vsc <- tclVar()
  tclvalue(vsc) <- hscale

  replot <- function(...) {
    tmp.l <- as.numeric(tclvalue(lam))

    par(mfcol=c(2,2))

    ty <- bct(y,tmp.l)
    hist(y, prob=TRUE, xlab=xlab, main = paste('Histogram of',xlab))
    xx <- seq(min(y),max(y), length=250)
    lines(xx, dnorm(xx, mean(y), sd(y)) )
    qqnorm(y, xlab=xlab)
    qqline(y)

    hist(ty, prob=TRUE, xlab=paste("Transformed",xlab),
         main=paste("Histogram of Transformed",xlab))
    xx <- seq(min(ty),max(ty), length=250)
    lines(xx,dnorm(xx, mean(ty), sd(ty)))
    qqnorm(ty, xlab=paste("Transformed",xlab))
    qqline(ty)
  }

  tt <- tktoplevel()
  tkwm.title(tt, "Box Cox Demo")

  img <- tkrplot(tt, replot, vscale=vscale, hscale=hscale)
  tkpack(img, side='top')

  tkpack(fr <- tkframe(tt), side='top')
  tkpack(tklabel(fr, text='lambda: '), side='left', anchor='s')
  tkpack(tkscale(fr, variable=lam, orient='horizontal',
                 command=function(...) tkrreplot(img,
                   hscale=as.numeric(tclvalue(hsc)),
                   vscale=as.numeric(tclvalue(vsc)) ),
                 from=-2, to=4, resolution=.05), side='right')

  tkpack(tfr <- tkframe(tt), side='bottom', fill='x')
  tkpack(tkbutton(tfr, text="Refresh", command=function() tkrreplot(img,
                                         hscale=as.numeric(tclvalue(hsc)),
                                         vscale=as.numeric(tclvalue(vsc)) ) ),
                  side='left',anchor='s')

  tkpack(tkbutton(tfr, text="Exit", command=function()tkdestroy(tt)),
             side='right',anchor='s')

  tkpack(tfr <- tkframe(tt), side='bottom', fill='x')
  tkpack(tklabel(tfr,text="Hscale: "), side='left')
  tkpack(tkentry(tfr,textvariable=hsc,width=6), side='left')
  tkpack(tklabel(tfr,text="      Vscale: "), side='left')
  tkpack(tkentry(tfr,textvariable=vsc,width=6), side='left')

  if(wait) {
    tkwait.window(tt)
    return( list(lambda = as.numeric(tclvalue(lam)),
                 y = y,
                 ty = bct(y,as.numeric(tclvalue(lam))) ) )
  } else {
    return(invisible(NULL))
  }
}

