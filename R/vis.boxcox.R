"vis.boxcox" <-
function(lambda = sample( c(-1,-0.5,0,1/3,1/2,1,2), 1) ) {


  x <- runif(100, 1, 10)
  y <- 3+2*x + rnorm(100)
  if ( min(y) <= 0 ) y <- y - min(y) + 0.05
  if (lambda==0) {
    y <- exp(y)
  } else {
    y <- y^(1/lambda)
  }

  if(!exists('slider.env')) slider.env <<-new.env()
  library(tcltk)

  lam <- 1 ; assign('lam',tclVar(lam), env=slider.env)

  bc.refresh <- function(...){
    lam <- as.numeric(evalq(tclvalue(lam), env=slider.env))

    old.par <- par(mfcol=c(2,2))
    on.exit(par(old.par))

    tmp1 <- lm(y~x)
    tmp2 <- lm(bct(y,lam)~x)
    plot(x,y,main="Raw Data")
    abline(tmp1)
    scatter.smooth(x,resid(tmp1),main="Raw Residuals",ylab='Residuals')
    abline(h=0, lty=2 )
    plot(x,bct(y,lam), main=bquote( lambda == .(lam) ),ylab="Transformed y" )
    abline(tmp2)
    scatter.smooth(x,resid(tmp2), main=bquote( lambda == .(lam) ),
                   ylab="Residuals")
    abline(h=0, lty=2)
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

