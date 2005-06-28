"vis.boxcoxu" <-
function(lambda = sample( c(-1, -0.5, 0, 1/3, 1/2, 1, 2), 1)) {
  y <- rnorm(1000, 7, 2)
  if( min(y) <= 0 ) y <- y - min(y)+0.05
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
