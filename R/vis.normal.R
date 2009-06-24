"vis.normal" <-
function(){

    if( !require(tcltk) ) stop('This function depends on the tcltk package')


  if(!exists('slider.env')) slider.env<<-new.env()
  #library(tcltk)

  mu <- 0; assign('mu',tclVar(mu),env=slider.env)
  sd <- 1; assign('sd',tclVar(sd),env=slider.env)
  s2 <- 1; assign('s2',tclVar(s2),env=slider.env)
  xmin <- -5; assign('xmin',tclVar(xmin),env=slider.env)
  xmax <- 5; assign('xmax',tclVar(xmax),env=slider.env)
  ymin <- 0; assign('ymin',tclVar(ymin),env=slider.env)
  ymax <- round(dnorm(0,0,.5),2); assign('ymax',tclVar(ymax),env=slider.env)

  sd.old <- sd
  s2.old <- s2

  norm.refresh <- function(...){

    mu <- as.numeric(evalq(tclvalue(mu), env=slider.env))
    sd <- as.numeric(evalq(tclvalue(sd), env=slider.env))
    s2 <- as.numeric(evalq(tclvalue(s2), env=slider.env))

    if(sd != sd.old) {
      s2 <- round(sd^2,5); # assign('s2',tclVar(s2),env=slider.env)
      try(eval(parse(text=paste("tclvalue(s2)<-",
                       s2,sep="")),env=slider.env));
      sd.old <<- sd; s2.old <<- s2
    }

    if(s2 != s2.old) {
      s2 <- as.numeric(evalq(tclvalue(s2), env=slider.env))
      sd <- round(sqrt(s2),5); # assign('sd',tclVar('sd'), env=slider.env)
      try(eval(parse(text=paste("tclvalue(sd)<-",
                       sd,sep="")),env=slider.env));
      sd.old <<- sd; s2.old <<- s2
    }

    xmin <- as.numeric(evalq(tclvalue(xmin), env=slider.env))
    xmax <- as.numeric(evalq(tclvalue(xmax), env=slider.env))
    ymin <- as.numeric(evalq(tclvalue(ymin), env=slider.env))
    ymax <- as.numeric(evalq(tclvalue(ymax), env=slider.env))

    xx <- seq(xmin,xmax, length=500)
    yy <- dnorm(xx,mu,sd)
    plot(xx,yy,type='l', xlim=c(xmin,xmax), ylim=c(ymin,ymax),
         ylab='',xlab='x')
    lines(c(mu,mu),c(par('usr')[3],dnorm(0,0,sd)), lty=2, col='blue')
    lines(c(mu,mu+sd), dnorm(sd,0,sd)*c(1,1), lty=2, col='blue')

  }



  m <- tktoplevel()
  tkwm.title(m,'Visualizing the Normal Distribution')
  tkwm.geometry(m,'+0+0')

  # mean
  tkpack(fr <- tkframe(m),side='top')
  tkpack(tklabel(fr, text='Mean', width='20'),side='right')
  tkpack(sc <- tkscale(fr, command=norm.refresh, from=-3, to=3,
                       orient='horiz',
                       resolution=0.1, showvalue=T),
         side='left')
  assign('sc',sc,env=slider.env)
  evalq(tkconfigure(sc, variable=mu),env=slider.env)

  # sd
  tkpack(fr <- tkframe(m),side='top')
  tkpack(tklabel(fr, text='Standard Deviation', width='20'),side='right')
  tkpack(sc <- tkscale(fr, command=norm.refresh, from=.5, to=3,
                       orient='horiz',
                       resolution=0.1, showvalue=T),
         side='left')
  assign('sc',sc,env=slider.env)
  evalq(tkconfigure(sc, variable=sd),env=slider.env)

  # variance
  tkpack(fr <- tkframe(m),side='top')
  tkpack(tklabel(fr, text='Variance', width='20'),side='right')
  tkpack(sc <- tkscale(fr, command=norm.refresh, from=.25, to=9,
                       orient='horiz',
                       resolution=0.1, showvalue=T),
         side='left')
  assign('sc',sc,env=slider.env)
  evalq(tkconfigure(sc, variable=s2),env=slider.env)


  # xmin
  tkpack(fr <- tkframe(m),side='top')
  tkpack(tklabel(fr, text='Xmin:', width=6), side='left')
  tkpack(e <- tkentry(fr,width=8), side='left')
  assign('e',e,env=slider.env)
  evalq(tkconfigure(e, textvariable=xmin), env=slider.env)

  # xmax
  tkpack(tklabel(fr, text='Xmax:', width=6), side='left')
  tkpack(e <- tkentry(fr,width=8), side='left')
  assign('e',e,env=slider.env)
  evalq(tkconfigure(e, textvariable=xmax), env=slider.env)

  # ymin
  tkpack(fr <- tkframe(m),side='top')
  tkpack(tklabel(fr, text='Ymin:', width=6), side='left')
  tkpack(e <- tkentry(fr,width=8), side='left')
  assign('e',e,env=slider.env)
  evalq(tkconfigure(e, textvariable=ymin), env=slider.env)

  # ymax
  tkpack(tklabel(fr, text='Ymax:', width=6), side='left')
  tkpack(e <- tkentry(fr,width=8), side='left')
  assign('e',e,env=slider.env)
  evalq(tkconfigure(e, textvariable=ymax), env=slider.env)


  tkpack(tkbutton(m, text="Refresh", command=norm.refresh),side='left')

  tkpack(tkbutton(m, text="Exit", command=function()tkdestroy(m)),
         side='right')

}

