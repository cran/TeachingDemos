"vis.gamma" <-
function(){

  if(!exists('slider.env')) slider.env<<-new.env()
  if(!require(tcltk)) {
      stop('This function needs the tcltk package')
  }

  shape <- 1; assign('shape',tclVar(shape),env=slider.env)
  rate  <- 1; assign('rate',tclVar(rate),env=slider.env)
  scale <- 1; assign('scale',tclVar(scale),env=slider.env)
  mean  <- 1; assign('mean', tclVar(mean), env=slider.env)
  sd    <- 1; assign('sd',tclVar(sd), env=slider.env)

  se <- 0; assign('se', tclVar(se), env=slider.env)
  sc2 <- 0; assign('sc2', tclVar(sc2), env=slider.env)
  sg <- 1; assign('sg', tclVar(sg), env=slider.env)

  xmin <- 0; assign('xmin',tclVar(xmin),env=slider.env)
  xmax <- 5; assign('xmax',tclVar(xmax),env=slider.env)
  ymin <- 0; assign('ymin',tclVar(ymin),env=slider.env)
  ymax <- 1; assign('ymax',tclVar(ymax),env=slider.env)

  old.shape <- shape
  old.rate  <- rate
  old.scale <- scale
  old.mean  <- mean
  old.sd    <- sd


  gamma.refresh <- function(...){

    shape <- as.numeric(evalq(tclvalue(shape), env=slider.env))
    rate  <- as.numeric(evalq(tclvalue(rate), env=slider.env))
    scale <- as.numeric(evalq(tclvalue(scale), env=slider.env))
    mean  <- as.numeric(evalq(tclvalue(mean), env=slider.env))
    sd    <- as.numeric(evalq(tclvalue(sd), env=slider.env))

    if ( shape != old.shape ) {

      mean <- shape * scale
      sd <- round( sqrt(shape)*scale, 6 );

      try(eval(parse(text=paste("tclvalue(mean)<-",
                       mean,sep="")),env=slider.env));
      try(eval(parse(text=paste("tclvalue(sd)<-",
                       sd,sep="")),env=slider.env));
      old.shape <<- shape; old.mean <<- mean; old.sd <<- sd

    }

    if ( rate != old.rate ) {

      scale <- round(1/rate, 6)
      mean <- shape * scale
      sd <- round( sqrt(shape)*scale, 6 );

      try(eval(parse(text=paste("tclvalue(scale)<-",
                       scale,sep="")),env=slider.env));
      try(eval(parse(text=paste("tclvalue(mean)<-",
                       mean,sep="")),env=slider.env));
      try(eval(parse(text=paste("tclvalue(sd)<-",
                       sd,sep="")),env=slider.env));
      old.rate <<- rate; old.scale <<- scale;
      old.mean <<- mean; old.sd <<- sd

    }

    if ( scale != old.scale ) {

      rate <- round(1/scale, 6)
      mean <- shape * scale
      sd <- round( sqrt(shape)*scale, 6 );

      try(eval(parse(text=paste("tclvalue(rate)<-",
                       rate,sep="")),env=slider.env));
      try(eval(parse(text=paste("tclvalue(mean)<-",
                       mean,sep="")),env=slider.env));
      try(eval(parse(text=paste("tclvalue(sd)<-",
                       sd,sep="")),env=slider.env));
      old.rate <<- rate; old.scale <<- scale;
      old.mean <<- mean; old.sd <<- sd

    }

    if ( mean != old.mean ) {

      shape <- round( (mean/sd)^2, 6 )
      scale <- round( mean/shape, 6 )
      rate <- round(1/scale, 6)

      try(eval(parse(text=paste("tclvalue(rate)<-",
                       rate,sep="")),env=slider.env));
      try(eval(parse(text=paste("tclvalue(shape)<-",
                       shape,sep="")),env=slider.env));
      try(eval(parse(text=paste("tclvalue(scale)<-",
                       scale,sep="")),env=slider.env));
      old.shape <<- shape; old.rate <<- rate; old.scale <<- scale;
      old.mean <<- mean; old.sd <<- sd

    }


    if ( sd != old.sd ) {

      shape <- round( (mean/sd)^2, 6 )
      scale <- round( mean/shape, 6 )
      rate <- round(1/scale, 6)

      try(eval(parse(text=paste("tclvalue(rate)<-",
                       rate,sep="")),env=slider.env));
      try(eval(parse(text=paste("tclvalue(shape)<-",
                       shape,sep="")),env=slider.env));
      try(eval(parse(text=paste("tclvalue(scale)<-",
                       scale,sep="")),env=slider.env));
      old.shape <<- shape; old.rate <<- rate; old.scale <<- scale;
      old.mean <<- mean; old.sd <<- sd

    }


    se <- as.numeric(evalq(tclvalue(se), env=slider.env))
    sc2 <- as.numeric(evalq(tclvalue(sc2), env=slider.env))
    sg <- as.numeric(evalq(tclvalue(sg), env=slider.env))

    xmin <- as.numeric(evalq(tclvalue(xmin), env=slider.env))
    xmax <- as.numeric(evalq(tclvalue(xmax), env=slider.env))
    ymin <- as.numeric(evalq(tclvalue(ymin), env=slider.env))
    ymax <- as.numeric(evalq(tclvalue(ymax), env=slider.env))

    xx <- seq(xmin,xmax, length=500)

    plot(xx,xx, xlim=c(xmin,xmax),ylim=c(ymin,ymax),
         xlab='x', ylab='y',type='n')

    if(se) {
      yye <- dexp(xx,1/mean)
      lines(xx,yye, lwd=3, col='green')
      lines(c(mean,mean),c(ymin,dexp(mean,1/mean)), lty=2, col='green')
      lines(c(mean,mean*2), dexp(mean*2, 1/mean)*c(1,1), lty=2, col='green')
    }

    if(sc2) {
      yyc <- dchisq(xx,mean)
      lines(xx,yyc, lwd=3, col='blue')
      lines(c(mean,mean),c(ymin,dchisq(mean,mean)), lty=2, col='blue')
      lines(c(mean,mean+sqrt(2*mean)), dchisq(mean+sqrt(2*mean), mean)*c(1,1),
            lty=2, col='blue')
    }

    if(sg) {
      yyg <- dgamma(xx,shape,rate)
      lines(xx,yyg, lwd=2)
      lines(c(mean,mean),c(ymin,dgamma(mean,shape,rate)), lty=2)
      lines(c(mean,mean+sd), dgamma(mean+sd, shape, rate)*c(1,1),
            lty=2)
    }

  }


  m <- tktoplevel()
  tkwm.title(m,'Visualizing the Gamma Distribution')
  tkwm.geometry(m,'+0+0')

  # shape
  tkpack(fr <- tkframe(m),side='top')
  tkpack(tklabel(fr, text='Shape', width='10'),side='right')
  tkpack(sc <- tkscale(fr, command=gamma.refresh, from=0.1, to=10,
                       orient='horiz',
                       resolution=0.1, showvalue=T),
         side='left')
  assign('sc',sc,env=slider.env)
  evalq(tkconfigure(sc, variable=shape),env=slider.env)

  # rate
  tkpack(fr <- tkframe(m),side='top')
  tkpack(tklabel(fr, text='Rate', width='10'),side='right')
  tkpack(sc <- tkscale(fr, command=gamma.refresh, from=0.1, to=10,
                       orient='horiz',
                       resolution=0.1, showvalue=T),
         side='left')
  assign('sc',sc,env=slider.env)
  evalq(tkconfigure(sc, variable=rate),env=slider.env)

  # scale
  tkpack(fr <- tkframe(m),side='top')
  tkpack(tklabel(fr, text='Scale', width='10'),side='right')
  tkpack(sc <- tkscale(fr, command=gamma.refresh, from=0.1, to=10,
                       orient='horiz',
                       resolution=0.1, showvalue=T),
         side='left')
  assign('sc',sc,env=slider.env)
  evalq(tkconfigure(sc, variable=scale),env=slider.env)

  # mean
  tkpack(fr <- tkframe(m),side='top')
  tkpack(tklabel(fr, text='Mean', width='10'),side='right')
  tkpack(sc <- tkscale(fr, command=gamma.refresh, from=0.1, to=100,
                       orient='horiz',
                       resolution=0.1, showvalue=T),
         side='left')
  assign('sc',sc,env=slider.env)
  evalq(tkconfigure(sc, variable=mean),env=slider.env)

  # sd
  tkpack(fr <- tkframe(m),side='top')
  tkpack(tklabel(fr, text='S.D.', width='10'),side='right')
  tkpack(sc <- tkscale(fr, command=gamma.refresh, from=0.1, to=40,
                       orient='horiz',
                       resolution=0.1, showvalue=T),
         side='left')
  assign('sc',sc,env=slider.env)
  evalq(tkconfigure(sc, variable=sd),env=slider.env)


  # show exponential
  tkpack(fr <- tkframe(m),side='top')
  tkpack(sc <- tkcheckbutton(fr, command=gamma.refresh),
         side='left')
  tkpack(tklabel(fr, text='Show Exponential Distribution', width='25'),
         side='left')
  assign('sc',sc,env=slider.env)
  evalq(tkconfigure(sc, variable=se),env=slider.env)

  # show chisquared
  tkpack(fr <- tkframe(m),side='top')
  tkpack(sc <- tkcheckbutton(fr, command=gamma.refresh),
         side='left')
  tkpack(tklabel(fr, text='Show Chi-squared Distribution', width='25'),
         side='left')
  assign('sc',sc,env=slider.env)
  evalq(tkconfigure(sc, variable=sc2),env=slider.env)

  # show gamma
  tkpack(fr <- tkframe(m),side='top')
  tkpack(sc <- tkcheckbutton(fr, command=gamma.refresh),
         side='left')
  tkpack(tklabel(fr, text='Show Gamma Distribution', width='25'),
         side='left')
  assign('sc',sc,env=slider.env)
  evalq(tkconfigure(sc, variable=sg),env=slider.env)


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


  tkpack(tkbutton(m, text="Refresh", command=gamma.refresh),side='left')

  tkpack(tkbutton(m, text="Exit", command=function()tkdestroy(m)),
         side='right')

}

