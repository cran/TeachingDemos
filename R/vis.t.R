"vis.t" <-
function(){

    if( !require(tcltk) ) stop('This function depends on the tcltk package')

    if(!exists('slider.env')) slider.env<<-new.env()

  df <- 1; assign('df',tclVar(df),envir=slider.env)
  sn <- 0; assign('sn',tclVar(sn),envir=slider.env)

  xmin <- -5; assign('xmin',tclVar(xmin),envir=slider.env)
  xmax <- 5; assign('xmax',tclVar(xmax),envir=slider.env)
  ymin <- 0; assign('ymin',tclVar(ymin),envir=slider.env)
  ymax <- round(dnorm(0,0,1),2); assign('ymax',tclVar(ymax),envir=slider.env)



  t.refresh <- function(...){

    df <- as.numeric(evalq(tclvalue(df), envir=slider.env))
    sn <- as.numeric(evalq(tclvalue(sn), envir=slider.env))

    xmin <- as.numeric(evalq(tclvalue(xmin), envir=slider.env))
    xmax <- as.numeric(evalq(tclvalue(xmax), envir=slider.env))
    ymin <- as.numeric(evalq(tclvalue(ymin), envir=slider.env))
    ymax <- as.numeric(evalq(tclvalue(ymax), envir=slider.env))

    xx <- seq(xmin,xmax, length=500)
    yyt <- dt(xx,df)

    if(sn){
      yyn <- dnorm(xx)
      plot(xx,yyn, lwd=3, col='skyblue', type='l',
           xlim=c(xmin,xmax), ylim=c(ymin,ymax),
           xlab='x', ylab='')
      lines(xx,yyt,lwd=2)
    } else {
      plot(xx,yyt,type='l', xlim=c(xmin,xmax), ylim=c(ymin,ymax),
           ylab='',xlab='x',lwd=2)
    }
  }


  m <- tktoplevel()
  tkwm.title(m,'Visualizing the t-Distribution')
  tkwm.geometry(m,'+0+0')

  # df
  tkpack(fr <- tkframe(m),side='top')
  tkpack(tklabel(fr, text='d.f.', width='5'),side='right')
  tkpack(sc <- tkscale(fr, command=t.refresh, from=1, to=50,
                       orient='horiz',
                       resolution=1, showvalue=T),
         side='left')
  assign('sc',sc,envir=slider.env)
  evalq(tkconfigure(sc, variable=df),envir=slider.env)

  # show normal
  tkpack(fr <- tkframe(m),side='top')
  tkpack(tklabel(fr, text='Show Normal Distribution', width='25'),side='right')
  tkpack(sc <- tkcheckbutton(fr, command=t.refresh),
         side='left')
  assign('sc',sc,envir=slider.env)
  evalq(tkconfigure(sc, variable=sn),envir=slider.env)


  # xmin
  tkpack(fr <- tkframe(m),side='top')
  tkpack(tklabel(fr, text='Xmin:', width=6), side='left')
  tkpack(e <- tkentry(fr,width=8), side='left')
  assign('e',e,envir=slider.env)
  evalq(tkconfigure(e, textvariable=xmin), envir=slider.env)

  # xmax
  tkpack(tklabel(fr, text='Xmax:', width=6), side='left')
  tkpack(e <- tkentry(fr,width=8), side='left')
  assign('e',e,envir=slider.env)
  evalq(tkconfigure(e, textvariable=xmax), envir=slider.env)

  # ymin
  tkpack(fr <- tkframe(m),side='top')
  tkpack(tklabel(fr, text='Ymin:', width=6), side='left')
  tkpack(e <- tkentry(fr,width=8), side='left')
  assign('e',e,envir=slider.env)
  evalq(tkconfigure(e, textvariable=ymin), envir=slider.env)

  # ymax
  tkpack(tklabel(fr, text='Ymax:', width=6), side='left')
  tkpack(e <- tkentry(fr,width=8), side='left')
  assign('e',e,envir=slider.env)
  evalq(tkconfigure(e, textvariable=ymax), envir=slider.env)


  tkpack(tkbutton(m, text="Refresh", command=t.refresh),side='left')

  tkpack(tkbutton(m, text="Exit", command=function()tkdestroy(m)),
         side='right')

}

