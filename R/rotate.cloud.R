"rotate.cloud" <-
function(x, ...){

  if(!require(tcltk)){stop('The tcltk package is needed')}
  if(!exists('slider.env')) slider.env <<-new.env()

  lab1 <- 'z'; assign('lab1', tclVar(lab1), envir=slider.env)
  lab2 <- 'y'; assign('lab2', tclVar(lab2), envir=slider.env)
  lab3 <- 'x'; assign('lab3', tclVar(lab3), envir=slider.env)

  val1 <-  40; assign('val1', tclVar(val1), envir=slider.env)
  val2 <-   0; assign('val2', tclVar(val2), envir=slider.env)
  val3 <- -60; assign('val3', tclVar(val3), envir=slider.env)

  cloud.options <- list(...)

  cloud.refresh <- function(...){

    lab1 <- evalq(tclvalue(lab1), envir=slider.env)
    lab2 <- evalq(tclvalue(lab2), envir=slider.env)
    lab3 <- evalq(tclvalue(lab3), envir=slider.env)

    val1 <- as.numeric(evalq(tclvalue(val1), envir=slider.env))
    val2 <- as.numeric(evalq(tclvalue(val2), envir=slider.env))
    val3 <- as.numeric(evalq(tclvalue(val3), envir=slider.env))


    sl <- list(val1,val2,val3)
    names(sl) <- c(lab1,lab2,lab3)

    cloud.options$x <- x
    cloud.options$screen <- sl

    print( do.call('cloud',cloud.options) )


  }

  m <- tktoplevel()
  tkwm.title(m,'Rotate Cloud plot')
  tkwm.geometry(m,'+0+0')

  # one
  tkpack(fr <- tkframe(m), side='top')
  tkpack(e <- tkentry(fr, width=2), side='left')
  tkpack(sc <- tkscale(fr, command=cloud.refresh, from=-180, to=180,
                       orient='horiz',
                       resolution=1, showvalue=T),
         side='left')

  assign('sc',sc,envir=slider.env)
  evalq(tkconfigure(sc, variable=val1), envir=slider.env)
  assign('e',e,envir=slider.env)
  evalq(tkconfigure(e,textvariable=lab1), envir=slider.env)

  # two
  tkpack(fr <- tkframe(m), side='top')
  tkpack(e <- tkentry(fr, width=2), side='left')
  tkpack(sc <- tkscale(fr, command=cloud.refresh, from=-180, to=180,
                       orient='horiz',
                       resolution=1, showvalue=T),
         side='left')

  assign('sc',sc,envir=slider.env)
  evalq(tkconfigure(sc, variable=val2), envir=slider.env)
  assign('e',e,envir=slider.env)
  evalq(tkconfigure(e,textvariable=lab2), envir=slider.env)

  # three
  tkpack(fr <- tkframe(m), side='top')
  tkpack(e <- tkentry(fr, width=2), side='left')
  tkpack(sc <- tkscale(fr, command=cloud.refresh, from=-180, to=180,
                       orient='horiz',
                       resolution=1, showvalue=T),
         side='left')

  assign('sc',sc,envir=slider.env)
  evalq(tkconfigure(sc, variable=val3), envir=slider.env)
  assign('e',e,envir=slider.env)
  evalq(tkconfigure(e,textvariable=lab3), envir=slider.env)

  tkpack(tkbutton(m, text="Refresh", command=cloud.refresh),side='left')

  tkpack(tkbutton(m, text="Exit", command=function()tkdestroy(m)),
         side='right')


}

