"rotate.wireframe" <-
function(x, ...){

  if(!exists('slider.env')) slider.env <<-new.env()
  #library(tcltk)

  lab1 <- 'z'; assign('lab1', tclVar(lab1), env=slider.env)
  lab2 <- 'y'; assign('lab2', tclVar(lab2), env=slider.env)
  lab3 <- 'x'; assign('lab3', tclVar(lab3), env=slider.env)

  val1 <-  40; assign('val1', tclVar(val1), env=slider.env)
  val2 <-   0; assign('val2', tclVar(val2), env=slider.env)
  val3 <- -60; assign('val3', tclVar(val3), env=slider.env)

  wire.options <- list(...)

  wire.refresh <- function(...){

    lab1 <- evalq(tclvalue(lab1), env=slider.env)
    lab2 <- evalq(tclvalue(lab2), env=slider.env)
    lab3 <- evalq(tclvalue(lab3), env=slider.env)

    val1 <- as.numeric(evalq(tclvalue(val1), env=slider.env))
    val2 <- as.numeric(evalq(tclvalue(val2), env=slider.env))
    val3 <- as.numeric(evalq(tclvalue(val3), env=slider.env))


    sl <- list(val1,val2,val3)
    names(sl) <- c(lab1,lab2,lab3)

    wire.options$x <- x
    wire.options$screen <- sl

    print( do.call('wireframe',wire.options) )


  }

  m <- tktoplevel()
  tkwm.title(m,'Rotate Wireframe plot')
  tkwm.geometry(m,'+0+0')

  # one
  tkpack(fr <- tkframe(m), side='top')
  tkpack(e <- tkentry(fr, width=2), side='left')
  tkpack(sc <- tkscale(fr, command=wire.refresh, from=-180, to=180,
                       orient='horiz',
                       resolution=1, showvalue=T),
         side='left')

  assign('sc',sc,env=slider.env)
  evalq(tkconfigure(sc, variable=val1), env=slider.env)
  assign('e',e,env=slider.env)
  evalq(tkconfigure(e,textvariable=lab1), env=slider.env)

  # two
  tkpack(fr <- tkframe(m), side='top')
  tkpack(e <- tkentry(fr, width=2), side='left')
  tkpack(sc <- tkscale(fr, command=wire.refresh, from=-180, to=180,
                       orient='horiz',
                       resolution=1, showvalue=T),
         side='left')

  assign('sc',sc,env=slider.env)
  evalq(tkconfigure(sc, variable=val2), env=slider.env)
  assign('e',e,env=slider.env)
  evalq(tkconfigure(e,textvariable=lab2), env=slider.env)

  # three
  tkpack(fr <- tkframe(m), side='top')
  tkpack(e <- tkentry(fr, width=2), side='left')
  tkpack(sc <- tkscale(fr, command=wire.refresh, from=-180, to=180,
                       orient='horiz',
                       resolution=1, showvalue=T),
         side='left')

  assign('sc',sc,env=slider.env)
  evalq(tkconfigure(sc, variable=val3), env=slider.env)
  assign('e',e,env=slider.env)
  evalq(tkconfigure(e,textvariable=lab3), env=slider.env)

  tkpack(tkbutton(m, text="Refresh", command=wire.refresh),side='left')

  tkpack(tkbutton(m, text="Exit", command=function()tkdestroy(m)),
         side='right')


}

