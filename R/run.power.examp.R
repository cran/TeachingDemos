run.power.examp.old <-
function(){
  slider( power.refresh,
         c('Sample Size','Standard Deviation','True Difference',
           'Alpha level'),
         c(1,0.25,-1,0.01),
         c(100,5,3,0.99),
         c(1,0.25,0.1,0.01),
         c(1,1,1,0.05),
         title="Power Demo") }

run.power.examp <- function(hscale=1.5, vscale=1.5, wait=FALSE) {

    if( !require(tkrplot) ) stop('This function depends on the tkrplot package being available')

    n <- tclVar()
    stdev <- tclVar()
    diff <- tclVar()
    alpha <- tclVar()
    xmin <- tclVar()
    xmax <- tclVar()

    tclvalue(n) <- 1
    tclvalue(stdev) <- 1
    tclvalue(diff) <- 1
    tclvalue(alpha) <- 0.05
    tclvalue(xmin) <- -2
    tclvalue(xmax) <- 4

    hsc <- tclVar()
    tclvalue(hsc) <- hscale
    vsc <- tclVar()
    tclvalue(vsc) <- vscale

    out <- numeric(1)

    replot <- function(...) {
        out <<- power.examp( as.numeric(tclvalue(n)),
                            as.numeric(tclvalue(stdev)),
                            as.numeric(tclvalue(diff)),
                            as.numeric(tclvalue(alpha)),
                            as.numeric(tclvalue(xmin)),
                            as.numeric(tclvalue(xmax)) )
    }

    tt <- tktoplevel()
    tkwm.title(tt, "Power Demo")

    img <- tkrplot(tt, replot, vscale=vscale, hscale=hscale)
    tkpack(img, side='left')

    tkpack(fr <- tkframe(tt), side='top', fill='x')
    tkpack(tklabel(fr, text="n: "), side='left')
    tkpack(tdspinner(fr, values=c(1,2,3,4,5,10,20,30,40,50),
                      width=5, textvariable=n,
                      command=function(...) tkrreplot(img,
                        hscale=as.numeric(tclvalue(hsc)),
                        vscale=as.numeric(tclvalue(vsc)) )
                      ), side='left')

    tkpack(fr <- tkframe(tt), side='top',fill='x')
    tkpack(tklabel(fr, text="Standard Deviation: "), side='left')
    tkpack(tkscale(fr, variable=stdev, orient='horizontal',
                   command=function(...) tkrreplot(img,
                     hscale=as.numeric(tclvalue(hsc)),
                     vscale=as.numeric(tclvalue(vsc)) ),
                   from=0.1, to=4, resolution=.05), side='right')

    tkpack(fr <- tkframe(tt), side='top',fill='x')
    tkpack(tklabel(fr, text="True Difference: "), side='left')
    tkpack(tkscale(fr, variable=diff, orient='horizontal',
                   command=function(...) tkrreplot(img,
                     hscale=as.numeric(tclvalue(hsc)),
                     vscale=as.numeric(tclvalue(vsc)) ),
                   from=0, to=4, resolution=.05), side='right')

    tkpack(fr <- tkframe(tt), side='top',fill='x')
    tkpack(tklabel(fr, text="alpha: "), side='left')
    tkpack(tkscale(fr, variable=alpha, orient='horizontal',
                   command=function(...) tkrreplot(img,
                     hscale=as.numeric(tclvalue(hsc)),
                     vscale=as.numeric(tclvalue(vsc)) ),
                   from=0.001, to=0.2, resolution=0.001), side='right')



    tkpack(tfr <- tkframe(tt), side='top', fill='x')
    tkpack(tklabel(tfr,text="x min: "), side='left')
    tkpack(tkentry(tfr,textvariable=xmin,width=6), side='left')
    tkpack(tklabel(tfr,text="      x max: "), side='left')
    tkpack(tkentry(tfr,textvariable=xmax,width=6), side='left')



    tkpack(tfr <- tkframe(tt), side='bottom', fill='x')
    tkpack(tklabel(tfr,text="Hscale: "), side='left')
    tkpack(tkentry(tfr,textvariable=hsc,width=6), side='left')
    tkpack(tklabel(tfr,text="      Vscale: "), side='left')
    tkpack(tkentry(tfr,textvariable=vsc,width=6), side='left')

    tkpack(tfr <- tkframe(tt), side='bottom', fill='x')
    tkpack(tkbutton(tfr, text="Refresh", command=function() tkrreplot(img,
                                         hscale=as.numeric(tclvalue(hsc)),
                                         vscale=as.numeric(tclvalue(vsc)) ) ),
           side='left',anchor='s')

    tkpack(tkbutton(tfr, text="Exit", command=function()tkdestroy(tt)),
           side='right',anchor='s')

    if(wait) {
        tkwait.window(tt)
        return(list( n=as.numeric(tclvalue(n)),
                     stdev=as.numeric(tclvalue(stdev)),
                     diff=as.numeric(tclvalue(diff)),
                     alpha=as.numeric(tclvalue(alpha)),
                     power=out
                    ))
    } else {
        return(invisible(NULL))
    }
}
