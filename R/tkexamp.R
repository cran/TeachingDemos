tkexamp <- function(FUN, param.list, vscale=1.5, hscale=1.5, wait=FALSE,
                    plotloc='top', print=FALSE,...) {

    if(!require("tkrplot")) {
        stop('The tkrplot package is needed')
    }


    ocl <- cl <- substitute(FUN)
    exargs <- as.list(quote(list()))

    PlotYet <- FALSE

    replot <- if(print){
        function() {
            if(PlotYet){
                print(eval(cl))
            }
        }
    } else {
        function() {
            if(PlotYet){
                eval(cl)
            }
        }
    }



    tt <- tktoplevel()
    tkwm.title(tt,'Tk Example')

    img <- tkrplot(tt, replot, vscale=vscale, hscale=hscale)
    tkpack(img, side=plotloc)

    hsc <- tclVar()
    tclvalue(hsc) <- hscale
    vsc <- tclVar()
    tclvalue(vsc) <- vscale

    fillframe <- function(frame,lst,pkdir,prfx) {
        for(i in seq_along(lst)) {
            vname <- paste(prfx, '.', i, sep='')
            el <- lst[[i]]
            eln <- names(lst)[i]
            if( is.list(el[[1]]) ){
                fr <- tkframe(frame,relief='ridge',borderwidth=3)
                tkpack(fr, side=pkdir)
                if(length(eln) && nchar(eln)){
                    tkpack(tklabel(fr, text=eln), side='top',anchor='nw')
                }
                Recall(fr,el,ifelse(pkdir=='top','left','top'),vname)
                next
            }
            if( tolower(el[[1]]) == 'numentry' ){
                tkpack(fr <- tkframe(frame),side=pkdir)
                tkpack(tklabel(fr,text=eln),
                       side=ifelse(pkdir=='top','left','top'))
                tmp <- tclVar()
                tclvalue(tmp) <- if ('init' %in% names(el)) el$init else 1
                alist <- list(fr, textvariable=tmp)
                el2 <- el[-1]
                el2$init <- NULL
                alist <- c(alist,el2)
                tkpack(do.call('tkentry',alist),side=pkdir)
                tmpcl <- as.list(cl)
                tmpl <-  list(substitute(as.numeric(tclvalue(VNAME)),
                                      list(VNAME=as.character(tmp))))
                names(tmpl) <- eln
                cl <<- as.call(c(tmpcl,tmpl))
                exargs <<- c(exargs, tmpl)
                next
            }
            if( tolower(el[[1]]) == 'entry' ){
                tkpack(fr <- tkframe(frame),side=pkdir)
                tkpack(tklabel(fr,text=eln),
                       side=ifelse(pkdir=='top','left','top'))
                tmp <- tclVar()
                tclvalue(tmp) <- if ('init' %in% names(el)) el$init else ""
                alist <- list(fr, textvariable=tmp)
                el2 <- el[-1]
                el2$init <- NULL
                alist <- c(alist,el2)
                tkpack(do.call('tkentry',alist),side=pkdir)
                tmpcl <- as.list(cl)
                tmpl <-  list(substitute(tclvalue(VNAME),
                                         list(VNAME=as.character(tmp))))
                names(tmpl) <- eln
                cl <<- as.call(c(tmpcl,tmpl))
                exargs <<- c(exargs, tmpl)
                next
            }
            if( tolower(el[[1]])== 'slider' ){
                tkpack(fr <- tkframe(frame), side=pkdir)
                tkpack(tklabel(fr,text=eln), side='left', anchor='s', pady=4)
                tmp <- tclVar()
                tclvalue(tmp) <- if('init' %in% names(el)) {
                    el$init
                } else if( 'from' %in% names(el) ) {
                    el$from
                } else { 1 }
                alist <- list(fr, variable=tmp, orient='horizontal',
                              command=function(...)tkrreplot(img,
                               hscale=as.numeric(tclvalue(hsc)),
                               vscale=as.numeric(tclvalue(vsc))) )
                el2 <- el[-1]
                el2$init <- NULL
                alist <- c(alist,el2)
                tkpack( do.call('tkscale',alist), side=pkdir)
                tmpcl <- as.list(cl)
                tmpl <- list(substitute(as.numeric(tclvalue(VNAME)),
                                        list(VNAME=as.character(tmp))))
                names(tmpl) <- eln
                cl <<- as.call(c(tmpcl,tmpl))
                exargs <<- c(exargs, tmpl)
                next
            }
            if( tolower(el[[1]])== 'vslider' ){
                tkpack(fr <- tkframe(frame), side=pkdir)
                tkpack(tklabel(fr,text=eln), side='left')
                tmp <- tclVar()
                tclvalue(tmp) <- if('init' %in% names(el)) {
                    el$init
                } else if( 'from' %in% names(el) ) {
                    el$from
                } else { 1 }
                alist <- list(fr, variable=tmp, orient='vertical',
                              command=function(...)tkrreplot(img,
                               hscale=as.numeric(tclvalue(hsc)),
                               vscale=as.numeric(tclvalue(vsc))) )
                el2 <- el[-1]
                el2$init <- NULL
                alist <- c(alist,el2)
                tkpack( do.call('tkscale',alist), side=pkdir)
                tmpcl <- as.list(cl)
                tmpl <- list(substitute(as.numeric(tclvalue(VNAME)),
                                        list(VNAME=as.character(tmp))))
                names(tmpl) <- eln
                cl <<- as.call(c(tmpcl,tmpl))
                exargs <<- c(exargs, tmpl)
                next
            }
            if( tolower(el[[1]])== 'spinbox' ){
                tkpack(fr <- tkframe(frame), side=pkdir)
                tkpack(tklabel(fr,text=eln),
                       side=ifelse(pkdir=='top','left','top'),anchor='nw')
                tmp <- tclVar()
                tclvalue(tmp) <- if('init' %in% names(el)) {
                    el$init
                } else if( 'from' %in% names(el) ) {
                    el$from
                } else { 1 }
                tmp2 <- tclvalue(tmp)  # fix strange resetting on first
                alist <- list(fr, textvariable=tmp,
                              command=function(...)tkrreplot(img,
                               hscale=as.numeric(tclvalue(hsc)),
                               vscale=as.numeric(tclvalue(vsc))) )
                el2 <- el[-1]
                el2$init <- NULL
                alist <- c(alist,el2)
                tkpack( do.call('tdspinner',alist), side=pkdir)
                tmpcl <- as.list(cl)
                tmpl <- list(substitute(as.numeric(tclvalue(VNAME)),
                                        list(VNAME=as.character(tmp))))
                names(tmpl) <- eln
                cl <<- as.call(c(tmpcl,tmpl))
                exargs <<- c(exargs, tmpl)
                tclvalue(tmp) <- tmp2 # rest of fix for reset
                next
            }
            if( tolower(el[[1]])== 'checkbox' ){
                tmp <- tclVar()
                tclvalue(tmp) <- if('init' %in% names(el)) {
                    el$init
                }  else { "F" }
                alist <- list(fr, variable=tmp,text=eln, onvalue="T",
                              offvalue="F",
                              command=function(...)tkrreplot(img,
                               hscale=as.numeric(tclvalue(hsc)),
                               vscale=as.numeric(tclvalue(vsc))) )
                el2 <- el[-1]
                el2$init <- NULL
                tmpvars <- if('values' %in% names(el)){
                    el$values
                } else { "" }
                el2$values <- NULL
                alist <- c(alist,el2)
                tkpack( do.call('tkcheckbutton',alist), side=pkdir)
                tmpcl <- as.list(cl)
                tmpl <- list(substitute(as.logical(tclvalue(VNAME)),
                                        list(VNAME=as.character(tmp))))
                names(tmpl) <- eln
                cl <<- as.call(c(tmpcl,tmpl))
                exargs <<- c(exargs, tmpl)
                next
            }
            if( tolower(el[[1]])== 'combobox' ){
                if( !have.ttk() ) stop('The combobox depends on having tcl 8.5 or higher, either install tcl 8.5 or rerun the function with a different control')
                tkpack(fr <- tkframe(frame), side=pkdir)
                tkpack(tklabel(fr,text=eln),
                       side=ifelse(pkdir=='top','left','top'))
                tmp <- tclVar()
                tclvalue(tmp) <- if('init' %in% names(el)) {
                    el$init
                }  else { "" }
                alist <- list(fr, textvariable=tmp)
                el2 <- el[-1]
                el2$init <- NULL
                tmpvars <- if('values' %in% names(el)){
                    el$values
                } else { "" }
                el2$values <- NULL
                alist <- c(alist,el2)
                tkpack( cb <-do.call('ttkcombobox',alist), side=pkdir)
                tkconfigure(cb, values=tmpvars)
                tkconfigure(cb, textvariable=tmp)
                tmpcl <- as.list(cl)
                tmpl <- list(substitute(tclvalue(VNAME),
                                        list(VNAME=as.character(tmp))))
                names(tmpl) <- eln
                cl <<- as.call(c(tmpcl,tmpl))
                exargs <<- c(exargs, tmpl)
                next
            }
            if( tolower(el[[1]])== 'radiobuttons' ){
                tkpack(fr <- tkframe(frame,relief='groove',borderwidth=3),
                       side=pkdir)
                tkpack(tklabel(fr,text=eln), side='top',
                       anchor='nw')
                tmp <- tclVar()
                tclvalue(tmp) <- if('init' %in% names(el)) {
                    el$init
                } else {
                    el$values[1]
                }
                el2 <- el[-1]
                tmp.vals <- el2$values
                el2$values <- NULL
                el2$init <- NULL
                alist <- list(fr, variable=tmp,
                              command=function()tkrreplot(img,
                               hscale=as.numeric(tclvalue(hsc)),
                               vscale=as.numeric(tclvalue(vsc))) )
                pkdir2 <- ifelse( pkdir=='top', 'left', 'top' )
                for( v in tmp.vals ){
                    tkpack( do.call('tkradiobutton', c(alist, value=v, text=v)),
                           side=pkdir2 )
                }
                tmpcl <- as.list(cl)
                tmpl <- list(substitute(tclvalue(VNAME),
                                        list(VNAME=as.character(tmp))))
                names(tmpl) <- eln
                cl <<- as.call(c(tmpcl,tmpl))
                exargs <<- c(exargs, tmpl)
                next
            }
        }
    }

    tkpack(tfr <- tkframe(tt),side='bottom', fill='x')

    tkpack(tkbutton(tfr, text="Refresh", command=function(){tkrreplot(img,
                                          hscale=as.numeric(tclvalue(hsc)),
                                          vscale=as.numeric(tclvalue(vsc)))} ),
           side='left',anchor='s')
    tkpack(tkbutton(tfr, text="Print Call",
                    command=function(){
                        tmp <- c(as.list(ocl),eval(as.call(exargs)))
                        cat(deparse(as.call(tmp)),"\n")
                        flush.console()
                    }),
           side='left',anchor='s')
    tkpack(tkbutton(tfr, text="Exit", command=function()tkdestroy(tt)),
           side='right',anchor='s')


    tkpack(tfr <- tkframe(tt), side='bottom', fill='x')
    tkpack(tklabel(tfr,text="Hscale: "), side='left')
    tkpack(tkentry(tfr,textvariable=hsc,width=6), side='left')
    tkpack(tklabel(tfr,text="      Vscale: "), side='left')
    tkpack(tkentry(tfr,textvariable=vsc,width=6), side='left')


    fillframe(tt, param.list, plotloc, 'tkv')
    PlotYet <- TRUE
    tkrreplot(img, hscale=as.numeric(tclvalue(hsc)),
              vscale=as.numeric(tclvalue(vsc)))



    if(wait){
        tkwait.window(tt)
        return(eval(as.call(exargs)))
    } else {
        return(invisible(NULL))
    }

}



#  tke.test <- list(Parameters=list(
#                   pch=list('spinbox',init=1,values=c(0:25,32:255),width=5),
#                   cex=list('slider',init=1.5,from=0.1,to=5,resolution=0.1),
#                   type=list('radiobuttons',init='b',
#                     values=c('p','l','b','o','c','h','s','S','n'),
#                          width=5),
#                   lwd=list('spinbox',init=1,from=0,to=5,increment=1,width=5),
#                   lty=list('spinbox',init=1,from=0,to=6,increment=1,width=5),
#                   xpd=list('checkbox')
#                   ))
#
#
#  tke.test3 <- list(Parameters=list(
#                   pch=list('spinbox',init=1,from=0,to=255,width=5),
#                   cex=list('slider',init=1.5,from=0.1,to=5,resolution=0.1),
#                   type=list('combobox',init='b',
#                     values=c('p','l','b','o','c','h','s','S','n'),
#                          width=5),
#                   lwd=list('spinbox',init=1,from=0,to=5,increment=1,width=5),
#                   lty=list('spinbox',init=1,from=0,to=6,increment=1,width=5)
#                   ))
#
#
#
#  tke.test2 <- list(pch=list('numentry',init=1,width=3),
#                   cex=list('slider',init=1,from=0.2,to=2.5,resolution=0.1),
#                   type=list('entry',init='p', width=5))
#
#
#
#  tke.test1 <- list(pch=list('numentry',init=1,width=3),
#                   cex=list('numentry',init=1),
#                   type=list('entry',init='p', width=5))
#
#
#
