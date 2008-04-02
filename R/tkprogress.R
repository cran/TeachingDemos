tkprogress <- function(numsteps=100, title='Progress') {

    tt <- tktoplevel()
    tkwm.title(tt,title)

    pg <- tclVar()
    tclvalue(pg) <- 0

    if( as.numeric(R.version$major) < 3 && as.numeric(R.version$mino) < 7 ) {
        bar <- tkscale(tt, variable=pg, from=0, to=numsteps, resolution=1,
                       orient='horizontal')
        tkpack(bar, side='top',fill='both')
    } else {
        bar <- ttkprogressbar(tt, variable=pg, maximum=numsteps)
        tkpack(bar, side='top',fill='both')
    }

    function(inc=1, set, quit=FALSE){
        if(quit){
            tkdestroy(tt)
            return(invisible(NULL))
        }

        if(!missing(set)) {
            tclvalue(pg) <<- set
            return(invisible(NULL))
        }

        tclvalue(pg) <<- as.numeric(tclvalue(pg)) + inc
    }
}

