TkBuildDist <- function(  x=seq(min+(max-min)/nbin/2,
                                max-(max-min)/nbin/2,
                                length.out=nbin),
                          min=0, max=10, nbin=10, logspline=TRUE,
                          intervals=FALSE) {

    if(logspline) logspline <- require(logspline)
    require(tkrplot)

    xxx <- x

    brks <- seq(min, max, length.out=nbin+1)
    nx <- seq( min(brks), max(brks), length.out=250 )

    lx <- ux <- 0
    first <- TRUE

    replot <- if(logspline) {
        if(intervals) {
            function() {
                hist(xxx, breaks=brks, probability=TRUE,xlab='', main='')
                xx <- cut(xxx, brks, labels=FALSE)
                fit <- oldlogspline( interval = cbind(brks[xx], brks[xx+1]) )
                lines( nx, doldlogspline(nx,fit), lwd=3 )
                if(first) {
                    first <<- FALSE
                    lx <<- grconvertX(min, to='ndc')
                    ux <<- grconvertX(max, to='ndc')
                }
            }
        } else {
            function() {
                hist(xxx, breaks=brks, probability=TRUE,xlab='', main='')
                fit <- logspline( xxx )
                lines( nx, dlogspline(nx,fit), lwd=3 )
                if(first) {
                    first <<- FALSE
                    lx <<- grconvertX(min, to='ndc')
                    ux <<- grconvertX(max, to='ndc')
                }
            }
        }
    } else {
        function() {
            hist(xxx, breaks=brks, probability=TRUE,xlab='',main='')
            if(first) {
                first <<- FALSE
                lx <<- grconvertX(min, to='ndc')
                ux <<- grconvertX(max, to='ndc')
            }
        }
    }

    tt <- tktoplevel()
    tkwm.title(tt, "Distribution Builder")

    img <- tkrplot(tt, replot, vscale=1.5, hscale=1.5)
    tkpack(img, side='top')

    tkpack( tkbutton(tt, text='Quit', command=function() tkdestroy(tt)),
           side='right')

    iw <- as.numeric(tcl('image','width',tkcget(img,'-image')))

    mouse1.down <- function(x,y) {
        tx <- (as.numeric(x)-1)/iw
        ux <- (tx-lx)/(ux-lx)*(max-min)+min
        xxx <<- c(xxx,ux)
        tkrreplot(img)
    }

    mouse2.down <- function(x,y) {
        if(length(xxx)) {
            tx <- (as.numeric(x)-1)/iw
            ux <- (tx-lx)/(ux-lx)*(max-min)+min
            w <- which.min( abs(xxx-ux) )
            xxx <<- xxx[-w]
            tkrreplot(img)
        }
    }

    tkbind(img, '<ButtonPress-1>', mouse1.down)
    tkbind(img, '<ButtonPress-2>', mouse2.down)
    tkbind(img, '<ButtonPress-3>', mouse2.down)

    tkwait.window(tt)

    out <- list(x=xxx)
    if(logspline) {
        if( intervals ) {
            xx <- cut(xxx, brks, labels=FALSE)
            out$logspline <- oldlogspline( interval = cbind(brks[xx], brks[xx+1]) )
        } else {
            out$logspline <- logspline(xxx)
        }
    }

    if(intervals) {
        out$intervals <- table(cut(xxx, brks))
    }

    out$breaks <- brks

    return(out)
}




TkBuildDist2 <- function( min=0, max=1, nbin=10, logspline=TRUE) {
    if(logspline) logspline <- require(logspline)
    require(tkrplot)

    xxx <- rep( 1/nbin, nbin )

    brks <- seq(min, max, length.out=nbin+1)
    nx <- seq( min, max, length.out=250 )

    lx <- ux <- ly <- uy <- 0
    first <- TRUE

    replot <- if(logspline) {
        function() {
            barplot(xxx, width=diff(brks), xlim=c(min,max), space=0,
                    ylim=c(0,0.5), col=NA)
            axis(1,at=brks)
            xx <- rep( 1:nbin, round(xxx*100) )
            capture.output(fit <- oldlogspline( interval = cbind(brks[xx], brks[xx+1]) ))
            lines( nx, doldlogspline(nx,fit)*(max-min)/nbin, lwd=3 )

            if(first) {
                first <<- FALSE
                lx <<- grconvertX(min, to='ndc')
                ly <<- grconvertY(0,   to='ndc')
                ux <<- grconvertX(max, to='ndc')
                uy <<- grconvertY(0.5, to='ndc')
            }
        }
    } else {
        function() {
            barplot(xxx, width=diff(brks), xlim=range(brks), space=0,
                    ylim=c(0,0.5), col=NA)
            axis(at=brks)
            if(first) {
                first <<- FALSE
                lx <<- grconvertX(min, to='ndc')
                ly <<- grconvertY(0,   to='ndc')
                ux <<- grconvertX(max, to='ndc')
                uy <<- grconvertY(0.5, to='ndc')
            }
        }
    }

    tt <- tktoplevel()
    tkwm.title(tt, "Distribution Builder")

    img <- tkrplot(tt, replot, vscale=1.5, hscale=1.5)
    tkpack(img, side='top')

    tkpack( tkbutton(tt, text='Quit', command=function() tkdestroy(tt)),
           side='right')

    iw <- as.numeric(tcl('image','width',tkcget(img,'-image')))
    ih <- as.numeric(tcl('image','height',tkcget(img,'-image')))



    md <- FALSE

    mouse.move <- function(x,y) {
        if(md) {
            tx <- (as.numeric(x)-1)/iw
            ty <- 1-(as.numeric(y)-1)/ih

            w <- findInterval(tx, seq(lx,ux, length=nbin+1))

            if( w > 0 && w <= nbin && ty >= ly && ty <= uy ) {
                 xxx[w] <<- 0.5*(ty-ly)/(uy-ly)
                xxx[-w] <<- (1-xxx[w])*xxx[-w]/sum(xxx[-w])

                tkrreplot(img)
            }
        }
    }

    mouse.down <- function(x,y) {
        md <<- TRUE
        mouse.move(x,y)
    }

    mouse.up <- function(x,y) {
        md <<- FALSE
    }

    tkbind(img, '<Motion>', mouse.move)
    tkbind(img, '<ButtonPress-1>', mouse.down)
    tkbind(img, '<ButtonRelease-1>', mouse.up)

    tkwait.window(tt)

    out <- list(breaks=brks, probs=xxx)
    if(logspline) {
        xx <- rep( 1:nbin, round(xxx*100) )
        out$logspline <- oldlogspline( interval = cbind(brks[xx], brks[xx+1]) )
    }

    return(out)
}
