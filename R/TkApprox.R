TkApprox <- function(x, y, type='b', snap.to.x=FALSE, digits=4,
                     cols=c('red','#009900','blue'),
                     xlab=deparse(substitute(x)), ylab=deparse(substitute(y)),
                     hscale=1.5, vscale=1.5, wait=TRUE,
                     ...) {
  if( !require(tkrplot) ) stop('This function depends on the tkrplot package being available')

  snap.x <- tclVar()
  tclvalue(snap.x) <- ifelse(snap.to.x,"T","F")

  xxx <- as.numeric(x)
  ax <- min(x)
  cx <- max(x)
  bx <- (ax+cx)/2

  xx <- c(ax,bx,cx)

  af <- approxfun(x,y)

  ay <- af(ax)
  by <- af(bx)
  cy <- af(cx)

  yy <- c(ay,by,cy)

  txtvar <- tclVar()
  tclvalue(txtvar) <- " \n \n "

  first <- TRUE
  ul <- ur <- 0

  replot <- function() {
    par(mar=c(5,4,4,4)+0.1)
    plot(x, y, type=type, xlab=xlab, ylab=ylab, ...)
    u <- par('usr')
    lines( c(xx[1],xx[1],u[1]), c(u[3],yy[1],yy[1]), col=cols[1] )
    lines( c(xx[2],xx[2],u[1]), c(u[3],yy[2],yy[2]), col=cols[2] )
    lines( c(xx[3],xx[3],u[1]), c(u[3],yy[3],yy[3]), col=cols[3] )
    mtext( format( xx, digits=digits), side=3, at=xx,
          line=1:3, col=cols)
    mtext( format( yy, digits=digits), side=4, at=yy,
          line=1:3, col=cols)
    tclvalue(txtvar) <<- paste( c('A:B   ','B:C   ','A:C   '),
                     format(pmax( xx[c(1,2,1)], xx[c(2,3,3)] ), digits=digits),
                     '-',
                     format(pmin( xx[c(1,2,1)], xx[c(2,3,3)] ), digits=digits),
                     '=',
                     format(abs( xx[c(1,2,1)] - xx[c(2,3,3)] ), digits=digits),
                     '     ',
                     format(pmax( yy[c(1,2,1)], yy[c(2,3,3)] ), digits=digits),
                     '-',
                     format(pmin( yy[c(1,2,1)], yy[c(2,3,3)] ), digits=digits),
                     '=',
                     format(abs( yy[c(1,2,1)] - yy[c(2,3,3)] ), digits=digits),
                            collapse="\n"
                     )
    if(first) {
      first <<- FALSE
      tmp <- cnvrt.coords(c(0,1),c(0,1), input='dev')$usr
      ul <<- tmp$x[1]
      ur <<- tmp$x[2]
    }
  }

  tt <- tktoplevel()
  tkwm.title(tt, "TkApprox")

  img <- tkrplot(tt, replot, vscale=vscale, hscale=hscale)
  tkpack(img, side='top')

  tkpack(tklabel(tt, textvariable=txtvar), side='top')

  tkpack(tkcheckbutton(tt,variable=snap.x, onvalue="T", offvalue="F",
                       text="Snap to points"),
         side='left')
  tkpack(tkbutton(tt, text='Quit', command=function() tkdestroy(tt)),
         side='right')

  md <- FALSE
  iw <- as.numeric(tcl('image','width',tkcget(img,'-image')))
  ih <- as.numeric(tcl('image','height',tkcget(img,'-image')))
  ccx <- ccy <- 0
  ci <- 0

  mouse.move <- function(x,y) {
    if(md) {
      tx <- (as.numeric(x)-1)/iw
      ccx <<- tx*ur + (1-tx)*ul
      if(as.logical(tclvalue(snap.x))) {
        ccx <<- xxx[ which.min( abs(ccx-xxx) ) ]
      }
      xx[ci] <<- ccx
      ccy <<- af(ccx)
      yy[ci] <<- ccy

      tkrreplot(img)
    }
  }

  mouse.down <- function(x,y) {
    tx <- (as.numeric(x)-1)/iw
    txx <- tx*ur + (1-tx)*ul
    ci <<- which.min( abs( txx - xx ) )
    md <<- TRUE
    mouse.move(x,y)
  }

  mouse.up <- function(x,y) {
    md <<- FALSE
  }

  tkbind(img, '<Motion>', mouse.move)
  tkbind(img, '<ButtonPress-1>', mouse.down)
  tkbind(img, '<ButtonRelease-1>', mouse.up)

  if(wait) {
    tkwait.window(tt)
    out <- list( x=xx, y=yy )
  } else {
    out <- NULL
  }

  invisible(out)

}

