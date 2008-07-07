"run.old.cor.examp" <-
function(n=100,seed) {
  if (!missing(seed)){ set.seed(seed) }

  x <- scale(matrix(rnorm(2*n,0,1), ncol=2))
  x <- x %*% solve( chol( cor(x) ) )
  xr <- range(x)

  cor.refresh <- function(...) {
    r <- slider(no=1)
    if ( r == 1 ) {
      cmat <- matrix( c(1,0,1,0),2 )
    } else if (r == -1) {
      cmat <- matrix( c(1,0,-1,0),2 )
    } else {
      cmat <- chol( matrix( c(1,r,r,1),2) )
    }
    new.x <- x %*% cmat

    plot(new.x, xlab='x',ylab='y', xlim=xr, ylim=xr)
    title(paste("r = ",round(cor(new.x[,1],new.x[,2]),3)))
  }

  slider( cor.refresh, 'Correlation', -1, 1, 0.01, 0,
         title="Correlation Demo")
  cor.refresh()
}

run.cor.examp <- function(n=100,seed,vscale=1.5,hscale=1.5,wait=FALSE) {

    if( !require(tkrplot) ) stop('This function depends on the tkrplot package being available')

    if(!missing(seed) ) set.seed(seed)

    x <- scale(matrix(rnorm(2*n,0,1), ncol=2))
    x <- x %*% solve( chol( cor(x) ) )
    xr <- range(x,-x)

    hsc <- tclVar()
    tclvalue(hsc) <- hscale
    vsc <- tclVar()
    tclvalue(vsc) <- vscale

    r <- tclVar()
    tclvalue(r) <- 0

    replot <- function(...) {
        tmp.r <- as.numeric(tclvalue(r))
        if( tmp.r == 1 ) {
            cmat <- matrix( c(1,0,1,0),2 )
        } else if(  tmp.r == -1 ) {
            cmat <- matrix( c(1,0,-1,0),2 )
        } else {
            cmat <- chol( matrix( c(1,tmp.r,tmp.r,1),2) )
        }
        new.x <- x %*% cmat

        plot(new.x, xlab='x', ylab='y', xlim=xr, ylim=xr)
        title(paste("r =", round( tmp.r, 3)))
    }

    tt <- tktoplevel()
    tkwm.title(tt, "Cor2 Example")

    img <- tkrplot(tt, replot, vscale=vscale, hscale=hscale)
    tkpack(img, side='top')


    tkpack(fr <- tkframe(tt), side='top')
    tkpack(tklabel(fr,text='r: '), side='left',anchor='s')
    tkpack(tkscale(fr, variable=r, orient='horizontal',
                   command=function(...) tkrreplot(img,
                    hscale=as.numeric(tclvalue(hsc)),
                    vscale=as.numeric(tclvalue(vsc)) ),
                   from=-1, to=1,
                   resolution=0.01), side='right')


    tkpack(tfr <- tkframe(tt), side='bottom', fill='x')
    tkpack(tkbutton(tfr, text="Refresh", command=function() tkrreplot(img,
                                          hscale=as.numeric(tclvalue(hsc)),
                                          vscale=as.numeric(tclvalue(vsc)) ) ),
           side='left',anchor='s')
    tkpack(tkbutton(tfr, text="Exit", command=function()tkdestroy(tt)),
           side='right',anchor='s')

    tkpack(tfr <- tkframe(tt), side='bottom', fill='x')
    tkpack(tklabel(tfr,text="Hscale: "), side='left')
    tkpack(tkentry(tfr,textvariable=hsc,width=6), side='left')
    tkpack(tklabel(tfr,text="      Vscale: "), side='left')
    tkpack(tkentry(tfr,textvariable=vsc,width=6), side='left')

    if(wait){
        tkwait.window(tt)
        tmp.r <- as.numeric(tclvalue(r))
        if( tmp.r == 1 ) {
            cmat <- matrix( c(1,0,1,0),2 )
        } else if(  tmp.r == -1 ) {
            cmat <- matrix( c(1,0,-1,0),2 )
        } else {
            cmat <- chol( matrix( c(1,tmp.r,tmp.r,1),2) )
        }
        new.x <- x %*% cmat
        return( list(x=new.x[,1], y=new.x[,2]) )
    } else {
        return(invisible(NULL))
    }
}
