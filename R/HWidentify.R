HWidentify <- function(x,y,label=seq_along(x), xlab=deparse(substitute(x)),
				ylab=deparse(substitute(y)), ...) {

	plot(x,y,xlab=xlab, ylab=ylab,...)

	dx <- grconvertX(x,to='ndc')
	dy <- grconvertY(y,to='ndc')

	mm <- function(buttons, xx, yy) {
		d <- (xx-dx)^2 + (yy-dy)^2
		if ( all( d > .01 ) ){
			plot(x,y,xlab=xlab,ylab=ylab,...)
			return()
		}
		w <- which.min(d)
		plot(x,y,xlab=xlab,ylab=ylab,...)
		points(x[w],y[w], cex=2, col='red')
		text(grconvertX(xx,from='ndc'),grconvertY(yy,from='ndc'),
			label[w], col='green', adj=c(0,0))
		return()
	}

	md <- function(buttons, xx, yy) {
		if (any(buttons=='2')) return(1)
		return()
	}

	getGraphicsEvent('Right Click to exit', onMouseMove = mm, onMouseDown=md)
	invisible()
}

# tmpx <- runif(25)
# tmpy <- rnorm(25)
# HWidentify(tmpx,tmpy,LETTERS[1:25], pch=letters)




HTKidentify <- function(x,y,label=seq_along(x), xlab=deparse(substitute(x)),
				ylab=deparse(substitute(y)), ...) {

    if( !require(tkrplot) ) stop ('tkrplot package is required')

	dx <- numeric(0)
	dy <- numeric(0)

	xx <- yy <- 0

	replot <- function() {
		d <- (xx-dx)^2 + (yy-dy)^2
		if ( all( d > .01 ) ){
			plot(x,y,xlab=xlab,ylab=ylab,...)
			if( length(dx)==0 ) {
				dx <<- grconvertX(x, to='ndc')
				dy <<- grconvertY(y, to='ndc')
			}
			return()
		}
		w <- which.min(d)
		plot(x,y,xlab=xlab,ylab=ylab,...)
		points(x[w],y[w], cex=2, col='red')
		text(grconvertX(xx,from='ndc'),grconvertY(yy,from='ndc'),
			label[w], col='green', adj=c(0,0))
	}

	tt <- tktoplevel()
	img <- tkrplot(tt, replot, hscale=1.5, vscale=1.5)
	tkpack(img, side='top')
	iw <- as.numeric(tcl("image","width", tkcget(img, "-image")))
	ih <- as.numeric(tcl("image","height", tkcget(img, "-image")))

	cc <- function(x,y) {
		x <- (as.real(x) -1)/iw
		y <- 1-(as.real(y)-1)/ih
		c(x,y)
	}

	mm <- function(x, y) {
		xy <- cc(x,y)
		xx <<- xy[1]
		yy <<- xy[2]
		tkrreplot(img)
	}

	tkbind(img, "<Motion>", mm)

	invisible()
}

# tmpx <- runif(25)
# tmpy <- rnorm(25)
# HTKidentify(tmpx,tmpy,LETTERS[1:25], pch=letters)









# getGraphicsEvent( onMouseDown = function(buttons, xx, yy) cat(buttons,'\n'),
#   onMouseUp=function(buttons, xx, yy) cat('up: ',buttons,'\n'),
#   onMouseMove=function(buttons, xx, yy) cat('move: ',buttons,' x:',xx,' y:',yy,'\n')
# )
