shadowtext <- function(x, y=NULL, labels, col='white', bg='black',
	theta= seq(pi/32, 2*pi, length.out=64), r=0.1, cex=1, ... ) {

	xy <- xy.coords(x,y)
	fx <- grconvertX(xy$x, to='nfc')
	fy <- grconvertY(xy$y, to='nfc')
	fxo <- r*strwidth('A', units='figure', cex=cex)
	fyo <- r*strheight('A', units='figure', cex=cex)
	
	for (i in theta) {
	  text(grconvertX(fx + cos(i)*fxo, from="nfc"),
	       grconvertY(fy + sin(i)*fyo, from="nfc"),
	       labels, cex=cex, col=bg, ...)
	}
	text(xy$x, xy$y, labels, cex=cex, col=col, ... ) 
}

