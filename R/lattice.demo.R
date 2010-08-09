"lattice.demo" <-
function(x,y,z, show3d=TRUE){

  if(!require(tcltk)){stop('The tcltk package is needed')}
  if(!exists('slider.env')) slider.env <<- new.env()

  if(!require(lattice)) stop('The lattice package is needed')

  center <- mean(z); assign('center',tclVar(center), env=slider.env)
  width <- diff(range(z))/20*3; assign('width',tclVar(width), env=slider.env)

  s3d <- 1; assign('s3d', tclVar(s3d), env=slider.env)

  lattice.refresh <- function(...){
    center <- as.numeric(evalq(tclvalue(center), env=slider.env))
    width <- as.numeric(evalq(tclvalue(width), env=slider.env))

    s3d <- as.numeric(evalq(tclvalue(s3d), env=slider.env))

    shingle.min <- max(min(z), center-width/2)
    shingle.max <- min(max(z), center+width/2)

    shingle.scaled.range <- c( (shingle.min-min(z))/diff(range(z)),
                               (shingle.max-min(z))/diff(range(z))) - 0.5

    if(s3d){
      print(xyplot(y~x|shingle(z,rbind(range(z),c(shingle.min,shingle.max))),
                   index.cond=list(2),
                   strip=strip.custom(strip.names=TRUE,strip.levels=TRUE),
                   par.strip.text=list(cex=0.75)),
            split=c(1,1,1,2), more=T)

      print(cloud(y~z+x, panel=function(x,y,z,...){
        panel.cloud(x,y,z,panel.3d.cloud=function(x,y,z,groups,...){
          panel.3dscatter(x,y,z,
                          groups= factor(x>shingle.scaled.range[1] & x <shingle.scaled.range[2]),
                          ...)
          panel.3dwire(x=shingle.scaled.range,
                       y=c(-.5,.5), z=rep(-.5,4), at=c(-.57,.57), ...)
        },...) }), split=c(1,2,1,2),more=F)

    } else {
      print(xyplot(y~x|shingle(z,rbind(range(z),c(shingle.min,shingle.max))),
                   index.cond=list(2),
                   strip=strip.custom(strip.names=TRUE,strip.levels=TRUE),
                   par.strip.text=list(cex=0.75)
                   ),
            split=c(1,1,1,1), more=F)

    }

  }

  m <- tktoplevel()
  tkwm.title(m,'Trellis/Lattice Demo')
  tkwm.geometry(m,'+0+0')

  # center
  tkpack(fr <- tkframe(m),side='top')
  tkpack(tklabel(fr, text='center', width='10'), side='right')
  tkpack(sc <- tkscale(fr, command=lattice.refresh, from=min(z), to=max(z),
                       orient='horiz',
                       resolution=diff(range(z))/25, showvalue=T),
         side='left')

  assign('sc',sc,env=slider.env)
  evalq(tkconfigure(sc, variable=center),env=slider.env)

  # width
  tkpack(fr <- tkframe(m), side='top')
  tkpack(tklabel(fr, text='width', width='10'), side='right')
  tkpack(sc <- tkscale(fr, command=lattice.refresh, from=diff(range(z))/20,
                       to=diff(range(z)),orient='horiz',
                       resolution=diff(range(z))/20, showvalue=T),
         side='left')
  assign('sc',sc,env=slider.env)
  evalq(tkconfigure(sc, variable=width), env=slider.env)

  # show 3d
  tkpack(fr <- tkframe(m), side='top')
  tkpack(sc <- tkcheckbutton(fr, command=lattice.refresh),
         side='left')
  tkpack(tklabel(fr, text='Show 3-D plot', width='25'),
         side='left')
  assign('sc',sc,env=slider.env)
  evalq(tkconfigure(sc, variable=s3d), env=slider.env)

  tkpack(tkbutton(m, text="Refresh", command=lattice.refresh),side='left')

  tkpack(tkbutton(m, text="Exit", command=function()tkdestroy(m)),
         side='right')

}

