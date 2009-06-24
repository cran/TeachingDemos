"run.hist.demo" <-
function(x) {

  pr <- pretty(x)
  xr <- range(pr)
  xr[1] <- 4*xr[1] - 3*min(x)
  xr[2] <- 4*xr[2] - 3*max(x)

  hist.refresh <- function(...) {

    hist(x,seq( slider(no=2), slider(no=3), length=slider(no=1)+1),
         xlim=xr)
    
  }

  slider(hist.refresh, c('Number of bins','Minimum','Maximum'),
         c(1, xr[1], max(x)),
         c(length(x),min(x),xr[2]),
         c(1, (min(x)-xr[1])/50, (xr[2]-max(x))/50),
         c(nclass.Sturges(x),min(pr),max(pr)),
         title="Histogram Demo")

}

