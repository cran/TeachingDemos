
#############################################################################
#                                                                           #
#               INTERACTIVE functions for TEACHING or EXPLORATION           #
#                                                                           #
#############################################################################
   tosave <- NULL

   tosave <- c(tosave, "interact.menu")
interact.menu <- function() {
   require(utils)
#   try(winMenuDel("Interact"), silent=T)
   winMenuAdd("Interact")
   winMenuAddItem("Interact","Histogram","interact.hist()")
   winMenuAddItem("Interact","Law of Large Numbers", "interact.lln()")
   winMenuAddItem("Interact","Central Limit Theorem","interact.clt()")   
   winMenuAddItem("Interact","Confidence intervals","interact.confint()")   
   winMenuAddItem("Interact","Power", "interact.power()")
   winMenuAddItem("Interact","Correlation","interact.correlation()")
   winMenuAddItem("Interact","Regression","interact.regression()")
   winMenuAddItem("Interact","PCA","interact.PCA()")
   winMenuAddItem("Interact/Max Likelihood Est","Normal","interact.mle.norm()")
   winMenuAddItem("Interact/Max Likelihood Est","Poisson","interact.mle.poiss()")
   winMenuAddItem("Interact","Probability vs Likelihood","interact.pbllh()")
   winMenuAddItem("Interact/Distributions","Normal","vis.normal()")
   winMenuAddItem("Interact/Distributions","Student\'s t","vis.t()")
   winMenuAddItem("Interact/Distributions","Poisson","vis.poiss()")
   winMenuAddItem("Interact/Distributions","Binomial","vis.binom()")
   winMenuAddItem("Interact/Distributions","Chi-squared","vis.chisq()")
}

interact.menu()

################################################################################
#                                                                              #
#                          INTERACTIVE HISTOGRAM                               #
#                                                                              #
# Allows you to change the number of classes and the upper and lower limits    #
# using a Tk widget. A development of Greg Snow's 'run.hist.demo' in package   #
# 'TeachingDemos'. Main enhancement is the ability to return the final product #
# as an object of class 'histogram'; also optionally adds kernel density and   #
# normal curve to the plot.                                                    #
################################################################################

   tosave <- c(tosave, "interact.hist")
interact.hist <- function (dat, name=NULL, dens=T, norm=F, color="grey") {

    require(tcltk)
    if (!exists("slider.env")) 
        slider.env <<- new.env()
    if(!missing(dat)) {
        if(is.null(name))
            name <- deparse(substitute(dat))
        dat <- dat[!is.na(dat)]
    } else {
        dat <- c(rnorm(50,2,1),rnorm(50,4,2))
        if(is.null(name))
           name <- "Example data"
    }
    pr <- pretty(dat)
    xr <- range(pr)
    xr[1] <- xr[1] - diff(pr[1:2])
    xr[2] <- xr[2] + diff(pr[1:2])
    nb <- nclass.Sturges(dat)
    assign("nb", tclVar(nb), env = slider.env)
    minb <- min(pr)
    assign("minb", tclVar(minb), env = slider.env)
    maxb <- max(pr)
    assign("maxb", tclVar(maxb), env = slider.env)
    sden <- as.numeric(dens)
    assign("sden", tclVar(sden), env = slider.env)
    snorm <- as.numeric(norm)
    assign("snorm", tclVar(snorm), env = slider.env)
 
    hist.refresh <- function(...) {
        minb <- min(as.numeric(evalq(tclvalue(minb), env = slider.env)),dat)
        maxb <- max(as.numeric(evalq(tclvalue(maxb), env = slider.env)),dat)
        nb <- as.numeric(evalq(tclvalue(nb), env = slider.env))
        sden <- as.numeric(evalq(tclvalue(sden), env = slider.env))
        snorm <- as.numeric(evalq(tclvalue(snorm), env = slider.env))

        interact.hist.out <<- hist(dat, seq(minb, maxb, length = nb + 1),
             xlim = xr, main=name,freq=F,col=color,xlab="Measurement")
        if(sden) lines(density(dat),col="red")
        if(snorm) curve(dnorm(x,mean(dat),sd(dat)), add=T, col="blue")
        rug(dat)
    }
    # initial plot display
    interact.hist.out <<- hist(dat, seq(min(pr), max(pr), length = nclass.Sturges(dat)+1), 
        xlim = xr, main=name,freq=F,col=color,xlab="Measurement")
    if(dens) lines(density(dat),col="red")
    if(norm) curve(dnorm(x,mean(dat),sd(dat)), add=T, col="blue")
    rug(dat)
    bringToTop()

    m <- tktoplevel()
    tkwm.title(m, "Interactive Histogram")
    tkwm.geometry(m, "+0+0")

    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Number of bins", width = "15"), side = "right")
    tkpack(sc <- tkscale(fr, command = hist.refresh, from = 1, to = length(dat), 
        orient = "horiz", resolution = 1, showvalue = T), side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = nb), env = slider.env)

    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Minimum", width = "15"), side = "right")
    tkpack(sc <- tkscale(fr, command = hist.refresh, from = xr[1], to = min(dat), 
        orient = "horiz", resolution = (min(dat) - xr[1])/50, showvalue = T), side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = minb), env = slider.env)

    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Maximum", width = "15"), side = "right")
    tkpack(sc <- tkscale(fr, command = hist.refresh, from = max(dat), to = xr[2], 
        orient = "horiz", resolution = (xr[2] - max(dat))/50, showvalue = T), side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = maxb), env = slider.env)

    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Show Density", width = "25"), side = "right")
    tkpack(sc <- tkcheckbutton(fr, command = hist.refresh), side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = sden), env = slider.env)

    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Show Normal Distribution", width = "25"), side = "right")
    tkpack(sc <- tkcheckbutton(fr, command = hist.refresh), side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = snorm), env = slider.env)

    tkpack(tkbutton(m, text = "Exit", command = function() tkdestroy(m)), 
        side = "right")

    cat("Result will be stored in the object \'interact.hist.out\'\n")
}

#####################################################################
#                                                                   #
#                  Demo for Law of Large Numbers                    #
#                                                                   #
#####################################################################
# Inspired by 'lln.demo' by Richard Royall & Jeffrey Blume, see
# http://harvardforest.fas.harvard.edu/personnel/web/aellison/publications/primer/primer.html

   tosave <- c(tosave, "interact.lln")
interact.lln <- function(data,name=NULL,nmax=50,mu=NULL,sigma=NULL)  {
   require(tcltk)
    if (!exists("slider.env")) 
        slider.env <<- new.env()
   # sanity checks:
   if(simdat <- missing(data)) { 
      if(is.null(mu)) mu <- 0
      if(is.null(sigma)) sigma <- 1
      if(is.null(name)) name <- "Simulated data"
   } else {
      if(is.null(name)) name <- deparse(substitute(data))
      stopifnot(length(data)>4 && is.numeric(data))
      if(is.na(sum(data))) stop("\'data\' contains missing values")
   }
   params.known <- !is.null(mu) && !is.null(sigma)
   if(is.null(mu)) {
      mu <- mean(data)
      mean.txt <- "Estimated population mean"
   } else mean.txt <- paste("Population mean (",mu,")",sep="")
   if(is.null(sigma)) sigma <- sd(data)
   ylim <- c(mu - 3*sigma, mu + 3*sigma)
    confint <- 0.95
    assign("confint", tclVar(confint), env = slider.env)
    sci <- 1
    assign("sci", tclVar(sci), env = slider.env)
    spi <- 0
    assign("spi", tclVar(spi), env = slider.env)

   replot <- function(...)  {
      confint <- as.numeric(evalq(tclvalue(confint), env = slider.env))
      sci <- as.numeric(evalq(tclvalue(sci), env = slider.env))
      spi <- as.numeric(evalq(tclvalue(spi), env = slider.env))
      width <- qnorm(1-(1-confint)/2)
      ssd <- x <- z <- seq(data)
      for(i in seq(data)) { 
         x[i] <- mean(data[1:i])
         ssd[i] <- sd(data[1:i])
      }
      plot(z, x, type = "n", xlab = "sample size (n)", ylim=ylim, ylab = " ", 
            main = paste("Means for samples of different sizes\n", name))
      lines(z,x,lty=1,col=1,lwd=2)
      leg.txt <- paste("Sample mean (last is ",signif(mean(data), 3),")", sep="")
      leg.lty <- 1 ; leg.col <- 1 ; leg.lwd <- 2
      abline(h=mu, lty=2, col=2)
      leg.txt <- c(mean.txt,leg.txt)
      leg.lty <- c(2,leg.lty) ; leg.col <- c(2,leg.col) ; leg.lwd <- c(1,leg.lwd)
      if(spi) {             # probability intervals (based on popn mean and sigma)
         lim1 <- mu+width*sigma/sqrt(z)
         lim2 <- mu-width*sigma/sqrt(z)
         lines(z, lim1, lty = 2,col=3)
         lines(z, lim2, lty = 2,col=3)
         leg.txt <- c(leg.txt, paste("Probability interval (last is ",
           signif(mu-width*sigma/sqrt(max(z)), 3)," to ",signif(mu+width*sigma/sqrt(max(z)), 3),")",sep=""))
         leg.lty <- c(leg.lty,2) ; leg.col <- c(leg.col,3) ; leg.lwd <- c(leg.lwd,1)
      }
      if(sci) {             # confidence intervals (based on sample mean and sd)
         lim1 <- x+width*ssd/sqrt(z)    # first element (n=1) is 'NA' so will not be plotted
         lim2 <- x-width*ssd/sqrt(z)
         lines(z, lim1, lty = 1,col=4)
         lines(z, lim2, lty = 1,col=4)
         leg.txt <- c(leg.txt, "Confidence interval based on sample")
         leg.lty <- c(leg.lty,1) ; leg.col <- c(leg.col,4) ; leg.lwd <- c(leg.lwd,1)
      }
      legend("topright", legend=leg.txt, lty=leg.lty, col=leg.col, lwd=leg.lwd,cex=0.8)
   }
   resample <- function(...) {
      data <<- if(simdat) rnorm(nmax, mu, sigma) else sample(data)
      replot()
   }
   resample()
   bringToTop()

    m <- tktoplevel()
    tkwm.title(m, "Law of Large Numbers")
    tkwm.geometry(m, "+0+0")

    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Confidence interval", width = "15"), side = "right")
    tkpack(sc <- tkscale(fr, command = replot, from = 0.5, to = 0.99, 
        orient = "horiz", resolution = 0.01, showvalue = T), side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = confint), env = slider.env)

    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Show Confidence Interval", width = "25"), side = "right")
    tkpack(sc <- tkcheckbutton(fr, command = replot), side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = sci), env = slider.env)
    if(params.known) {
       tkpack(fr <- tkframe(m), side = "top")
       tkpack(tklabel(fr, text = "Show Probability Interval", width = "25"), side = "right")
       tkpack(sc <- tkcheckbutton(fr, command = replot), side = "left")
       assign("sc", sc, env = slider.env)
       evalq(tkconfigure(sc, variable = spi), env = slider.env)
    }
    tkpack(tkbutton(m, text = "New sample", command = resample), 
        side = "left")

    tkpack(tkbutton(m, text = "Exit", command = function() tkdestroy(m)), 
        side = "right")
}

#####################################################################
#                                                                   #
#           Interactive demo of Central Limit Theorem               #
#                                                                   #
# Based on Greg Snow's function 'clt.examp' with lots of changes    #
#####################################################################

   tosave <- c(tosave, "interact.clt")
interact.clt <- function (reps = 50000, nclass = 16, show.pop = T) 
{
   require(tcltk)
   if (!exists("slider.env")) 
        slider.env <<- new.env()
    pnorm <- rnorm(reps)
    pexp <- rexp(reps, 1/3)
    pops <- list(
       norm = hist(pnorm[abs(pnorm)<2.5], plot=FALSE, nclass=20),
       exp = hist(pexp[pexp<12],plot=FALSE, nclass=nclass),
       unif = hist(runif(reps),plot=FALSE, nclass=nclass),
       beta = hist(rbeta(reps, 0.35, 0.25),plot=FALSE, nclass=nclass))
    plotnames <- c("Normal", "Exponential","Uniform","Beta")

    n <- 1
    assign("n", tclVar(n), env = slider.env)
    spop <- show.pop
    assign("spop", tclVar(spop), env = slider.env)
 
   clt.refresh <- function(...) {
      old.par <- par(oma = c(2.2, 0, 2, 0), mar=c(2,4,4,2), mgp=c(2,.5,0),
            mfrow = c(2, 2)) ; on.exit(par(old.par))
      n <- as.numeric(evalq(tclvalue(n), env = slider.env))
      spop <- as.numeric(evalq(tclvalue(spop), env = slider.env))
      repsn <- ceiling(reps/n)
       mats <- list(
          norm.mat = matrix(rnorm(n * repsn), ncol = n),
          exp.mat = matrix(rexp(n * repsn, 1/3), ncol = n),
          unif.mat <- matrix(runif(n * repsn), ncol = n),
          beta.mat <- matrix(rbeta(n * repsn, 0.35, 0.25), ncol = n))
       for(i in 1:4) {
          thismean <- rowMeans(mats[[i]])
          x <- seq(min(thismean), max(thismean), length = 50)
          tmp.hist <- hist(thismean, plot = FALSE, prob = TRUE, nclass = nclass)
          thismax <- max(tmp.hist$density, dnorm(x, mean(thismean), sd(thismean)))
          if(spop || n == 1) {
             plot(pops[[i]], freq=FALSE, main = plotnames[i], xlab = "", col = "pink", 
                border = "red", ylim = c(0, thismax))
             if(n > 1) {
                hist(thismean, freq=FALSE, col = "skyblue", nclass = nclass, add=TRUE)
                plot(pops[[i]], freq=FALSE, border="red", add=TRUE)
             }
          } else
             hist(thismean, freq=FALSE, main = plotnames[i], xlab = "", col = "skyblue",
                nclass = nclass, ylim = c(0, thismax))
          lines(x, dnorm(x, mean(thismean), sd(thismean)))
       }
       mtext(paste("sample size =", n), outer = TRUE, cex = 1.1)
    }
    clt.refresh()
    bringToTop()

    m <- tktoplevel()
    tkwm.title(m, "Central Limit Theorem")
    tkwm.geometry(m, "+0+0")

    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Sample size", width = "15"), side = "right")
    tkpack(sc <- tkscale(fr, command = clt.refresh, from = 1, to = 50, 
        orient = "horiz", resolution = 1, showvalue = T), side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = n), env = slider.env)

    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Show Population Distribution", width = "25"), side = "right")
    tkpack(sc <- tkcheckbutton(fr, command = clt.refresh), side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = spop), env = slider.env)
 
    tkpack(tkbutton(m, text = "Exit", command = function() tkdestroy(m)), 
        side = "right")
}

#####################################################################
#                                                                   #
#               Demonstration of Confidence Intervals               #
#                                                                   #
# Greg Snow's function, with t-distribution as the default          #
#####################################################################

   tosave <- c(tosave, "interact.confint")
interact.confint <- function (reps = 100, seed, method = "t", n = 25) 
{
    if (!missing(seed)) 
        set.seed(seed)
    if (!exists("slider.env")) 
        slider.env <<- new.env()
    library(tcltk)
    conf.level <- 0.95
    assign("conf.level", tclVar(conf.level), env = slider.env)

    data <- matrix(rnorm(n * reps, 100, 10), ncol = n)
    rmeans <- rowMeans(data)
    ci.refresh <- function(...) {
        conf.level <- as.numeric(evalq(tclvalue(conf.level), env = slider.env))
        switch(method, Z = , z = {
            lower <- qnorm((1 - conf.level)/2, rmeans, 10/sqrt(n))
            upper <- qnorm(1 - (1 - conf.level)/2, rmeans, 10/sqrt(n))
        }, T = , t = {
            cv.l <- qt((1 - conf.level)/2, n - 1)
            cv.u <- qt(1 - (1 - conf.level)/2, n - 1)
            rsds <- sqrt(apply(data, 1, var))/sqrt(n)
            lower <- rmeans + cv.l * rsds
            upper <- rmeans + cv.u * rsds
        }, BOTH = , Both = , both = {
            lz <- qnorm((1 - conf.level)/2, rmeans, 10/sqrt(n))
            uz <- qnorm(1 - (1 - conf.level)/2, rmeans, 10/sqrt(n))
            cv.l <- qt((1 - conf.level)/2, n - 1)
            cv.u <- qt(1 - (1 - conf.level)/2, n - 1)
            rsds <- sqrt(apply(data, 1, var))/sqrt(n)
            lt <- rmeans + cv.l * rsds
            ut <- rmeans + cv.u * rsds
            lower <- c(rbind(lt, lz, 100))
            upper <- c(rbind(ut, uz, 100))
            reps <- reps * 3
            rmeans <- rep(rmeans, each = 3)
            rmeans[c(F, F, T)] <- NA
        }, stop("method must be z, t, or both"))
        xr <- 100 + 4.5 * c(-1, 1) * 10/sqrt(n)
        plot(lower, seq(1, reps), type = "n", xlim = xr, xlab = "Confidence Interval", 
            ylab = "Index")
        abline(v = qnorm(c((1 - conf.level)/2, 1 - (1 - conf.level)/2), 
            100, 10/sqrt(n)), col = "lightgreen")
        if (method == "both" || method == "Both" || method == 
            "BOTH") {
            title(main = "Confidence intervals based on both distributions", 
                sub = "Upper interval is Z in each pair")
        }
        else {
            title(main = paste("Confidence intervals based on", 
                method, "distribution"))
        }
        colr <- ifelse(lower > 100, "blue", ifelse(upper < 100, 
            "red", "black"))
        abline(v = 100)
        segments(lower, 1:reps, upper, 1:reps, col = colr)
        points(rmeans, seq(along = rmeans), pch = "|", col = colr)
        invisible(NULL)
    }
    ci.refresh()
    bringToTop()

    m <- tktoplevel()
    tkwm.title(m, "Confidence Intervals")
    tkwm.geometry(m, "+0+0")

    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Confidence Level", width = "15"), side = "right")
    tkpack(sc <- tkscale(fr, command = ci.refresh, from = 0.5, to = 0.995, 
        orient = "horiz", resolution = 0.005, showvalue = T), side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = conf.level), env = slider.env)

    tkpack(tkbutton(m, text = "Exit", command = function() tkdestroy(m)), 
        side = "right")
}

#####################################################################
#                                                                   #
#           Interactive demo of power calculations                  #
#                                                                   #
# Based on Greg Snow's function 'run.power.examp', but now using t- #
# distribution with a noncentrality parameter, instead of normal.   #
# Also some cosmetic changes.                                       #
#####################################################################

   tosave <- c(tosave, "interact.power")
interact.power <- function (n = 3, stdev = 1, diff = 1, alpha = 0.05, xmin = -2, xmax = 4)
{
    require(tcltk)
    if (!exists("slider.env")) 
        slider.env <<- new.env()
    assign("n", tclVar(n), env = slider.env)
    assign("stdev", tclVar(stdev), env = slider.env)
    assign("diff", tclVar(diff), env = slider.env)
    assign("alpha", tclVar(alpha), env = slider.env)

    power.refresh <- function (...) {
        n <- as.numeric(evalq(tclvalue(n), env = slider.env))
        stdev <- as.numeric(evalq(tclvalue(stdev), env = slider.env))
        diff <- as.numeric(evalq(tclvalue(diff), env = slider.env))
        alpha <- as.numeric(evalq(tclvalue(alpha), env = slider.env))

        old.par <- par(mfrow = c(2, 1), oma = c(0, 0, 3.1, 0),
            mar=c(2,1,2,1)) ; on.exit(par(old.par))
        se <- stdev/sqrt(n)
        x <- seq(xmin, xmax, length = 100)
        r <- qt(1 - alpha, n-1) * se

        ymax <- dt(0, n-1) * 7/6
        ### Upper graph ###
        plot(x, dt(x/se,n-1), type = "n", ylim = c(0,ymax), xaxt="n", yaxt="n", 
            xaxs="i", yaxs="i",ylab = "", xlab="", main = "")
        title(main="Null Distribution", adj=0, cex.main=.9)
        polygon(c(r, r, x[x > r],max(x)), c(0, dt(c(r, x[x > r])/se, n-1),0), col = "pink")
        lines(x, dt(x/se, n-1), col = "red")
        lines(x=c(0,0),y=c(ymax,-1),xpd=T, lty=3)
        lines(x=c(r,r),y=c(ymax,-1),xpd=T, col="red")
        texpos <- -ymax*0.15
        text(r, texpos, xpd=T, labels="<-- rejection region -->", adj = 0, col="red")
        text(r,texpos, xpd=T, labels=signif(r,3), pos=2, col="red")
        legend("topright", xjust = 1, bty = "n", 
            fill = "pink", legend = expression(alpha))
        ### Lower graph ###
        plot(x, dt(x/se,n-1), type = "n", ylim = c(0,ymax), ylab = "", yaxt="n", 
            xaxs="i", yaxs="i",xlab="", main = "")
        title(main="Alternative Distribution", adj=0, cex.main=0.9)
#   With improved reporting of warnings under R 2.3.0, 'dt(*,ncp=)' produces reams of 
#   "full precision was not achieved" warnings, which I've simply suppressed pending 
#   a fix to 'dt()'!
#        polygon(c(r, r, x[x > r], max(x)), c(0, dt(c(r, x[x > r])/se, n-1,diff/se), 0), col = "lightblue")
        suppressWarnings(polygon(c(r, r, x[x > r], max(x)), c(0, dt(c(r, x[x > r])/se, n-1,diff/se), 0), col = "lightblue"))
#        lines(x, dt(x/se, n-1,diff/se), col = "blue")
        suppressWarnings(lines(x, dt(x/se, n-1,diff/se), col = "blue"))
        lines(x=c(0,0),y=c(0,99),xpd=T, lty=3)
        lines(x=c(r,r),y=c(0,99),xpd=T, col="red")
        legend("topright", xjust = 1, bty = "n", 
             fill = "lightblue", legend = "Power")
#        mtext(paste("se =", signif(se, 3), "     t* =", signif(r, 
#            3), "     power =", round(1 - pt(r/se, n-1, diff/se), 3), 
#            "\n n =", n, "     sd =", stdev, "     diff =", diff, 
#            "     alpha =", alpha), outer = TRUE, line = 0, cex = 1.2)
        suppressWarnings(mtext(paste("se =", signif(se, 3), "     t* =", signif(r, 
            3), "     power =", round(1 - pt(r/se, n-1, diff/se), 3), 
            "\n n =", n, "     sd =", stdev, "     diff =", diff, 
            "     alpha =", alpha), outer = TRUE, line = 0, cex = 1.2) )
    }
    power.refresh()
    bringToTop()

    m <- tktoplevel()
    tkwm.title(m, "Power t-test")
    tkwm.geometry(m, "+0+0")

    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Sample size", width = "15"), side = "right")
    tkpack(sc <- tkscale(fr, command = power.refresh, from = 2, to = 100, 
        orient = "horiz", resolution = 1, showvalue = T), side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = n), env = slider.env)

    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Standard Deviation", width = "15"), side = "right")
    tkpack(sc <- tkscale(fr, command = power.refresh, from = 0.25, to = 5, 
        orient = "horiz", resolution = 0.25, showvalue = T), side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = stdev), env = slider.env)

    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "True difference", width = "15"), side = "right")
    tkpack(sc <- tkscale(fr, command = power.refresh, from = -1, to = 3, 
        orient = "horiz", resolution = 0.1, showvalue = T), side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = diff), env = slider.env)

    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Alpha Level", width = "15"), side = "right")
    tkpack(sc <- tkscale(fr, command = power.refresh, from = 0.01, to = 0.99, 
        orient = "horiz", resolution = 0.01, showvalue = T), side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = alpha), env = slider.env)

    tkpack(tkbutton(m, text = "Exit", command = function() tkdestroy(m)), 
        side = "right")
}

#####################################################################
#                                                                   #
#     Interactive display of correlation coefficient r and r^2      #
#                                                                   #
# Closely based on Greg Snow's 'run.cor2.examp'                     #
#####################################################################

   tosave <- c(tosave, "interact.correlation")
interact.correlation <- function (n = 100, seed) 
{
    if (!missing(seed))
        set.seed(seed)
    if (!exists("slider.env")) 
        slider.env <<- new.env()
    library(tcltk)

    x <- scale(matrix(rnorm(2 * n, 0, 1), ncol = 2))
    x <- x %*% solve(chol(cor(x)))
    xr <- range(x)
    r.old <- r <- r2 <- 0
    assign("r", tclVar(r), env = slider.env)
    assign("r2", tclVar(r2), env = slider.env)

    cor.refresh <- function(...) {
        r <- as.numeric(evalq(tclvalue(r), env = slider.env))
        r2 <- as.numeric(evalq(tclvalue(r2), env = slider.env))
        if (r != r.old) {
            eval(parse(text = paste("tclvalue(r2)<-", r^2, sep = "")), env = slider.env)
            r.old <<- r
        }
        else {
            r <- ifelse(r < 0, -sqrt(r2), sqrt(r2))
            eval(parse(text = paste("tclvalue(r)<-", r, sep = "")), env = slider.env)
            r.old <<- as.numeric(evalq(tclvalue(r), env = slider.env)) # clumsy but effective
        }
        if (r == 1) {
            cmat <- matrix(c(1, 0, 1, 0), 2)
        }
        else if (r == -1) {
            cmat <- matrix(c(1, 0, -1, 0), 2)
        }
        else {
            cmat <- chol(matrix(c(1, r, r, 1), 2))
        }
        new.x <- x %*% cmat
        plot(new.x, xlab = "x", ylab = "y", xlim = xr, ylim = xr)
        title(paste("r = ", round(cor(new.x[, 1], new.x[, 2]), 
            3), "\nr^2 = ", round(r^2, 3)))
    }
    cor.refresh()
    bringToTop()

    m <- tktoplevel()
    tkwm.title(m, "Correlation Demo")
    tkwm.geometry(m, "+0+0")

    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Correlation (r)", width = "15"), side = "right")
    tkpack(sc <- tkscale(fr, command = cor.refresh, from = -1, 
        to = 1, orient = "horiz", resolution = 0.01, showvalue = T), side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = r), env = slider.env)
  
    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "r^2", width = "15"), side = "right")
    tkpack(sc <- tkscale(fr, command = cor.refresh, from = 0, 
        to = 1, orient = "horiz", resolution = 0.01, showvalue = T), side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = r2), env = slider.env)

    tkpack(tkbutton(m, text = "Exit", command = function() tkdestroy(m)), 
        side = "right")
}

#######################################################################
#                                                                     #
#         Interactive display of regression (or correlation)          #
#                                                                     #
# A development of Greg Snow's 'put.points.demo', with xlim and ylim  #
# changed to deal sensibly with user's 'x' and 'y' values. Clicking   #
# outside the user area of the plot adds (removes, etc) a point.      #
# Note that the 'LS' line plotted is a _regression_ of 'y' on 'x'.    #
# The function returns the final set of points plotted, invisibly.    #
#######################################################################

   tosave <- c(tosave, "interact.regression")
interact.regression <- function (x = NULL, y = NULL, lsline = TRUE) 
{
   isDemo <- is.null(x) || is.null(y)
   if(isDemo) {
      x <- y <- NULL
   } else {
      xlab <- deparse(substitute(x))
      ylab <- deparse(substitute(y))
      user.x <- x
      user.y <- y
   }
   convert <- function(xy) {
      cusr <- par("usr")
      cplt <- par("plt")
      x <- (xy$x - cusr[1])/(cusr[2] - cusr[1])
      x <- x * (cplt[2] - cplt[1]) + cplt[1]
      y <- (xy$y - cusr[3])/(cusr[4] - cusr[3])
      y <- y * (cplt[4] - cplt[3]) + cplt[3]
      return(list(x = x, y = y))
   }
   old.par <- par(no.readonly = T) ; on.exit(par(old.par))
   mode = "add"
   layout(matrix(c(2, 1), nrow = 1), width = c(3, 1))
   repeat {
      # setup "menu" area
      par(mar = c(0, 0, 0, 0), usr = c(0, 1, 0, 1))
      frame()
      box()
      abline(h = c(0.8, 0.6))
      text(rep(0.5, 5), c(0.9, 0.725, 0.525, 0.325, 0.125),lab = c("End", "LS Line", "Add Point", "Delete Point", "Move Point"))
      lines(c(0.25, 0.25, 0.75, 0.75, 0.25), c(0.85, 0.95, 0.95, 0.85, 0.85))
      points(rep(0.5, 4), c(0.675, 0.475, 0.275, 0.075), pch = c(ifelse(lsline, 7, 0),
         ifelse(mode == "add", 16, 1), ifelse(mode == "del", 16, 1), ifelse(mode == "mov", 16, 1)), cex = 2.5)
      # draw main plot area
      par(mar = c(5, 4, 4, 3) + 0.1) # right margin increased from 1 to 3
      if (length(x) == 0) {
         plot(5, 5, type = "n", xlim = c(0, 10), ylim = c(0,10), xlab = "x", ylab = "y")
      } else {
         if(isDemo)  {
            plot(x, y, xlab = "x", ylab = "y", xlim=range(0,10,x),ylim=range(0,10,y))
         } else {
            plot(x, y, xlim=range(x,user.x), ylim=range(y,user.y), xlab = xlab, ylab = ylab, type="n")
            points(user.x, user.y, pch=4, col="green")
            points(x, y)
         }
         if (lsline && length(x) > 1) {
            tmp.fit <- lm(y ~ x)
            abline(tmp.fit)
            title(paste("r =", round(cor(x, y), 2), "r^2 =", round(cor(x, y)^2, 2), 
                "\nSlope =", round(coef(tmp.fit)[2], 2), "Intercept =", round(coef(tmp.fit)[1], 2)))
         }
         else {
            title(paste("r =", round(cor(x, y), 4), "r^2 =", 
               round(cor(x, y)^2, 4)))
         }
      }
      locpnt <- locator(1)  
      locpnt2 <- convert(locpnt)
      if (locpnt2$x > 1) { # condition changed from locpnt to locpnt2, so click in margin adds point
         if (locpnt2$y > 0.8) {
            break
         }
         if (locpnt2$y > 0.6) {
            lsline <- !lsline
            next
         }
         if (locpnt2$y > 0.4) {
            mode <- "add"
            next
         }
         if (locpnt2$y > 0.2) {
            mode <- "del"
            next
         }
         mode <- "mov"
         next
      }
      else {
         if (mode == "add") {
            x <- c(x, locpnt$x)
            y <- c(y, locpnt$y)
            next
         }
         if (mode == "del") {
            min.i <- which.min((x - locpnt$x)^2 + (y - locpnt$y)^2)
            x <- x[-min.i]
            y <- y[-min.i]
            next
         }
         if (mode == "mov") {
            mov.i <- which.min((x - locpnt$x)^2 + (y - locpnt$y)^2)
            points(x[mov.i], y[mov.i], pch = 16)
            locpnt <- locator(1)
            x[mov.i] <- locpnt$x
            y[mov.i] <- locpnt$y
            next
        }
      }
   }
   invisible(cbind(x=x,y=y))
}

#######################################################################
#                                                                     #
#      Interactive display of Principle Components Analysis           #
#                                                                     #
#######################################################################

   tosave <- c(tosave, "interact.PCA")
interact.PCA <- function(x=rnorm(5)+3,y=rnorm(5)+2,grps=NULL) {
   mx <- mean(x) ; my <- mean(y)
   cx <- x-mx ; cy <- y-my
   r <- sqrt(cx^2+cy^2) ; th <- atan2(cy,cx)
   if(is.null(grps)) {
      grps <- 18
      mcex <- 1
   } else mcex <- 0.7
   plot(x,y,type="n", asp=1,xlab=deparse(substitute(x)),ylab=deparse(substitute(y)),
      main="Principle Components Analysis (PCA)", sub="Click to position 1st Principle Axis")
   points(x,y,pch=grps,cex=mcex) ; points(mx,my,pch=3,cex=2,col="lightgreen")
   repeat {
      l <- locator(1)
      if(is.null(l)) break
      lx <- l$x-mx ; ly <- l$y-my
      alp <- atan(ly/lx)
      b <- r * sin(th-alp) ; a <- r * cos(th-alp)
      px <- a*cos(alp)+mx ; py <- a*sin(alp)+my
      plot(x,y,type="n", asp=1,xlab=deparse(substitute(x)),ylab=deparse(substitute(y)),
         main=paste("Trial position of 1st Pinciple Axis\nSS =",round(sum(b^2),3)), 
         sub="Click to position 1st Principle Axis")
      points(x,y,pch=grps,cex=mcex) ; points(mx,my,pch=3,cex=2,col="lightgreen")
      gr <- ly/lx ; abline(my-gr*mx,gr,col="red")
      segments(x,y,px,py,lty=3,col="blue")
   }
   plot(x,y,type="n", asp=1,xlab=deparse(substitute(x)),ylab=deparse(substitute(y)),
      main=paste("First Principle Axis (blue)\nSS =",round(sum(b^2),3)), 
      sub="Click on axis to show scores")
   points(x,y,pch=grps,cex=mcex) ; points(mx,my,pch=3,cex=2,col="lightgreen")
   abline(my-gr*mx,gr,col="blue")
   points(px,py,pch=1,col="blue", xpd=TRUE)
   segments(x,y,px,py,lty=3,col="blue")
   ided <- identify(px,py,round(a,2), cex=0.7, xpd=TRUE)
   if(length(ided) == length(x))
      repeat {
         l <- locator(1)
         if(is.null(l)) break
      }
   plot(x,y,type="n", asp=1,xlab=deparse(substitute(x)),ylab=deparse(substitute(y)),
      main=paste("Second Principle Axis (red)\nSS =",round(sum(a^2),3)), 
      sub="Click on 2nd axis to show scores")
   points(x,y,pch=grps,cex=mcex) ; points(mx,my,pch=3,cex=2,col="lightgreen")
   abline(my-gr*mx,gr,col="blue")
   points(px,py,pch=1,col="blue", xpd=TRUE)
   gr <- -lx/ly ; abline(my-gr*mx,gr,col="red")
   alp2 <- alp + pi/2
   a2 <- r * cos(th-alp2)
   px <- a2 * cos(alp2)+mx ; py <- a2*sin(alp2)+my
   segments(x,y,px,py,lty=3,col="red")
   points(px,py,pch=1,col="red", xpd=TRUE)
   identify(px,py,round(a2,2), cex=0.7, xpd=TRUE)
   cbind("Original.x"=x, "Original.y"=y, "myComp.1"=a, "myComp.2"=b)
}

##########################################################################
#                                                                        #
#             Maximum Likelihood Estimator demonstrations                #
#                                                                        #
# The first, using a normal function to predict likelihood for mean and  #
# sd, is by Greg Snow. The second, using a Poisson likelihood function,  #
# is my adaptation.                                                      #
##########################################################################

   tosave <- c(tosave, "interact.mle.norm")
interact.mle.norm <- function(x = runif(10, 5, 15), start.mean = mean(x) - start.sd, 
        start.sd = 1.2 * sqrt(var(x))) {
   x <- mle.demo(x = x, start.mean = start.mean, start.sd = start.sd)
   n <- length(x) ; xbar <- mean(x)
   cat("Mean using sum(x)/n :          ", round(xbar,2), "\n")
   cat("S.d. using sum(squares)/(n-1) :", round(sqrt(sum((x-xbar)^2)/(n-1)),2), "\n")
   cat("S.d. using sum(squares)/ n :   ", round(sqrt(sum((x-xbar)^2)/n),2), "\n")
   invisible(x)
}

   tosave <- c(tosave, "mle.demo")
mle.demo <- function (x = rnorm(10, 10, 2), start.mean = mean(x) - start.sd, 
    start.sd = 1.2 * sqrt(var(x))) 
{
    if (!exists("slider.env")) 
        slider.env <<- new.env()
    library(tcltk)
    mu <- start.mean
    assign("mu", tclVar(mu), env = slider.env)
    sig <- start.sd
    assign("sig", tclVar(sig), env = slider.env)
    .mu <- .sig <- .ll <- numeric(0)
    mle.refresh <- function(...) {
        mu <- as.numeric(evalq(tclvalue(mu), env = slider.env))
        sig <- as.numeric(evalq(tclvalue(sig), env = slider.env))
        old.par <- par(no.readonly = T)
        on.exit(par(old.par))
        par(mar = c(5, 4, 0, 2) + 0.1)
        .mu <<- c(.mu, mu)
        .sig <<- c(.sig, sig)
        ll <- sum(dnorm(x, mu, sig, TRUE))
        .ll <<- c(.ll, ll)
        layout(matrix(1:3, ncol = 1), heights = c(2, 1, 1))
        xx <- seq(min(x) - 1.2 * (max(x) - min(x)), max(x) + 
            1.2 * (max(x) - min(x)), length = 250)
        plot(xx, dnorm(xx, mu, sig), type = "l", ylim = c(0, 
            dnorm(0, 0, 0.5 * sqrt(var(x)))), xlab = "x", ylab = "Likelihood")
        segments(x, 0, x, dnorm(x, mu, sig))
        points(x, dnorm(x, mu, sig))
        points(x, rep(0, length(x)), pch=16)
        text(xx[1], dnorm(0, 0, 0.5 * sqrt(var(x))) * 0.9, paste("Log Likelihood =", 
            format(ll, digit = 5)), adj = 0, cex = 3)
        plot(.mu, .ll, xlab = expression(mu), ylab = "Log Likelihood")
        points(mu, ll, pch = 16, col = "red")
        plot(.sig, .ll, xlab = expression(sigma), ylab = "Log Likelihood")
        points(sig, ll, pch = 16, col = "red")
    }
    mle.refresh()
    bringToTop()

    m <- tktoplevel()
    tkwm.title(m, "Maximum Likelihood Estimation")
    tkwm.geometry(m, "+0+0")
    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "mu", width = "10"), side = "right")
    tmp <- pretty(c(start.mean - 2 * start.sd, start.mean + 3 * 
        start.sd), 100)
    tkpack(sc <- tkscale(fr, command = mle.refresh, from = min(tmp), 
        to = max(tmp), orient = "horiz", resolution = tmp[2] - 
            tmp[1], showvalue = T), side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = mu), env = slider.env)
    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "sigma", width = "10"), side = "right")
    tmp <- pretty(c(0.5 * start.sd, 2 * start.sd), 100)
    tkpack(sc <- tkscale(fr, command = mle.refresh, from = min(tmp), 
        to = max(tmp), orient = "horiz", resolution = tmp[2] - 
            tmp[1], showvalue = T), side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = sig), env = slider.env)
    tkpack(tkbutton(m, text = "Refresh", command = mle.refresh), 
        side = "left")
    tkpack(tkbutton(m, text = "Exit", command = function() tkdestroy(m)), 
        side = "right")
    return(invisible(x))
}

    tosave <- c(tosave, "interact.mle.poiss")
interact.mle.poiss <- function (x = rpois(10, 3), start.lambda = 1.8) 
{
    # Poisson version of MLE demo
    if (!exists("slider.env")) 
        slider.env <<- new.env()
    library(tcltk)
    lamb <- start.lambda
    assign("lamb", tclVar(lamb), env = slider.env)
    .lamb <- .ll <- numeric(0)
    # 'x' values need to be stacked for plotting
    stopifnot(all(x >= 0))
    x <- round(x)
    xx <- seq(0,round(max(x)*1.2))
    xsk <- x
    for(i in 0:max(x)) {
        npt <- sum(x==i)
        if(npt > 1)
            xsk[x==i] <- seq(i-.25,i+.25,length=npt)
    }
    mle.refresh <- function(...) {
        lamb <- as.numeric(evalq(tclvalue(lamb), env = slider.env))
        old.par <- par(no.readonly = T)
        on.exit(par(old.par))
        par(mar = c(5, 4, 0, 2) + 0.1)
        .lamb <<- c(.lamb, lamb)
        ll <- sum(dpois(x, lamb, TRUE))
        .ll <<- c(.ll, ll)
        layout(matrix(1:2, ncol = 1), heights = c(3, 1))
        ylim <- range(0,dpois(xx, lamb)*1.2)
        plot(xx, dpois(xx, lamb), type = "n", ylim = ylim,
            xlim=c(-.5,max(xx)+.5), xlab = "x", ylab = "Probability")
        rect(xx-.4,0,xx+.4,dpois(xx,lamb),col="grey", border=FALSE)
        segments(xsk, 0, xsk, dpois(x, lamb), col="blue")
#        points(xsk, dpois(x, lamb), col="blue", pch="-")
        points(xsk, rep(0, length(x)), pch=16, col="blue")
        text(xx[1], ylim[2]* 0.9, paste("Log Likelihood =", 
            format(ll, digit = 5)), adj = 0, cex = 1.5)
        plot(.lamb, .ll, xlab = expression(lambda), ylab = "Log Likelihood")
        points(lamb, ll, pch = 16, col = "red")
    }
    mle.refresh()
    bringToTop()

    m <- tktoplevel()
    tkwm.title(m, "Maximum Likelihood Estimation")
    tkwm.geometry(m, "+0+0")
    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "lambda", width = "10"), side = "right")
    tkpack(sc <- tkscale(fr, command = mle.refresh, from = 0.1, 
        to = max(x), orient = "horiz", resolution = 0.1, showvalue = T), side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = lamb), env = slider.env)
    tkpack(tkbutton(m, text = "Refresh", command = mle.refresh), 
        side = "left")
    tkpack(tkbutton(m, text = "Exit", command = function() tkdestroy(m)), 
        side = "right")
    return(invisible(x))
}


############################################################################
#                                                                          #
#          The relationship between Probability and Likelihood             #
#                                                                          #
# Plot 3D graph showing relationship between probability and likelihood    #
# for Bernoilli trials. Cf Bolstad p.124                                   #
############################################################################

   tosave <- c(tosave, "interact.pbllh")
interact.pbllh <- function (size,show.3d=0,show.curve=0,show.llh=0) 
{
    require(scatterplot3d)
    require(tcltk)
    if (!exists("slider.env")) 
        slider.env <<- new.env()
    if(missing(size))
        repeat {
            choice <- winDialogString("How many trials? (Enter a positive integer)", "10")
            if(is.null(choice)) stop("Action cancelled by user")
            size <- round(as.numeric(choice))
            if(!is.na(size) && size > 0) break
        }
    y <- round(size/2)
    assign("y", tclVar(y), env = slider.env)
    p <- 0.5
    assign("p", tclVar(p), env = slider.env)
    s3d <- show.3d
    assign("s3d", tclVar(s3d), env = slider.env)
    scurve <- show.curve
    assign("scurve", tclVar(scurve), env = slider.env)
    sllh <- show.llh
    assign("sllh", tclVar(sllh), env = slider.env)
    pbllh.refresh <- function(...) {
        y <- as.numeric(evalq(tclvalue(y), env = slider.env))
        p <- as.numeric(evalq(tclvalue(p), env = slider.env))
        s3d <- as.numeric(evalq(tclvalue(s3d), env = slider.env))
        scurve <- as.numeric(evalq(tclvalue(scurve), env = slider.env))
        sllh <- as.numeric(evalq(tclvalue(sllh), env = slider.env))

        old.par <- par(no.readonly=T) ; on.exit(par(old.par))
        layout(rbind(c(1,3),c(2,0)), heights=c(2,1), widths=c(1,1))

        # This does the 3d bit:
        if(s3d || scurve) {
            zmax <- 0.5
            pb <- seq(0,1,length=50)
            sp <- scatterplot3d(x=c(0,size), y=c(0,1), z = c(0,zmax), xlab="No. of Successes (y)", 
                   ylab="", zlab="", z.ticklabs=NA)
            dr <- sp$xyz.convert
            for(suc in 0:size) {
               if(scurve) {
                   zz <- pmin(c(0, dbinom(suc,size,pb), 0,0),zmax)
                   yy <- c(0, pb, 1,0)
                   xx <- rep(suc,length(yy))
                   polygon(dr(xx,yy,zz), col=(if(sllh && suc==y) "pink" else "lightblue"))
               }
               if(s3d)
                   sp$points3d(c(suc,suc),c(p,p),c(0,dbinom(suc,size,p)),
                       type="l", col="red",lwd=2)
            }
            sp$box3d()
            text(par("usr")[2],0,"p", adj=0, xpd=TRUE)
        } else plot(0,0,type="n", xaxt="n",yaxt="n",xlab="",ylab="",bty="n") # plot nothing!
        # probability plot:
        par(mar=c(4,7,2,4), mgp=c(2,.8,0))
        plot(0:size,dbinom(0:size,size,p),type="h",col="red",lwd=2,
           xlab="No. of Successes (y)", ylab="Probability", yaxs="i", bty="l",
           main=paste("Probability of \'y\' given p =",p))
        # likelihood plot:
        if(sllh) {
            par(mar=c(6,8,9,5))
            pb <- seq(0,1,length=50)
            llh <- dbinom(y,size,pb)
            plot(pb,llh, type="n", xaxs="i", yaxs="i", ylab="Likelihood", xlab="p",
               bty="l", main=paste("Likelihood of \'p\' given y =",y))
            polygon(c(0,pb,1),c(0,llh,0),col="pink")
        } else plot(0,0,type="n", xaxt="n",yaxt="n",xlab="",ylab="",bty="n") # plot nothing!
    }
    pbllh.refresh()
    bringToTop()

    m <- tktoplevel()
    tkwm.title(m, "Probability vs Likelihood")
    tkwm.geometry(m, "+0+0")

    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Prob. of success in 1 trial (p)", width = "25"), side = "right")
    tkpack(sc <- tkscale(fr, command = pbllh.refresh, from = 0, 
        to = 1, orient = "horiz", resolution = 0.02, showvalue = T), side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = p), env = slider.env)

    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "No. of Successes (y)", width = "25"), side = "right")
    tkpack(sc <- tkscale(fr, command = pbllh.refresh, from = 0, 
        to = size, orient = "horiz", resolution = 1, showvalue = T), side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = y), env = slider.env)

    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Show 3d Probability", width = "25"), side = "right")
    tkpack(sc <- tkcheckbutton(fr, command = pbllh.refresh), side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = s3d), env = slider.env)

    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Show 3d Curve", width = "25"), side = "right")
    tkpack(sc <- tkcheckbutton(fr, command = pbllh.refresh), side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = scurve), env = slider.env)

    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Show Likelihood", width = "25"), side = "right")
    tkpack(sc <- tkcheckbutton(fr, command = pbllh.refresh), side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = sllh), env = slider.env)

    tkpack(tkbutton(m, text = "Refresh", command = pbllh.refresh), 
        side = "left")
    tkpack(tkbutton(m, text = "Exit", command = function() tkdestroy(m)), 
        side = "right")
}

##########################################################################
#                                                                        #
#                        DISTRIBUTION FUNCTIONS                          #
#                                                                        #
# The plots of basic probability distributions from Greg Snow's package  #
# 'TeachingDemos', reprised here so that they can be run without needing #
# to install the package (and its zillions of 'required' friends!).      #
##########################################################################

   tosave <- c(tosave, "vis.normal")
vis.normal <- function () 
{
    if (!exists("slider.env")) 
        slider.env <<- new.env()
    library(tcltk)
    mu <- 0
    assign("mu", tclVar(mu), env = slider.env)
    sd <- 1
    assign("sd", tclVar(sd), env = slider.env)
    s2 <- 1
    assign("s2", tclVar(s2), env = slider.env)
    xmin <- -5
    assign("xmin", tclVar(xmin), env = slider.env)
    xmax <- 5
    assign("xmax", tclVar(xmax), env = slider.env)
    ymin <- 0
    assign("ymin", tclVar(ymin), env = slider.env)
    ymax <- round(dnorm(0, 0, 0.5), 2)
    assign("ymax", tclVar(ymax), env = slider.env)
    sd.old <- sd
    s2.old <- s2
    norm.refresh <- function(...) {
        mu <- as.numeric(evalq(tclvalue(mu), env = slider.env))
        sd <- as.numeric(evalq(tclvalue(sd), env = slider.env))
        s2 <- as.numeric(evalq(tclvalue(s2), env = slider.env))
        if (sd != sd.old) {
            s2 <- round(sd^2, 5)
            try(eval(parse(text = paste("tclvalue(s2)<-", s2, 
                sep = "")), env = slider.env))
            sd.old <<- sd
            s2.old <<- s2
        }
        if (s2 != s2.old) {
            s2 <- as.numeric(evalq(tclvalue(s2), env = slider.env))
            sd <- round(sqrt(s2), 5)
            try(eval(parse(text = paste("tclvalue(sd)<-", sd, 
                sep = "")), env = slider.env))
            sd.old <<- sd
            s2.old <<- s2
        }
        xmin <- as.numeric(evalq(tclvalue(xmin), env = slider.env))
        xmax <- as.numeric(evalq(tclvalue(xmax), env = slider.env))
        ymin <- as.numeric(evalq(tclvalue(ymin), env = slider.env))
        ymax <- as.numeric(evalq(tclvalue(ymax), env = slider.env))
        xx <- seq(xmin, xmax, length = 500)
        yy <- dnorm(xx, mu, sd)
        plot(xx, yy, type = "l", xlim = c(xmin, xmax), ylim = c(ymin, 
            ymax), ylab = "", xlab = "x")
        lines(c(mu, mu), c(par("usr")[3], dnorm(0, 0, sd)), lty = 2, 
            col = "blue")
        lines(c(mu, mu + sd), dnorm(sd, 0, sd) * c(1, 1), lty = 2, 
            col = "blue")
    }
    norm.refresh()
    bringToTop()

    m <- tktoplevel()
    tkwm.title(m, "Normal Distribution")
    tkwm.geometry(m, "+0+0")
    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Mean", width = "20"), side = "right")
    tkpack(sc <- tkscale(fr, command = norm.refresh, from = -3, 
        to = 3, orient = "horiz", resolution = 0.1, showvalue = T), 
        side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = mu), env = slider.env)
    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Standard Deviation", width = "20"), 
        side = "right")
    tkpack(sc <- tkscale(fr, command = norm.refresh, from = 0.5, 
        to = 3, orient = "horiz", resolution = 0.1, showvalue = T), 
        side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = sd), env = slider.env)
    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Variance", width = "20"), side = "right")
    tkpack(sc <- tkscale(fr, command = norm.refresh, from = 0.25, 
        to = 9, orient = "horiz", resolution = 0.1, showvalue = T), 
        side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = s2), env = slider.env)
    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Xmin:", width = 6), side = "left")
    tkpack(e <- tkentry(fr, width = 8), side = "left")
    assign("e", e, env = slider.env)
    evalq(tkconfigure(e, textvariable = xmin), env = slider.env)
    tkpack(tklabel(fr, text = "Xmax:", width = 6), side = "left")
    tkpack(e <- tkentry(fr, width = 8), side = "left")
    assign("e", e, env = slider.env)
    evalq(tkconfigure(e, textvariable = xmax), env = slider.env)
    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Ymin:", width = 6), side = "left")
    tkpack(e <- tkentry(fr, width = 8), side = "left")
    assign("e", e, env = slider.env)
    evalq(tkconfigure(e, textvariable = ymin), env = slider.env)
    tkpack(tklabel(fr, text = "Ymax:", width = 6), side = "left")
    tkpack(e <- tkentry(fr, width = 8), side = "left")
    assign("e", e, env = slider.env)
    evalq(tkconfigure(e, textvariable = ymax), env = slider.env)
    tkpack(tkbutton(m, text = "Refresh", command = norm.refresh), 
        side = "left")
    tkpack(tkbutton(m, text = "Exit", command = function() tkdestroy(m)), 
        side = "right")
}

   tosave <- c(tosave, "vis.t")
vis.t <- function () 
{
    if (!exists("slider.env")) 
        slider.env <<- new.env()
    library(tcltk)
    df <- 1
    assign("df", tclVar(df), env = slider.env)
    sn <- 0
    assign("sn", tclVar(sn), env = slider.env)
    xmin <- -5
    assign("xmin", tclVar(xmin), env = slider.env)
    xmax <- 5
    assign("xmax", tclVar(xmax), env = slider.env)
    ymin <- 0
    assign("ymin", tclVar(ymin), env = slider.env)
    ymax <- round(dnorm(0, 0, 1), 2)
    assign("ymax", tclVar(ymax), env = slider.env)
    t.refresh <- function(...) {
        df <- as.numeric(evalq(tclvalue(df), env = slider.env))
        sn <- as.numeric(evalq(tclvalue(sn), env = slider.env))
        xmin <- as.numeric(evalq(tclvalue(xmin), env = slider.env))
        xmax <- as.numeric(evalq(tclvalue(xmax), env = slider.env))
        ymin <- as.numeric(evalq(tclvalue(ymin), env = slider.env))
        ymax <- as.numeric(evalq(tclvalue(ymax), env = slider.env))
        xx <- seq(xmin, xmax, length = 500)
        yyt <- dt(xx, df)
        if (sn) {
            yyn <- dnorm(xx)
            plot(xx, yyn, lwd = 3, col = "skyblue", type = "l", 
                xlim = c(xmin, xmax), ylim = c(ymin, ymax), xlab = "x", 
                ylab = "")
            lines(xx, yyt, lwd = 2)
        }
        else {
            plot(xx, yyt, type = "l", xlim = c(xmin, xmax), ylim = c(ymin, 
                ymax), ylab = "", xlab = "x", lwd = 2)
        }
    }
    t.refresh()
    bringToTop()

    m <- tktoplevel()
    tkwm.title(m, "Student\'s t Distribution")
    tkwm.geometry(m, "+0+0")
    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "d.f.", width = "5"), side = "right")
    tkpack(sc <- tkscale(fr, command = t.refresh, from = 1, to = 50, 
        orient = "horiz", resolution = 1, showvalue = T), side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = df), env = slider.env)
    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Show Normal Distribution", width = "25"), 
        side = "right")
    tkpack(sc <- tkcheckbutton(fr, command = t.refresh), side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = sn), env = slider.env)
    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Xmin:", width = 6), side = "left")
    tkpack(e <- tkentry(fr, width = 8), side = "left")
    assign("e", e, env = slider.env)
    evalq(tkconfigure(e, textvariable = xmin), env = slider.env)
    tkpack(tklabel(fr, text = "Xmax:", width = 6), side = "left")
    tkpack(e <- tkentry(fr, width = 8), side = "left")
    assign("e", e, env = slider.env)
    evalq(tkconfigure(e, textvariable = xmax), env = slider.env)
    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Ymin:", width = 6), side = "left")
    tkpack(e <- tkentry(fr, width = 8), side = "left")
    assign("e", e, env = slider.env)
    evalq(tkconfigure(e, textvariable = ymin), env = slider.env)
    tkpack(tklabel(fr, text = "Ymax:", width = 6), side = "left")
    tkpack(e <- tkentry(fr, width = 8), side = "left")
    assign("e", e, env = slider.env)
    evalq(tkconfigure(e, textvariable = ymax), env = slider.env)
    tkpack(tkbutton(m, text = "Refresh", command = t.refresh), 
        side = "left")
    tkpack(tkbutton(m, text = "Exit", command = function() tkdestroy(m)), 
        side = "right")
}

   tosave <- c(tosave, "vis.binom")
vis.binom <- function () 
{
    if (!exists("slider.env")) 
        slider.env <<- new.env()
    library(tcltk)
    n <- 10
    assign("n", tclVar(n), env = slider.env)
    p <- 0.5
    assign("p", tclVar(p), env = slider.env)
    sn <- 0
    assign("sn", tclVar(sn), env = slider.env)
    sp <- 0
    assign("sp", tclVar(sp), env = slider.env)
    binom.refresh <- function(...) {
        n <- as.numeric(evalq(tclvalue(n), env = slider.env))
        p <- as.numeric(evalq(tclvalue(p), env = slider.env))
        sn <- as.numeric(evalq(tclvalue(sn), env = slider.env))
        sp <- as.numeric(evalq(tclvalue(sp), env = slider.env))
        mu <- p * n
        sd <- sqrt(n * p * (1 - p))
        if (sn) {
            xx <- seq(-1, n + 1, length = 250)
            plot(xx, dnorm(xx, mu, sd), type = "l", col = "green", 
                ylim = range(0, dnorm(mu, mu, sd), dbinom(seq(0, 
                  n), n, p)), xlab = "x", ylab = "Probability")
            if (sp) {
                points(seq(0, n), dpois(seq(0, n), mu), type = "h", 
                  col = "blue")
                points(seq(0, n), dpois(seq(0, n), mu), pch = "-", 
                  col = "blue", cex = 2)
            }
            abline(h = 0)
            lines(xx, dnorm(xx, mu, sd), col = "green")
            points(seq(0, n), dbinom(seq(0, n), n, p), type = "h")
            points(seq(0, n), dbinom(seq(0, n), n, p), type = "p")
        }
        else {
            if (sp) {
                plot(seq(0, n), dpois(seq(0, n), mu), type = "h", 
                  col = "blue", xlim = c(-1, n + 1), xlab = "x", 
                  ylab = "Probability", ylim = range(0, dpois(seq(0, 
                    n), mu), dbinom(seq(0, n), n, p)))
                points(seq(0, n), dpois(seq(0, n), mu), pch = "-", 
                  col = "blue", cex = 2)
                points(seq(0, n), dbinom(seq(0, n), n, p), type = "h")
            }
            else {
                plot(seq(0, n), dbinom(seq(0, n), n, p), type = "h", 
                  xlim = c(-1, n + 1), xlab = "x", ylab = "Probability")
            }
            abline(h = 0)
            points(seq(0, n), dbinom(seq(0, n), n, p))
        }
        title(paste("Mean =", round(mu, 3), "Std. Dev. =", round(sd, 
            3)))
    }
    binom.refresh()
    bringToTop()

    m <- tktoplevel()
    tkwm.title(m, "Binomial Distribution")
    tkwm.geometry(m, "+0+0")
    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "n", width = "10"), side = "right")
    tkpack(sc <- tkscale(fr, command = binom.refresh, from = 1, 
        to = 100, orient = "horiz", resolution = 1, showvalue = T), 
        side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = n), env = slider.env)
    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "p", width = "10"), side = "right")
    tkpack(sc <- tkscale(fr, command = binom.refresh, from = 0, 
        to = 1, orient = "horiz", resolution = 0.01, showvalue = T), 
        side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = p), env = slider.env)
    tkpack(fr <- tkframe(m), side = "top")
    tkpack(sc <- tkcheckbutton(fr, command = binom.refresh), 
        side = "left")
    tkpack(tklabel(fr, text = "Show Normal Approximation", width = "25"), 
        side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = sn), env = slider.env)
    tkpack(fr <- tkframe(m), side = "top")
    tkpack(sc <- tkcheckbutton(fr, command = binom.refresh), 
        side = "left")
    tkpack(tklabel(fr, text = "Show Poisson Approximation", width = "25"), 
        side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = sp), env = slider.env)
    tkpack(tkbutton(m, text = "Refresh", command = binom.refresh), 
        side = "left")
    tkpack(tkbutton(m, text = "Exit", command = function() tkdestroy(m)), 
        side = "right")
}


# these last ones are mine, but modified from Greg Snow's functions:
   tosave <- c(tosave, "vis.poiss")
vis.poiss <- function () 
{
    if (!exists("slider.env")) 
        slider.env <<- new.env()
    library(tcltk)
    n <- 20
    lambda <- 1.8
    assign("lambda", tclVar(lambda), env = slider.env)
    sn <- 0
    assign("sn", tclVar(sn), env = slider.env)
    poiss.refresh <- function(...) {
        lambda <- as.numeric(evalq(tclvalue(lambda), env = slider.env))
        sn <- as.numeric(evalq(tclvalue(sn), env = slider.env))
        plot(seq(0, n), dpois(seq(0, n), lambda), type = "h", 
             col = "blue", xlim = c(-1, n + 1), xlab = "x", 
             ylab = "Probability", ylim=range(0,dpois(seq(0, n), lambda)*1.1))
        if (sn) {
            xx <- seq(-1, n + 1, length = 250)
            lines(xx, dnorm(xx, lambda, sqrt(lambda)), col = "green")
        } 
        points(seq(0, n), dpois(seq(0, n), lambda), pch = "-", 
              col = "blue", cex = 2)
        abline(h=0)
        title(paste("Lambda =", round(lambda, 3)))
    }
    poiss.refresh()
    bringToTop()

    m <- tktoplevel()
    tkwm.title(m, "Poisson Distribution")
    tkwm.geometry(m, "+0+0")

    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "lambda", width = "10"), side = "right")
    tkpack(sc <- tkscale(fr, command = poiss.refresh, from = 0.1, 
        to = n, orient = "horiz", resolution = 0.1, showvalue = T), 
        side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = lambda), env = slider.env)

    tkpack(fr <- tkframe(m), side = "top")
    tkpack(sc <- tkcheckbutton(fr, command = poiss.refresh), 
        side = "left")
    tkpack(tklabel(fr, text = "Show Normal Approximation", width = "25"), 
        side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = sn), env = slider.env)

    tkpack(tkbutton(m, text = "Exit", command = function() tkdestroy(m)), 
        side = "right")
}

   tosave <- c(tosave, "vis.chisq")
vis.chisq <- function () 
{
    if (!exists("slider.env")) 
        slider.env <<- new.env()
    library(tcltk)
    df <- 1
    assign("df", tclVar(df), env = slider.env)
    xmin <- 0
    assign("xmin", tclVar(xmin), env = slider.env)
    xmax <- 5
    assign("xmax", tclVar(xmax), env = slider.env)
    ymin <- 0
    assign("ymin", tclVar(ymin), env = slider.env)
    ymax <- 4  #  round(dchisq(0, 1), 2)
    assign("ymax", tclVar(ymax), env = slider.env)
    t.refresh <- function(...) {
        df <- as.numeric(evalq(tclvalue(df), env = slider.env))
        xmin <- as.numeric(evalq(tclvalue(xmin), env = slider.env))
        xmax <- as.numeric(evalq(tclvalue(xmax), env = slider.env))
        ymin <- as.numeric(evalq(tclvalue(ymin), env = slider.env))
        ymax <- as.numeric(evalq(tclvalue(ymax), env = slider.env))
        xx <- seq(xmin, xmax, length = 500)
        yyt <- dchisq(xx, df)
        plot(xx, yyt, type = "l", xlim = c(xmin, xmax), ylim = c(ymin, 
            ymax), ylab = "", xlab = "x", lwd = 2, main="Chi-squared distribution")
    }
    t.refresh()
    bringToTop()

    m <- tktoplevel()
    tkwm.title(m, "Chisquared Distribution")
    tkwm.geometry(m, "+0+0")
    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "d.f.", width = "5"), side = "right")
    tkpack(sc <- tkscale(fr, command = t.refresh, from = 1, to = 50, 
        orient = "horiz", resolution = 1, showvalue = T), side = "left")
    assign("sc", sc, env = slider.env)
    evalq(tkconfigure(sc, variable = df), env = slider.env)
    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Xmin:", width = 6), side = "left")
    tkpack(e <- tkentry(fr, width = 8), side = "left")
    assign("e", e, env = slider.env)
    evalq(tkconfigure(e, textvariable = xmin), env = slider.env)
    tkpack(tklabel(fr, text = "Xmax:", width = 6), side = "left")
    tkpack(e <- tkentry(fr, width = 8), side = "left")
    assign("e", e, env = slider.env)
    evalq(tkconfigure(e, textvariable = xmax), env = slider.env)
    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Ymin:", width = 6), side = "left")
    tkpack(e <- tkentry(fr, width = 8), side = "left")
    assign("e", e, env = slider.env)
    evalq(tkconfigure(e, textvariable = ymin), env = slider.env)
    tkpack(tklabel(fr, text = "Ymax:", width = 6), side = "left")
    tkpack(e <- tkentry(fr, width = 8), side = "left")
    assign("e", e, env = slider.env)
    evalq(tkconfigure(e, textvariable = ymax), env = slider.env)
    tkpack(tkbutton(m, text = "Refresh", command = t.refresh), 
        side = "left")
    tkpack(tkbutton(m, text = "Exit", command = function() tkdestroy(m)), 
        side = "right")
}

#####################################################################################

save(list=tosave, file="interact_fun.Rda")
rm(list=tosave)
rm("tosave")
try(detach("file:interact_fun.Rda"), silent=T)
attach(what="interact_fun.Rda")

