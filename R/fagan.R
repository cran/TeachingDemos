fagan.plot<-function(probs.pre.test, LR, test.result="+") {
    opar <- par(no.readonly = T)
    on.exit(par = opar)
    par(mar = c(1.5, 6, 2, 6))
    stato <- ifelse(test.result == "+", "disease", "no disease")
    if (probs.pre.test > 1 | probs.pre.test < 0 | LR < 0 | is.infinite(LR) |
        is.nan(LR) | test.result %in% c("+", "-") == F)
        stop("wrong values !!")
    else logits <- function(p) log(p/(1 - p))
    logits.pre <- logits(probs.pre.test)
    logits.post <- log(LR) + logits.pre
    probs.post.test <- exp(logits.post)/(1 + exp(logits.post))
    compl.logit.pre <- logits(1 - probs.pre.test)
    LR.vec <- c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2,
        0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000)
    prob.vec <- c(0.001, 0.002, 0.003, 0.005, 0.007, 0.01, 0.02,
        0.03, 0.05, 0.07, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7,
        0.8, 0.9, 0.93, 0.95, 0.97, 0.98, 0.99, 0.993, 0.995,
        0.997, 0.998, 0.999)
    plot(0, 0, type = "n", ylim = range(logits(prob.vec)), axes = F,
        xlab = "", ylab = "")
    axis(2, rev(logits(prob.vec)), 100 * prob.vec, pos = -1,
        las = 1, cex.axis = 0.7)
    axis(2, rev(logits(prob.vec)), 100 * prob.vec, pos = -1,
        tck = 0.03, labels = F)
    axis(4, logits(prob.vec), 100 * prob.vec, pos = 1, las = 1,
        cex.axis = 0.7)
    axis(4, logits(prob.vec), 100 * prob.vec, pos = 1, tck = 0.03,
        labels = F)
    axis(2, log(LR.vec[1:10])/2, LR.vec[1:10], pos = 0, las = 1,
        cex.axis = 0.7)
    axis(2, log(LR.vec[1:10])/2, LR.vec[1:10], pos = 0, tck = 0.03,
        labels = F)
    axis(4, log(LR.vec[10:19])/2, LR.vec[10:19], pos = 0, las = 1,
        cex.axis = 0.7)
    axis(4, log(LR.vec[10:19])/2, LR.vec[10:19], pos = 0, tck = 0.03,
        labels = F)
    text(0, 4.5, "Likelihood ratio", cex = 1.2)
    segments(-1, compl.logit.pre, 1, logits.post, lwd = 1.5,
        col = 2)
    mtext(side = 2, text = "Pre test probability(%)", line = 2,
        cex = 1.2)
    mtext(side = 4, text = "Post test probability(%)", line = 2,
        cex = 1.2, las = 3)
    title(main = "Fagan's nomogram")
    text(0, -6.3, paste("Pre test prob. of disease =", round(100 *
        probs.pre.test, 2), "% \n", "Likelihood ratio ", "=",
        round(LR, 2), "\n", "Post test prob. of", stato, "=",
        ifelse(test.result == "+", round(100 * probs.post.test,
            2), round(100 * (1 - probs.post.test), 2)), "%"),
        cex = 0.7)
}



plotFagan<-function(){
  refresh.code <- function(...) {
    probs.pre.test <- slider(no = 1)
    LR <- slider(no = 2)
    test.result <- slider(obj.name = "test.result")
    fagan.plot(probs.pre.test, LR, test.result)
  }
  slider(refresh.code, sl.names =
         c("pre test probability", "Likelihood Ratio"),
         sl.mins = c(0, 0.01), sl.maxs = c(1, 100),
         title = "Bayes nomogram",
         sl.defaults = c(0.5, 1),
         sl.deltas = c(0.01, 0.01), but.functions = list(function(...) {
           slider(obj.name = "test.result", obj.value = "+")
           refresh.code()
         }, function(...) {
           slider(obj.name = "test.result", obj.value = "-")
           refresh.code()
         }), but.names = c("positive result", "negative result"))
  slider(obj.name = "test.result", obj.value = "+")
  invisible(NULL)
}


plotFagan2<-function(){
  refresh.code <- function(...) {
    probs.pre.test <- slider(no = 1)
    LR <- slider(no=2)/(1-slider(no=3))
    test.result <- slider(obj.name = "test.result")
    fagan.plot(probs.pre.test, LR, test.result)
  }
  slider(refresh.code, sl.names = c("pre test probability",
                         "Sensitivity","Specificity"), sl.mins = c(0, 0, 0),
         sl.maxs = c(1, 1, 1), title = "Bayes nomogram",
         sl.defaults = c(0.5, 0.95, 0.95),
         sl.deltas = c(0.01, 0.001, 0.001),
         but.functions = list(function(...) {
           slider(obj.name = "test.result", obj.value = "+")
           refresh.code()
         }, function(...) {
           slider(obj.name = "test.result", obj.value = "-")
           refresh.code()
         }), but.names = c("positive result", "negative result"))
  slider(obj.name = "test.result", obj.value = "+")
  invisible(NULL)
}


