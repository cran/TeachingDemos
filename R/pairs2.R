pairs2 <- function (x, y, labels, panel = points, ...,
                    row1attop = TRUE, gap = 1) {
    localAxis <- function(side, x, y, xpd, bg, col = NULL, main,
                          oma, .. ) {
        if (side%%2 == 1)
            Axis(x, side = side, xpd = NA, ...)
        else Axis(y, side = side, xpd = NA, ...)
    }
    localPlot <- function(..., main, oma, font.main, cex.main) plot(...)
    localPanel <- function(..., main, oma, font.main, cex.main) panel(...)

    dots <- list(...)
    nmdots <- names(dots)
    if (!is.matrix(x)) {
        x <- as.data.frame(x)
        for (i in seq_along(names(x))) {
            if (is.factor(x[[i]]) || is.logical(x[[i]]))
                x[[i]] <- as.numeric(x[[i]])
            if (!is.numeric(unclass(x[[i]])))
                stop("non-numeric argument to 'pairs'")
        }
    } else if (!is.numeric(x))
        stop("non-numeric argument to 'pairs'")

    if (!is.matrix(y)) {
        y <- as.data.frame(y)
        for (i in seq_along(names(y))) {
            if (is.factor(y[[i]]) || is.logical(y[[i]]))
                y[[i]] <- as.numeric(y[[i]])
            if (!is.numeric(unclass(y[[i]])))
                stop("non-numeric argument to 'pairs'")
        }
    } else if (!is.numeric(x))
        stop("non-numeric argument to 'pairs'")

    panel <- match.fun(panel)

    nc.x <- ncol(x)
    nc.y <- ncol(y)
    if (nc.x < 2)
        stop("only one column in the 'x' argument to 'pairs'")
    if (nc.y < 2)
        stop("only one column in the 'y' argument to 'pairs'")
    has.labs <- TRUE
    if (missing(labels)) {
        labels <- colnames(x)
        if (is.null(labels))
            labels <- paste("var", 1:nc)
    }
    else if (is.null(labels))
        has.labs <- FALSE
    oma <- if ("oma" %in% nmdots)
        dots$oma
    else NULL
    main <- if ("main" %in% nmdots)
        dots$main
    else NULL
    if (is.null(oma)) {
        oma <- c(4, 4, 4, 4)
        if (!is.null(main))
            oma[3] <- 6
    }
    opar <- par(mfrow = c(nc, nc), mar = rep.int(gap/2, 4), oma = oma)
    on.exit(par(opar))
    for (i in if (row1attop)
        1:nc
    else nc:1) for (j in 1:nc) {
        localPlot(x[, j], x[, i], xlab = "", ylab = "", axes = FALSE,
            type = "n", ...)
        if (i == j || (i < j && has.lower) || (i > j && has.upper)) {
            box()
            if (i == 1 && (!(j%%2) || !has.upper || !has.lower))
                localAxis(1 + 2 * row1attop, x[, j], x[, i],
                  ...)
            if (i == nc && (j%%2 || !has.upper || !has.lower))
                localAxis(3 - 2 * row1attop, x[, j], x[, i],
                  ...)
            if (j == 1 && (!(i%%2) || !has.upper || !has.lower))
                localAxis(2, x[, j], x[, i], ...)
            if (j == nc && (i%%2 || !has.upper || !has.lower))
                localAxis(4, x[, j], x[, i], ...)
            mfg <- par("mfg")
            if (i == j) {
                if (has.diag)
                  localDiagPanel(as.vector(x[, i]), ...)
                if (has.labs) {
                  par(usr = c(0, 1, 0, 1))
                  if (is.null(cex.labels)) {
                    l.wid <- strwidth(labels, "user")
                    cex.labels <- max(0.8, min(2, 0.9/max(l.wid)))
                  }
                  text.panel(0.5, label.pos, labels[i], cex = cex.labels,
                    font = font.labels)
                }
            }
            else if (i < j)
                localLowerPanel(as.vector(x[, j]), as.vector(x[,
                  i]), ...)
            else localUpperPanel(as.vector(x[, j]), as.vector(x[,
                i]), ...)
            if (any(par("mfg") != mfg))
                stop("the 'panel' function made a new plot")
        }
        else par(new = FALSE)
    }
    if (!is.null(main)) {
        font.main <- if ("font.main" %in% nmdots)
            dots$font.main
        else par("font.main")
        cex.main <- if ("cex.main" %in% nmdots)
            dots$cex.main
        else par("cex.main")
        mtext(main, 3, 3, TRUE, 0.5, cex = cex.main, font = font.main)
    }
    invisible(NULL)
}
