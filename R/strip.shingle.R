"strip.shingle" <-
function (which.given, which.panel, var.name, factor.levels, 
    shingle.intervals, strip.names = c(FALSE, TRUE), style = 1, 
    bg = trellis.par.get("strip.background")$col[which.given], 
    fg = trellis.par.get("strip.shingle")$col[which.given], par.strip.text = trellis.par.get("add.text")) 
{
    pushViewport(viewport(y = (which.given - 0.5)/length(which.panel), 
        height = 1/length(which.panel), name = paste("strip.default", 
            which.given, sep = ".")))
    name <- var.name[which.given]
    level <- which.panel[which.given]
    strip.names <- rep(strip.names, length = 2)
    if (is.null(factor.levels)) {
        if (is.null(shingle.intervals)) 
            stop("both factor.levels and shingle.intervals cannot be NULL")
        strip.names <- strip.names[2]
        grid.rect(gp = gpar(fill = bg, col = bg))
        t <- range(shingle.intervals)
        r <- (range(shingle.intervals[level, ]) - t[1])/diff(t)
        grid.rect(x = unit(r %*% c(0.5, 0.5), "npc"), width = max(unit(c(diff(r), 
            1), c("npc", "mm"))), gp = gpar(col = fg, fill = fg))
        if (strip.names){
          name <- sub('equal\.count.([a-zA-Z. ]+).*$','\\1', name)
          name <- sub('shingle.([a-zA-Z. ]+).*$','\\1',name)
          name <-paste(name,': ', format(shingle.intervals[level,1], digits=4),
                       ' to ', format(shingle.intervals[level,2], digits=4),
                       sep='')

            grid.text(label = name, gp = gpar(col = par.strip.text$col, 
                alpha = par.strip.text$alpha, fontfamily = par.strip.text$fontfamily, 
                fontface = chooseFace(par.strip.text$fontface, 
                  par.strip.text$font), cex = 0.75*par.strip.text$cex))
        }
    }
    else if (is.null(shingle.intervals)) {
        strip.names <- strip.names[1]
        x <- factor.levels
        num <- length(x)
        if (style == 1) {
            grid.rect(gp = gpar(fill = bg, col = bg))
            if (strip.names) {
                grid.text(name, x = unit(0.5, "npc") - unit(1, 
                  "mm"), gp = gpar(col = par.strip.text$col, 
                  alpha = par.strip.text$alpha, fontfamily = par.strip.text$fontfamily, 
                  fontface = chooseFace(par.strip.text$fontface, 
                    par.strip.text$font), cex = par.strip.text$cex), 
                  just = "right")
                grid.text(":", x = unit(0.5, "npc"), gp = gpar(col = par.strip.text$col, 
                  alpha = par.strip.text$alpha, fontfamily = par.strip.text$fontfamily, 
                  fontface = chooseFace(par.strip.text$fontface, 
                    par.strip.text$font), cex = par.strip.text$cex))
                grid.text(x[level], x = unit(0.5, "npc") + unit(1, 
                  "mm"), gp = gpar(col = par.strip.text$col, 
                  alpha = par.strip.text$alpha, fontfamily = par.strip.text$fontfamily, 
                  fontface = chooseFace(par.strip.text$fontface, 
                    par.strip.text$font), cex = par.strip.text$cex), 
                  just = "left")
            }
            else grid.text(label = x[level], gp = gpar(col = par.strip.text$col, 
                alpha = par.strip.text$alpha, fontfamily = par.strip.text$fontfamily, 
                fontface = chooseFace(par.strip.text$fontface, 
                  par.strip.text$font), cex = par.strip.text$cex))
        }
        else if (style == 2) {
            grid.rect(x = unit((2 * level - 1)/(2 * num), "npc"), 
                width = unit(1/num, "npc"), gp = gpar(fill = fg, 
                  col = fg))
            grid.text(label = x, x = (2 * 1:num - 1)/(2 * num), 
                gp = gpar(col = par.strip.text$col, alpha = par.strip.text$alpha, 
                  fontfamily = par.strip.text$fontfamily, fontface = chooseFace(par.strip.text$fontface, 
                    par.strip.text$font), cex = par.strip.text$cex))
        }
        else if (style == 3) {
            grid.rect(gp = gpar(fill = bg, col = bg))
            grid.rect(x = unit((2 * level - 1)/(2 * num), "npc"), 
                width = unit(1/num, "npc"), gp = gpar(fill = fg, 
                  col = fg))
            grid.text(label = if (strip.names) 
                paste(name, x[level], sep = ": ")
            else x[level], gp = gpar(col = par.strip.text$col, 
                alpha = par.strip.text$alpha, fontfamily = par.strip.text$fontfamily, 
                fontface = chooseFace(par.strip.text$fontface, 
                  par.strip.text$font), cex = par.strip.text$cex))
        }
        else if (style == 4) {
            grid.rect(gp = gpar(fill = bg, col = bg))
            grid.rect(x = unit((2 * level - 1)/(2 * num), "npc"), 
                width = unit(1/num, "npc"), gp = gpar(fill = fg, 
                  col = fg))
            grid.text(label = x, x = (2 * 1:num - 1)/(2 * num), 
                gp = gpar(col = par.strip.text$col, alpha = par.strip.text$alpha, 
                  fontfamily = par.strip.text$fontfamily, fontface = chooseFace(par.strip.text$fontface, 
                    par.strip.text$font), cex = par.strip.text$cex))
        }
        else if (style >= 5) {
            grid.rect(gp = gpar(fill = bg, col = bg))
            grid.text(label = x[level], x = (2 * level - 1)/(2 * 
                num), gp = gpar(col = par.strip.text$col, alpha = par.strip.text$alpha, 
                fontfamily = par.strip.text$fontfamily, fontface = chooseFace(par.strip.text$fontface, 
                  par.strip.text$font), cex = par.strip.text$cex))
        }
    }
    strip.border <- trellis.par.get("strip.border")
    grid.rect(gp = gpar(col = rep(strip.border$col, length = which.given)[which.given], 
        lty = rep(strip.border$lty, length = which.given)[which.given], 
        lwd = rep(strip.border$lwd, length = which.given)[which.given], 
        alpha = rep(strip.border$alpha, length = which.given)[which.given], 
        fill = "transparent"))
    upViewport()
}

if(require(lattice)){
  environment(strip.shingle) <- environment(strip.default)
}
