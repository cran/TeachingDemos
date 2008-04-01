tdspinner <- function(parent, ...) {
    # this is a quick hack to provide spinboxes without loading tcltk2
    tkwidget(parent, "spinbox", ...)
}
