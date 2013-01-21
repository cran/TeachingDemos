TkListView <- function(list){
  if( !require(tcltk) ) {
    stop('This function is dependent on the tcltk package')
  }
  if( !have.ttk() ) {
    stop('this function depends on having tcl 8.5 or higher')
  }

    tt <- tktoplevel()
    tkwm.title(tt, deparse(substitute(list)))

    fr1 <- tkframe(tt)
    tkpack(fr1, '-side', 'left', '-fill', 'both', '-expand', 0)
    tree <- tcltk::ttktreeview(fr1, '-selectmode','browse','-columns',1,height=21)
    scrtree1 <- tkscrollbar(fr1, command=function(...)tkyview(tree,...))

    tkconfigure(tree, yscrollcommand=function(...)tkset(scrtree1,...))

    tkpack(scrtree1, side='right', fill='y',expand=1)
    tkpack(tree, side='right',fill='both',expand=1)


    fr2 <- tkframe(tt)
    tkpack(fr2, '-side','top','-fill','both','-expand',1)

    txt <- tktext(fr2, bg="white", font="courier", wrap='none', width=40)
    scrtxt1 <- tkscrollbar(fr2, command=function(...)tkyview(txt,...))
    scrtxt2 <- tkscrollbar(fr2, command=function(...)tkxview(txt,...), orient='horizontal')
    tkconfigure(txt, yscrollcommand=function(...)tkset(scrtxt1,...),
                xscrollcommand=function(...)tkset(scrtxt2,...))

    tkgrid(txt,scrtxt1, sticky='nsew')
    tkgrid(scrtxt2,sticky='nsew')
    tkgrid.columnconfigure(fr2, 0, weight=1)
    tkgrid.rowconfigure(fr2, 0, weight=1)

    buildtree <- function(list, tree, parent) {
	str.info <- capture.output( str(list, max.level=1, give.attr=FALSE,
                                        no.list=TRUE) )
	str.info <- gsub(' ','\\\\ ',str.info)

	n <- length(list)
	nms <- names(list)
	if( is.null(nms) ) nms <- rep('', n)

	for( i in seq(length.out=n) ){
            id <- paste(parent, '.', i, sep='')
            nm <- nms[i]
            if(nm == '') nm <- paste('[[',i,']]',sep='')
            tkinsert(tree, parent, 'end', '-id', id, '-text', nm, '-values', str.info[i])
            if( is.list(list[[i]]) ){
                Recall( list[[i]], tree, id )
            } else if( !is.null(attributes(list[[i]])) ) {
                tkinsert(tree, id, 'end','-id', paste(id,'.a',sep=''), '-text', '<<Attributes>>')
                Recall( attributes(list[[i]]), tree, paste(id,'.a',sep='') )
            }
	}
	tmp <- as.list(attributes(list))
	tmp$names <- NULL
	if( length(tmp) ) {
            tkinsert(tree, parent, 'end', '-id', paste(parent,'.aa',sep=''), '-text', '<<Attributes>>')
            Recall( tmp, tree, paste(parent,'.aa',sep='') )
	}
    }

    buildtree(list, tree, '')


    getx <- function(list){
        tmp <- tclvalue(tkselect(tree))
        tmp2 <- strsplit(tmp, '\\.')[[1]][-1]

        sb <- function(y, list) {
            if (any( y %in% c('a','aa') ) ) {
                a <- which(y %in% c('a','aa'))[1]
                tmp <- if( a==1 ) {
                    as.list(attributes( list ) )
                } else {
                    y1 <- y[ seq(length.out=a-1) ]
                    as.list(attributes( list[[ as.numeric(y1) ]] ))
                }
                if( a == length(y) ) return(tmp)
                y2 <- y[ seq( from=a+1, length.out = length(y) - a) ]
                if( y[a] == 'aa' ) tmp$names <- NULL
                Recall(y2,tmp)
            } else {
                tmp <- as.numeric(y)
                list[[tmp]]
            }
        }
        sb(tmp2,list)
    }

    pr <- tkbutton(tt, text='print', command=function(...) {
        tmp <- capture.output(print(getx(list)))
        tkdelete(txt, '1.0','end')
        tkinsert(txt, 'end', paste(tmp, collapse='\n'))
    }
                   )
    st <- tkbutton(tt, text='str',   command=function(...) {
        tmp <- capture.output(print(str(getx(list))))
        tkdelete(txt, '1.0','end')
        tkinsert(txt, 'end', paste(tmp, collapse='\n'))
    }
                   )

    tkpack(pr, side='top', anchor='w')
    tkpack(st, side='top', anchor='w')

    fr3 <- tkframe(tt)
    tkpack(fr3, side='top', expand=1, fill='x')

    cmd <- tclVar('summary(x)')
    eve <- tkentry(fr3, textvariable=cmd)
    ev  <- tkbutton(fr3, text='Eval:', command=function(...) {
        tmp <- capture.output( eval(parse(text=tclvalue(cmd)), list(x=getx(list))))
        tkdelete(txt, '1.0', 'end')
        tkinsert(txt, 'end', paste(tmp, collapse='\n'))
    }
                    )

    tkpack(ev, side='left')
    tkpack(eve, side='left')

    tkpack(tkbutton(tt, text='Quit', command=function() tkdestroy(tt)),
           side='bottom', anchor='e')

    invisible(NULL)
}
