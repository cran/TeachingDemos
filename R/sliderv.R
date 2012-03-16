"sliderv" <-
function(refresh.code,names,minima,maxima,resolutions,starts,
   title="control",no=0, set.no.value=0) {

  if(no!=0)
    return(as.numeric(tclvalue(get(paste("slider",no,sep=""),envir=slider.env))))

  if(set.no.value[1]!=0){
    try(eval(parse(text=paste("tclvalue(slider",set.no.value[1],")<-",
                     set.no.value[2],sep="")),envir=slider.env));
    return(set.no.value[2])
   }

  if(!exists("slider.env")) slider.env<<-new.env()
  #library(tcltk);
  nt<-tktoplevel(); tkwm.title(nt,title); tkwm.geometry(nt,"+0+0")
  for(i in seq(names))
    eval(parse(text=paste("assign(\"slider",i,"\",tclVar(starts[i]),envir=slider.env)",sep="")))
  for(i in seq(names)){
    tkpack(fr<-tkframe(nt),side='left');
    lab<-tklabel(fr, text=names[i], width="1")
    sc<-tkscale(fr, command=refresh.code, from=minima[i],
                to=maxima[i], showvalue=T, resolution=resolutions[i])
    assign("sc",sc,envir=slider.env); tkpack(lab,sc,side="top")
    eval(parse(text=paste("tkconfigure(sc,variable=slider",i,")",sep="")),
         envir=slider.env)
  }

  tkpack(fr<-tkframe(nt),fill="x")
  tkpack(tkbutton(fr, text="Exit", command=function()tkdestroy(nt)),
         side="right")
  tkpack(tkbutton(fr, text="Reset",
                  command=function(){ for(i in seq(starts))
                                        eval(parse(text=paste("tclvalue(slider",i,")<-",starts[i],sep="")),envir=slider.env)
                                      refresh.code()
                                    } ),side="left")
}

