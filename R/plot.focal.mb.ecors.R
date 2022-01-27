plot.focal.mb.ecors<-function(x,window.radius,zoom=12){
  list2env(x,envir=environment())

  Map$setCenter(center[1],center[2],zoom)

  comando.visuali<-c()
  comando.visualiA<-c()
  comando.visualiB<-c()
  for (i in 1:length(years)){
    par.vis.mb<-list(palette=palette.mb$color,values=palette.mb$lu.class,min=0,max=max(palette.mb$pixel.value))

    mbi<-mb$select(paste0("classification_",years[i]))
    #ee_print(mbi)
    mbi.f<-mbi$focalMode(radius=window.radius,
                         kernelType="circle",
                         units="meters",
                         iterations=1)

    prov<-Map$addLayer(mbi,par.vis.mb,paste0(years[i]))
    assign(paste0("map.mb",i),prov,envir=.GlobalEnv)

    prov.f<-Map$addLayer(mbi.f,par.vis.mb,paste0(years[i],"_focal_",window.radius,"m"))
    assign(paste0("map.mb.f",i),prov.f,envir=.GlobalEnv)

    if(length(comando.visualiA)==0){comando.visualiA<-c("map.mb1")} else {
      comando.visualiA<-paste0(comando.visualiA,"+map.mb",i)}

    if(length(comando.visualiB)==0){comando.visualiB<-c("map.mb.f1")} else {
      comando.visualiB<-paste0(comando.visualiB,"+map.mb.f",i)}
  }

  leg.mb<<-Map$addLegend(par.vis.mb,name="Legend",color_mapping="character",position="topright")
  comando.visuali<-paste0(comando.visualiA,"|",comando.visualiB,"+leg.mb")
  if(is.null(site.gee)==F){
    map.site<<-Map$addLayer(site.gee,name="site")
    comando.visuali<-paste0(comando.visuali,"+map.site")
  }
  if(is.null(samples.gee)==F){
    map.samples<<-Map$addLayer(samples.gee,name="samples")
    comando.visuali<-paste0(comando.visuali,"+map.samples")
  }
  if(is.null(polygons.gee)==F){
    map.polygons<<-Map$addLayer(polygons.gee,name="polygons")
    comando.visuali<-paste0(comando.visuali,"+map.polygons")
  }

  cat("Opening map in new window (eg browser) can improve visualization")

  return(eval(parse(text=comando.visuali),envir=.GlobalEnv)) #TODO: ao converter para função precisa colocar return() & pensar se isso e 2 assign acima não devem usar envir=.GlobalEnv
}
#TESTE
# plot.focal.mb.ecors(resp,80)
