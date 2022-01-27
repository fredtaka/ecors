plot.mb.ecors<-function(x,zoom=12){

  list2env(x,envir=environment())

  Map$setCenter(center[1],center[2],zoom)
  comando.visuali<-c()
  for (i in 1:length(years)){
    par.vis.mb<-list(palette=palette.mb$color,values=palette.mb$lu.class,bands=paste0("classification_",years[i]),min=0,max=max(palette.mb$pixel.value))
    prov<-Map$addLayer(mb,par.vis.mb,paste0(years[i]))
    assign(paste0("map.mb",i),prov,envir=.GlobalEnv)
    if(length(comando.visuali)==0){comando.visuali<-c("map.mb1")} else {
      comando.visuali<-paste0(comando.visuali,"+map.mb",i)}
  }
  leg.mb<<-Map$addLegend(par.vis.mb,name="Legend",color_mapping="character",position="topright")
  comando.visuali<-paste0(comando.visuali,"+leg.mb")
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

  return(eval(parse(text=comando.visuali),envir=.GlobalEnv))
}
#TESTE
# plot.mb.ecors(x=resp,zoom=12)
