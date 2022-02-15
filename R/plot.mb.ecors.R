#' Plot mb.ecors object
#'
#' Plot MapBiomas images processed by get.mb.ecors along with site and sample polygons.
#'
#' @param x mb.ecors object (from get.mb.ecors).
#' @param zoom initial zoom value.
#' @param legend choose to show the legend or not.
#'
#' @return
#' Google Earth Engine containers objects are exported to .GlobalEnv to be used in rgee functions and avoid errors (elapsed time limit):
#' \itemize{
#' \item map.samples: sample polygons (evaluation carried out in the surroundings of these polygons),
#' \item map.site: site polygon (evaluation carried out in the surroundings of this polygon),
#' \item map.polygons: polygon for land use evaluation (evaluation carried out inside these polygons),
#' \item map.buffer(i): polygons of buffer areas (if selected in get.mb.ecors);
#' \item map.mb(i): plot object of a raster image (one object per image),
#' \item leg.mb: legend.}
#'
#' @export
#' @import rgee
#' @import googledrive
#' @import sf
#' @import dplyr
#'
#' @examples
#' #Get a mb.ecors class object
#' FAL.IBGE.JBB<-sf::st_read(system.file("extdata/FAL.IBGE.JBB.gpkg", package="ecors"))
#' test.points<-sf::st_read(system.file("extdata/Points_tests.gpkg", package="ecors"))
#' test.retangles<-sf::st_read(system.file("extdata/Plots_tests.gpkg", package="ecors"))
#'
#' # Get data (projecting to UTM 32S zone to performe buffer operations)
#' mb2000_2010<-get.mb.ecors(site=FAL.IBGE.JBB, points=test.points, plots=test.retangles,
#'      polygons=NULL, id.column=1, projected=F, custom.crs=32723,
#'      collection.mb=6, years=c(2000,2010), resolution=30, evaluate="surroundings.site",
#'      buffer1=5000, buffer2=10000, buffer3=NULL, cumulative.surroundings=F)
#'
#' #Plotting
#' plot.mb.ecors(x=mb2000_2010)

plot.mb.ecors<-function(x,zoom=12,legend=T){

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
  if(legend==T){
    leg.mb<<-Map$addLegend(par.vis.mb,name="Legend",color_mapping="character",position="topright")
    comando.visuali<-paste0(comando.visuali,"+leg.mb")
  }
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
  if(is.null(polig.mb1.gee)==F){
    map.buffer1<<-Map$addLayer(polig.mb1.gee,name="buffer1")
    comando.visuali<-paste0(comando.visuali,"+map.buffer1")
  }
  if(is.null(polig.mb2.gee)==F){
    map.buffer2<<-Map$addLayer(polig.mb2.gee,name="buffer2")
    comando.visuali<-paste0(comando.visuali,"+map.buffer2")
  }
  if(is.null(polig.mb3.gee)==F){
    map.buffer3<<-Map$addLayer(polig.mb3.gee,name="buffer3")
    comando.visuali<-paste0(comando.visuali,"+map.buffer3")
  }

  cat("Opening map in new window (eg browser) can improve visualization")

  return(eval(parse(text=comando.visuali),envir=.GlobalEnv))
}
