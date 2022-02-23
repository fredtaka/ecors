#' Distance to Land Use class
#'
#' Calculates the minimum or average distance of reference polygons (site, samples, polygons - as defined in function get.lu.ecors) to pixels of a given land use class in each year.
#'
#' @param x lu.ecors object (from get.lu.ecors).
#' @param class.value select a land use class (number).
#' @param stat.dist select "min" or "mean" statistics.
#' @param max.dist Maximum assessed distance (m). Very large values can take a long time to process or generate errors.
#' @param focal should use moving window (kernel) to remove atypical pixels (isolated pixels)? Pixels in the moving window are replaced by the "mode" value in the area.
#' @param window.radius radius of moving window (m).
#'
#' @details
#' The use of "focal" preprocessing is recommended, mainly with the use of minimum distance statistics, since a single isolated pixel caused by misclassification can seriously compromise the result.
#' However, it is important to assess whether this procedure is not removing real data. Linear features such as gallery forests or water bodies are particularly susceptible to be removed by this technique. Always visually evaluate the result of the focal technique with the plot.focal.ecors function before running dist.lu.ecors.
#' Buffers will not be used in this analysis. Distance will be computed from the original polygons.
#'
#' @return
#' List with result tables and metadata.
#'
#' @export
#'
#' @import rgee
#' @import rgeeExtra
#' @import googledrive
#' @import sf
#' @import dplyr
#'
#' @examples
#' # Get a lu.ecors class object
#' FAL.IBGE.JBB<-sf::st_read(system.file("extdata/FAL.IBGE.JBB.gpkg", package="ecors"))
#' test.points<-sf::st_read(system.file("extdata/Points_tests.gpkg", package="ecors"))
#' test.retangles<-sf::st_read(system.file("extdata/Plots_tests.gpkg", package="ecors"))
#'
#' # Get lu.ecors object (buffer size will not be considered in dist.lu.ecors)
#' lu2000_2010<-get.lu.ecors(site=FAL.IBGE.JBB, points=test.points, plots=test.retangles,
#'      polygons=NULL, id.column=1, projected=F, custom.crs=32723,
#'      collection.lu="mapbiomas6", years=c(2000,2010), resolution=30, evaluate="surroundings.samples",
#'      buffer1=1, buffer2=NULL, buffer3=NULL, cumulative.surroundings=F)
#'
#' dist_to_3<-dist.lu.ecors(x=lu2000_2010, class.value=3, stat.dist="mean",
#'                            max.dist=5000, focal=T, window.radius=90)

dist.lu.ecors<-function(x, class.value, stat.dist, max.dist, focal, window.radius){

  if(class(x)!="lu.ecors"){stop("Argument x must be a lu.ecors class object.")}

  lu.prov<-eval(parse(text=x$object.name))

  if(focal==F & is.null(window.radius)==F){stop("Argument window.radius need to be NULL when focal is set to FALSE")}

  npolyg<-polig.lu0.gee$size()$getInfo()
  if(npolyg>1){lista.polig<-ee$List(0:(npolyg-1))}

  #funções
  fun.dist.min<-function(mi){
    polig.lu0.i<-ee$Feature(polig.lu0.gee$toList(npolyg)$get(mi))
    dist.polig.lu0.i<-ee$FeatureCollection(polig.lu0.i)$distance(searchRadius=max.dist) #para visualização precisa definir escala

    interior<-ee$Image$constant(1)$clip(polig.lu0.i$geometry())$mask()
    interior<-interior$eq(0)
    dist.polig.lu0.i<-dist.polig.lu0.i$updateMask(interior)
    dist.polig.lu0.i<-dist.polig.lu0.i$updateMask(lui$eq(class.value))

    polig.area.aval<-polig.lu0.i$buffer(distance=max.dist)
    dist.i<-dist.polig.lu0.i$reduceRegion(
      reducer=ee$Reducer$min(),
      geometry=polig.area.aval$geometry(),
      scale=x$resolution)
    return(dist.i)
  }

  fun.dist.med<-function(mi){
    polig.lu0.i<-ee$Feature(polig.lu0.gee$toList(npolyg)$get(mi))
    dist.polig.lu0.i<-ee$FeatureCollection(polig.lu0.i)$distance(searchRadius=max.dist) #para visualização precisa definir escala

    interior<-ee$Image$constant(1)$clip(polig.lu0.i$geometry())$mask()
    interior<-interior$eq(0)
    dist.polig.lu0.i<-dist.polig.lu0.i$updateMask(interior)
    dist.polig.lu0.i<-dist.polig.lu0.i$updateMask(lui$eq(class.value))

    polig.area.aval<-polig.lu0.i$buffer(distance=max.dist)
    dist.i<-dist.polig.lu0.i$reduceRegion(
      reducer=ee$Reducer$mean(),
      geometry=polig.area.aval$geometry(),
      scale=x$resolution)
    return(dist.i)
  }

  #rodando
  result.dist.lu<-x$polig.lu0
  for (i in 1:length(x$years)){
    lui<-lu.prov$select(paste0("classification_",x$years[i]))

    if(focal==T){
      lui<-lui$focalMode(radius=window.radius,
                         kernelType="circle",
                         units="meters",
                         iterations=1)
    }
    if(npolyg==1){#rgee não aceita ee$List de tamanho menor que 2 no map
      if(stat.dist=="min"){dist.i<-fun.dist.min(mi=0)$getInfo()}
      if(stat.dist=="mean"){dist.i<-fun.dist.med(mi=0)$getInfo()}
    }
    if(npolyg>1){
      if(stat.dist=="min"){dist.i<-lista.polig$map(ee_utils_pyfunc(fun.dist.min))$getInfo()}
      if(stat.dist=="mean"){dist.i<-lista.polig$map(ee_utils_pyfunc(fun.dist.med))$getInfo()}
    }

    if(npolyg==1){
      if(is.null(dist.i[["distance"]])){
        result.dist.lu[1,paste0("d.",stat.dist,"_",x$years[i])]<-NA} else {
          result.dist.lu[1,paste0("d.",stat.dist,"_",x$years[i])]<-round(dist.i[["distance"]])}

    }
    if(npolyg>1){
      for (j in 1:nrow(result.dist.lu)){
        if(is.null(dist.i[[j]][["distance"]])){
          result.dist.lu[j,paste0("d.",stat.dist,"_",x$years[i])]<-NA} else {
            result.dist.lu[j,paste0("d.",stat.dist,"_",x$years[i])]<-round(dist.i[[j]][["distance"]])}
      }
    }
  }

  if(focal==T){cat(paste0("Focal: image rendered by moving window with mode value.\nMoving window radius (m): ", window.radius,
                          "\nUse plot.focal.lu.ecors to check if focal processing made sense: adjust moving window radius if needed.\n"))}
  cat(paste0("\nMaximum evaluated distance (m): ",max.dist, "\nPixels size (m): ", x$resolution,
             "\nObs.: Interior of polygons was not considered.
      \nEvaluated Land Use class: ", x$legend.lu$lu.class[x$legend.lu$pixel.value==class.value], " (pixel value: ",class.value,")\n"))

  results.lu<-list()

  results.lu[["distance"]]$metadata<-x #recycling

  #adding itens
  results.lu[["distance"]]$metadata<-list(dist.lu.ecors.date.time=Sys.time(),
                                          n.polyg=npolyg,max.dist=max.dist,
                                          lu.class.evaluated=paste0(x$legend.lu$lu.class[x$legend.lu$pixel.value==class.value], " (pixel value: ",class.value,")"),
                                          focal=focal,window.radius=window.radius)


  results.lu[["distance"]]$sf<-result.dist.lu

  results.lu[["distance"]]$dist_table<-st_drop_geometry(result.dist.lu)

  cat("\nDistance table \n")
  print(results.lu[["distance"]]$dist_table)

  class(results.lu)<-"lu.ecors"

  return(results.lu)
}

