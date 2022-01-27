#' Distance to Land Use class
#'
#'calculates the minimum or average distance of reference polygons (site, samples, polygons - as defined in function get.mb.ecors) to pixels of a given land use class in each year.
#'
#' @param x mb.ecors object (from get.mb.ecors).
#' @param class.value select a land use class (number).
#' @param stat.dist select "min" or "mean" statistics.
#' @param max.dist Maximum assessed distance (m). Very large values can take a long time to process or generate errors.
#' @param focal should use moving window (kernel) to remove atypical pixels (isolated pixels)? Pixels in the moving window are replaced by the "mode" value in the area.
#' @param window.radius radius of moving window (m).
#'
#' @details
#' The use of "focal" preprocessing is recommended, mainly with the use of minimum distance statistics, since a single isolated pixel caused by misclassification can seriously compromise the result.
#' However, it is important to assess whether this procedure is not removing real data. Linear features such as gallery forests or water bodies are particularly susceptible to be removed by this technique. Always visually evaluate the result of the focal technique with the plot.focal.ecors function before running dist.mb.ecors.
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
#' #get a mb.ecors class object
#' FAL.IBGE.JBB<-st_read(system.file("extdata/FAL.IBGE.JBB.gpkg", package="ecors"))
#' test.points<-st_read(system.file("extdata/Points_tests.gpkg", package="ecors"))
#' test.retangles<-st_read(system.file("extdata/Plots_tests.gpkg", package="ecors"))
#'
#' mb2000_2010<-get.mb.ecors(site=FAL.IBGE.JBB, points=test.points, plots=test.retangles,
#'      polygons=NULL, id.column=1, projected=F, custom.crs=32723,
#'      collection.mb=6, years=c(2000,2010), resolution=30, evaluate="surroundings.samples",
#'      buffer1=NULL, buffer2=NULL, buffer3=NULL, cumulative.surroundings=F)
#'
#' dist_to_3<-dist.mb.ecors(x=resp, class.value=3, stat.dist="mean", max.dist=5000, focal=T, window.radius=90)

dist.mb.ecors<-function(x, class.value, stat.dist, max.dist, focal, window.radius){

  if(class(x)!="mb.ecors"){stop("Argument x must be a mb.ecors class object.")}

  list2env(x,envir=environment())

  if(focal==F & is.null(window.radius)==F){stop("Argument window.radius need to be NULL when focal is set to FALSE")}

  npolyg<-polig.mb0.gee$size()$getInfo()
  if(npolyg>1){lista.polig<-ee$List(0:(npolyg-1))}

  #funções
  fun.dist.min<-function(mi){
    polig.mb0.i<-ee$Feature(polig.mb0.gee$toList(npolyg)$get(mi))
    dist.polig.mb0.i<-ee$FeatureCollection(polig.mb0.i)$distance(searchRadius=max.dist) #para visualização precisa definir escala

    interior<-ee$Image$constant(1)$clip(polig.mb0.i$geometry())$mask()
    interior<-interior$eq(0)
    dist.polig.mb0.i<-dist.polig.mb0.i$updateMask(interior)
    dist.polig.mb0.i<-dist.polig.mb0.i$updateMask(mbi$eq(class.value))

    polig.area.aval<-polig.mb0.i$buffer(distance=max.dist)
    dist.i<-dist.polig.mb0.i$reduceRegion(
      reducer=ee$Reducer$min(),
      geometry=polig.area.aval$geometry(),
      scale=resolution)
    return(dist.i)
  }

  fun.dist.med<-function(mi){
    polig.mb0.i<-ee$Feature(polig.mb0.gee$toList(npolyg)$get(mi))
    dist.polig.mb0.i<-ee$FeatureCollection(polig.mb0.i)$distance(searchRadius=max.dist) #para visualização precisa definir escala

    interior<-ee$Image$constant(1)$clip(polig.mb0.i$geometry())$mask()
    interior<-interior$eq(0)
    dist.polig.mb0.i<-dist.polig.mb0.i$updateMask(interior)
    dist.polig.mb0.i<-dist.polig.mb0.i$updateMask(mbi$eq(class.value))

    polig.area.aval<-polig.mb0.i$buffer(distance=max.dist)
    dist.i<-dist.polig.mb0.i$reduceRegion(
      reducer=ee$Reducer$mean(),
      geometry=polig.area.aval$geometry(),
      scale=resolution)
    return(dist.i)
  }

  #rodando
  result.dist.mb<-polig.mb0
  for (i in 1:length(years)){
    mbi<-mb$select(paste0("classification_",years[i]))

    if(focal==T){
      mbi<-mbi$focalMode(radius=window.radius,
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
        result.dist.mb[1,paste0("d.",substr(stat.dist,1,3),"_",years[i])]<-NA} else {
          result.dist.mb[1,paste0("d.",substr(stat.dist,1,3),"_",years[i])]<-round(dist.i[["distance"]])}

    }
    if(npolyg>1){
      for (j in 1:nrow(result.dist.mb)){
        if(is.null(dist.i[[j]][["distance"]])){
          result.dist.mb[j,paste0("d.",substr(stat.dist,1,3),"_",years[i])]<-NA} else {
            result.dist.mb[j,paste0("d.",substr(stat.dist,1,3),"_",years[i])]<-round(dist.i[[j]][["distance"]])}
      }
    }
  }

  if(focal==T){cat(paste0("Focal: image rendered by moving window with mode value.\nMoving window radius (m): ", window.radius,
                          "\nUse plot.focal.mb.ecors to check if focal processing made sense: adjust moving window radius if needed.\n"))}
  cat(paste0("\nMaximum evaluated distance (m): ",max.dist, "\nPixels size (m): ", resolution,
             "\nObs.: Interior of polygons was not considered.
      \nEvaluated Land Use class: ", legend.mb$lu.class[legend.mb$pixel.value==class.value], " (pixel value: ",class.value,")\n"))

  results.mb<-list()

  results.mb[["distance"]]$metadata<-list(get.mb.ecor.date.time=get.mb.ecor.date.time, dist.mb.ecors.date.time=Sys.time(),
                                          collection.mb=collection.mb, years=years, resolution=resolution,
                                          evaluate=evaluate, cumulative.surroundings=cumulative.surroundings, n.polyg=npolyg,max.dist=max.dist,
                                          lu.class.evaluated=paste0(legend.mb$lu.class[legend.mb$pixel.value==class.value], " (pixel value: ",class.value,")"),
                                          focal=focal,window.radius=window.radius)
  results.mb[["distance"]]$sf<-result.dist.mb

  results.mb[["distance"]]$dist_table<-st_drop_geometry(result.dist.mb)

  cat("\nDistance table \n")
  print(results.mb[["distance"]]$dist_table)

  class(results.mb)<-"mb.ecors"

  return(results.mb)
}

