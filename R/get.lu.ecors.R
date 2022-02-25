#' Get Land Use data to ecors
#'
#'Get Land Use data from Google Earth Engine for your study site and integrate with study polygons.
#'
#' @param site polygon of study site (sf object). Evaluation carried out in the surroundings of this polygon.
#' @param points sampling points (sf object). Evaluation carried out in the surroundings of these polygons.
#' @param plots sampling plots (sf object). Evaluation carried out in the surroundings of these points.
#' @param polygons polygon for land use evaluation. Evaluation carried out inside these polygons.
#' @param id.column number of id.column in your points, plots or polygons objects (need to be the same for all).
#' @param projected are the provided sf objects projected? (scale = m)
#' @param custom.crs choose a crs code to project sf objects prior to buffer processing.
#' @param collection.lu Land Use collection. Available options are currently "mapbiomas5" and "mapbiomas6" (MapBiomas collections 5 and 6). Select the most recent (higher number) if there is no reason not to.
#' @param years numerical vector.
#' @param resolution select pixel size (m) for analysis and download.
#' @param evaluate indicate whether analysis should be performend on the "surroundings.samples", "surroundings.site" or "inside.polygons". Option "distance.samples" should be used when you want to use only the dist.lu.ecors function where buffers are not used (other options of the evaluate argument do not preclude the usage of dist.lu.ecors).
#' @param buffer1 nearest buffer radius (m).
#' @param buffer2 middle buffer radius (m).
#' @param buffer3 farthest buffer radius (m).
#' @param cumulative.surroundings should the area of each buffer be cumulative with those contained in them? (buffer3 should contain buffer2? buffer2 should contain buffer1?)
#' @param online.storage select online storage integration (mandatory for images download). Options are "drive" for Google Drive, "gcs" for Google Cloud Storage or NULL.
#'
#' @details
#' Sites, plots and points represent places where you want to know the effect of land use carried out in the surroundings (buffer zones). If you want to define the area within which the land use will be evaluated, use the polygons option. For site, points and parcels it is necessary to inform at least one buffer size. \cr
#' Regardless of where you want to perform the analyses, all these geometries can be informed simultaneously so that these polygons can be used in the map visualization.
#'
#' @return Object of the "lu.ecors" class with metadata and pre-processed data to be used in the count.lu.ecors, dist.lu.ecors, plot.focal.lu.ecors, plot.lu.ecors or download.lu.ecors functions. Aditional Google Earth Engine container object lu (Land Use image: years stored as bands) is exported to .GlobalEnv to be used in rgee functions and avoid errors (elapsed time limit). \cr
#'
#' @import rgee
#' @import rgeeExtra
#' @import googledrive
#' @import sf
#' @import dplyr
#' @export
#'
#' @examples
#' FAL.IBGE.JBB<-sf::st_read(system.file("extdata/FAL.IBGE.JBB.gpkg", package="ecors"))
#' test.points<-sf::st_read(system.file("extdata/Points_tests.gpkg", package="ecors"))
#' test.plots<-sf::st_read(system.file("extdata/Plots_tests.gpkg", package="ecors"))
#'
#' # Get data (projecting to UTM 32S zone to performe buffer operations)
#' lu2000_2010<-get.lu.ecors(site=FAL.IBGE.JBB, points=test.points, plots=test.plots,
#'      polygons=NULL, id.column=1, projected=F, custom.crs=32723,
#'      collection.lu="mapbiomas6", years=c(2000,2010), resolution=30, evaluate="surroundings.site",
#'      buffer1=5000, buffer2=10000, buffer3=NULL, cumulative.surroundings=F)
#'
#'
get.lu.ecors<-function(site=NULL, points=NULL, plots=NULL, polygons=NULL, id.column=1, projected=FALSE, custom.crs=NULL, collection.lu="mapbiomas6", years, resolution=30, evaluate,
                       buffer1=0, buffer2=0, buffer3=0, cumulative.surroundings=F, online.storage="drive"){

  ### Organizando
  get.lu.ecor.date.time<-Sys.time() #para identificar a data/hora de início de execução nos arquivos de saída

  if(is.null(online.storage)){ee_Initialize(user = 'ndef', drive = F, gcs = F)}
  if(online.storage=="gcs"){ee_Initialize(user = 'ndef', drive = F, gcs = T)}
  if(online.storage=="drive"){ee_Initialize(user = 'ndef', drive = T, gcs = F)}


  if(collection.lu%in%c("mapbiomas5","mapbiomas6")==F){stop("Currently, only \"mapbiomas5\",\"mapbiomas6\" (MapBiomas collections 5 and 6) are supported (use the latest if you do not reasons to do otherwise).")}

  if(evaluate%in%c("surroundings.samples", "surroundings.site", "inside.polygons", "distance.samples")==F){
    stop("Argument evaluate must be surroundings.samples, surroundings.site, inside.polygons or distance.samples.")}

  #zero or NULL will disable a buffer
  if(is.null(buffer1)){buffer1<-0}
  if(is.null(buffer2)){buffer2<-0}
  if(is.null(buffer3)){buffer3<-0}

  if(buffer1==0 & (buffer2>0 | buffer3>0)){
    stop("If buffer1 = 0, buffer2 and buffer3 should be 0.")
  }

  if(buffer2==0 & buffer3>0){
    stop("If buffer2 = 0, buffer3 should be 0.")
  }

  if(evaluate%in%c("surroundings.samples", "surroundings.site") & !(buffer1>0 | buffer2>0 | buffer3>0 )){
    stop(print("Need to specify buffer values when evaluate is set to \"surroundings.samples\" or \"surroundings.site\""))
  }

  if(evaluate=="distance.samples" & (buffer1>0 | buffer2>0 | buffer3>0 )){
    stop(print("Argument evaluate = \"distance.samples\" do not supports buffers."))
  }

  if(projected==F & is.null(custom.crs) & (buffer1>0 | buffer2>0 | buffer3>0 )){
    stop("Is not possible to use buffers with unprojected spatial objects (site, points, plots or polygons). Project these spatial objects prior run get.lu.ecors or provide custom.crs for projection.")}

  if(is.null(site)==F){
    if(is.null(custom.crs)){site<-st_transform(site,crs=custom.crs)}
    site.gee<-sf_as_ee(site)
  } else {site.gee<-NULL}


  samples<-NULL

  #points->circles
  if(sum(class(points)=="sf")) {

    if(projected==F & is.null(custom.crs)){
      stop(print("You need to project your spatial objects with points before run get.lu.ecors or provide custom.crs parameter"))
    }

    if(projected==T & is.null(custom.crs)){
      circles<-st_buffer(points,dist=0.5) #here is different from get.ecors
    }

    if(is.null(custom.crs)==F){
      points.crs<-st_crs(points)
      points<-st_transform(points,crs=custom.crs)
      circles<-st_buffer(points,dist=0.5)
      circles<-st_transform(circles,crs=points.crs)
    }

    cat("\n Points converted to 1 m diameter circles. \n") #here is different from get.ecors

    names(circles)[names(circles)==attr(circles,"sf_column")]<-"geometry" #geometry may or not have this name. This makes consistent between objects.
    st_geometry(circles)<-"geometry"
    names(circles)[id.column]<-"id"
    circles<-circles[,id.column]
    circles$type<-"circles"
    circles.gee<-sf_as_ee(circles)
    samples<-circles #se houver points e plots, vai corrigir adiante
  } else {cat("\n No valid file with points \n")}


  #plots (several differences from get.ecors: do not apply buffer here)
  if(sum(class(plots)=="sf")){

    names(plots)[names(plots)==attr(plots,"sf_column")]<-"geometry" #geometry may or not have this name. This makes consistent between objects.
    st_geometry(plots)<-"geometry"
    names(plots)[id.column]<-"id"
    plots<-plots[,id.column]
    plots$type<-"plots"
    samples<-plots #pode sobrescrever mas se houver points e plots, vai corrigir adiante
  } else {cat("\n No valid file with plots \n")}


  #samples (consolidanting)
  if(sum(class(points)=="sf",class(plots)=="sf")==2) {
    samples<-rbind(circles,plots)
  }
  if(is.null(samples)){samples.gee<-NULL} else {samples.gee<-sf_as_ee(samples)}


  #polygons
  if(is.null(polygons)==F){polygons.gee<-sf_as_ee(polygons)
  } else {polygons.gee<-NULL}

  #MapBiomas
  if(collection.lu=="mapbiomas5"){
    lu<-ee$Image("projects/mapbiomas-workspace/public/collection5/mapbiomas_collection50_integration_v1")
    legend.lu<-read.csv(system.file("extdata","MB.col5.legend.csv",package="ecors"))
    if(sum(years<1985)+sum(years>2019)>0){stop("\nDates for MapBiomas Collection 5 must be between 1985 and 2019")}
  } #legenda obtida de: https://github.com/mapbiomas-brazil/integration-toolkit/blob/master/mapbiomas-integration-toolkit.js

  if(collection.lu=="mapbiomas6"){
    lu<-ee$Image("projects/mapbiomas-workspace/public/collection6/mapbiomas_collection60_integration_v1")
    legend.lu<-read.csv(system.file("extdata","MB.col6.legend.csv",package="ecors"))
    if(sum(years<1985)+sum(years>2020)>0){stop("\nDates for MapBiomas Collection 6 must be between 1985 and 2020")}
  }

  lu<-lu$select(paste0("classification_",years))

  lu<<-lu

  palette.lu<-legend.lu
  for (i in 0:max(palette.lu$pixel.value)) {#preenchendo vazios na sequencia de valores de pixel
    if(i%in%palette.lu$pixel.value ==F){palette.lu<-rbind(palette.lu,data.frame(color="#ffffff",pixel.value=i,lu.class=paste0("NA",i)))}
  }
  palette.lu<-palette.lu%>%arrange(pixel.value)

  if(is.numeric(buffer1)==F){buffer1<-0}
  if(is.numeric(buffer2)==F){buffer2<-0}
  if(is.numeric(buffer3)==F){buffer3<-0}

  if(evaluate=="surroundings.site"){
    polig.lu0<-site
    cat(paste("\nEvaluating the area of the site buffers without taking into account the interior of the site. If you want to consider the interior of sites, use option \"polygons\"\n"))

    polig.lu1<-st_buffer(site, buffer1)
    if(buffer2 > 0){polig.lu2<-st_buffer(site, buffer2)} else {polig.lu2<-NULL}
    if(buffer3 > 0){polig.lu3<-st_buffer(site, buffer3)} else {polig.lu3<-NULL}

    suppressWarnings(if(cumulative.surroundings==F){
      if(buffer3 > 0){st_geometry(polig.lu3)<-st_geometry(st_difference(polig.lu3,polig.lu2))}
      if(buffer2 > 0){st_geometry(polig.lu2)<-st_geometry(st_difference(polig.lu2,polig.lu1))}
      st_geometry(polig.lu1)<-st_geometry(st_difference(polig.lu1,site))
    })
    suppressWarnings(if(cumulative.surroundings==T){
      if(buffer3 > 0){st_geometry(polig.lu3)<-st_geometry(st_difference(polig.lu3,site))}
      if(buffer2 > 0){st_geometry(polig.lu2)<-st_geometry(st_difference(polig.lu2,site))}
      st_geometry(polig.lu1)<-st_geometry(st_difference(polig.lu1,site))
    })
  }

  if(evaluate=="surroundings.samples"){
    polig.lu0<-samples
    cat(paste("\nEvaluating the area of the samples buffers without taking into account the interior of the plots. If you want to consider the interior of polygons, use option \"polygons\".\n"))
    polig.lu1<-st_buffer(samples, buffer1)
    if(buffer2 > 0){polig.lu2<-st_buffer(samples, buffer2)} else {polig.lu2<-NULL}
    if(buffer3 > 0){polig.lu3<-st_buffer(samples, buffer3)} else {polig.lu3<-NULL}

    suppressWarnings(if(cumulative.surroundings==F){
      if(buffer3 > 0){for(i in 1:nrow(samples)){st_geometry(polig.lu3[i,])<-st_geometry(st_difference(polig.lu3[i,],polig.lu2[i,]))}}
      if(buffer2 > 0){for(i in 1:nrow(samples)){st_geometry(polig.lu2[i,])<-st_geometry(st_difference(polig.lu2[i,],polig.lu1[i,]))}}
      for(i in 1:nrow(samples)){st_geometry(polig.lu1[i,])<-st_geometry(st_difference(polig.lu1[i,],samples[i,]))}
    })
    suppressWarnings(if(cumulative.surroundings==T){
      if(buffer3 > 0){for(i in 1:nrow(samples)){st_geometry(polig.lu3[i,])<-st_geometry(st_difference(polig.lu3[i,],samples[i,]))}}
      if(buffer2 > 0){for(i in 1:nrow(samples)){st_geometry(polig.lu2[i,])<-st_geometry(st_difference(polig.lu2[i,],samples[i,]))}}
      for(i in 1:nrow(samples)){st_geometry(polig.lu1[i,])<-st_geometry(st_difference(polig.lu1[i,],samples[i,]))}
    })
  }

  if(evaluate=="inside.polygons"){
    polig.lu0<-polygons
    if(buffer1 > 0 | buffer2 > 0 | buffer2 > 0){
      cat(paste("\nEvaluating the area of the interior of polygons +  buffers.\n")) } else {
        cat(paste("\nEvaluating the area of the interior of polygons.\n")) }
    if(buffer1 > 0){polig.lu1<-st_buffer(polygons, buffer1)} else {polig.lu1<-NULL}
    if(buffer2 > 0){polig.lu2<-st_buffer(polygons, buffer2)} else {polig.lu2<-NULL}
    if(buffer3 > 0){polig.lu3<-st_buffer(polygons, buffer3)} else {polig.lu3<-NULL}

    suppressWarnings(if(cumulative.surroundings==F){
      if(buffer3 > 0){for(i in 1:nrow(samples)){st_geometry(polig.lu3[i,])<-st_geometry(st_difference(polig.lu3[i,],polig.lu2[i,]))}}
      if(buffer2 > 0){for(i in 1:nrow(samples)){st_geometry(polig.lu2[i,])<-st_geometry(st_difference(polig.lu2[i,],polig.lu1[i,]))}}
    })
  }

  if(evaluate=="distance.samples"){
    polig.lu0<-samples
    polig.lu1<-NULL
    polig.lu2<-NULL
    polig.lu3<-NULL
  }


  #areas
  if(buffer1==0){area.m2<-data.frame(id=st_drop_geometry(polig.lu0)[,id.column],polygons.m2=as.numeric(st_area(polig.lu0)))}
  if(buffer2==0 & buffer1>0){area.m2<-data.frame(id=st_drop_geometry(polig.lu0)[,id.column],polygons.m2=as.numeric(st_area(polig.lu0)),buffer1.m2=as.numeric(st_area(polig.lu1)))}
  if(buffer2>0 & buffer3==0){area.m2<-data.frame(id=st_drop_geometry(polig.lu0)[,id.column],polygons.m2=as.numeric(st_area(polig.lu0)),buffer1.m2=as.numeric(st_area(polig.lu1)),buffer2.m2=as.numeric(st_area(polig.lu2)))}
  if(buffer3>0){area.m2<-data.frame(id=st_drop_geometry(polig.lu0)[,id.column],polygons.m2=as.numeric(st_area(polig.lu0)),buffer1.m2=as.numeric(st_area(polig.lu1)),buffer2.m2=as.numeric(st_area(polig.lu2)),buffer3.m2=as.numeric(st_area(polig.lu3)))}


  polig.lu0.gee<-sf_as_ee(polig.lu0)
  if(buffer1 > 0){polig.lu1.gee<-sf_as_ee(polig.lu1)} else {polig.lu1.gee=NULL}
  if(buffer2 > 0){polig.lu2.gee<-sf_as_ee(polig.lu2)} else {polig.lu2.gee=NULL}
  if(buffer3 > 0){polig.lu3.gee<-sf_as_ee(polig.lu3)} else {polig.lu3.gee=NULL}

  # polig.lu0.gee<<-polig.lu0.gee
  # polig.lu1.gee<<-polig.lu1.gee
  # polig.lu2.gee<<-polig.lu2.gee
  # polig.lu3.gee<<-polig.lu3.gee


  out.get.lu.ecors<-list(get.lu.ecor.date.time=get.lu.ecor.date.time,
                         collection.lu=collection.lu, legend.lu=legend.lu, palette.lu=palette.lu,
                         years=years, resolution=resolution, post.processing="none", object.name="lu",
                         evaluate=evaluate, cumulative.surroundings=cumulative.surroundings,
                         buffer1=buffer1, buffer2=buffer2, buffer3=buffer3,
                         polig.lu0=polig.lu0, polig.lu1=polig.lu1, polig.lu2=polig.lu2, polig.lu3=polig.lu3, area.m2=area.m2,
                         site.gee=site.gee, samples.gee=samples.gee, polygons.gee=polygons.gee,
                         polig.lu0.gee=polig.lu0.gee, polig.lu1.gee=polig.lu1.gee, polig.lu2.gee=polig.lu2.gee, polig.lu3.gee=polig.lu3.gee
                         )
  #lu e polig.lu saem como <<- (para evitar erro de tempo limite excedido)
  class(out.get.lu.ecors)<-"lu.ecors"

  return(out.get.lu.ecors)

}
