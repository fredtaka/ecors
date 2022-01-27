# pasta<-"/home/fred/Documentos/PELD 4/GIS" #este caminho não deve ser terminado com /
# poligonos<-st_read(dsn=file.path(pasta,"poligonos landuse de teste(3 poligonos).gpkg"))
# sitio<-st_read(dsn=file.path(pasta,"FAL.IBGE.JBB.gpkg"))
# pontos<-st_read(dsn=file.path(pasta,"Pontos de teste.gpkg"))
# parcelas<-st_read(dsn=file.path(pasta,"Parcelas de teste.gpkg"))

#id.column: número da coluna com ID. Deve ser válido para todos os objetos sf apresentados; geometria deve ser a última coluna

# collection.mb<-6 #opções disponíveis são 5 e 6. Selecione a mais recente (maior número) se não houver motivo contrário.
# evaluate<-"surroundings.site" #opções: "surroundings.samples", "surroundings.site", "inside.polygons"
# #obs. opção "polígono" considera o interior do polígono e, caso especificado, adicionado da área do buffer
# cumulative.surroundings<-T # T: entorno2 incluirá área do entorno1, entorno3 incluirá a área dos entornos1 e entorno2;
# # F: cada área de entorno não terá área em comum com os demais.
#
# buffer1<-0 # distância em metros para construção do buffer do entorno imediato do site ou das sampless
# buffer2<-0 # distância em metros para construção do buffer do entorno intermediário do site ou das sampless
# buffer3<-0 # distância em metros para construção do buffer do entorno distante do site ou das sampless
# #obs. se quiser só 1 buffer, use somente o buffer1 independente do valor. Se quiser só 2 buffers, use o buffer1 e buffer2
# #Buffers não utilizados podem ser deixados com valor NA


#' Get MapBiomas data to ecors
#'
#'Get MapBiomas data from Google Earth Engine for your study site and integrate with study polygons.
#'
#' @param site polygon of study site (sf object). Evaluation carried out in the surroundings of this polygon.
#' @param points sampling points (sf object). Evaluation carried out in the surroundings of these polygons.
#' @param plots sampling plots (sf object). Evaluation carried out in the surroundings of these points.
#' @param polygons polygon for land use evaluation. Evaluation carried out inside these polygons.
#' @param id.column number of id.column in your points, plots or polygons objects (need to be the same for all).
#' @param projected are the provided sf objects projected? (scale = m)
#' @param custom.crs choose a crs code to project sf objects prior to buffer processing.
#' @param collection.mb MapBiomas collection number. Available options are 5 and 6. Select the most recent (higher number) if there is no reason not to.
#' @param years numerical vector.
#' @param resolution select pixel size (m) for analysis and download.
#' @param evaluate indicate whether analysis should be performend on the "surroundings.samples", "surroundings.site" or "inside.polygons"
#' @param buffer1 nearest buffer radius (m).
#' @param buffer2 middle buffer radius (m).
#' @param buffer3 farthest buffer radius (m)
#' @param cumulative.surroundings should the area of each buffer be cumulative with those contained in them? (buffer3 should contain buffer2? buffer2 should contain buffer1?)
#'
#' @details
#' Sites, plots and points represent places where you want to know the effect of land use carried out in the surroundings (buffer zones). If you want to define the area within which the land use will be evaluated, use the polygons option. For site, points and parcels it is necessary to inform at least one buffer size. \cr
#' Regardless of where you want to perform the analyses, all these geometries can be informed simultaneously so that these polygons can be used in the map visualization.
#'
#' @return Object of the "mb.ecors" class with metadata and pre-processed data to be used in the count.mb.ecors, dist.mb.ecors, plot.focal.mb.ecors, plot.mb.ecors or download.mb.ecors functions. Aditional Google Earth Engine containers objects are exported to .GlobalEnv to be used in rgee functions and avoid errors (elapsed time limit): \cr
#' \itemize {
#' \item mb (MapBiomas image: years stored as bands),
#' \item polig.mb0.gee (reference polygons),
#' \item polig.mb1.gee (buffer1 polygons),
#' \item polig.mb2.gee (buffer2 polygons),
#' \item polig.mb3.gee (buffer3 polygons).}
#'
#' @import rgee
#' @import rgeeExtra
#' @import googledrive
#' @import sf
#' @import dplyr
#' @export
#'
#' @examples
#' FAL.IBGE.JBB<-st_read(system.file("extdata/FAL.IBGE.JBB.gpkg", package="ecors"))
#' test.points<-st_read(system.file("extdata/Points_tests.gpkg", package="ecors"))
#' test.retangles<-st_read(system.file("extdata/Plots_tests.gpkg", package="ecors"))

#' # Get data (projecting to UTM 32S zone to performe buffer operations)
#' mb2000_2010<-get.mb.ecors(site=FAL.IBGE.JBB, points=test.points, plots=test.retangles,
#'      polygons=NULL, id.column=1, projected=F, custom.crs=32723,
#'      collection.mb=6, years=c(2000,2010), resolution=30, evaluate="surroundings.site",
#'      buffer1=5000, buffer2=10000, buffer3=NULL, cumulative.surroundings=F)

#'
#'
get.mb.ecors<-function(site=NULL, points=NULL, plots=NULL, polygons=NULL, id.column=1, projected=FALSE, custom.crs=NULL, collection.mb=6, years, resolution=30, evaluate,
                       buffer1=0, buffer2=0, buffer3=0, cumulative.surroundings=F){

  ### Organizando
  get.mb.ecor.date.time<-Sys.time() #para identificar a data/hora de início de execução nos arquivos de saída

  ee_Initialize(user = 'ndef', drive = TRUE)

  if(evaluate%in%c("surroundings.samples", "surroundings.site", "inside.polygons")==F){
    stop("Argument evaluate must be surroundings.samples, surroundings.site or inside.polygons.")}

  if(collection.mb%in%c(5,6)==F){stop("Supported MapBiomas collections are: 5 and 6 (use the latest if you do not reasons to do otherwise).")}

  if(projected==F & is.null(custom.crs) & (buffer1>0 | buffer2>0 | buffer3>0 )){
    stop("Is not possible to use buffers with unprojected spatial objects (site, points, plots or polygons). Project these spatial objects prior run get.mb.ecors or provide custom.crs for projection.")}

  if(is.null(site)==F){
    if(is.null(custom.crs)){site<-st_transform(site,crs=custom.crs)}
    site.gee<-sf_as_ee(site)
  } else {site.gee<-NULL}

  if(is.null(points)==F){
    if(is.null(custom.crs)){points<-st_transform(points,crs=custom.crs)}
    if(projected!=T & is.null(custom.crs)){ stop(print("You need to project points object before run get.mb.ecors or provide custom.crs parameter for projection."))}
    circles<-st_buffer(points,dist=0.5)
    cat("\n Points converted to 1 m diameter circles. \n")
    circles<-circles[,c(id.column,ncol(circles))]
    circles[,ncol(circles)+1]<-"circles"
    names(circles)[c(1,ncol(circles))]<-c("id","type")
    st_geometry(circles)<-"geometry"
    samples<-circles #se houver points e plots, vai corrigir adiante
    samples.gee<-sf_as_ee(samples)
  }

  if(is.null(plots)==F){
    plots<-plots[,c(id.column,ncol(plots))]
    plots[,ncol(plots)+1]<-"plots"
    names(plots)[c(1,ncol(plots))]<-c("id","type")
    st_geometry(plots)<-"geometry"
    if(is.null(custom.crs)){plots<-st_transform(plots,crs=custom.crs)}
    if(is.null(points)==F){samples<-rbind(circles,plots)}else{samples<-plots}
    samples.gee<-sf_as_ee(samples) #sobreescrevendo objeto potencialmente criado para points
  }

  if(is.null(points)&is.null(plots)){samples.gee<-NULL}

  if(is.null(polygons)==F){polygons.gee<-sf_as_ee(polygons)
  } else {polygons.gee<-NULL}

  if(collection.mb==5){
    mb<-ee$Image("projects/mapbiomas-workspace/public/collection5/mapbiomas_collection50_integration_v1")
    legend.mb<-read.csv(system.file("extdata","MB.col5.legend.csv",package="ecors"))
    legend.mb<-legend.mb%>%setorder(cols="lu.class")%>%filter(lu.class!="-")
    if(sum(years<1985)+sum(years>2019)>0){stop("\nDatas para Coleção 5 devem ser entre 1985 e 2019")}
  } #legenda obtida de: https://github.com/mapbiomas-brazil/integration-toolkit/blob/master/mapbiomas-integration-toolkit.js

  if(collection.mb==6){
    mb<-ee$Image("projects/mapbiomas-workspace/public/collection6/mapbiomas_collection60_integration_v1")
    legend.mb<-read.csv(system.file("extdata","MB.col6.legend.csv",package="ecors"))
    legend.mb<-legend.mb%>%setorder(cols="lu.class")%>%filter(lu.class!="-")
    if(sum(years<1985)+sum(years>2020)>0){stop("\nDatas para Coleção 6 devem ser entre 1985 e 2020")}
  }

  mb<<-mb

  palette.mb<-legend.mb
  for (i in 0:max(palette.mb$pixel.value)) {#preenchendo vazios na sequencia de valores de pixel
    if(i%in%palette.mb$pixel.value ==F){palette.mb<-rbind(palette.mb,data.frame(color="#ffffff",pixel.value=i,lu.class=paste0("NA",i)))}
  }
  palette.mb<-palette.mb%>%arrange(pixel.value)

  #To get center
  if(evaluate=="surroundings.site"){center.poly.transf<-site.gee$geometry(1)$transform()}
  if(evaluate=="surroundings.samples"){center.poly.transf<-samples.gee$geometry(1)$transform()}
  if(evaluate=="inside.polygons"){center.poly.transf<-polygons$geometry(1)$transform()}

  center<-center.poly.transf$centroid()$getInfo()$coordinates #esse comando é incompatível com UTM

  if(is.numeric(buffer1)==F){buffer1<-0}
  if(is.numeric(buffer2)==F){buffer2<-0}
  if(is.numeric(buffer3)==F){buffer3<-0}

  if(evaluate=="surroundings.site"){
    polig.mb0<-site
    cat(paste("\nEvaluating the area of the site buffers without taking into account the interior of the site. If you want to consider the interior of sites, use option \"polygons\"\n"))

    polig.mb1<-st_buffer(site, buffer1)
    if(buffer2 > 0){polig.mb2<-st_buffer(site, buffer2)} else {polig.mb2<-NULL}
    if(buffer3 > 0){polig.mb3<-st_buffer(site, buffer3)} else {polig.mb3<-NULL}

    suppressWarnings(if(cumulative.surroundings==F){
      if(buffer3 > 0){st_geometry(polig.mb3)<-st_geometry(st_difference(polig.mb3,polig.mb2))}
      if(buffer2 > 0){st_geometry(polig.mb2)<-st_geometry(st_difference(polig.mb2,polig.mb1))}
      st_geometry(polig.mb1)<-st_geometry(st_difference(polig.mb1,site))
    })
    suppressWarnings(if(cumulative.surroundings==T){
      if(buffer3 > 0){st_geometry(polig.mb3)<-st_geometry(st_difference(polig.mb3,site))}
      if(buffer2 > 0){st_geometry(polig.mb2)<-st_geometry(st_difference(polig.mb2,site))}
      st_geometry(polig.mb1)<-st_geometry(st_difference(polig.mb1,site))
    })
  }

  if(evaluate=="surroundings.samples"){
    polig.mb0<-samples
    cat(paste("\nEvaluating the area of the samples buffers without taking into account the interior of the plots. If you want to consider the interior of polygons, use option \"polygons\"."))
    polig.mb1<-st_buffer(samples, buffer1)
    if(buffer2 > 0){polig.mb2<-st_buffer(samples, buffer2)} else {polig.mb2<-NULL}
    if(buffer3 > 0){polig.mb3<-st_buffer(samples, buffer3)} else {polig.mb3<-NULL}

    suppressWarnings(if(cumulative.surroundings==F){
      if(buffer3 > 0){for(i in 1:nrow(samples)){st_geometry(polig.mb3[i,])<-st_geometry(st_difference(polig.mb3[i,],polig.mb2[i,]))}}
      if(buffer2 > 0){for(i in 1:nrow(samples)){st_geometry(polig.mb2[i,])<-st_geometry(st_difference(polig.mb2[i,],polig.mb1[i,]))}}
      for(i in 1:nrow(samples)){st_geometry(polig.mb1[i,])<-st_geometry(st_difference(polig.mb1[i,],samples[i,]))}
    })
    suppressWarnings(if(cumulative.surroundings==T){
      if(buffer3 > 0){for(i in 1:nrow(samples)){st_geometry(polig.mb3[i,])<-st_geometry(st_difference(polig.mb3[i,],samples[i,]))}}
      if(buffer2 > 0){for(i in 1:nrow(samples)){st_geometry(polig.mb2[i,])<-st_geometry(st_difference(polig.mb2[i,],samples[i,]))}}
      for(i in 1:nrow(samples)){st_geometry(polig.mb1[i,])<-st_geometry(st_difference(polig.mb1[i,],samples[i,]))}
    })
  }

  if(evaluate=="inside.polygons"){
    polig.mb0<-polygons
    if(buffer1 > 0 | buffer2 > 0 | buffer2 > 0){
      cat(paste("\nEvaluating the area of the interior of polygons +  buffers.\n")) } else {
        cat(paste("\nEvaluating the area of the interior of polygons.\n")) }
    if(buffer1 > 0){polig.mb1<-st_buffer(polygons, buffer1)} else {polig.mb1<-polygons}
    if(buffer2 > 0){polig.mb2<-st_buffer(polygons, buffer2)} else {polig.mb2<-NULL}
    if(buffer3 > 0){polig.mb3<-st_buffer(polygons, buffer3)} else {polig.mb3<-NULL}

    suppressWarnings(if(cumulative.surroundings==F){
      if(buffer3 > 0){for(i in 1:nrow(samples)){st_geometry(polig.mb3[i,])<-st_geometry(st_difference(polig.mb3[i,],polig.mb2[i,]))}}
      if(buffer2 > 0){for(i in 1:nrow(samples)){st_geometry(polig.mb2[i,])<-st_geometry(st_difference(polig.mb2[i,],polig.mb1[i,]))}}
    })
  }

  polig.mb0.gee<-sf_as_ee(polig.mb0)
  if(buffer1 > 0){polig.mb1.gee<-sf_as_ee(polig.mb1)} else {polig.mb1.gee=NULL}
  if(buffer2 > 0){polig.mb2.gee<-sf_as_ee(polig.mb2)} else {polig.mb2.gee=NULL}
  if(buffer3 > 0){polig.mb3.gee<-sf_as_ee(polig.mb3)} else {polig.mb3.gee=NULL}

  polig.mb0.gee<<-polig.mb0.gee
  polig.mb1.gee<<-polig.mb1.gee
  polig.mb2.gee<<-polig.mb2.gee
  polig.mb3.gee<<-polig.mb3.gee

  out.get.mb.ecors<-list(get.mb.ecor.date.time=get.mb.ecor.date.time,
                         site.gee=site.gee, samples.gee=samples.gee, polygons.gee=polygons.gee, center=center,
                         projected=projected, custom.crs=custom.crs,
                         collection.mb=collection.mb, legend.mb=legend.mb, palette.mb=palette.mb,
                         years=years, resolution=resolution,
                         evaluate=evaluate, cumulative.surroundings=cumulative.surroundings,
                         buffer1=buffer1, buffer2=buffer2, buffer3=buffer3,
                         polig.mb0=polig.mb0, polig.mb1=polig.mb1, polig.mb2=polig.mb2, polig.mb3=polig.mb3)
  #mb e polig.mb saem como <<- (para evitar erro de tempo limite excedido)
  return(out.get.mb.ecors)

}
