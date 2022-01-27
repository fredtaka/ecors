#' Download images
#'
#' Download from Google Earth Engine all images selected, masked and (optionally) composed by get.ecors function.
#'
#' @param x ecors object (from get.ecors).
#' @param ecors.type type of images processed by get.ecors. Options are "original", "filtered", "mask", "composite". More information in detais section.
#' @param subset numerical vector with selected images (sequential number of images, eg. c(1, 2, 5) )
#' @param vis.bands include vizualization bands in the download?
#' @param new.list.bands selects a subset of bands to download (among those previously chosen in get.ecors).
#' @param ref.site use site extent as reference for the downloaded images extent?
#' @param ref.samples use samples extent as reference for the downloaded images extent?
#' @param new.ref.poly set a new polygon as reference for the downloaded images extent.
#' @param exp.degree number of degrees to expand (and in all directions) the extent of downloaded images (decimal degree format).
#' @param images.folder local folder to save images files.
#' @param clear.prov delete ALL files from temporary folder (ecors_temp) in your Google Drive account after download?
#'
#' @details
#' Argument ecors.type selects which processing level of get.ecors will be downloaded. Option "original" use all images available in the period without processing, "filtered" use only images approved in get.ecors quality control.
#' Option "mask" use same images as the previous one but with bad pixels masked, "composite" use compositions performed on the images of the previous option. \cr
#' If more than one reference type is give for the extent (ref.site, ref.samples, new.ref.poly) the extent that encompasses all these geometries will be used.
#'
#' @return
#' Prints images table and saves images in local system.
#' @export
#' @import rgee
#' @import googledrive
#' @import sf
#' @import dplyr
#'
#' @examples
#' #get a ecors class object
#' FAL.IBGE.JBB<-st_read(system.file("extdata/FAL.IBGE.JBB.gpkg", package="ecors"))
#' test.retangles<-st_read(system.file("extdata/Plots_tests.gpkg", package="ecors"))
#' test.points<-st_read(system.file("extdata/Points_tests.gpkg", package="ecors"))
#'
#' d2020<-get.ecors(site=FAL.IBGE.JBB, points=test.points, plots=test.retangles, buffer.points=500, buffer.plots=500,
#'     eval.area="site", projected=F, custom.crs=32723,
#'     collection="LANDSAT/LC08/C02/T1_L2", start=c("2020-01-01"), end=c("2021-01-01"),
#'     bands.eval="B3", bands.vis=T, indices=c("NDVI"), resolution=30,
#'     pOK=0.3, c.prob=NULL, c.dist=100, clouds.sentinel=NULL, cirrus.threshold=NULL, NIR.threshold=NULL, CDI.threshold=NULL, dmax.shadow=NULL,
#'     seasons=list(s1=c(11,12,1,2), s2=c(3,4), s3=c(5,6,7,8), s4=c(9,10)), sort.by="season", composite=NULL)
#'
#' #download
#' download.ecors(x=d2020, ecors.type="composite",subset=NULL,vis.bands=T,
#'    new.list.bands=NULL,ref.site=T,ref.samples=T,new.ref.poly=NULL,exp.degree=0.05,
#'    images.folder=getwd(),clear.prov=F)
#'
download.ecors<-function(x, ecors.type,subset=NULL,vis.bands=T,new.list.bands=NULL,ref.site=T,ref.samples=T,new.ref.poly=NULL,exp.degree=0.05,images.folder=getwd(),clear.prov=F){

  if(class(x)!="ecors"){stop("Argument x must be a ecors class object.")}

  list2env(x,envir=environment())

  if(substr(images.folder,nchar(images.folder),nchar(images.folder))=="/"){images.folder<-substr(images.folder,1,nchar(images.folder)-1)}

  if(ecors.type%in%c("original","filtered","mask","composite")==F){stop("Invalid option on ecors.type. Must be: original, filtered, mask or composite.")}
  if(clear.prov==T){
    warning(call.=F,immediate.=T, "clear.prov=T \nThe entire ecors_temp folder will be erased in your Google Drive account at the end of this function! \nOld data in ecors_temp also will be lost.")
    Sys.sleep(5)}

  poli.usados<-NULL
  if(ref.site==T & ref.samples==F){poli.usados<-site.gee}#por enquanto é FeatureCollection
  if(ref.site==F & ref.samples==T){poli.usados<-samples.gee}#por enquanto é FeatureCollection
  if(ref.site==T & ref.samples==T){poli.usados<-site.gee$merge(samples.gee)}#por enquanto é FeatureCollection

  if(is.null(new.ref.poly)==F & is.null(poli.usados)==T){poli.usados<-sf_as_ee(new.ref.poly)}
  if(is.null(new.ref.poly)==F & is.null(poli.usados)==F){
    prov<-sf_as_ee(new.ref.poly)
    poli.usados<-poli.usados$merge(prov)
  }

  poli.usados<-poli.usados$geometry(1)$transform() #coleção de coleções -> coleção e converte para geometria com WG84 (default) #valor 1: margem de erro para conversão (obrigatório nesse caso)
  poli.bounds<-poli.usados$bounds()$getInfo()
  poli.bounds<-unlist(poli.bounds)
  poli.ext<-as.numeric(c(poli.bounds[3],poli.bounds[4],poli.bounds[7],poli.bounds[8]))+c(-exp.degree,-exp.degree,exp.degree,exp.degree)
  poli.ext.gee<-ee$Geometry$Rectangle(coords=poli.ext,proj="EPSG:4326")

  if(ecors.type=="original"){colle.download<-ee$ImageCollection(colle)}
  if(ecors.type=="filtered"){colle.download<-ee$ImageCollection(colle.filt)}
  if(ecors.type=="mask"){colle.download<-ee$ImageCollection(colle.mask)}
  if(ecors.type=="composite"){colle.download<-ee$ImageCollection(colle.mask.compo)}

  if(is.null(subset)==F){colle.download<-ee$ImageCollection(colle.download[[subset]])}

  bands.collection<-colle.download[[1]]$bandNames()$getInfo()

  if(is.null(new.list.bands)==F){
    if(sum(new.list.bands%in%c(bands.collection,indices))<length(new.list.bands)){
      cat( paste("\nAvailable bands are: \n"),bands.collection, indices,"\n ")
      stop("Invalid band name.")
    }
    if(length(new.list.bands)>1){
      cat("\nIf you have problems to download custom bands: is not possible to download simultaneously bands of different data types (ex. optical bands, pixel quality and calculated indices).\n \n___ ")
    }
        bands.download<-new.list.bands} else {#caso não especifique new.list.bands será:
      bands.download<-bands.eval
      }

  if(vis.bands==T){bands.download<-unique(c(bands.download,d.vpar$bands))}

  cat(paste("\nCollection of images obtained by running get.ecors function on",get.ecor.date.time,"/nImage table/n"))

  if(sort.by=="season"){print(images.table%>%select(-images,-rep.season.image))}else{#else representa sort.by=="month"
    print(images.table%>%select(-images,-rep.month.image))}

  #download propriamente dito
  ee_imagecollection_to_local(
    ic = colle.download$select(bands.download),
    container = "ecors_temp",
    scale=resolution,
    region = poli.ext.gee,
    timePrefix=T,
    dsn=file.path(images.folder,paste0(ecors.type,"_",format(Sys.time(),"%H.%M"),"_")))

  if(length(indices)>0 & is.null(new.list.bands)){

    cat("\nDonwloading indices image")
    ee_imagecollection_to_local(
      ic = colle.download$select(indices),
      container = "ecors_temp",
      scale=resolution,
      region = poli.ext.gee,
      timePrefix=T,
      dsn=file.path(images.folder,paste0(ecors.type,"_indices_",format(Sys.time(),"%H.%M"),"_")))
  }

  if(clear.prov==T){ee_clean_container(name = "ecors_temp", type = "drive", quiet = FALSE)} else {
    cat("\nImage files are also stored in your Google Drive account on ecors_temp folder.\n") }

}
