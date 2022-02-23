#' Download Land Use images
#'
#' Download from Google Earth Engine all Land Use images selected by get.lu.ecors function.
#'
#' @param x lu.ecors object (from get.lu.ecors).
#' @param exp.degree number of degrees to expand (and in all directions) the extent of downloaded images (decimal degree format).
#' @param images.folder local folder to save images files.
#' @param clear.prov delete ALL files from temporary folder (ecors_temp) in your Google Drive account after download?
#'
#' @return
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
#' #get a lu.ecors class object
#' FAL.IBGE.JBB<-sf::st_read(system.file("extdata/FAL.IBGE.JBB.gpkg", package="ecors"))
#' test.points<-sf::st_read(system.file("extdata/Points_tests.gpkg", package="ecors"))
#' test.plots<-sf::st_read(system.file("extdata/Plots_tests.gpkg", package="ecors"))
#'
#' lu2000_2010<-get.lu.ecors(site=FAL.IBGE.JBB, points=test.points, plots=test.plots,
#'      polygons=NULL, id.column=1, projected=F, custom.crs=32723,
#'      collection.lu="mapbiomas6", years=c(2000,2010), resolution=30, evaluate="surroundings.site",
#'      buffer1=10000, buffer2=NULL, buffer3=NULL, cumulative.surroundings=F)
#'
#' #download
#' download.lu.ecors(x=lu2000_2010, exp.degree=0.05, images.folder=getwd(), clear.prov=F)

download.lu.ecors<-function(x, exp.degree=0.05, images.folder=getwd(), clear.prov=F){

  if(class(x)!="lu.ecors"){stop("Argument x must be a lu.ecors class object.")}

  lu.prov<-eval(parse(text=x$object.name))

  if(substr(images.folder,nchar(images.folder),nchar(images.folder))=="/"){images.folder<-substr(images.folder,1,nchar(images.folder)-1)}

  poli.usados<-st_geometry(st_transform(x$polig.lu0,4326))

  if(x$buffer3>0){poli.usados<-c(poli.usados,st_geometry(st_transform(x$polig.lu3,4326)))} else {
    if(x$buffer2>0){poli.usados<-c(poli.usados,st_geometry(st_transform(x$polig.lu2,4326)))} else {
      if(x$buffer1>0){poli.usados<-c(poli.usados,st_geometry(st_transform(x$polig.lu1,4326)))}
    }
  }

  poli.ext.lu<-poli.usados%>%st_bbox()%>%
    as.vector()+c(-exp.degree,-exp.degree,exp.degree,exp.degree)
  poli.ext.lu.gee<-ee$Geometry$Rectangle(coords=poli.ext.lu,proj="EPSG:4326")


  #download propriamente dito
  gdrive<-ee_image_to_drive(
    image = lu.prov,
    folder = "rgee_prov",
    region = poli.ext.lu.gee)

  gdrive$start() #executando
  ee_monitoring(gdrive)

  ee_drive_to_local(
    task=gdrive,
    dsn=file.path(images.folder,paste0("LU",x$evaluate,"_",format(Sys.time(),"%H.%M"),".tif")))

  if(clear.prov==T){ee_clean_container(name = "rgee_prov", type = "drive", quiet = FALSE)} else {
    cat("\nImage files are also stored in your Google Drive account on rgee_prov folder.\n") }
}
