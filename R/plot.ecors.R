#' Plot ecors object
#'
#' Plot Google Earth Engine images processed by get.ecors along with site and sample polygons.
#'
#' @param x ecors object (from get.ecors).
#' @param ecors.type type of images processed by get.ecors. Options are "original", "filtered", "mask", "filtered|mask", "filtered+mask", "composite". More information in detais section.
#' @param visualization choose preset options "vis.bands" (default visualization), "pan" (panchromatic band), "pan.sharpening" (panchromatic sharpening) or use "custom" to select bands.
#' @param zoom initial zoom value.
#' @param defaults if TRUE, uses default values of pixel.min, pixel.max, image.gamma and/or rescaling.
#' @param legend choose to show the legend or not.
#' @param bands select a single band to plot or three bands to plot RGB image.
#' @param pixel.min lower pixel value used to scale the plot.
#' @param pixel.max higher pixel value used to scale the plot.
#' @param image.gamma gamma correction.
#'
#' @details
#' Argument ecors.type selects which processing level of get.ecors will be ploted. Option "original" use all images available in the period without processing, "filtered" use only images approved in get.ecors quality control.
#' Option "mask" use same images as the previous one but with bad pixels masked, "composite" use compositions performed on the images of the previous option.
#' Options "filtered|mask" and "filtered+mask" show images from both collections. Option 2 and 3 show images from both collections, in the first the images are toggled with the slider and in the last option the images from both collections are stacked. in the first the images are toggled with the slider and in the last option the images from both collections are stacked
#'
#' @return
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
#' #ploting
#' plot.ecors(x=d2020, ecors.type="mask", visualization="vis.bands", defaults=T,
#'            legend=T, bands=NULL, pixel.min=NULL, pixel.max=NULL, image.gamma=NULL)
#'
#' plot.ecors(x=d2020, ecors.type="mask", visualization="custom", defaults=F,
#'            legend=T, bands="NDVI", pixel.min=NULL, pixel.max=NULL, image.gamma=NULL)
#'
plot.ecors<-function(x, ecors.type, visualization="vis.bands", zoom=10, defaults=T, legend=F, bands=NULL,
                     pixel.min=NULL, pixel.max=NULL, image.gamma=NULL){

  if(class(x)!="ecors"){stop("Argument x must be a ecors class object.")}

  list2env(x,envir=environment())

  if(is.null(site.gee)==F){
    poligono<-site.gee$geometry()$transform()#converte para WG84 (default)
  } else {
    poligono<-samples.gee$geometry()$transform()#converte para WG84 (default)
  }
    centro<-poligono$centroid()$getInfo()$coordinates #esse comando é incompatível com UTM

  if(ecors.type%in%c("original","filtered","mask","filtered|mask","filtered+mask","composite")==F){stop("Invalid option to argument ecors.type")}

  if((is.null(pixel.min)==F & is.null(pixel.max)==T) | (is.null(pixel.min)==T & is.null(pixel.max)==F)){stop("When set a value to pixel.min or pixel.max are needed setting values to both arguments.")}

  if(length(visualization)>1){
    stop("Choose only one option to visualization parameter")}

  if(visualization %in% c("pan","pan.sharpening") & is.null(d.vpar.pan)){
    stop("Panchromatic band is not available to this ecors.type")}

  if(visualization!="custom" & is.null(bands)==F){
    stop("Is not possible to set visualization to vis.bands or pan or pan.sharpening and choose custom bands")}

    if(visualization=="custom" & is.null(bands)){
      stop("Is not possible to set visualization to custom and without inform custom bands")}


  if(defaults==T & is.null(bands)==F){
    warning("Setting defaults=T with custom bands may not get good results\n ")}

  if(defaults==T & (is.null(pixel.min)==F | is.null(pixel.max)==F | is.null(image.gamma)==F)){
    stop("Is not possible to set defaults=T and choose custom values to pixel.min, pixel.max or image.gamma")}

  Map$setCenter(centro[1],centro[2],zoom)
  n.imagens<-nrow(images.table) # número de imagens filtradas

  if(ecors.type=="original"){
    colle.plot<-colle
    n.img<-colle$size()$getInfo()
    dates.plot<-dates.inter
    tipo.plot<-"ori_"}

  if(ecors.type=="filtered"){
    colle.plot<-colle.filt
    n.img<-n.imagens
    dates.plot<-images.table$OKdates
    tipo.plot<-"filt_"}

  if(ecors.type=="mask"){
    colle.plot<-colle.mask
    n.img<-n.imagens
    dates.plot<-images.table$OKdates
    tipo.plot<-"mask_"}

  if(ecors.type=="filtered|mask" | ecors.type=="filtered+mask"){
    colle.plot<-colle.filt
    colle.plotB<-colle.mask
    n.img<-n.imagens
    dates.plot<-images.table$OKdates
    tipo.plot<-"filt_"}

  if(ecors.type=="composite"){
    colle.plot<-colle.mask.compo
    if(sort.by=="season"){
      n.img<-length(unique(images.table$rep.season))
      dates.plot<-unique(images.table$rep.season)
    }
    if(sort.by=="month"){
      n.img<-length(unique(images.table$rep.month))
      dates.plot<-unique(images.table$rep.month)
    }
    tipo.plot<-"compo_"}

  if(visualization=="vis.bands"){colle.plot<-colle.plot$select(d.vpar$bands)}
  if(visualization=="pan"){colle.plot<-colle.plot$select(d.vpar.pan$bands)}
  if(visualization=="pan.sharpening"){
    colle.plot<-colle.plot$select(c(d.vpar$bands,d.vpar.pan$bands))
    if(is.null(pixel.min)==F & is.null(pixel.max)==F){
      colle.plot<-colle.plot$map(function(imagem){
        imagem$unitScale(pixel.min,pixel.max)})
    } else {
      #baseado no: https://gis.stackexchange.com/questions/313394/normalization-in-google-earth-engine
      listabandas<-ee$List(imagem$bandNames())
      colle.plot<-colle.plot$map(function(imagem){
        imgMinMax<-imagem$reduceRegion(
          reducer=ee$Reducer$minMax(),
          geometry=poligono,
          scale=escala,
          maxPixels=10e9)
        unitScale<-ee$ImageCollection$fromImages(imagem$bandNames()$map(ee_utils_pyfunc(function(nomebanda){
          nomebanda<-ee$String(nomebanda)
          band<-imagem$select(nomebanda)
          band$unitScale(ee$Number(imgMinMax$get(nomebanda$cat('_min'))), ee$Number(imgMinMax$get(nomebanda$cat('_max'))))
        })))$toBands()$rename(imagem$bandNames())
      })
    }
    #fusão das imagens
    colle.plot<-colle.plot$map(function(imagem){
      rgb<-imagem$select(d.vpar$bands)
      pan<-imagem$select(d.vpar.pan$bands)
      huesat<-rgb$rgbToHsv()$select("hue","saturation")
      sharpened<-ee$Image$cat(huesat,pan)$hsvToRgb()
      return(sharpened)
    })
  }

  if(defaults==T){
    if(visualization=="vis.bands"){vpar<-d.vpar}
    if(visualization=="pan"){vpar<-d.vpar.pan}
    if(visualization=="pan.sharpening"){vpar<-list(min=0,max=1)}
    if(is.null(d.vpar$reescalonar)==F){
      f.reescalonar<-function(imagem){imagem$multiply(vpar$reescalonar$multiplica)$add(vpar$reescalonar$soma)}
      colle.plot<-colle.plot$map(f.reescalonar)
      if(ecors.type=="filtered|mask" | ecors.type=="filtered+mask"){
        colle.plotB<-colle.plotB$map(f.reescalonar)}
    }
  }

  if(defaults==T & is.null(bands)==F){#muda as bandas definidas acima
    vpar$bands<-bands
  }

  if(defaults==F & visualization=="vis.bands"){
    if(is.null(image.gamma)){image.gamma<-1}
    vpar<-list(bands=d.vpar$bands,min=pixel.min,max=pixel.max,gamma=image.gamma)
  }

  if(defaults==F & visualization=="pan"){
    if(is.null(image.gamma)){image.gamma<-1}
    vpar<-list(bands=d.vpar.pan$bands,min=pixel.min,max=pixel.max,gamma=image.gamma)
  }

  if(defaults==F & visualization=="pan.sharpening"){
    if(is.null(image.gamma)==F){warning("Value of image.gamma is ignored with pan.sharpening\n ",call.=F)}
    vpar<-list(min=0,max=1) #valores já foram usados na seção de normalização
  }

  if(defaults==F & is.null(bands)==F){
    if(is.null(image.gamma)){image.gamma<-1}
    vpar<-list(bands=bands,min=pixel.min,max=pixel.max,gamma=image.gamma)}

  cat(paste0("\n Available bands/indices:"),paste0(c(bands.used,indices)))

  comando.visuali<-c()
  if(ecors.type=="filtered|mask" | ecors.type=="filtered+mask"){comando.visualiB<-c()}

  for (i in 1:n.img){
    cat(paste0("\n Processing image ",i," of ",n.img,". Started at ",format(Sys.time(),"%H:%M")))
    prov<-Map$addLayer(colle.plot[[i]],vpar,paste(tipo.plot,dates.plot[i]))
    assign(paste0("img",i),prov,envir=.GlobalEnv)
    if(length(comando.visuali)==0){comando.visuali<-c("img1")} else {
      comando.visuali<-paste0(comando.visuali,"+img",i)}

    if(ecors.type=="filtered|mask" | ecors.type=="filtered+mask"){
      provB<-Map$addLayer(colle.plotB[[i]],vpar,paste("mask_",dates.plot[i]))
      assign(paste0("imgB",i),provB,envir=.GlobalEnv)
      if(length(comando.visualiB)==0){comando.visualiB<-c("imgB1")} else {
        comando.visualiB<-paste0(comando.visualiB,"+imgB",i)}
    }
  }

  img.samples<<-Map$addLayer(samples.gee,name="Samples") # atribuindo ao .GlobalEnv
  img.site<<-Map$addLayer(site.gee,name="Site") # atribuindo ao .GlobalEnv

  comando.visuali<-paste0(comando.visuali,"+img.site+img.samples")

  if(legend==T){
    leg<<-Map$addLegend(vpar)
    comando.visuali<-paste0(comando.visuali,"+leg")
  }

  if(ecors.type=="filtered|mask"){
    comando.visuali<-paste0(comando.visuali,"|",comando.visualiB)
  }
  if(ecors.type=="filtered+mask"){
    comando.visuali<-paste0(comando.visualiB,"+",comando.visuali)
  }

  cat("  \n\n")
  if(sort.by=="season"){
    if(is.null(images.table$rep.season.image)==F){print(images.table%>%select(-images,-rep.season.image))} else {print(images.table%>%select(-images))} #else representa sort.by=="month"
  }

  if(ecors.type%in%c("mask","filtered|mask","composite")){
    cat("\nMasked images display information from the lower layers in the removed areas (low quality). Evaluate image with only one layer enabled per stack.\n ")}
  cat(paste("\nPlot can be repeated or modified by running the following command in terminal:\n",comando.visuali,"\n "))
  if(is.null(clouds.sentinel)==F) {if(clouds.sentinel=="CDI" & ecors.type%in%c("mask","filtered|mask","filtered+mask","composite")){warning("CDI-based cloud filter processing is time consuming. Images may take some time to be generated even after displaying other image elements/layers",call.=F)}}
  return(eval(parse(text=comando.visuali),envir=.GlobalEnv))
}
