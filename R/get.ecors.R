#' Get data to ecors
#'
#' Get data from Google Earth Engine for your study site and integrate with study polygons and sampling periods.
#'
#' @param site polygon of study site (sf object).
#' @param points sampling points (sf object).
#' @param plots sampling plots (sf object).
#' @param id.column number of id.column in your points and/or plots objects (need to be the same for both).
#' @param buffer.points radius (m) of buffer of points. Need to be > 0.
#' @param buffer.plots radius (m) of buffer of plots.
#' @param projected are the provided sf objects projected? (scale = m)
#' @param custom.crs choose a crs code to project sf objects prior to buffer processing.
#' @param pOK minimum proportion of pixels aproved in quality control on the evaluated area to use a image.
#' @param c.dist additional distance (m) around pre-identified clouds that will be disregarded in the mask (when clouds.sentinel="CDI", c.dist should be multiples of 100 m). Usefull to avoid false negative cloud detection in cloud edges.
#' @param collection Google Earth Engine collection name.
#' @param start initial date (year-month-day).
#' @param end final date (year-month-day).
#' @param bands.eval bands needed for analysis (bands needed to calculate indices or visualization will be added automatically).
#' @param bands.vis include bands for visualization?
#' @param indices select indices to be produced. Available options are "NDVI", "EVI", "NBR".
#' @param resolution select pixel size (m) for (most of) quality control procedures. This value will be passed to stats.ecors and download.ecors.
#' @param eval.area choose evaluated area for quality control: site or samples.
#' @param clouds.sentinel method for cloud detection in Sentinel-2 MSI. Options are the collection "default" (using built in Quality Analysis of these collections; several false negative pixels; not detect cloud shadows in TOA images), "CDI" (more time consuming) or NULL.
#' @param c.prob set the cloud probability threshold value to exclude pixels in Sentinel-2 MSI imagens. It could remove additional pixels with clouds.sentinel="default" (not compatible with "CDI" method).
#' @param cirrus.threshold set threshold value for method CDI (see Frantz et al. 2018 for details).
#' @param NIR.threshold set threshold value for method CDI (see Frantz et al. 2018 for details).
#' @param CDI.threshold set threshold value for method CDI (see Frantz et al. 2018 for details).
#' @param dmax.shadow set the maximum distance that CDI Algorithm will search for clouds. Large values take a lot of processing time or may crash Google Earth Engine. Value in meters (in multiples of 100 m).
#' @param seasons month allocation (numerical form) in up to four seasons. Use the list structure list(s1=c(), s2=c(), s3=c(), s4=c()) keeping the items you don't want to use empty. More information in the exemples section.
#' @param group.by should data be grouped by "season" or "month"? This parameter influences how the data will be summarized in stats.ecors and (if selected) how the images will be composited.
#' @param composite method for generating composite images in the collection Available options to argument composite: min, max, mean, median and NULL (disable composition).
#' @param online.storage select online storage integration (mandatory for images download). Options are "drive" for Google Drive, "gcs" for Google Cloud Storage or NULL.
#'
#' @details
#' Currently ecors supports the following remote sensing data collections:
#'
#'#' \cr
#' Landsat 9
#' \itemize{
#' \item "LANDSAT/LC09/C02/T1_L2" [Collection 2 - Surface Reflectance](https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC09_C02_T1_L2)
#' \item "LANDSAT/LC09/C02/T1_TOA" [Collection 2 - Top of Atmosphere Reflectance](https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC09_C02_T1_TOA)
#' \item "LANDSAT/LC09/C02/T1" [Collection 2 - Raw Images](https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC09_C02_T1)
#' }
#'
#' \cr
#' Landsat 8
#' \itemize{
#' \item "LANDSAT/LC08/C02/T1_L2" [Collection 2 - Surface Reflectance](https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC08_C02_T1_L2)
#' \item "LANDSAT/LC08/C02/T1_TOA" [Collection 2 - Top of Atmosphere Reflectance](https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC08_C02_T1_TOA)
#' \item "LANDSAT/LC08/C02/T1" [Collection 2 - Raw Images](https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC08_C02_T1)
#' \item "LANDSAT/LC08/C01/T1_SR" [Collection 1 - Surface Reflectance](https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC08_C01_T1_SR)
#' \item "LANDSAT/LC08/C01/T1_TOA" [Collection 1 - Top of Atmosphere Reflectance](https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC08_C01_T1_TOA)
#' \item "LANDSAT/LC08/C01/T1" [Collection 1 - Raw Images](https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC08_C01_T1)
#' }
#'
#' \cr
#' Landsat 7
#' \itemize{
#' \item "LANDSAT/LE07/C02/T1_L2" [Collection 2 - Surface Reflectance](https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LE07_C02_T1_L2)
#' \item "LANDSAT/LE07/C02/T1_TOA" [Collection 2 - Top of Atmosphere Reflectance](https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LE07_C02_T1_TOA)
#' \item "LANDSAT/LE07/C02/T1" [Collection 2 - Raw Images](https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LE07_C02_T1)
#' \item "LANDSAT/LE07/C01/T1_TOA" [Collection 1 - Top of Atmosphere Reflectance](https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LE07_C02_T1_TOA)
#' \item "LANDSAT/LE07/C01/T1" [Collection 1 - Raw Images](https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LE07_C02_T1)
#' }
#'
#' \cr
#' Sentinel-2 MSI (Multispectral Instrument)
#' \itemize{
#' \item "COPERNICUS/S2_SR" [Surface Reflectance](https://developers.google.com/earth-engine/datasets/catalog/COPERNICUS_S2_SR)
#' \item "COPERNICUS/S2" [Top of Atmosphere Reflectance](https://developers.google.com/earth-engine/datasets/catalog/COPERNICUS_S2)
#' }
#'
#' \cr
#' Global Precipitation Measurement (GPM)
#' \itemize{
#' \item "NASA/GPM_L3/IMERG_V06" [Global Precipitation Measurement (GPM) v6 - every three hours](https://developers.google.com/earth-engine/datasets/catalog/NASA_GPM_L3_IMERG_V06)
#' \item "NASA/GPM_L3/IMERG_MONTHLY_V06" [Monthly Global Precipitation Measurement (GPM) v6](https://developers.google.com/earth-engine/datasets/catalog/NASA_GPM_L3_IMERG_MONTHLY_V06)
#' }

#' @return Object of the "ecors" class with metadata and pre-processed data to be used in the stats.ecors, plot.ecors or download.ecors functions. Aditional Google Earth Engine containers objects are exported to .GlobalEnv to be used in rgee functions and avoid errors (elapsed time limit): \cr
#' \itemize{
#' \item colle (all images available in the period),
#' \item colle.filt (images approved in get.ecors quality control),
#' \item colle.mask (same as the previous one but with bad pixels masked),
#' \item colle.mask.compo (collection of compositions performed on the images of the previous collection).}
#'
#'
#' @references
#' Pixel quality control information: \cr
#' Frantz, D., Hass, E., Uhl, A., Stoffels, J., & Hill, J. (2018). Improvement of the Fmask algorithm for Sentinel-2 images: Separating clouds from bright surfaces based on parallax effects. Remote sensing of environment, 215, 471-481. \cr
#' <https://www.usgs.gov/media/files/landsat-8-9-olitirs-collection-2-level-1-data-format-control-book> \cr
#' <https://www.usgs.gov/media/files/landsat-8-9-olitirs-collection-2-level-2-data-format-control-book> \cr
#' <https://www.usgs.gov/media/files/landsat-7-etm-collection-2-level-2-data-format-control-book> \cr
#' <https://www.usgs.gov/media/files/landsat-4-5-tm-collection-2-level-2-data-format-control-book> \cr
#' <https://www.usgs.gov/media/files/landsat-8-collection-1-land-surface-reflectance-code-product-guide> \cr
#' <https://www.usgs.gov/media/files/landsat-4-7-collection-1-surface-reflectance-code-ledaps-product-guide> \cr
#' <https://www.usgs.gov/core-science-systems/nli/landsat/landsat-collection-1-level-1-quality-assessment-band?qt-science_support_page_related_con=0#qt-science_support_page_related_con> \cr
#'
#' @examples
#' FAL.IBGE.JBB<-sf::st_read(system.file("extdata/FAL.IBGE.JBB.gpkg", package="ecors"))
#' test.points<-sf::st_read(system.file("extdata/Points_tests.gpkg", package="ecors"))
#' test.plots<-sf::st_read(system.file("extdata/Plots_tests.gpkg", package="ecors"))
#'
#' #library(ecors)
#'
#' # Get data (projecting to UTM 32S zone to performe buffer operations)
#' d2020<-get.ecors(site=FAL.IBGE.JBB, points=test.points, plots=test.plots, buffer.points=500, buffer.plots=500,
#'     eval.area="site", projected=F, custom.crs=32723,
#'     collection="LANDSAT/LC08/C02/T1_L2", start=c("2020-01-01"), end=c("2020-12-31"),
#'     bands.eval="SR_B3", bands.vis=T, indices=c("NDVI"), resolution=30,
#'     pOK=0.3, c.prob=NULL, c.dist=100, clouds.sentinel=NULL, cirrus.threshold=NULL, NIR.threshold=NULL, CDI.threshold=NULL, dmax.shadow=NULL,
#'     seasons=list(s1=c(11,12,1,2), s2=c(3,4), s3=c(5,6,7,8), s4=c(9,10)), group.by="month", composite="mean")
#'
#' @import rgee
#' @import rgeeExtra
#' @import googledrive
#' @import sf
#' @import dplyr
#' @export

get.ecors<-function(site=NULL, points=NULL, plots=NULL, id.column=1, buffer.points=1, buffer.plots=0,
                    projected=FALSE, custom.crs=NULL,
                    collection, start, end, bands.eval=NULL, bands.vis=T, indices=c("NDVI", "EVI", "NBR"), resolution,
                    eval.area="site", pOK=0.8, c.dist, clouds.sentinel=NULL, c.prob=NULL,
                    cirrus.threshold=NULL, NIR.threshold=NULL, CDI.threshold=NULL, dmax.shadow=NULL,
                    seasons=list(s1=c(), s2=c(), s3=c(), s4=c()), group.by="month", composite=NULL,
                    online.storage="drive")


{

  if(is.null(online.storage)){ee_Initialize(user = 'ndef', drive = F, gcs = F)}
  if(online.storage=="gcs"){ee_Initialize(user = 'ndef', drive = F, gcs = T)}
  if(online.storage=="drive"){ee_Initialize(user = 'ndef', drive = T, gcs = F)}

  #############################################
  ##### CONFIGURAÇÕES DAS COLEÇÕES ############
  #############################################

  if(collection%in%c(
    "COPERNICUS/S2_SR",
    "COPERNICUS/S2",
    "LANDSAT/LC09/C02/T1_L2",
    "LANDSAT/LC09/C02/T1_TOA",
    "LANDSAT/LC09/C02/T1",
    "LANDSAT/LC08/C02/T1_L2",
    "LANDSAT/LC08/C02/T1_TOA",
    "LANDSAT/LC08/C02/T1",
    "LANDSAT/LC08/C01/T1_SR",
    "LANDSAT/LC08/C01/T1_TOA",
    "LANDSAT/LC08/C01/T1",
    "LANDSAT/LE07/C02/T1_L2",
    "LANDSAT/LE07/C02/T1_TOA",
    "LANDSAT/LE07/C02/T1",
    "LANDSAT/LE07/C01/T1_TOA",
    "LANDSAT/LE07/C01/T1",
    "NASA/GPM_L3/IMERG_V06",
    "NASA/GPM_L3/IMERG_MONTHLY_V06")==F){stop(collection,"\nColeção não suportada. Consulte a lista de opções na documentação.\n")}

  # ### coleção
  # if(collection==""){
  #
  #   #datas
  #
  #   #visualização
  #
  #   #bandas dos indices
  #   lista.bandas.indices<-list(BLUE="",RED="",NIR="",SWIR2="")
  #
  #   #pixels de qualidade
  #   banda.qualidade<-""
  #   banda.qualidade.auxiliar<-NULL
  #   tipo.qualidade.auxiliar<-NULL
  #   valOK<-c() #implementadas opções com 1 ou 3 valores
  # }


  ### Sentinel-2 MSI - surface reflectance
  if(collection=="COPERNICUS/S2_SR"){
    #datas
    periodo.sat<-list(start=as.Date("2017-03-28"),end=as.Date("3000-01-01"))

    #visualização
    d.vpar<-list(bands=c("B4","B3","B2"),min=0,max=3000)
    d.vpar.pan<-NULL

    #bandas dos indices
    lista.bandas.indices<-list(BLUE="B2",RED="B4",NIR="B8",SWIR2="B12")

    #pixels de qualidade
    banda.qualidade<-"SCL"
    banda.qualidade.auxiliar<-"MSK_CLDPRB"
    tipo.qualidade.auxiliar<-c("c.prob","customQA") # default: só SCL (categorias, incluindo sombras); c.prob: SCL+MSK_CLDPRB; customQA: algoritmo próprio com CDI+c.dist(opcional)
    valOK<-c(4,5,6)
  }

  ### Sentinel-2 MSI - top of atmosphere
  if(collection=="COPERNICUS/S2"){
    #datas
    periodo.sat<-list(start=as.Date("2015-06-23"),end=as.Date("3000-01-01"))

    #visualização
    d.vpar<-list(bands=c("B4","B3","B2"),min=0,max=3000)
    d.vpar.pan<-NULL

    #bandas dos indices
    lista.bandas.indices<-list(BLUE="B2",RED="B4",NIR="B8",SWIR2="B12")

    #pixels de qualidade
    banda.qualidade<-"QA60"
    banda.qualidade.auxiliar<-"probability"
    tipo.qualidade.auxiliar<-c("c.prob","customQA") # default: só QA60 (2 categorias, NÃO considera sombras); c.prob: QA60+probability; customQA: algoritmo próprio com CDI+c.dist(opcional)
    valOK<-c(0)
  }

  ### TODO: inserir LS9 e Coleção 2 do ToA e Raw do LS8 e LS7
  #estudar diferença do QA_PIXEL quando o Bit 2 é usado (ex. LS8 C2 surface Reflectance) e quando não é usado (ex. LS8 C2 ToA e Raw)
  #indicador de qualidade parece razoavelmente padronizado na C2 (que é totalmente diferente da C1)
  # https://www.binary-code.org/binary/16bit/0000000001000000/

  ### Landsat 8 (coleção 2)
  if(collection=="LANDSAT/LC08/C02/T1_L2"){
    #datas
    periodo.sat<-list(start=as.Date("2013-04-11"),end=as.Date("3000-01-01"))

    #visualização
    d.vpar<-list(bands=c("SR_B4","SR_B3","SR_B2"),min=0,max=0.3,reescalonar=list(multiplica=0.0000275,soma=-0.2))
    d.vpar.pan<-NULL

    #bandas dos indices
    lista.bandas.indices<-list(BLUE="SR_B2",RED="SR_B4",NIR="SR_B5",SWIR2="SR_B7")

    #pixels de qualidade
    banda.qualidade<-"QA_PIXEL"
    banda.qualidade.auxiliar<-"ST_CDIST"
    tipo.qualidade.auxiliar<-"c.dist"
    valOK<-c(21824) #inclui água com o código 21952 . #TODO Entretanto isso traz implicação no comando imagem<-imagem$select(banda.qualidade)$eq(valOK)  #TODO
  }

  ### Landsat 8 (coleção 1) - Surface Reflectance
  if(collection=="LANDSAT/LC08/C01/T1_SR"){
    #datas
    periodo.sat<-list(start=as.Date("2013-04-11"),end=as.Date("3000-01-01"))

    #visualização
    d.vpar<-list(bands=c("B4","B3","B2"),min=0,max=3000,gamma=1.4)
    d.vpar.pan<-NULL

    #bandas dos indices
    lista.bandas.indices<-list(BLUE="B2",RED="B4",NIR="B5",SWIR2="B7")

    #pixels de qualidade
    banda.qualidade<-"pixel_qa"
    banda.qualidade.auxiliar<-NULL
    tipo.qualidade.auxiliar<-NULL
    valOK<-c(322)
  }

  ### Landsat 8 - Top of Atmosphere
  if(collection=="LANDSAT/LC08/C01/T1_TOA"){
    #datas
    periodo.sat<-list(start=as.Date("2013-04-11"),end=as.Date("3000-01-01"))

    #visualização
    d.vpar<-list(bands=c("B4","B3","B2"),min=0,max=0.4)
    d.vpar.pan<-list(bands=c("B8"),min=0,max=0.4)

    #bandas dos indices
    lista.bandas.indices<-list(BLUE="B2",RED="B4",NIR="B5",SWIR2="B7")

    #pixels de qualidade
    banda.qualidade<-"BQA"
    banda.qualidade.auxiliar<-NULL
    tipo.qualidade.auxiliar<-NULL
    valOK<-c(2720)
  }

  ### Landsat 8 - Raw
  if(collection=="LANDSAT/LC08/C01/T1"){
    #datas
    periodo.sat<-list(start=as.Date("2013-04-11"),end=as.Date("3000-01-01"))

    #visualização
    d.vpar<-list(bands=c("B4","B3","B2"),min=0,max=30000)
    d.vpar.pan<-list(bands=c("B8"),min=0,max=30000)

    #bandas dos indices
    lista.bandas.indices<-list(BLUE="B2",RED="B4",NIR="B5",SWIR2="B7")

    #pixels de qualidade
    banda.qualidade<-"BQA"
    banda.qualidade.auxiliar<-NULL
    tipo.qualidade.auxiliar<-NULL
    valOK<-c(2720)
  }

  ### Landsat 7 (coleção 2)
  if(collection=="LANDSAT/LE07/C02/T1_L2"){
    #datas
    periodo.sat<-list(start=as.Date("1999-01-01"),end=as.Date("3000-01-01"))

    #visualização
    d.vpar<-list(bands=c("SR_B3","SR_B2","SR_B1"),min=0,max=0.3,reescalonar=list(multiplica=0.0000275,soma=-0.2))
    d.vpar.pan<-NULL

    #bandas dos indices
    lista.bandas.indices<-list(BLUE="SR_B1",RED="SR_B3",NIR="SR_B4",SWIR2="SR_B7")

    #pixels de qualidade
    banda.qualidade<-"QA_PIXEL"
    banda.qualidade.auxiliar<-"ST_CDIST"
    tipo.qualidade.auxiliar<-"c.dist"
    valOK<-c(5440)
  }

  ### Landsat 7 - Top of Atmosphere
  if(collection=="LANDSAT/LE07/C01/T1_TOA"){
    #datas
    periodo.sat<-list(start=as.Date("1999-01-01"),end=as.Date("3000-01-01"))

    #visualização
    d.vpar<-list(bands=c("B3","B2","B1"),min=0,max=0.4,gamma=1.2)
    d.vpar.pan<-list(bands=c("B8"),min=0,max=0.4,gamma=1.2)

    #bandas dos indices
    lista.bandas.indices<-list(BLUE="B1",RED="B3",NIR="B4",SWIR2="B7")

    #pixels de qualidade
    banda.qualidade<-"BQA"
    banda.qualidade.auxiliar<-NULL
    tipo.qualidade.auxiliar<-NULL
    valOK<-c(672)
  }

  ### Landsat 7 - Raw
  if(collection=="LANDSAT/LE07/C01/T1"){
    #datas
    periodo.sat<-list(start=as.Date("1999-01-01"),end=as.Date("3000-01-01"))

    #visualização
    d.vpar<-list(bands=c("B3","B2","B1"),min=0,max=255)
    d.vpar.pan<-list(bands=c("B8"),min=0,max=255)

    #bandas dos indices
    lista.bandas.indices<-list(BLUE="B1",RED="B3",NIR="B4",SWIR2="B7")

    #pixels de qualidade
    banda.qualidade<-"BQA"
    banda.qualidade.auxiliar<-NULL
    tipo.qualidade.auxiliar<-NULL
    valOK<-c(672)
  }

  ### GPM: Global Precipitation Measurement (GPM) v6
  if(collection=="NASA/GPM_L3/IMERG_V06"){
    #datas
    periodo.sat<-list(start=as.Date("2000-06-01"),end=as.Date("3000-01-01"))

    #visualização
    d.vpar<-list(bands=c("precipitationCal"),min=0,max=5,palette=c('#000096','#0064ff', '#00b4ff', '#33db80', '#9beb4a','#ffeb00', '#ffb300', '#ff6400', '#eb1e00', '#af0000'))
    d.vpar.pan<-NULL

    #bandas dos indices
    lista.bandas.indices<-NULL

    #pixels de qualidade
    banda.qualidade<-NULL
    banda.qualidade.auxiliar<-NULL
    tipo.qualidade.auxiliar<-NULL
    valOK<-NULL
  }

  ### GPM: Monthly Global Precipitation Measurement (GPM) v6
  if(collection=="NASA/GPM_L3/IMERG_MONTHLY_V06"){
    #datas
    periodo.sat<-list(start=as.Date("2000-06-01"),end=as.Date("3000-01-01"))

    #visualização
    d.vpar<-list(bands=c("precipitation"),min=0,max=1.5,palette=c('#000096','#0064ff', '#00b4ff', '#33db80', '#9beb4a','#ffeb00', '#ffb300', '#ff6400', '#eb1e00', '#af0000'))

    d.vpar.pan<-NULL

    #bandas dos indices
    lista.bandas.indices<-NULL

    #pixels de qualidade
    banda.qualidade<-NULL
    banda.qualidade.auxiliar<-NULL
    tipo.qualidade.auxiliar<-NULL
    valOK<-NULL
  }

  ##########################
  ##### Organizando ########
  ##########################

  get.ecor.date.time<-Sys.time() #para identificar a data/hora de início de execução nos arquivos de saída

  tryCatch({
    start<-as.Date(start)
    end<-as.Date(end)
  },error=function(e){cat("Error in start or end date. Do these dates exist? (eg did you indicate the 31st of the month with 30 days?)")})

  if(start < periodo.sat$start | end > periodo.sat$end ){stop(paste("Selected period exceeds data availability . \n Collection",collection, "have data from", periodo.sat$start, "to",ifelse(periodo.sat$end>as.Date("2500-01-01"),"present (with delay)",periodo.sat$end)),".")}

  if(is.null(bands.eval)&bands.vis==F&(is.null(indices)|length(indices)==0)){stop("Need to select bands, indices or set bands.vis = T")}
  if(is.null(bands.eval)&bands.vis==F){bandas<-c()}
  if(is.null(bands.eval)){bands.eval<-d.vpar$bands}

  bandas<-bands.eval
  if ("EVI"%in%indices){bandas<-sort(unique(c(bandas,lista.bandas.indices$BLUE,lista.bandas.indices$RED,lista.bandas.indices$NIR)))}
  if ("NDVI"%in%indices){bandas<-sort(unique(c(bandas,lista.bandas.indices$RED,lista.bandas.indices$NIR)))}
  if ("NBR"%in%indices){bandas<-sort(unique(c(bandas,lista.bandas.indices$NIR,lista.bandas.indices$SWIR2)))}
  if (bands.vis==T){bandas<-sort(unique(c(bandas,d.vpar$bands,d.vpar.pan$bands)))}

  if(is.null(c.dist)==F){if(c.dist==0){c.dist<-NULL}}
  if(is.null(c.prob)==F & ifelse(is.null(tipo.qualidade.auxiliar),T,"c.prob"%in%tipo.qualidade.auxiliar==F)){stop("Selected collection do not support cloud probability quality criterion. Set c.prob=NULL")}
  if(is.null(c.prob)==F){if(c.prob==0){c.prob<-NULL}}

  if(is.null(clouds.sentinel)==F){
    if(collection%in%c("COPERNICUS/S2_SR","COPERNICUS/S2")==F){stop("Selected collection do not support clouds.sentinel methods. Set clouds.sentinel=NULL")}
    if(clouds.sentinel!="CDI" & collection=="COPERNICUS/S2"){warning("Methods default and c.prob for cloud detection in Sentinel TOA does not take cloud shadows into account. Consider using clouds.sentinel=\"CDI\" or using Sentinel SR data (collection COPERNICUS/S2_SR)")}
    if(clouds.sentinel=="default" & (is.null(c.prob)==F | is.null(c.dist)==F)){stop("Method clouds.sentinel=\"default\" do not support c.prob or c.dist. Set both arguments to NULL.")}
  }

  if(is.null(banda.qualidade)){
    if(is.null(c.dist)==F|is.null(c.prob)==F|is.null(pOK)==F){stop("Selected collection do not supports cloud detection. Set c.dist, c.prob and pOK to NULL")}
  }

  cirrusthreshold<-cirrus.threshold #pontos causam problemas para variáveis usadas dentro do rgee (usei com ponto para manter consistência)
  NIRthreshold<-NIR.threshold
  CDIthreshold<-CDI.threshold
  dmaxshadow<-dmax.shadow

  if(is.null(seasons)){seasons=list(s1=c(), s2=c(), s3=c(), s4=c())}

  if (length(unlist(seasons))!=length(unique(unlist(seasons)))){stop("Each month can only be allocated to one season. If you need other ways of separating dates (eg by day of the month), disable the use of stations and post-process the data for all dates.")}

  #Estações - checagens e organização
  if(length(unlist(seasons))>0){group.by<-"season"} else {group.by<-"month"}

  if(group.by=="season"){
    if(sum(duplicated(unlist(seasons)))>0){stop("A month cannot belong to more than one season.")}

    prov<-c(unlist(seasons)[1]:12,1:12)
    ord.seasons<-list(s1=c(),s2=c(),s3=c(),s4=c())
    if(is.null(seasons$s1)==F){ord.seasons$s1<-which(prov%in%seasons$s1)[1:length(seasons$s1)]}
    if(is.null(seasons$s2)==F){ord.seasons$s2<-which(prov%in%seasons$s2)[1:length(seasons$s2)]}
    if(is.null(seasons$s3)==F){ord.seasons$s3<-which(prov%in%seasons$s3)[1:length(seasons$s3)]}
    if(is.null(seasons$s4)==F){ord.seasons$s4<-which(prov%in%seasons$s4)[1:length(seasons$s4)]}

    if(
      if(is.null(ord.seasons$s1)==F){sum(between(x=c(ord.seasons$s2,ord.seasons$s3,ord.seasons$s4),min(ord.seasons$s1),max(ord.seasons$s1)))} else{F} +
      if(is.null(ord.seasons$s2)==F){sum(between(x=c(ord.seasons$s1,ord.seasons$s3,ord.seasons$s4),min(ord.seasons$s2),max(ord.seasons$s2)))} else{F} +
      if(is.null(ord.seasons$s3)==F){sum(between(x=c(ord.seasons$s1,ord.seasons$s2,ord.seasons$s4),min(ord.seasons$s3),max(ord.seasons$s3)))} else{F} +
      if(is.null(ord.seasons$s4)==F){sum(between(x=c(ord.seasons$s1,ord.seasons$s2,ord.seasons$s3),min(ord.seasons$s4),max(ord.seasons$s4)))} else{F} > 0){
      stop("Seasons cannot encompass other seasons. Example: If you want to study a station and the transitions \n Do not use: s1=c(5,10), s2=c(6,7,8,9), s3=c(), s4=c() \n Use instead: s1=c(5), s2=c(6,7,8,9), s3=c(10), s4=c()")
    }
    rm(prov)
    rm(ord.seasons)
  }

  if(is.null(composite)==F && composite%in%c("min","max","mean","median")==F){stop("Available options to argument composite: min, max, mean, median")}


  #################################################################
  ### Organizando polígonos (site, circles, plots, samples) #######
  #################################################################

  samples<-NULL

  #points->circles
  if(sum(class(points)=="sf")) {

    if(projected==F & is.null(custom.crs)){
      stop(print("You need to project your spatial objects that you intend to apply buffers before run get.ecors or provide custom.crs parameter"))
    }

    if(projected==T & is.null(custom.crs)){
      circles<-st_buffer(points,dist=buffer.points)
    }

    if(is.null(custom.crs)==F){
      points.crs<-st_crs(points)
      points<-st_transform(points,crs=custom.crs)
      circles<-st_buffer(points,dist=buffer.points)
      circles<-st_transform(circles,crs=points.crs)
    }

    names(circles)[names(circles)==attr(circles,"sf_column")]<-"geometry" #geometry may or not have this name. This makes consistent between objects.
    st_geometry(circles)<-"geometry"
    names(circles)[id.column]<-"id"
    circles<-circles[,id.column]
    circles$type<-"circles"
    circles.gee<-sf_as_ee(circles)
    samples<-circles #se houver points e plots, vai corrigir adiante
  } else {cat("\n No valid file with points \n")}

  #plots
  if(sum(class(plots)=="sf")){

    names(plots)[names(plots)==attr(plots,"sf_column")]<-"geometry" #geometry may or not have this name. This makes consistent between objects.
    st_geometry(plots)<-"geometry"
    names(plots)[id.column]<-"id"
    plots<-plots[,id.column]

    if (buffer.plots>0){
      if(projected==F & is.null(custom.crs)){
        stop(print("You need to project your spatial objects that you intend to apply buffers before run get.ecors or provide custom.crs parameter"))
      }

      if(projected==T & is.null(custom.crs)){
        plots.buf<-st_buffer(plots,dist=buffer.plots)
      }

      if(is.null(custom.crs)==F){
        plots.crs<-st_crs(plots)
        plots<-st_transform(plots,crs=custom.crs)
        plots.buf<-st_buffer(plots,dist=buffer.plots)
        plots.buf<-st_transform(plots.buf,crs=plots.crs)
      }

      plots.buf$type<-"plots.buffer"

    } else {
      plots.buf<-plots
      plots.buf$type<-"plots"}

    samples<-plots.buf #pode sobrescrever mas se houver points e plots, vai corrigir adiante
  } else {cat("\n No valid file with plots \n")}

  #Organizando objeto samples
  if(sum(class(points)=="sf",class(plots)=="sf")==2) {
    samples<-rbind(circles,plots.buf)
  }
  if(is.null(samples)==F){
    area.m2<-as.numeric(st_area(samples)) #para metadata
  }else{area.m2<-NULL}

  if(eval.area=="site"){
    if(is.null(site) || "sf" %in% class(site)==F){stop(print("When eval.area=\"site\" you need to set a sf object to site argument."))}
    eval.area.gee<-sf_as_ee(site)
  }
  if(eval.area=="samples"){
    if(is.null(samples) || "sf" %in% class(samples)==F){stop(print("When eval.area=\"samples\" you need to set a sf object to samples argument."))}
    eval.area.gee<-sf_as_ee(samples)}

  if(is.null(site)){site.gee<-NULL} else
    {site.gee<-sf_as_ee(site)
    cat("\n Site polygon loaded \n")
    }

  if(is.null(plots)==F){cat("\n Plots loaded \n")}
  if(is.null(points)==F){cat("\n Points loaded \n")}


  if(is.null(samples)){samples.gee<-NULL} else {samples.gee<-sf_as_ee(samples)}

  ##################################################
  ### Seleção de imagens nas datas de interesse ####
  ##################################################

  colle.ori<-ee$ImageCollection(collection)$filterBounds(eval.area.gee)$filterDate(as.character(start),as.character(end))
  colle.ori<-colle.ori$map(function(imagem){
    year_month_day<-ee$Date(imagem$get('system:time_start'))$format("YYYY-MM-dd")
    imagem$set("year_month_day",year_month_day)
  })

  colle.ori.dates<-as.Date("1900-01-01")
  n.imagens.ori<-colle.ori$size()$getInfo()
  for (i in 1:n.imagens.ori){
    colle.ori.dates[i]<-colle.ori[[i]]$get("year_month_day")$getInfo()
    cat("\n Original image (before filtering)",i,":", format(colle.ori.dates[i]))
  }

  if(n.imagens.ori<2){stop("get.ecors needs at least 2 images to proceed. Increase start to end interval")}

  if(is.null(unlist(seasons))==F){dates.inter<-colle.ori.dates[as.numeric(format(colle.ori.dates,format="%m"))%in%unlist(seasons)]} else {dates.inter<-colle.ori.dates}
  colle<-colle.ori$filter(ee$Filter$inList("year_month_day",ee$List(as.character(dates.inter))))#removendo meses que não são de interesse
  cat(paste0("\n\nNumber of images in months of interest: ",length(dates.inter)))

  if(n.imagens.ori<2){stop("get.ecors needs at least 2 images in months of interest to proceed. Increase start to end interval")}


  ###########################################
  #### Filtrando imagens boas no sítio ######
  ###########################################

  if(is.null(banda.qualidade)==F){
    # Sentinel - Identificação de núvens e sombras baseada no CDI
    # Frantz, D., Hass, E., Uhl, A., Stoffels, J., & Hill, J. (2018). Improvement of the Fmask algorithm for Sentinel-2 images: Separating clouds from bright surfaces based on parallax effects. Remote sensing of environment, 215, 471-481.
    indexJoin<-function(collectionA, collectionB, propertyName) {
      joined<-ee$ImageCollection(ee$Join$saveFirst(propertyName)$apply(
        primary=collectionA,
        secondary=collectionB,
        condition=ee$Filter$equals(leftField="system:index",rightField="system:index")
      ))
      return(joined$map(function(imagem) {
        imagem$addBands(ee$Image(imagem$get(propertyName)))
      }))
    }

    if(is.null(clouds.sentinel)==F && clouds.sentinel=="CDI"){
      fun.nuvens.sombras<-function(imagem){
        cdi<-ee$Algorithms$Sentinel2$CDI(imagem)
        cdiBin<-cdi$lt(CDIthreshold)

        not_water<-imagem$select("SCL")$neq(6)
        dark_pixels<-imagem$select("B8")$lt(NIRthreshold*10000)$multiply(not_water)$rename("dark_pixels")

        cirrus<-imagem$select("B10")$multiply(0.0001)$gt(cirrusthreshold)
        nuvem<-cdiBin$add(cirrus)
        nuvem<-nuvem$gt(0)
        nuvemfocal<-nuvem$focalMode(3) # remover pixels isolados

        dnuvem<-nuvemfocal$reproject(crs=nuvem$projection(), scale=100)
        dnuvem<-dnuvem$fastDistanceTransform(units="pixels")$sqrt()
        sombra<-dnuvem$select("distance")$lt(dmaxshadow*0.01) # distância m para pixels (100 m)
        sombra<-sombra$multiply(dark_pixels)

        customQA<-nuvemfocal$add(sombra)$gt(0)$rename("customQA")
        customQA<-customQA$focalMode(1.5)
        imagem<-imagem$addBands(customQA)
        return(imagem)
      }

      fun.nuvens.sombrasTOA<-function(imagem){#sem o not_water
        cdi<-ee$Algorithms$Sentinel2$CDI(imagem)
        cdiBin<-cdi$lt(CDIthreshold)

        dark_pixels<-imagem$select("B8")$lt(NIRthreshold*10000)$rename("dark_pixels")

        cirrus<-imagem$select("B10")$multiply(0.0001)$gt(cirrusthreshold)
        nuvem<-cdiBin$add(cirrus)
        nuvem<-nuvem$gt(0)
        nuvemfocal<-nuvem$focalMode(3) # remover pixels isolados

        dnuvem<-nuvemfocal$reproject(crs=nuvem$projection(), scale=100)
        dnuvem<-dnuvem$fastDistanceTransform(units="pixels")$sqrt()
        sombra<-dnuvem$select("distance")$lt(dmaxshadow*0.01) # distância m para pixels (100 m)
        sombra<-sombra$multiply(dark_pixels)

        customQA<-nuvemfocal$add(sombra)$gt(0)$rename("customQA")
        customQA<-customQA$focalMode(1.5)
        imagem<-imagem$addBands(customQA)
        return(imagem)
      }

      fun.c.dist<-function(imagem){
        dnuvem<-imagem$select("customQA")
        dnuvem<-dnuvem$reproject(crs=dnuvem$projection(), scale=100)
        dnuvem<-dnuvem$fastDistanceTransform(units="pixels")$sqrt()
        customQA<-dnuvem$select("distance")$lt(c.dist*0.01)$rename("customQA") # distância metros para pixels (100 m)
        imagem<-imagem$addBands(customQA,overwrite=T)
        return(imagem)
      }

      if(collection=="COPERNICUS/S2_SR"){
        sentinelTOA<-ee$ImageCollection("COPERNICUS/S2")$filterBounds(eval.area.gee)$filterDate(as.character(start),as.character(end))$select(c("B7","B8","B8A","B10"))
        colle<-indexJoin(colle,sentinelTOA,"l1c")
        colle<-ee$ImageCollection(colle$map(fun.nuvens.sombras))
      }

      if(collection=="COPERNICUS/S2"){
        colle<-ee$ImageCollection(colle$map(fun.nuvens.sombrasTOA))
      }

      if(is.null(c.dist)==F){
        colle<-ee$ImageCollection(colle$map(fun.c.dist))
      }
      banda.qualidade<-"customQA"
      banda.qualidade.auxiliar<-NULL
      c.dist<-NULL
      valOK<-0
    } #fechando função CDI

    if(collection=="COPERNICUS/S2"){
      sentinelcloudprob<-ee$ImageCollection("COPERNICUS/S2_CLOUD_PROBABILITY")$filterBounds(eval.area.gee)$filterDate(as.character(start),as.character(end))
      colle<-indexJoin(colle,sentinelcloudprob,"l1c")
    }
    ###

    colle<<-colle

    #base para máscara e quantidade pixels OK
    #dois critérios
    if(sum(tipo.qualidade.auxiliar%in%c("c.prob","c.dist")) & (is.null(c.prob)==F | is.null(c.dist)==F)){
      colle.quali<-colle$select(c(banda.qualidade,banda.qualidade.auxiliar))

      if(sum(tipo.qualidade.auxiliar=="c.dist")){
        pixelOK<-colle.quali$map(function(imagem){
          filtro1<-imagem$select(banda.qualidade.auxiliar)$gt(c.dist) #limiar inferior
          imagem<-imagem$select(banda.qualidade)$eq(valOK)
          imagem<-imagem$multiply(filtro1)
          imagem<-imagem$set("npixelOK",imagem$reduceRegion(
            reducer=ee$Reducer$sum()$unweighted(),
            geometry=eval.area.gee,
            scale=resolution)$get(banda.qualidade))
          return(imagem)
        })
      }
      if(sum(tipo.qualidade.auxiliar=="c.prob")){
        if(length(valOK)==1){
          pixelOK<-colle.quali$map(function(imagem){
            filtro1<-imagem$select(banda.qualidade.auxiliar)$lt(c.prob) #limiar superior
            imagem<-imagem$select(banda.qualidade)$eq(valOK)
            imagem<-imagem$multiply(filtro1)
            imagem<-imagem$set("npixelOK",imagem$reduceRegion(
              reducer=ee$Reducer$sum()$unweighted(),
              geometry=eval.area.gee,
              scale=resolution)$get(banda.qualidade))
            return(imagem)
          })}
        if(length(valOK)==3){
          pixelOK<-colle.quali$map(function(imagem){
            filtro1<-imagem$select(banda.qualidade.auxiliar)$lt(c.prob) #limiar superior
            imagem<-imagem$select(banda.qualidade)$expression(
              "(b(0) == v1 ||b(0) == v2 || b(0) == v3)",
              list(v1=valOK[1],v2=valOK[2],v3=valOK[3]))
            imagem<-imagem$multiply(filtro1)
            imagem<-imagem$set("npixelOK",imagem$reduceRegion(
              reducer=ee$Reducer$sum()$unweighted(),
              geometry=eval.area.gee,
              scale=resolution)$get(banda.qualidade))
            return(imagem)
          })}
      }
    }

    #um critério
    if(is.null(c.prob) & is.null(c.dist)){
      colle.quali<-colle$select(banda.qualidade)

      if(length(valOK)==1){
        pixelOK<-colle.quali$map(function(imagem){
          imagem<-imagem$eq(valOK)
          imagem<-imagem$set("npixelOK",imagem$reduceRegion(
            reducer=ee$Reducer$sum()$unweighted(),
            geometry=eval.area.gee,
            scale=resolution)$get(banda.qualidade))
        })
      }
      if(length(valOK)==3){
        pixelOK<-colle.quali$map(function(imagem){
          imagem<-imagem$expression(
            "(b(0) == v1 ||b(0) == v2 || b(0) == v3)",
            list(v1=valOK[1],v2=valOK[2],v3=valOK[3]))
          imagem<-imagem$set("npixelOK",imagem$reduceRegion(
            reducer=ee$Reducer$sum()$unweighted(),
            geometry=eval.area.gee,
            scale=resolution)$get(banda.qualidade))
        })
      }
    }

    #Pixels na área (não muda ao longo do tempo)
    npixel.eval.area<-colle.quali[[1]]$reduceRegion(
      reducer=ee$Reducer$count(),
      geometry=eval.area.gee,
      scale=resolution)$get(banda.qualidade)$getInfo()

    minpixelOK<-pOK*npixel.eval.area

    pixelOK.filt<-pixelOK$filterMetadata("npixelOK","greater_than",minpixelOK)

  } #fim: imagens sem pixel de qualidade desviam esse bloco)

  #Bloco: Quando não há bandas de qualidade
  if(is.null(banda.qualidade)){
    pseudo.banda.qualidade<-colle[[1]]$bandNames()$get(0)$getInfo()
    colle.pseudo.quali<-colle$select(pseudo.banda.qualidade)

    pixelOK<-colle.pseudo.quali$map(function(imagem){# nome mais apropriado seria pseudo.pixelOK (mantido por compatibilidade adiante)
      imagem<-imagem$lt(1000000000) # gera camada de pixels = 1
      imagem<-imagem$set("npixelOK",imagem$reduceRegion(
        reducer=ee$Reducer$sum()$unweighted(),
        geometry=eval.area.gee,
        scale=resolution)$get(pseudo.banda.qualidade)) #calcula número total de pixels
    })
    pixelOK.filt<-pixelOK #mantido nome impreciso por compatibilidade adiante
  }
  ###

  n.imagens<-pixelOK.filt$size()$getInfo()#quantidade de imagens selecionadas
  if (n.imagens>0){cat("\n",n.imagens,"selected images from a total of",pixelOK$size()$getInfo(), "images available in the period of interest \n ")} else{stop("No images selected in the chosen period with the requested quality level. Change/enlarge the period or reduce the value of the pOK argument.")}

  if(n.imagens<2){stop("get.ecors needs at least 2 images selected in quality control to proceed. Increase start to end interval or decrease quality parameters.")}

  #Lista de imagens filtrada
  lista.imagens<-c()
  cat("\nImage codes:")
  for (i in 1:n.imagens){
    lista.imagens[i]<-pixelOK.filt[[i]]$get("system:index")$getInfo()
    cat("\n Image",i,"of",n.imagens,":", lista.imagens[i])
  }

  #Filtrando coleção
  colle.filt<-colle$filter(ee$Filter$inList("system:index",ee$List(lista.imagens)))$select(bandas)

  #Organizando: lista de datas filtrada
  OKdates<-as.Date("1900-01-01")
  cat("\n\nImage dates:")

  for (i in 1:n.imagens){
    OKdates[i]<-ee$Date(colle.filt[[i]]$get('system:time_start'))$format("YYYY-MM-dd")$getInfo()
    cat("\n Image",i,"of",n.imagens,":", format(OKdates[i]))
  }

  ##############################
  ### Processamento índices ####
  ##############################

  add.image.index<-function(collection,selected.indices){

    if ("EVI"%in%selected.indices){
      collection<-collection$map(function(imagem){
        evi<-collection[[i]]$expression(
          "2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))",
          list("NIR"=collection[[i]]$select(lista.bandas.indices$NIR),
               "RED"=collection[[i]]$select(lista.bandas.indices$RED),
               "BLUE"=collection[[i]]$select(lista.bandas.indices$BLUE)))
        evi<-evi$rename("EVI")
        imagem<-imagem$addBands(evi)
      })
    }

    if ("NDVI"%in%selected.indices){
      collection<-collection$map(function(imagem){
        ndvi<-imagem$normalizedDifference(c(lista.bandas.indices$NIR,lista.bandas.indices$RED))$rename("NDVI")
        imagem<-imagem$addBands(ndvi)
      })
    }

    if ("NBR"%in%selected.indices){
      collection<-collection$map(function(imagem){
        nbr<-imagem$normalizedDifference(c(lista.bandas.indices$NIR,lista.bandas.indices$SWIR2))$rename("NBR")
        imagem<-imagem$addBands(nbr)
      })
    }

    return(collection)
  }

  colle.filt<-add.image.index(collection=colle.filt,selected.indices=indices)
  colle.filt<<-colle.filt

  ##############################################
  ### expectativa de dados e dados obtidos #####
  ##############################################

  # tabela de expectativas de dados (por mês)
  dates.table<-data.frame(date=seq(from=start,to=end,by="month"), season=NA, rep=0)
  dates.table$year<-as.numeric(format(dates.table$date,format="%Y"))
  dates.table$month<-as.numeric(format(dates.table$date,format="%m"))
  dates.table$season[dates.table$month%in%seasons$s1]<-"s1"
  dates.table$season[dates.table$month%in%seasons$s2]<-"s2"
  dates.table$season[dates.table$month%in%seasons$s3]<-"s3"
  dates.table$season[dates.table$month%in%seasons$s4]<-"s4"

  single.season<-F #starting value (could be changed in the next lines)

  if (group.by=="season"){
    if(is.null(seasons$s2)){
      single.season<-T
      seasons$s2<-tail(seasons$s1,n=1)+1 #pseudo s2 (will be removed in the final dates.table)
      if(seasons$s2==13){seasons$s2<-1}
    }
    seasons.used<-c("s1","s2","s3","s4")[lengths(seasons)>0]
    contador<-data.frame(matrix(nrow=1,ncol=length(seasons.used),c(0))) #versão para estações
    names(contador)<-seasons.used
  }

  if (group.by=="month"){
    seasons.used<-1:12
    dates.table$season<-dates.table$month #provisório para usar versão genérica do contador de repetições -> vai ser mudado adiante para "month"
    contador<-data.frame(matrix(nrow=1,ncol=12,c(0))) #versão para meses
    names(contador)<-1:12
  }

  dates.table<-na.exclude(dates.table)

  est.anterior<-c()
  for(i in 1:nrow(dates.table)){
    if(dates.table$season[i]%in%seasons.used | dates.table$season[i]=="month"){
      if(is.null(est.anterior)){
        contador[dates.table$season[i]]<-1
        dates.table$rep[i]<-1
        est.anterior<-dates.table$season[i]} else {
          if(is.na(dates.table$season[i-1])==F & est.anterior==dates.table$season[i]){
            dates.table$rep[i]<-contador[1,dates.table$season[i]]}
          if(is.na(dates.table$season[i-1])==T & est.anterior==dates.table$season[i]){
            contador[1,dates.table$season[i]]<-contador[1,dates.table$season[i]]+1
            dates.table$rep[i]<-contador[1,dates.table$season[i]]
            est.anterior<-dates.table$season[i]}
          if(est.anterior!=dates.table$season[i]){
            contador[1,dates.table$season[i]]<-contador[1,dates.table$season[i]]+1
            dates.table$rep[i]<-contador[1,dates.table$season[i]]
            est.anterior<-dates.table$season[i]}
        }
    }
  }

  if(single.season==T){                             #removing pseudo s2
    dates.table$season[dates.table$season=="s2"]<-NA
    dates.table<-na.exclude(dates.table)
    seasons<-list(s1=seasons$s1,s2=c(),s3=c(),s4=c())
    }


  #imagens disponíveis com estações e repetições
  images.table<-data.frame(images=lista.imagens, OKdates=OKdates, season=NA, rep=NA)
  images.table$year<-as.numeric(format(images.table$OKdates,format="%Y"))
  images.table$month<-as.numeric(format(images.table$OKdates,format="%m"))
  images.table$season[images.table$month%in%seasons$s1]<-"s1"
  images.table$season[images.table$month%in%seasons$s2]<-"s2"
  images.table$season[images.table$month%in%seasons$s3]<-"s3"
  images.table$season[images.table$month%in%seasons$s4]<-"s4"

  for(i in 1:nrow(images.table)){
    images.table$rep[i]<-dates.table$rep[dates.table$year==images.table$year[i] & dates.table$month==images.table$month[i]]
  }

  #Indicando número de imagens por mês e estação e corrigindo tabelas sem estações
  dates.table$n.imagesOK<-NA
  for (i in 1:nrow(dates.table)){dates.table$n.imagesOK[i]<-nrow(images.table%>%filter(year==dates.table$year[i],month==dates.table$month[i]))}
  if(group.by=="month"){#para análises por mês
    dates.table$season<-"not.eval"
    images.table$season<-"not.eval"
  }

  #####################################
  ### Composição e Máscara ############
  #####################################

  #completando images.table: rep.season/month, imagem
  if(group.by=="month"){
    if(max(images.table$rep)<10){#nome da coluna vai ser corrigido no final
      images.table<-images.table%>%mutate(rep.season=paste("r",rep,sprintf("m%02d",month),sep=""))
    } else {
      images.table<-images.table%>%mutate(rep.season=paste(sprintf("r%02d",rep),sprintf("m%02d",month),sep=""))
    }
  }
  if(group.by=="season"){
    if(max(images.table$rep)<10){
      images.table<-images.table%>%mutate(rep.season=paste("r",rep,season,sep=""))
    } else {
      images.table<-images.table%>%mutate(rep.season=paste(sprintf("r%02d",rep),sprintf("%02d",season),sep=""))
    }
  }
  images.table$rep.season.image[1]<-paste(images.table$rep.season[1],"i1",sep="")
  contador<-1
  for(i in 2:nrow(images.table)){
    if(images.table$rep.season[i-1]==images.table$rep.season[i]){
      contador<-contador+1
      images.table$rep.season.image[i]<-paste(images.table$rep.season[i],"i",contador,sep="")} else {
        contador<-1
        images.table$rep.season.image[i]<-paste(images.table$rep.season[i],"i1",sep="")}
  }
  rm(contador)

  ### Máscara e metadados com estação e repetição
  colle.mask<-colle.filt
  cat("\n")
  for (i in 1:n.imagens){
    cat("\n Masking images and adding information in the metadata: Image",i,"of",n.imagens,"- processing at",format(Sys.time(),"%H:%M"))
    colle.mask[[i]]<-colle.mask[[i]]$updateMask(pixelOK.filt[[i]])
    if(is.null(composite)==F){colle.mask[[i]]<-colle.mask[[i]]$set("rep_season",images.table$rep.season[i])}
  } #rep_season não será gravada nos metadados caso composição=NULL
  cat("\n ")
  colle.mask<<-colle.mask
  ####### Composição de pixels ########

  if(length(unique(images.table$rep.season)) > 1){
    grupos.compo<-ee$List(unique(images.table$rep.season))} else {
      grupos.compo<-"unico"}

  #início do bloco composição
  if(is.null(composite)==F && composite%in%c("min","max","mean","median")){
    if(composite=="min"){
      fun.compositora<-function(mi){
        imagem<-colle.mask$filterMetadata("rep_season","equals",mi)%>%
          ee$ImageCollection$min()
        return( imagem$set("system:index",mi) )
      }
    }

    if(is.null(composite)==F && composite=="max"){
      fun.compositora<-function(mi){
        imagem<-colle.mask$filterMetadata("rep_season","equals",mi)%>%
          ee$ImageCollection$max()
        return( imagem$set("system:index",mi) )
      }
    }

    if(is.null(composite)==F && composite=="mean"){
      fun.compositora<-function(mi){
        imagem<-colle.mask$filterMetadata("rep_season","equals",mi)%>%
          ee$ImageCollection$mean()
        return( imagem$set("system:index",mi) )
      }
    }

    if(is.null(composite)==F && composite=="median"){
      fun.compositora<-function(mi){
        imagem<-colle.mask$filterMetadata("rep_season","equals",mi)%>%
          ee$ImageCollection$median()
        return( imagem$set("system:index",mi) )
      }
    }

    #executando
    if(grupos.compo=="unico"){
      colle.mask.compo<-fun.compositora(unique(images.table$rep.season))#saída é uma imagem
      colle.mask.compo<-ee$ImageCollection(colle.mask.compo) }#transformando em coleção
    if(grupos.compo!="unico"){
      colle.mask.compo<-grupos.compo$map(ee_utils_pyfunc(fun.compositora))#saída é uma lista de imagens
      colle.mask.compo<-ee$ImageCollection(colle.mask.compo) }#transformando em coleção

  }#fim do bloco composição

  if(is.null(composite)){colle.mask.compo<-NULL}

  colle.mask.compo<<-colle.mask.compo

  if(group.by=="month"){images.table<-mutate(images.table,rep.month=rep.season,rep.month.image=rep.season.image,rep.season=NULL,rep.season.image=NULL)} #corrigindo nome

  out.get.ecors<-list(get.ecor.date.time=get.ecor.date.time, collection=collection, clouds.sentinel=clouds.sentinel, start=start, end=end, dates.inter=dates.inter,
                       composite=composite, group.by=group.by, seasons=seasons, dates.table=dates.table, images.table=images.table,
                       bands.eval=bands.eval, indices=indices, bands.used=bandas, d.vpar=d.vpar, d.vpar.pan=d.vpar.pan, resolution=resolution,
                       buffer.points=buffer.points, buffer.plots=buffer.plots, area.m2=area.m2,
                       site.gee=site.gee, samples.gee=samples.gee) #reduzido (sem coleções)
  #objetos super assign: colle, colle.filt, colle.mask, colle.mask.compo
  class(out.get.ecors)<-"ecors"

  return(out.get.ecors)
}



