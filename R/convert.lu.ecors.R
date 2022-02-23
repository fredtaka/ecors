#' Convert Land Use classes
#'
#'
#' Re-assign land use classes by converting pixel values.
#'
#' @param x lu.ecors object (from get.lu.ecors).
#' @param old.value vector of original pixel values wich will be replaced.
#' @param new.value vector of pixel values that will replace the original values.
#' @param new.lu.class land use class names that will replace the originals (optional).
#' @param new.color colors used in plots that will replace the originals (optional).
#'
#' @return
#' Google Earth Engine container object lu.conv are exported to .GlobalEnv to be used in rgee functions and avoid errors (elapsed time limit). Object with metadata is returned.
#'
#' @export
#' @import rgee
#' @import sf
#' @import dplyr
#'
#' @examples
#' #Get a lu.ecors class object
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
#' # Grouping several native land use classes
#' converted<-convert.lu.ecors(x=lu2000_2010,old.value = c(1,3,4,10,11,12,13),
#'                              new.value = rep(1,times=7), new.lu.class = rep("Native",times=7))
#'
#' #Plotting
#' plot(x=converted, comp=lu2000_2010)
#'

convert.lu.ecors<-function(x,old.value,new.value,new.lu.class,new.color=NULL){
  if(class(x)!="lu.ecors"){stop("Argument x must be a lu.ecors class object.")}

  lu.prov<-eval(parse(text=x$object.name))

  if(length(old.value)!=length(new.value)){stop("old.value and new.value have different length")}

  if(is.null(new.lu.class)){new.lu.class<-rep(NA,length(new.value))}

  if(length(new.value)!=length(new.lu.class)){stop("new.value and new.lu.class have different length")}

  if(is.null(new.color)==F){
    if(length(new.value)!=length(new.color)){stop("new.value and new.color have different length")}
  }

  x$legend.lu$original.value<-x$legend.lu$pixel.value
  x$legend.lu$original.lu.class<-x$legend.lu$lu.class

  #create new legend
  for (i in 1:length(old.value)){
    x$legend.lu$pixel.value[x$legend.lu$original.value==old.value[i]]<-new.value[i]
    x$legend.lu$lu.class[x$legend.lu$original.value==old.value[i]]<-new.lu.class[i]
    if(is.null(new.color)==F){
      x$legend.lu$color[x$legend.lu$original.value==old.value[i]]<-new.color[i]}
  }

  if(is.null(new.color)){
    for(i in unique(x$legend.lu$pixel.value)){
      x$legend.lu$color[x$legend.lu$pixel.value==i]<-x$legend.lu$color[x$legend.lu$pixel.value==i][1] #pick the first value for all values of same category
    }
  }

  #create new palette
  x$palette.lu<-select(x$legend.lu,color,pixel.value,lu.class)
  x$legend.lu<-select(x$legend.lu,-color) #color do not make sense anymore

  for (i in 0:max(x$palette.lu$pixel.value)) {#preenchendo vazios na sequencia de valores de pixel
    if(i%in%x$palette.lu$pixel.value ==F){x$palette.lu<-rbind(x$palette.lu,data.frame(color="#ffffff",pixel.value=i,lu.class=paste0("NA",i)))}
  }
  x$palette.lu<-x$palette.lu[!duplicated(x$palette.lu),]

  x$palette.lu<-arrange(x$palette.lu,pixel.value)

  list.years<-paste0("classification_",x$years)

  originals<-x$legend.lu$original.value
  target<-x$legend.lu$pixel.value

  if(length(x$years) == 1){
    lu.conv<-lu.prov$remap(
      from=originals,
      to=target,
      bandName=list.years)
    lu.conv<-lu.conv$rename(list.years)#checar se comporta o paste0("classification_",x$years)
  }

  if(length(x$years) > 1){
    lu.conv<-lu.prov$select(list.years[1])$rename("temp")

    for (i in list.years){

      prov<-lu.prov$remap(
        from=originals,
        to=target,
        bandName=i)
      prov<-prov$rename(i)
      lu.conv<-lu.conv$addBands(prov)
    }
    lu.conv<-lu.conv$select(list.years)

  }

  lu.conv<<-lu.conv

  cat(paste("\nObject lu.conv was exported to .GlobalEnv \n"))

  #update additional metadata
  x$object.name<-"lu.conv"

  if(x$post.processing=="none"){
    x$post.processing<-"converted pixel values"
  } else {x$post.processing<-c(x$post.processing,"converted pixel values")}

  return(x)

}
