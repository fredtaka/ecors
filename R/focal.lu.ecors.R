#' Focal reducer on lu.ecors object
#'
#'
#' Applies a morphological reducer filter (focal) on Land Use images
#'
#' @param x lu.ecors object (from get.lu.ecors).
#' @param window.radius radius of moving window (m).
#'
#' @details
#' Applies a focal (moving window) preprocessing on Land Use images to remove isolated pixels.
#' The window size indicates the size of the neighborhood area of the evaluated pixel used to calculate the "mode" value that will be used to replace the evaluated pixel value. Process is repeated successively for each pixel in the image.
#' Larger window.radius values cause more aggressive removal/simplification.
#' It is encouraged that the effect of this pre-processing be visually evaluated as it may not have been able to remove isolated pixels (potential misclassification) or it may have removed real data (linear features such as gallery forests or rivers are particularly susceptible to being removed by this technique).
#'
#' @return
#' Google Earth Engine container object are exported to .GlobalEnv to be used in rgee functions and avoid errors (elapsed time limit). Object name will be created using command execution time to avoid overwriting and to associate Google Earth Engine container object with a lu.ecors class object which contain metadata.
#'
#' @import rgee
#' @export
#'
#' @examples
#' # Get a lu.ecors class object
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
#' # Focal post-processing
#' f_lu2000_2010<-focal.lu.ecors(x=lu2000_2010,window.radius=80)
#'
#' # Visually checking
#' plot(x=lu2000_2010, comp=f_lu2000_2010)
#'
focal.lu.ecors<-function(x, window.radius){

  if(class(x)!="lu.ecors"){stop("Argument x must be a lu.ecors class object.")}

  lu.prov<-eval(parse(text=x$object.name))

  lu.f<-lu.prov$focalMode(radius=window.radius,
                       kernelType="circle",
                       units="meters",
                       iterations=1)

  lu.f.name<-paste0("lu.focal.",format(Sys.time(),"%H_%M"))

  assign(lu.f.name,lu.f,envir=.GlobalEnv)

  cat(paste("\nObject",lu.f.name,"was exported to .GlobalEnv \n"))

  if(x$post.processing=="none"){
    x$post.processing<-paste0("focal window.radius = ",window.radius,"m")
  } else {x$post.processing<-c(x$post.processing,paste0("focal window.radius = ",window.radius,"m"))}

  x$object.name<-lu.f.name

  return(x)
}
