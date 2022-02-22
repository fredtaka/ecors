#' Focal reducer on mb.ecors object
#'
#'
#' Applies a morphological reducer filter (focal) on MapBiomas images
#'
#' @param x mb.ecors object (from get.mb.ecors).
#' @param window.radius radius of moving window (m).
#'
#' @details
#' Applies a focal (moving window) preprocessing on MapBiomas images to remove isolated pixels.
#' The window size indicates the size of the neighborhood area of the evaluated pixel used to calculate the "mode" value that will be used to replace the evaluated pixel value. Process is repeated successively for each pixel in the image.
#' Larger window.radius values cause more aggressive removal/simplification.
#' It is encouraged that the effect of this pre-processing be visually evaluated as it may not have been able to remove isolated pixels (potential misclassification) or it may have removed real data (linear features such as gallery forests or rivers are particularly susceptible to being removed by this technique).
#'
#' @return
#' Google Earth Engine container object are exported to .GlobalEnv to be used in rgee functions and avoid errors (elapsed time limit). Object name will be created using command execution time to avoid overwriting and to associate Google Earth Engine container object with a mb.ecors class object which contain metadata.
#'
#' @import rgee
#' @export
#'
#' @examples
#' # Get a mb.ecors class object
#' FAL.IBGE.JBB<-sf::st_read(system.file("extdata/FAL.IBGE.JBB.gpkg", package="ecors"))
#' test.points<-sf::st_read(system.file("extdata/Points_tests.gpkg", package="ecors"))
#' test.retangles<-sf::st_read(system.file("extdata/Plots_tests.gpkg", package="ecors"))
#'
#' # Get data (projecting to UTM 32S zone to performe buffer operations)
#' mb2000_2010<-get.mb.ecors(site=FAL.IBGE.JBB, points=test.points, plots=test.retangles,
#'      polygons=NULL, id.column=1, projected=F, custom.crs=32723,
#'      collection.mb=6, years=c(2000,2010), resolution=30, evaluate="surroundings.site",
#'      buffer1=5000, buffer2=10000, buffer3=NULL, cumulative.surroundings=F)
#'
#' # Focal post-processing
#' f_mb2000_2010<-focal.mb.ecors(x=mb2000_2010,window.radius=80)
#'
#' # Visually checking
#' plot(x=mb2000_2010, comp=f_mb2000_2010)
#'
focal.mb.ecors<-function(x, window.radius){

  if(class(x)!="mb.ecors"){stop("Argument x must be a mb.ecors class object.")}

  mb.prov<-eval(parse(text=x$object.name))

  mb.f<-mb.prov$focalMode(radius=window.radius,
                       kernelType="circle",
                       units="meters",
                       iterations=1)

  mb.f.name<-paste0("mb.focal.",format(Sys.time(),"%H_%M"))

  assign(mb.f.name,mb.f,envir=.GlobalEnv)

  cat(paste("\nObject",mb.f.name,"was exported to .GlobalEnv \n"))

  if(x$post.processing=="none"){
    x$post.processing<-paste0("focal window.radius = ",window.radius,"m")
  } else {x$post.processing<-c(x$post.processing,paste0("focal window.radius = ",window.radius,"m"))}

  x$object.name<-mb.f.name

  return(x)
}
