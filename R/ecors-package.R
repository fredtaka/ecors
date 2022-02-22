#' ecors: Remote Sensing data acquisition and processing for ecology with Google Earth Engine
#'
#' It simplifies the acquisition of data in Google Earth Engine and
#' performs several steps of organization and processing typical of ecological studies,
#' allowing the visualization of maps, data download and production of simple statistics
#' commonly used in ecological studies. It aims to simplify the use of commands from the
#' rgee package and the Google Earth Engine platform with a style of functions more
#' familiar to R users who usually perform statistical analysis for ecological studies.
#' Requires pre-registration on the Google Earth Engine platform (https://earthengine.google.com/).
#'
#' @details
#' Google Earth Engine main catalog
#' \itemize{
#'    \item [get.ecors]	Get data to ecors
#'    \item [stats.ecors]	Descritive statistics on ecors objects
#'    \item [plot.ecors]	Plot ecors object
#'    \item [download.ecors]	Download images
#'}
#'\cr
#'MapBiomas catalog
#'\itemize{
#'    \item [get.mb.ecors]	Get MapBiomas data to ecors
#'    \item [dist.mb.ecors]	Distance to Land Use class
#'    \item [count.mb.ecors]	Count pixels of each Land Use class
#'    \item [plot.focal.mb.ecors]	Plot mb.ecors object
#'    \item [plot.mb.ecors]	Plot mb.ecors object
#'    \item [download.mb.ecors]	Download MapBiomas images
#'}
#'
#' @seealso
#' \itemize{
#'    \item <https://earthengine.google.com/>
#'    \item <https://github.com/r-spatial/rgee/> or [rgee]
#'}
#'
#' @docType package
#' @name ecors
NULL
#> NULL
