#' ecors: Remote Sensing data acquisition and processing for ecology with Google Earth Engine
#'
#' Package to simplify the acquisition of data in Google Earth Engine and performs several steps of organization and processing typical of ecological studies, allowing pre-processing, visualization, data download and calculation of simple statistics commonly used in ecological studies. It aims to simplify the use of rgee and Google Earth Engine commands, performing internally several data manipulation steps. The function syntax follows a familiar style for R users who routinely perform statistical analysis for ecological studies. Requires pre-registration on the Google Earth Engine platform (https://earthengine.google.com/).
#' \cr
#' @details
#' Google Earth Engine main catalog
#' \itemize{
#'    \item [get.ecors]	Get data to ecors
#'    \item [stats.ecors]	Descritive statistics on ecors objects
#'    \item [plot.ecors]	Plot ecors object
#'    \item [download.ecors]	Download images
#' }
#' \cr
#' Land Use data
#' \itemize{
#'    \item [get.lu.ecors]	Get Land Use data to ecors
#'    \item [focal.lu.ecors] Applies a morphological reducer filter (focal) on Land Use images
#'    \item [convert.lu.ecors] Re-assign land use classes by converting pixel values
#'    \item [quantify.lu.ecors]	Quantify the pixels of each Land Use class
#'    \item [dist.lu.ecors]	Distance to Land Use class
#'    \item [plot.lu.ecors]	Plot mb.ecors object
#'    \item [download.lu.ecors]	Download Land Use images
#'}
#'
#' \cr
#' Misc
#' \itemize{
#'    \item [wakeup.gee] If you receive an error message regarding the Google Earth Engine server being disconnected due to inactivity, use this command to re-establish the connection
#' }
#'
#' @seealso
#' \itemize{
#'    \item <https://earthengine.google.com/>
#'    \item <https://github.com/r-spatial/rgee/> or [rgee]
#' }
#'
#' @docType package
#' @name ecors
NULL
#> NULL
