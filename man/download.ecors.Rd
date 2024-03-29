% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download.ecors.R
\name{download.ecors}
\alias{download.ecors}
\title{Download images}
\usage{
download.ecors(
  x,
  ecors.type,
  subset = NULL,
  vis.bands = T,
  new.list.bands = NULL,
  ref.site = F,
  ref.samples = F,
  new.ref.poly = NULL,
  exp.degree = 0.05,
  images.folder = getwd(),
  clear.prov = F
)
}
\arguments{
\item{x}{ecors object (from get.ecors).}

\item{ecors.type}{type of images processed by get.ecors. Options are "original", "filtered", "mask", "composite". More information in detais section.}

\item{subset}{numerical vector with selected images (sequential number of images, eg. c(1, 2, 5) )}

\item{vis.bands}{include vizualization bands in the download?}

\item{new.list.bands}{selects a subset of bands to download (among those previously chosen in get.ecors).}

\item{ref.site}{use site extent as reference for the downloaded images extent? (TRUE or FALSE)}

\item{ref.samples}{use samples extent as reference for the downloaded images extent? (TRUE or FALSE)}

\item{new.ref.poly}{set a new polygon as reference for the downloaded images extent.}

\item{exp.degree}{number of degrees to expand (and in all directions) the extent of downloaded images (decimal degree format).}

\item{images.folder}{local folder to save images files.}

\item{clear.prov}{delete ALL files from temporary folder (ecors_temp) in your Google Drive account after download?}
}
\value{
Prints images table and saves images in local system.
}
\description{
Download from Google Earth Engine all images selected, masked and (optionally) composed by get.ecors function.
}
\details{
Argument ecors.type selects which processing level of get.ecors will be downloaded. Option "original" use all images available in the period without processing, "filtered" use only images approved in get.ecors quality control.
Option "mask" use same images as the previous one but with bad pixels masked, "composite" use compositions performed on the images of the previous option. \cr
If more than one reference type is give for the extent (ref.site, ref.samples, new.ref.poly) the extent that encompasses all these geometries will be used.
}
\examples{
#Get a ecors class object
FAL.IBGE.JBB<-sf::st_read(system.file("extdata/FAL.IBGE.JBB.gpkg", package="ecors"))
test.plots<-sf::st_read(system.file("extdata/Plots_tests.gpkg", package="ecors"))
test.points<-sf::st_read(system.file("extdata/Points_tests.gpkg", package="ecors"))

#library(ecors)

d2020<-get.ecors(site=FAL.IBGE.JBB, points=test.points, plots=test.plots, buffer.points=500, buffer.plots=500,
    eval.area="site", projected=F, custom.crs=32723,
    collection="LANDSAT/LC08/C02/T1_L2", start=c("2020-01-01"), end=c("2020-12-31"),
    bands.eval="SR_B3", bands.vis=T, indices=c("NDVI"), resolution=30,
    pOK=0.3, c.prob=NULL, c.dist=100, clouds.sentinel=NULL, cirrus.threshold=NULL, NIR.threshold=NULL, CDI.threshold=NULL, dmax.shadow=NULL,
    seasons=list(s1=c(11,12,1,2), s2=c(3,4), s3=c(5,6,7,8), s4=c(9,10)), group.by="season", composite=NULL)

#Download
download.ecors(x=d2020, ecors.type="composite",vis.bands=T,
   new.list.bands=NULL,ref.site=T,ref.samples=T,exp.degree=0.05,
   images.folder=getwd(),clear.prov=F)

}
