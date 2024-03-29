% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert.lu.ecors.R
\name{convert.lu.ecors}
\alias{convert.lu.ecors}
\title{Convert Land Use classes}
\usage{
convert.lu.ecors(x, old.value, new.value, new.lu.class, new.color = NULL)
}
\arguments{
\item{x}{lu.ecors object (from get.lu.ecors).}

\item{old.value}{vector of original pixel values wich will be replaced.}

\item{new.value}{vector of pixel values that will replace the original values.}

\item{new.lu.class}{land use class names that will replace the originals (optional).}

\item{new.color}{colors used in plots that will replace the originals (optional).}
}
\value{
Google Earth Engine container object lu.conv are exported to .GlobalEnv to be used in rgee functions and avoid errors (elapsed time limit). Object with metadata is returned.
}
\description{
Re-assign land use classes by converting pixel values.
}
\examples{
#Get a lu.ecors class object
FAL.IBGE.JBB<-sf::st_read(system.file("extdata/FAL.IBGE.JBB.gpkg", package="ecors"))
test.points<-sf::st_read(system.file("extdata/Points_tests.gpkg", package="ecors"))
test.plots<-sf::st_read(system.file("extdata/Plots_tests.gpkg", package="ecors"))

# Get data (projecting to UTM 32S zone to performe buffer operations)
lu2000_2010<-get.lu.ecors(site=FAL.IBGE.JBB, points=test.points, plots=test.plots,
     polygons=NULL, id.column=1, projected=F, custom.crs=32723,
     collection.lu="mapbiomas6", years=c(2000,2010), resolution=30, evaluate="surroundings.site",
     buffer1=5000, buffer2=10000, buffer3=NULL, cumulative.surroundings=F)

# Grouping several native land use classes
converted<-convert.lu.ecors(x=lu2000_2010,old.value = c(1,3,4,10,11,12,13),
                             new.value = rep(1,times=7), new.lu.class = rep("Native",times=7))

#Plotting
plot(x=converted, comp=lu2000_2010)

}
