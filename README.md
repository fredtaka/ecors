# ecors
#### Remote Sensing data acquisition and processing for ecology with Google Earth Engine.

<br>
<br>

R Package to simplify the acquisition of data in Google Earth Engine and performs several steps of organization and processing typical of ecological studies, allowing pre-processing, visualization, data download and calculation of simple statistics commonly used in ecological studies. It aims to simplify the use of rgee and Google Earth Engine commands, performing internally several data manipulation steps. The function syntax follows a familiar style for R users who perform statistical analysis in ecology.


## Instalation

Requires pre-registration on the Google Earth Engine platform (https://earthengine.google.com/).


Install package dependencies from CRAN:

``` r
install.packages("sf")
install.packages("googledrive")
install.packages("dplyr")
install.packages("readODS")
install.packages("remotes")
install.packages("rgee")
```

Install additional dependency from GitHub:

```r
library(remotes) #derived from devtools package
install_github("r-earthengine/rgeeExtra")
```

Configure rgee Phyton integration. See [rgee page](https://github.com/r-spatial/rgee#installation) for details or follow the steps below:

```r
library(rgee)
ee_install()
ee_check() # Check non-R dependencies (optional)
```

Install ecors:

```r
install_github("fredtaka/ecors")
```

## Hello World

### 1. Get good Landsat images of my study site

Load some sf features to use as site (polygons), plots (polygons) and points (points):

```r
library(sf)
FAL.IBGE.JBB<-st_read(system.file("extdata/FAL.IBGE.JBB.gpkg", package="ecors"))
test.plots<-st_read(system.file("extdata/Plots_tests.gpkg", package="ecors"))
test.points<-st_read(system.file("extdata/Points_tests.gpkg", package="ecors"))
```

Ask ecors to select good images and make some pre-processing:

- Set sf objects for study site and sampling points and plots
- Ask to make a buffer of 100 m around points and plots (area of influence on my data collected in the field)
- Choose crs code for projecting the polygons and points. In the location used for the example, WGS 84 / UTM zone 23S is a good choice (EPSG code 32723). Use <https://epsg.io/> to help you find crs codes (EPSG) for your location
- Choose remote sensing collection
- Choose date range
- Select bands and resolution and ask for NDVI calculation
- Define minimum image quality indicators on the study site or sampling units
- State how the months should be grouped into seasons
- Ask for a composite image of each season
- (see ?get.ecors for further options)

```r
library(ecors)
d2020<-get.ecors(site=FAL.IBGE.JBB, points=test.points, plots=test.plots, 
    buffer.points=100, buffer.plots=100, 
    projected=F, custom.crs=32723,
    collection="LANDSAT/LC08/C02/T1_L2", 
    start=c("2020-01-01"), end=c("2020-12-31"),
    bands.eval=c("SR_B3","SR_B4"), bands.vis=T, indices=c("NDVI"), resolution=30,
    eval.area="samples", pOK=0.7, c.prob=NULL, c.dist=100,
    seasons=list(s1=c(11,12,1,2), s2=c(3,4), s3=c(5,6,7,8), s4=c(9,10)), group.by="season", 
    composite="mean")
```

### 2. Create interactive visualizations

Plotting options:

- Comparing selected imagens: original and cloud masked;
- NDVI;
- Composite images of each season.

```r
plot(x=d2020, ecors.type="filtered+mask")
plot(x=d2020, ecors.type="mask", visualization="custom", defaults=F,
     legend=T, bands="NDVI", pixel.min=-1, pixel.max=1, image.gamma=NULL)
plot(x=d2020, ecors.type="composite")
```

### 3. Descriptive statistics

Compute the average of the pixel values of each sampling unit (buffer around points and buffered plots) in each image and in the set of images for each season:

```r
mean_d2020<-stats.ecors(x=d2020, edge.pixels="weighted", remove.samples=list(num.pixelOK=10,prop.pixelOK=0.8),
                  summarizing="all", by.image.save=T, summary.save=T,
                  stats=list(mean=T,median=F,sd=F,count=F),spreadsheet.folder=getwd() )
mean_d2020
```

### 4. Download

Download the images cropped to include the study site and small extra area around it:

```r
download.ecors(x=d2020, ecors.type="composite",vis.bands=T,
               ref.site=T,exp.degree=0.05,
               images.folder=getwd(),clear.prov=F)
```

### 5. Get land use maps

Get land use map in the areas surrounding the site:

- Set sf object for study site
- Choose crs code for projecting the polygons
- Choose land use collection
- Choose years
- Ask to make a buffers around the site: 3 different influence zones on the site

```r
lu90_00_20<-get.lu.ecors(site=FAL.IBGE.JBB,
     projected=F, custom.crs=32723,
     collection.lu="mapbiomas6", years=c(1990,2000,2020),
     evaluate="surroundings.site", 
     buffer1=5000, buffer2=10000, buffer3=20000, cumulative.surroundings=F)
```

### 6. Plot land use maps

Create a interactive visualization: 

```r
plot(x=lu90_00_20)
```

### 7. Descriptive statistics on land use maps

Quantifies the pixels of each land use class in each inﬂuence zone on the site (results will be saved as files):

```r
quantify.lu.ecors(x=lu90_00_20, save.format=c("ods"))
```

Calculate the minimum distance from sampling points (mostly savanna) to areas of forest vegetation (gallery forests):
- Get the land use data
- Merge two land use classes that represents forests (pixel value 1 and 3)
- Calculate the minimum distance to a forest pixel

```r
lu20<-get.lu.ecors(points=test.points,
                   projected=F, custom.crs=32723,
                   collection.lu="mapbiomas6", years=c(2020),
                   evaluate="distance.samples")
     
conv_lu20<-convert.lu.ecors(x=lu20,old.value = c(1,3),
                             new.value = c(1,1), new.lu.class = c("Forest","Forest"))
     
dist_to_forest<-dist.lu.ecors(x=conv_lu20, class.value=1, stat.dist="min", max.dist=5000)
```

### 8. Download land use maps

Download land use maps cropped to include the study site and small extra area around it:

```r
download.lu.ecors(x=lu90_00_20, exp.degree=0.05)
```


