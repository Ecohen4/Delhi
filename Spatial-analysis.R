# Install pacakges
## The following function will load the packages required for this tutorial.  If a package cannot be found in your instance of Rstudio, it will automatically be insalled.
## require() returns (invisibly) a logical indicating whether the required package is available.
load_install<-function(lib){
  if(! require(lib, character.only=TRUE)) install.packages(lib, character.only=TRUE)
  library(lib, character.only=TRUE, quietly=TRUE)
}

## the required libraries (e.g. packages)
Thelib<-c("sp", "rgdal")

## apply the function
lapply(Thelib, load_install)

# load data
# load("/Users/elliotcohen/Downloads/IND_adm3.RData")

# WRIA example by Steven Brey @ mazamascience.com
# First load the data from the Washington department of ecology website

# Data source
# Data:      ftp://www.ecy.wa.gov/gis_a/hydro/wria.zip
# Metadata:  http://www.ecy.wa.gov/services/gis/data/hydro/wria.htm

# Create a directory for the data
localDir <- 'R_GIS_data'
if (!file.exists(localDir)) {
  dir.create(localDir)
}
# Download the 5mb file of WRIA data
# url <- 'ftp://www.ecy.wa.gov/gis_a/hydro/wria.zip' # no longer exists...
url <- 'ftp://www.ecy.wa.gov/gis_a/hydro/NHDmajorareas.zip'
file <- paste(localDir,basename(url),sep='/')
if (!file.exists(file)) {
  download.file(url, file)
  unzip(file,exdir=localDir)
}
# Show the unzipped files
list.files(localDir)

# layerName is the name of the unzipped shapefile without file type extensions
layerName <- "WRIA_poly"
# Read in the data
data_projected <- readOGR(dsn=localDir, layer="NHD_MajorAreas")

# What is this thing and what's in it?
class(data_projected)
slotNames(data_projected)
# It's an S4 "SpatialPolygonsDataFrame" object with the following slots:
# [1] "data"        "polygons"    "plotOrder"   "bbox"        "proj4string"

# What does the data look like with the default plotting command?
plot(data_projected)


