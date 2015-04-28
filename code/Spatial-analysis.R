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
# Mapping Delhi districts

# Install packages
# install.packages('rgdal')
# install.packages('sp')
# install.packages('maps')
# install.packages('maptools')
# install.packages('rgeos')
# install.packages('spatstat')
# install.packages('deldir')

# Load packages
require(rgdal)
require(sp)
require(maps)
require(maptools)
require(rgeos)
require(spatstat)
require(deldir)

# Set directory to folder containing .shp and other files
setwd("~/Documents/SLDC_big_data/MappingFiles")

# See: 'Read OGR vector maps into Spatial objects'
ogrListLayers("Districts.shp")
shape=readOGR("Districts.shp", layer="Districts")

# Plot shape
plot(shape)

# Read in data containing electricity grid coordinates
setwd("~/Documents/SLDC_big_data")
gridData <- read.csv('grid_coord.csv')
gridCoord <- data.frame(x=gridData$long, y=gridData$lat)

# Slightly modified Voronoi function, takes an additional
# spatial polygons argument and extends to that box:
# Source: https://stackoverflow.com/questions/12156475/combine-voronoi-polygons-and-maps/12159863#12159863
voronoipolygons <- function(x,poly) {
  require(deldir)
  if (.hasSlot(x, 'coords')) {
    crds <- x@coords
  } else crds <- x
  bb = bbox(poly)
  rw = as.numeric(t(bbox(poly)))
  z <- deldir(crds[,1], crds[,2],rw=rw)
  w <- tile.list(z)
  polys <- vector(mode='list', length=length(w))
  require(sp)
  for (i in seq(along=polys)) {
    pcrds <- cbind(w[[i]]$x, w[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP <- SpatialPolygons(polys)

  voronoi <- SpatialPolygonsDataFrame(SP, data=data.frame(x=crds[,1],
                                                          y=crds[,2],
                                                          row.names=sapply(slot(SP, 'polygons'),
                                                                           function(x) slot(x, 'ID'))))
  return(voronoi)
}

# Create Voronoi polygons for grid areas
gridArea <-voronoipolygons(gridCoord, shape)

# Plot Voronoi polygons for Delhi
plot(gridArea)

# Set the proj4 strings equal to one another
proj4string(gridArea) <- proj4string(shape)

# Find intersection of (1) polygons based on grid coordinates and (2) Delhi district boundaries
gridDelhi = gIntersection(shape, gridArea, byid=TRUE)

# Plot the intersection
plot(gridDelhi)

# Label grid names
points(x=gridData$long, y=gridData$lat, col=gridData$color, pch=19, cex=1.8)
text(x=gridData$long, y=gridData$lat, labels=gridData$grid_name, cex=0.6, pos=2, col=gridData$Discom)
legend(77.320784, 28.628659, legend=c("NDPL", "BRPL", "BYPL", "MES"),
       fill=c("red", "blue", "black", "green"), cex=1.0, bty="n")

