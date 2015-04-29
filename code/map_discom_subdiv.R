require('rgdal')
require('rgeos')
require('sp')
require('RColorBrewer')

# Read in .shp file of discom areas and plot
setwd("~/Documents/github/Delhi/data/Delhi .shp files")
discom <- readOGR("delhi_licensee_final.shp", layer="delhi_licensee_final")

# change projection to get planar coordinates.  This is to use gArea() to calculate area
discom2 <- spTransform(discom, CRS("+init=epsg:32646"))
colors <- palette(brewer.pal(5, "Pastel1"))
plot(discom2, col=colors, lty='dashed', main="Delhi Discom Areas & Administrative Districts")

# Add DISCOM labels
text(x=coordinates(discom2)[,1], y=coordinates(discom2)[,2], labels=discom2@data$discom)


## Load .shp file for administrative SUB-DISTRICTS
subdiv <- readOGR("delhi_subdiv.shp", layer="delhi_subdiv")

# change projection to get planar coordinates.  This is to use gArea() to calculate area
subdiv2 <- spTransform(subdiv, CRS("+init=epsg:32646"))
plot(subdiv2, lwd=2, add=TRUE)

# To create the intersection shapefile, set the proj4 strings equal to one another
proj4string(discom2) <- proj4string(subdiv2)

# Create intersection of subdistricts and grid polygons
intersect <- gIntersection(discom2, subdiv2, byid=TRUE)

area <- c()
id <- c()
L <- length(intersect)
for (i in 1:L) {
  t <- intersect[i]
  area <- c(area,(t@polygons[[1]]@area))
  id <- c(id,as.character(t@polygons[[1]]@ID))
}

intersect_data <- data.frame(cbind(id,area))
id_lst <- strsplit(id, " ")
id_df <- do.call(rbind.data.frame, id_lst)
colnames(id_df) <- c("discom_id","subdiv_id")
intersect_data <- cbind(intersect_data,id_df)
intersect_data$area <- as.numeric(as.character(intersect_data$area))

# Plot the intersection
plot(intersect,col=colors, lty='dashed', main="Delhi Discom Areas & SubDistricts")
text(x=coordinates(intersect)[,1], y=coordinates(intersect)[,2], labels=intersect_data$id)


### calculate area ratio
discom_area <- aggregate(.~discom_id, FUN=sum, data=intersect_data[,c('discom_id','area')])
colnames(discom_area) <- c('discom_id','discom_area')
subdiv_area <- aggregate(.~subdiv_id, FUN=sum, data=intersect_data[,c('subdiv_id','area')])
colnames(subdiv_area) <- c('subdiv_id','subdiv_area')

area_df <- merge(intersect_data,discom_area,by=c('discom_id'))
area_df <- merge(area_df,subdiv_area,by=c('subdiv_id'))

area_df$ratio_discom <- area_df$area/area_df$discom_area
area_df$ratio_subdiv <- area_df$area/area_df$subdiv_area

discom_name <- as.data.frame(discom@data)
discom_name$discom_id <- seq(0,4,1)
subdiv_name <- as.data.frame(subdiv@data)
subdiv_name$subdiv_id <- seq(0,26,1)

area_df <- merge(area_df,discom_name,by=c('discom_id'))
area_df <- merge(area_df,subdiv_name,by=c('subdiv_id'))
colnames(area_df) <- c("subdiv_id","discom_id","intersection_id","intersection_area","discom_area","subdiv_area","discom_ratio","subdiv_ratio","discom_name","subdiv_name", "district_name" )

write.csv(area_df,"area_ratio_discom_subdiv.csv")