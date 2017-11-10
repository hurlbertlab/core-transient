# setwd("C:/git/core-transient")
#'#' Please download and install the following packages:
library(raster)
library(maps)
library(sp)
library(rgdal)
library(maptools)
library(rgeos)
library(dplyr)
library(fields)
library(tidyr)
library(ggplot2)
library(nlme)
library(gridExtra)
library(wesanderson)
library(stats)
library(gimms)
library(devtools)
library(geometry)
library(DBI)
library(RSQLite) #absolutely necessary for NDVI data
library(rdataretriever)
library(magrittr)
library(stringr)
# To run this script, you need temperature, precip, etc data, 
# which are currently stored in the following directories off of github: 

# Data directories
tempdatadir = '//bioark.ad.unc.edu/HurlbertLab/GIS/ClimateData/BIOCLIM_meanTemp/'
precipdata = '//bioark.ad.unc.edu/HurlbertLab/GIS/ClimateData/2-25-2011/prec/'
ndvidata = "//bioark.ad.unc.edu/HurlbertLab/GIS/MODIS NDVI/"
BBS = '//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/'
geog = "//bioark.ad.unc.edu/HurlbertLab/GIS/geography/"

#all focal rtes with all possible pairings
bbs_latlon = read.csv(paste(BBS, "good_rtes2.csv", sep = ""), header = TRUE)

#exclude routes that have missing above OR below scale data, such that sites are only calculated for routes that cover all 83 scales
bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
bbs_latlon = filter(bbs_latlon, stateroute %in% bbs_allscales$focalrte)


sites = data.frame(longitude = bbs_latlon$Longi, latitude = bbs_latlon$Lati) 
#points(sites$longitude, sites$latitude, col= "red", pch=16) #check on map

# Makes routes into a spatialPointsDataframe
latlon = na.omit(bbs_latlon)
coordinates(latlon)=c('Longi', 'Lati')
projection(latlon) = CRS("+proj=longlat +ellps=WGS84") 
#out of order? YUP. 
#had Lati, Longi -> needed to be Longi, Lati. 

prj.string <- CRS("+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km")
# original in Sara's code: "+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km"
# Transforms routes to an equal-area projection - see previously defined prj.string
routes.laea = spTransform(latlon, CRS = CRS("+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km")) 
#works w/blank projection; doesn't work otherwise
#keep receiving "non finite transformation detected: Lat Longi" error and 
#"error failure in points, 532 projected points not finite 

##### extracting elevation data ####
# A function that draws a circle of radius r around a point: p (x,y)
RADIUS = 40

make.cir = function(p,r){
  points=c()
  for(i in 1:360){
    theta = i*2*pi/360
    y = p[2] + r*cos(theta)
    x = p[1] + r*sin(theta)
    points = rbind(points,c(x,y))
  }
  points=rbind(points,points[1,])
  circle=Polygon(points,hole=F)
  circle
}

#routes.laea@data$dId_site = paste(routes.laea@data$datasetID, routes.laea@data$site, sep = "_")
#routes.laea@data$unique = 1:1003 unnecessary since still have stateroute ID's


#Draw circles around all routes 
circs = sapply(1:nrow(routes.laea@data), function(x){
  circ =  make.cir(routes.laea@coords[x,],RADIUS)
  circ = Polygons(list(circ),ID=routes.laea$stateroute[x]) 
}
)

circs.sp = SpatialPolygons(circs, proj4string=CRS("+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km"))

# read in elevation raster at 2.5 km resolution (see above)
NorthAm = readOGR(dsn = "//bioark.ad.unc.edu/HurlbertLab/GIS/geography", layer = "continent")
NorthAm2 = spTransform(NorthAm, CRS("+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km"))

plot(NorthAm, xlim = c(-160, -60), ylim = c(25, 70))
# Check that circle locations look right #big surprise, they don't -> fix projection!!!
plot(circs.sp, add = TRUE) #looks great

clip<-function(raster,shape) {
  a1_crop<-crop(raster,shape)
  step1<-rasterize(shape,a1_crop)
  a1_crop*step1}


#elev
elev = raster::getData("worldclim", var = "alt", res = 2.5) #raster::getData("alt", country = 'USA', res = 2.5)
str(elev)
elev2 = projectRaster(elev, crs = CRS("+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km")) #should work, just needs time
elev3 <- raster::mask(elev2, NorthAm2)

test = clip(elev2, NorthAm2)

elev.point = raster::extract(elev3, routes.laea)
elev.mean = raster::extract(elev3, circs.sp, fun = mean, na.rm=T)
elev.var = raster::extract(elev3, circs.sp, fun = var, na.rm=T)

env_elev = data.frame(routes = routes.laea, 
                      elev.point = elev.point, 
                      elev.mean = elev.mean, elev.var = elev.var)

#write.csv(env_elev, "scripts/R-scripts/scale_analysis/env_elev.csv", row.names = FALSE)

#ndvi 
#gimms
#ndvi_gimms_raw <- get_bbs_gimms_ndvi()
ndvi_gimms_raw = read.csv("data/BBS/ndvi_raw.csv") #Sara version, sourced from tabular data folder 

ndvi_data_summer <- ndvi_gimms_raw %>%
  filter(!is.na(ndvi), month %in% c('may', 'jun', 'jul', 'aug'), year > 2000) %>%
  group_by(site_id, year) %>% #calc avg across summer months for each year
  summarise(ndvi_sum = mean(ndvi), na.rm = TRUE) %>%
  group_by(site_id) %>% #calc avg across years
  summarise(ndvi_mean = mean(ndvi_sum), 
            ndvi_var = var(ndvi_sum), na.rm = TRUE) %>% 
  ungroup()

write.csv(ndvi_data_summer, "scripts/R-scripts/scale_analysis/ndvi_summer.csv", row.names = FALSE) #updated with correct NDVI extraction
#5015 routes with associated ndvi data
#means calculated across 40 km buffer zone, use as would means from other vars 
#write.csv(env_ndvi, "scripts/R-scripts/scale_analysis/env_ndvi.csv", row.names = FALSE)
#updated 05/31 

#precip 
prec = raster::getData("worldclim", var = "prec", res = 2.5)  
prec2 = sum(prec)
prec2 = prec2/1000 #convert to m from mm
plot(prec2) #plotting correctly

prec2 = projectRaster(prec2, crs = CRS("+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km")) #worked 
#need to reset CRS here bc otherwise spatialpoints objects like routes.laea get coerced to the default CRS of prec2
#prec3 <- raster::mask(prec2, NorthAm2) check with Sara on masking data

prec.point = raster::extract(prec2, routes.laea) #working!!!! 
prec.mean = raster::extract(prec2, circs.sp, fun = mean, na.rm=T)
prec.var = raster::extract(prec2, circs.sp, fun = var, na.rm=T)

env_prec = data.frame(routes = routes.laea, 
                      prec.point = prec.point, 
                      prec.mean = prec.mean, prec.var = prec.var)
#write.csv(env_prec, "scripts/R-scripts/scale_analysis/env_prec.csv", row.names = FALSE) # updated 05/12

#temp 
temp = raster::getData("worldclim", var = "tmean", res = 2.5) 
temp2 = temp/10 #taking to degrees Celsius in correct units
temp3 = mean(temp2) #stack/brick format to layer
plot(temp3)

temp4 = projectRaster(temp3, crs = CRS("+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km")) #should work, just needs time
#temp3 <- raster::mask(temp2, NorthAm2) #again, check with Sara on skipping this

temp.point = raster::extract(temp4, routes.laea)
temp.mean = raster::extract(temp4, circs.sp, fun = mean, na.rm=T)
temp.var = raster::extract(temp4, circs.sp, fun = var, na.rm=T)

env_temp = data.frame(routes = routes.laea, 
                      temp.point = temp.point, 
                      temp.mean = temp.mean, temp.var = temp.var)
#write.csv(env_temp, "scripts/R-scripts/scale_analysis/env_temp.csv", row.names = FALSE)

####Merge env df's together into one with relevant stateroutes, mean, and var data 
env_elev = read.csv("scripts/R-scripts/scale_analysis/env_elev.csv", header = TRUE)
env_ndvi = read.csv("scripts/R-scripts/scale_analysis/ndvi_summer.csv", header = TRUE)
env_prec = read.csv("scripts/R-scripts/scale_analysis/env_prec.csv", header = TRUE)
env_temp = read.csv("scripts/R-scripts/scale_analysis/env_temp.csv", header = TRUE)

bbs_envs = env_elev %>%
  left_join(env_ndvi, by = c("routes.stateroute" = "site_id")) %>% 
  left_join(env_prec, by = "routes.stateroute") %>%
  left_join(env_temp, by = "routes.stateroute") %>%
  dplyr::select(stateroute = routes.stateroute, 
         elev.mean, elev.var, 
         ndvi.mean = ndvi_mean, ndvi.var = ndvi_var, #I don't have var for ndvi, FIX 09/21
         prec.mean, prec.var, 
         temp.mean, temp.var) %>%
  filter(temp.mean != "NA", prec.mean != "NA", elev.mean != "NA", ndvi.mean != "NA") #945 routes when NA obs removed
write.csv(bbs_envs, "scripts/R-scripts/scale_analysis/bbs_envs.csv", row.names = FALSE) 
#current version 09/21
