# setwd("C:/git/core_scale")
#'#' Please download and install the following packages:
library(tidyverse)
library(sp)
library(raster)
library(rgdal)

library(maptools)
library(rgeos)
library(fields)
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
# To run this script, you need temperature, precip, etc data, 
# which are currently stored in the following directories off of github: 

# Data directories
tempdatadir = '//bioark.ad.unc.edu/HurlbertLab/GIS/ClimateData/BIOCLIM_meanTemp/'
precipdata = '//bioark.ad.unc.edu/HurlbertLab/GIS/ClimateData/2-25-2011/prec/'
ndvidata = "//bioark.ad.unc.edu/HurlbertLab/GIS/MODIS NDVI/"
BBS = '//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/'
geog = "//bioark.ad.unc.edu/HurlbertLab/GIS/geography/"

#all focal rtes with all possible pairings
bbs_latlon = read.csv("intermed/good_rtes2.csv", header = TRUE)

#exclude routes that have missing above OR below scale data, such that sites are only calculated for routes that cover all 83 scales
bbs_allscales = read.csv("intermed/bbs_allscales.csv", header = TRUE)
bbs_latlon = filter(bbs_latlon, stateroute %in% bbs_allscales$focalrte)

# 
# sites = data.frame(longitude = bbs_latlon$Longi, latitude = bbs_latlon$Lati) 
#points(sites$longitude, sites$latitude, col= "red", pch=16) #check on map

# Makes routes into a spatialPointsDataframe
latlon = na.omit(bbs_latlon)
coordinates(latlon)=c('longitude', 'latitude')
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

plot(NorthAm2)
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

write.csv(env_elev, "intermed/env_elev.csv", row.names = FALSE)

#ndvi 
#gimms
#ndvi_gimms_raw <- get_bbs_gimms_ndvi()
ndvi_gimms_raw = read.csv("intermed/ndvi_raw.csv") #Sara version, sourced from tabular data folder 

ndvi_data_summer <- ndvi_gimms_raw %>%
  filter(!is.na(ndvi), month %in% c('may', 'jun', 'jul', 'aug'), year > 2000) %>%
  group_by(site_id, year) %>% #calc avg across summer months for each year
  summarise(ndvi_sum = mean(ndvi), na.rm = TRUE) %>%
  group_by(site_id) %>% #calc avg across years
  summarise(ndvi_mean = mean(ndvi_sum), 
            ndvi_var = var(ndvi_sum), na.rm = TRUE) %>% 
  ungroup()

write.csv(ndvi_data_summer, "intermed/ndvi_summer.csv", row.names = FALSE) #updated with correct NDVI extraction
#5015 routes with associated ndvi data
#means calculated across 40 km buffer zone, use as would means from other vars 
#updated 05/07/2018 


####Merge env df's together into one with relevant stateroutes, mean, and var data 
env_elev = read.csv("intermed/env_elev.csv", header = TRUE)
env_ndvi = read.csv("intermed/ndvi_summer.csv", header = TRUE)

bbs_envs = env_elev %>%
  left_join(env_ndvi, by = c("routes.stateroute" = "site_id")) %>%
  dplyr::select(stateroute = routes.stateroute, 
         elev.mean, elev.var, 
         ndvi.mean = ndvi_mean, ndvi.var = ndvi_var) %>%
  filter(elev.var != "NA" & ndvi.var != "NA") #927 routes when NA obs removed

#check/Pare down routes to exclude routes that are missing above OR below scale
bbs_allscales = read.csv("intermed/bbs_allscales.csv", header = TRUE)
bbs_envs = filter(bbs_envs, stateroute %in% bbs_allscales$focalrte) #still 927, all scales accounted for 
write.csv(bbs_envs, "intermed/bbs_envs.csv", row.names = FALSE) #updated 05/07
#05/07 updated

####Pair env data to secondary rtes associated with each focal rte#### 
#calc variance across scales for each focal rte#
bbs_envs = read.csv("intermed/bbs_envs.csv", header = TRUE) #10/06
dist.df = read.csv("intermed/dist_df.csv", header = TRUE)

#need to pair down by routes existing in bbs_envs (which have been sorted appropriately) and then calculated the top n 66 based on distance
dist.df2 = filter(dist.df, rte1 %in% bbs_envs$stateroute & rte2 %in% bbs_envs$stateroute) 

#num of rows matches num of rows in dts.df, good 
#now calc var for each focal rte (rte1)
top_envhetero = data.frame(stateroute = NULL,
                           scale = NULL, 
                           reg_ndvi_v = NULL,
                           reg_elev_v = NULL,
                           reg_ndvi_m = NULL,
                           reg_elev_m = NULL)

focal_rtes = unique(bbs_envs$stateroute)
#need to structure using focal_clustr template 
scales = c(2:66)
output = data.frame(stateroute = NULL,
                    scale = NULL, 
                    reg_ndvi_v = NULL,
                    reg_elev_v = NULL,
                    reg_ndvi_m = NULL,
                    reg_elev_m = NULL)

#add nu scale component for calculating means at each scale interval 
for(r in focal_rtes){
  for(nu in scales) { #just running for top scale for now 
    #to characterize total var that can be encompassed on a rte
    
    #takes dist.df and generates a new list that changes based on which route in uniqrtes is being focused on 
    #and the length of the list varies with the scale or nu 
    rte_group = dist.df2 %>% 
      filter(rte1 == r) %>% 
      top_n(nu, desc(dist)) %>%
      dplyr::select(rte2) %>% as.vector()
    
    tempenv = bbs_envs %>%
      filter(stateroute %in% rte_group$rte2)
    
    #get variance of rte means, calc across entire rte_group according to scale   
    temp = data.frame(stateroute = r,
                      scale = nu,
                      reg_ndvi_v = var(tempenv$ndvi.mean, na.rm = TRUE), #fix missing values!!!!
                      reg_elev_v = var(tempenv$elev.mean), #bc each of these values is calculated across the 2ndary rtes for each focal rte
                      reg_ndvi_m = mean(tempenv$ndvi.mean), 
                      reg_elev_m = mean(tempenv$elev.mean)) #a vector is going in and a single val is coming out
    #top_zhull = zhull_df$vol)
    
    output = rbind(output, temp)
  }
  reg_envhetero = rbind(top_envhetero, output) #may need a second level of output rbinding here
}

write.csv(reg_envhetero, "intermed/reg_envhetero.csv", row.names = FALSE)
#updated 05/07

#zm from 40 km buffer circle raster clip too

####Coef vs env hetero models####
reg_envhetero = read.csv("intermed/reg_envhetero.csv", header = TRUE) #landscape habitat vars 2:66 scale
bbs_envs = read.csv("intermed/bbs_envs.csv", header = TRUE) #single rte habitat vars 1 scale

#Merge top_envhetero to coefs for comparing env variation for a site to its associated AUC 
#at the scale of a landscape and scale of a rte

#also prep datasets for merge 
bbs_envs = bbs_envs[, 1:5] %>% 
  mutate(scale = 1)

reg_envhetero = reg_envhetero %>% 
  rename(elev.mean = reg_elev_m, 
         elev.var = reg_elev_v, 
         ndvi.mean = reg_ndvi_m, 
         ndvi.var = reg_ndvi_v) 

reg_envhetero$scale = as.numeric(reg_envhetero$scale)

env_all = reg_envhetero %>% 
  full_join(bbs_envs) %>% 
  arrange(stateroute, scale)

write.csv(env_all, "intermed/env_all.csv", row.names = FALSE)  
#updated 05/07 