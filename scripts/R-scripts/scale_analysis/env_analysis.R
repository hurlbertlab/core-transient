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


# To run this script, you need temperature, precip, etc data, 
# which are currently stored in the following directories off of github: 

# Data directories
tempdatadir = '//bioark.ad.unc.edu/HurlbertLab/GIS/ClimateData/BIOCLIM_meanTemp/'
precipdata = '//bioark.ad.unc.edu/HurlbertLab/GIS/ClimateData/2-25-2011/prec/'
ndvidata = "//bioark.ad.unc.edu/HurlbertLab/GIS/MODIS NDVI/"
BBS = '//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/'
geog = "//bioark.ad.unc.edu/HurlbertLab/GIS/geography/"

bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
bbs_latlon = read.csv(paste(BBS, "good_rtes2.csv", sep = ""), header = TRUE)
bbs_allscales = dplyr::rename(bbs_latlon, focalrte = stateroute) %>%
  right_join(bbs_allscales, by = "focalrte")
sites = data.frame(longitude = bbs_latlon$Longi, latitude = bbs_latlon$Lati) 
#points(sites$longitude, sites$latitude, col= "red", pch=16) #check on map

#find proj of elev -> may need to do for each raster set? 
elev = raster::getData("worldclim", var = "alt", res = 2.5) #raster::getData("alt", country = 'USA', res = 2.5)
str(elev)
#"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

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

plot(NorthAm2)
# Check that circle locations look right #big surprise, they don't -> fix projection!!!
plot(circs.sp, add = TRUE) #looks great

clip<-function(raster,shape) {
  a1_crop<-crop(raster,shape)
  step1<-rasterize(shape,a1_crop)
  a1_crop*step1}

elev2 = projectRaster(elev, crs = CRS("+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km")) #should work, just needs time
elev3 <- raster::mask(elev2, NorthAm2)

test = clip(elev2, NorthAm2)

elev.point = raster::extract(elev3, routes.laea)
elev.mean = raster::extract(elev3, circs.sp, fun = mean, na.rm=T)
elev.var = raster::extract(elev3, circs.sp, fun = var, na.rm=T)

env_elev = data.frame(routes = routes.laea, 
                      elev.point = elev.point, 
                      elev.mean = elev.mean, elev.var = elev.var)

#write.csv(env_elev, "C:/git/core-transient/scripts/R-scripts/scale_analysis/env_elev.csv", row.names = FALSE)

#ndvi 
ndvi = raster(paste(ndvidata, "Vegetation_Indices_may-aug_2000-2010.gri", sep = "")) #can't find on getData
str(ndvi)
#layer format; need to define projection

ndvi2 = projectRaster(ndvi, crs = CRS("+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km")) #should work, just needs time
ndvi3 <- raster::mask(ndvi2, NorthAm2)

ndvi.point = raster::extract(ndvi3, routes.laea)
ndvi.mean = raster::extract(ndvi3, circs.sp, fun = mean, na.rm=T)
ndvi.var = raster::extract(ndvi3, circs.sp, fun = var, na.rm=T)

env_ndvi = data.frame(routes = routes.laea, 
                      ndvi.point = ndvi.point, 
                      ndvi.mean = ndvi.mean, ndvi.var = ndvi.var)
#write.csv(env_ndvi, "C:/git/core-transient/scripts/R-scripts/scale_analysis/env_ndvi.csv", row.names = FALSE)

#precip #fix because rasterstack may not be compatible
prec = raster::getData("worldclim", var = "prec", res = 2.5)  
str(prec) #stack format
prec2 = raster(prec)
prec2 = projectRaster(prec, crs = CRS("+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km")) #should work, just needs time
prec3 <- raster::mask(prec2, NorthAm2)

prec.point = raster::extract(prec3, routes.laea)
prec.mean = raster::extract(prec3, circs.sp, fun = mean, na.rm=T)
prec.var = raster::extract(prec3, circs.sp, fun = var, na.rm=T)

env_prec = data.frame(routes = routes.laea, 
                      prec.point = prec.point, 
                      prec.mean = prec.mean, prec.var = prec.var)
write.csv(env_prec, "C:/git/core-transient/scripts/R-scripts/scale_analysis/env_prec.csv", row.names = FALSE)

#temp 
temp = raster::getData("worldclim", var = "tmean", res = 2.5) 
temp2 = raster(temp) #stack format
temp2 = projectRaster(temp, crs = CRS("+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km")) #should work, just needs time
temp3 <- raster::mask(temp2, NorthAm2)

temp.point = raster::extract(temp3, routes.laea)
temp.mean = raster::extract(temp3, circs.sp, fun = mean, na.rm=T)
temp.var = raster::extract(temp3, circs.sp, fun = var, na.rm=T)

env_temp = data.frame(routes = routes.laea, 
                      temp.point = temp.point, 
                      temp.mean = temp.mean, temp.var = temp.var)
write.csv(env_temp, "C:/git/core-transient/scripts/R-scripts/scale_analysis/env_temp.csv", row.names = FALSE)


####Coef vs env variation models####
env_elev = read.csv("scripts/R-scripts/scale_analysis/env_elev.csv", header = TRUE)
env_ndvi = read.csv("scripts/R-scripts/scale_analysis/env_ndvi.csv", header = TRUE)
coefs = read.csv("scripts/R-scripts/scale_analysis/coefs.csv", header = TRUE)
uniq_env = unique(bbs_envs[, c('focalrte', 'temp', 'vartemp', 'meanP', 'varP', 'ndvi', 'varndvi', 'elev')])
env_coefs = inner_join(coefs, uniq_env, by = c('stateroute' = 'focalrte'))
covmatrix = round(cor(coefs[, 2:ncol(coefs)]), 2)


# nested loop for examining variation in coefs/fitted curves explained by env vars 
rsqrd_df = data.frame(dep = character(), ind = character(), r2 = numeric())

for (d in 2:25) {
  for (i in 26:ncol(env_coefs)) {
    tempmod = lm(env_coefs[,d] ~ env_coefs[,i])
    tempdf = data.frame(dep = names(env_coefs)[d], 
                        ind = names(env_coefs)[i], 
                        r2 = summary(tempmod)$r.squared)
    rsqrd_df = rbind(rsqrd_df, tempdf)
  }
}
#write.csv(rsqrd_df, "scripts/R-scripts/scale_analysis/mod_rsqrds.csv", row.names = FALSE) #updated 03/28 with elev


####Visually Characterizing r2 vals####
rsqrd_df = read.csv("scripts/R-scripts/scale_analysis/mod_rsqrds.csv", header = TRUE)

ggplot(data = rsqrd_df, aes(x = ind, y = r2, fill = ind))+geom_boxplot()+theme_classic()+
  scale_fill_manual(values = wes_palette("BottleRocket"))+theme(legend.position="none")+
  labs(x = "Environmental variables", y = "Variation Explained (R^2)")

#excluding transient data for incompleteness
rsub_i = rsqrd_df %>%
  filter(dep == "OA.i" | dep == "ON.i" | dep == "CA.i" | dep == "CN.i") %>%
  filter(ind == "elev" | ind == "meanP" | ind == "ndvi" | ind == "temp")
rsub_i = droplevels(rsub_i) #removing ghost levels to ensure correct plotting/analyses

ggplot(data = rsub_i, aes(x = ind, y = r2)) + geom_boxplot()+theme_classic() #what I used for poster w/out color 


#separate analysis for just transients since relationship not immediately apparent
rsub_t = rsqrd_df %>%
  filter(dep == "TAexp" | dep == "TApow" | dep == "TNexp" | dep == "TNpow") %>%
  filter(ind == "elev" | ind == "meanP" | ind == "ndvi" | ind == "temp")
rsub_t = droplevels(rsub_t) #removing ghost levels to ensure correct plotting/analyses

ggplot(data = rsub_t, aes(x = ind, y = r2)) + geom_boxplot()+theme_classic() #elev explains more variation in the transients

####Variance Partitioning of Env Predictors####
#would I be basing my total remaining unexplained variation off of the meanOcc~logA relationship? (OA.i?)
#so the 12% remaining
#focusing just on OA.i and main env vars
#how do variance partitioning with more than 4 parts? 

globalmod<-lm(OA.i~elev+meanP+temp+ndvi, data=env_coefs)
mod1<-lm(OA.i~elev, data=env_coefs)
mod2<-lm(OA.i~meanP, data=env_coefs)
mod3<-lm(OA.i~ndvi, data=env_coefs)
mod4<-lm(OA.i~temp, data=env_coefs)
#and then Euclid_mod2
summary(globalmod)$r.squared
summary(mod1)$r.squared
summary(mod2)$r.squared
summary(mod3)$r.squared
summary(mod4)$r.squared 


#running with mods 2+3 bc best ranked and most interesting 
a= summary(globalmod)$r.squared - summary(mod2)$r.squared
a
c= summary(globalmod)$r.squared - summary(mod3)$r.squared
c
b= summary(mod2)$r.squared - c
b
d= 1- summary(globalmod)$r.squared
d
#isn't it ok that d = ~87.5% tho, given that the r^2 for occ~logA was 88%? 
