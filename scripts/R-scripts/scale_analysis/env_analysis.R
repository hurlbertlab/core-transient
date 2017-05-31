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

bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)

#all focal rtes with all possible pairings

bbs_latlon = read.csv(paste(BBS, "good_rtes2.csv", sep = ""), header = TRUE)
bbs_allscales = dplyr::rename(bbs_latlon, focalrte = stateroute) %>%
  right_join(bbs_allscales, by = "focalrte")
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

#write.csv(env_elev, "scripts/R-scripts/scale_analysis/env_elev.csv", row.names = FALSE)

#ndvi 
ndvi = raster(paste(ndvidata, "Vegetation_Indices_may-aug_2000-2010.gri", sep = "")) #fine for now, troubleshoot NDVI next 
str(ndvi)
#layer format; need to define projection
ndvi2 = ndvi/10000 
ndvi2 = projectRaster(ndvi2, crs = CRS("+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km")) #should work, just needs time
ndvi3 <- raster::mask(ndvi2, NorthAm2)

ndvi.point = raster::extract(ndvi3, routes.laea)
ndvi.mean = raster::extract(ndvi3, circs.sp, fun = mean, na.rm=T)
ndvi.var = raster::extract(ndvi3, circs.sp, fun = var, na.rm=T)

env_ndvi = data.frame(routes = routes.laea, 
                      ndvi.point = ndvi.point, 
                      ndvi.mean = ndvi.mean, ndvi.var = ndvi.var)
#write.csv(env_ndvi, "scripts/R-scripts/scale_analysis/env_ndvi.csv", row.names = FALSE)
#updated 05/15 to reflect dividing ndvi/10,000 to fix value scaling

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
env_ndvi = read.csv("scripts/R-scripts/scale_analysis/env_ndvi.csv", header = TRUE)
env_prec = read.csv("scripts/R-scripts/scale_analysis/env_prec.csv", header = TRUE)
env_temp = read.csv("scripts/R-scripts/scale_analysis/env_temp.csv", header = TRUE)

bbs_envs = env_elev %>%
  left_join(env_ndvi, by = "routes.stateroute") %>% 
  left_join(env_prec, by = "routes.stateroute") %>%
  left_join(env_temp, by = "routes.stateroute") %>%
  select(stateroute = routes.stateroute, elev.point, elev.mean, elev.var, 
         ndvi.point, ndvi.mean, ndvi.var,
         prec.point, prec.mean, prec.var, 
         temp.point, temp.mean, temp.var)
write.csv(bbs_envs, "scripts/R-scripts/scale_analysis/bbs_envs.csv", row.names = FALSE)
#current version all vars up to date except ndvi 05/15

####Calc z-scores, quantiles pre-variance loop####
bbs_envs = read.csv("scripts/R-scripts/scale_analysis/bbs_envs.csv", header = TRUE)

#alt simplistic standardization using z scores
bbs_envs$ztemp = (bbs_envs$temp.mean - mean(bbs_envs$temp.mean)) / sd(bbs_envs$temp.mean)
bbs_envs$zprec = (bbs_envs$prec.mean - mean(bbs_envs$prec.mean)) / sd(bbs_envs$prec.mean)
bbs_envs$zelev = (bbs_envs$elev.mean - mean(bbs_envs$elev.mean)) / sd(bbs_envs$elev.mean)
bbs_envs$zndvi = ((bbs_envs$ndvi.mean) - mean(na.exclude(bbs_envs$ndvi.mean))) / sd(na.exclude(bbs_envs$ndvi.mean)) 
#NA's for ndvi vals around statertes in the 3000's
#with z scores

# Calc quantiles
bbs_envs$ndvi_q= rank(bbs_envs$ndvi.mean)/nrow(bbs_envs) #needs to be corrected since NA values in ndvi col
bbs_envs$elev_q= rank(bbs_envs$elev.mean)/nrow(bbs_envs) 
bbs_envs$temp_q= rank(bbs_envs$temp.mean)/nrow(bbs_envs) 
bbs_envs$prec_q= rank(bbs_envs$prec.mean)/nrow(bbs_envs)
#write.csv(bbs_envs, "scripts/R-scripts/scale_analysis/bbs_envs.csv", row.names = FALSE)
#with z scores and quantiles both

####Convex polygon comparison of variables####
#not variances yet 
#notes on geometry package and min convex polygon:
#convhulln from geometry package, optimized by qhull -> convex hull 
#http://www.qhull.org/html/qconvex.htm#synopsis
bbs_envs = read.csv("scripts/R-scripts/scale_analysis/bbs_envs.csv", header = TRUE)
#subset to just appropriate dims for convhulln 
sub_envs = bbs_envs %>% select(temp_q, prec_q, elev_q, ndvi_q) %>% filter(ndvi_q != 'NA') #cuts down to 982 


hull = convhulln(sub_envs, "FA")
hull$area #189.74
hull$vol #66.22 

#multi panel plot comparing spread of routes bet diff z scores 
#for entire set, vol is 66.22 
#should I do for each stateroute...? and compare? 

####Pair env data to secondary rtes associated with each focal rte; calc variance for each focal rte####
bbs_envs = read.csv("scripts/R-scripts/scale_analysis/bbs_envs.csv", header = TRUE)
dist.df = read.csv("scripts/R-scripts/scale_analysis/dist_df.csv", header = TRUE)

#num of rows matches num of rows in dts.df, good 
#now calc var for each focal rte (rte1)
focal_var = data.frame(stateroute = NULL, ndvi_v = NULL, elev_v = NULL, prec_v = NULL, temp_v = NULL)
focal_qv = data.frame(stateroute = NULL, ndvi_qv = NULL, elev_qv = NULL, prec_qv = NULL, temp_qv = NULL)
focal_rtes = unique(bbs_envs$stateroute)

for(r in focal_rtes){
  rte_group = dist.df %>% 
    filter(rte1 == r) %>% 
    top_n(66, desc(dist)) %>%
    select(rte2) %>% as.vector()
  
  tempenv = bbs_envs %>%
    filter(stateroute %in% rte_group$rte2)
   
  temp = data.frame(stateroute = r,
                    ndvi_v = var(tempenv$zndvi),
                    elev_v = var(tempenv$zelev), #bc each of these values is calculated across the 2ndary rtes for each focal rte
                    prec_v = var(tempenv$zprec), #such that all 66 2ndary rtes will be summed into one variance value for each focal rte
                    temp_v = var(tempenv$ztemp)) 
  temp2 = data.frame(stateroute = r,
                     ndvi_qv = var(tempenv$ndvi_q),
                     elev_qv = var(tempenv$elev_q), #bc each of these values is calculated across the 2ndary rtes for each focal rte
                     prec_qv = var(tempenv$prec_q), #such that all 66 2ndary rtes will be summed into one variance value for each focal rte
                     temp_qv = var(tempenv$temp_q)) 
  
  focal_var = rbind(focal_var, temp)
  focal_qv = rbind(focal_qv, temp2)
}
write.csv(focal_var, "scripts/R-scripts/scale_analysis/focal_var.csv", row.names = FALSE)
write.csv(focal_qv, "scripts/R-scripts/scale_analysis/focal_qv.csv", row.names = FALSE)
#updated 05/15

####Elev vs NDVI plotting####
focal_qv = read.csv("scripts/R-scripts/scale_analysis/focal_qv.csv", header = TRUE)
focal_var = read.csv("scripts/R-scripts/scale_analysis/focal_var.csv", header = TRUE)
bbs_envs = read.csv("scripts/R-scripts/scale_analysis/bbs_envs.csv", header = TRUE)

#elev vs ndvi on plot - variance of quantile scores
q_scores = ggplot(focal_qv, aes(x = ndvi_qv, y = elev_qv))+geom_point()+theme_classic()+ggtitle("Variance of quantiles")
z_scores = ggplot(focal_var, aes(x = ndvi_v, y = elev_v))+geom_point()+theme_classic()+ggtitle("Variance of z-scores")
#elev vs ndvi on plot - straight z scores, no var calc 
z_raw = ggplot(bbs_envs, aes(x=zndvi, y = zelev))+geom_point()+theme_classic()+ggtitle("Z scores of raw data")
qz = grid.arrange(q_scores, z_scores)
z_raw 

#compare GIMMS to old MODIS based raster data
#may-aug, 2000-2014

#gimms
ndvi_data_raw <- get_bbs_gimms_ndvi()

ndvi_data_summer <- ndvi_data_raw %>%
  filter(!is.na(ndvi), month %in% c('may', 'jun', 'jul', 'aug'), year > 2000) %>%
  group_by(site_id, year) %>% #calc avg across summer months for each year
  summarise(ndvi_sum = mean(ndvi)) %>%
  group_by(site_id) %>% #calc avg across years
  summarise(ndvi_mean = mean(ndvi_sum)) %>% 
  ungroup()
#only 429 rows/unique combos of route and ndvi when ndvi avgd across summer months, years past 2000
#in looking at the raw data, a TON of NaN's were produced, so still not worth it to use gimms seemingly 

#modis still has 1003 vals - why? 
ndvi_modis = read.csv("scripts/R-scripts/scale_analysis/env_ndvi.csv", header = TRUE)



write.csv(ndvi_data_raw, "data/BBS/ndvi_raw.csv", row.names = FALSE)
write.csv(ndvi_data_summer, "scripts/R-scripts/scale_analysis/ndvi_summer.csv", row.names = FALSE)



####Coef vs env variation models####
bbs_envs = read.csv("scripts/R-scripts/scale_analysis/bbs_envs.csv", header = TRUE)
coefs = read.csv("scripts/R-scripts/scale_analysis/coefs.csv", header = TRUE)
env_coefs = inner_join(coefs, bbs_envs, by = "stateroute")
covmatrix = round(cor(coefs[, 2:ncol(coefs)]), 2)
covmatrix

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
#write.csv(rsqrd_df, "scripts/R-scripts/scale_analysis/rsqrd_df.csv", row.names = FALSE) #updated 05/07 with new env extracted vars


####Visually Characterizing r2 vals####
rsqrd_df = read.csv("scripts/R-scripts/scale_analysis/mod_rsqrds.csv", header = TRUE)

ggplot(data = rsqrd_df, aes(x = ind, y = r2, fill = ind))+geom_boxplot()+theme_classic()+
  scale_fill_manual(values = wes_palette("BottleRocket"))+theme(legend.position="none")+
  labs(x = "Environmental variables", y = "Variation Explained (R^2)")

#excluding transient data for incompleteness
rsub_i = rsqrd_df %>%
  filter(dep == "OA.i" | dep == "ON.i" | dep == "CA.i" | dep == "CN.i")
rsub_i = droplevels(rsub_i) #removing ghost levels to ensure correct plotting/analyses

ggplot(data = rsub_i, aes(x = ind, y = r2)) + geom_boxplot()+theme_classic() #what I used for poster w/out color 


#separate analysis for just transients since relationship not immediately apparent
rsub_t = rsqrd_df %>%
  filter(dep == "TAexp" | dep == "TApow" | dep == "TNexp" | dep == "TNpow")
rsub_t = droplevels(rsub_t) #removing ghost levels to ensure correct plotting/analyses

ggplot(data = rsub_t, aes(x = ind, y = r2)) + geom_boxplot()+theme_classic() #elev explains more variation in the transients

#what we expected: 
#Homogenous communities (i.e. low ndvi, low elev values)
  #i as a coefficient should be lower, accumulation of core species more linear early on in scale relationship 
  #k is difficult to predict but is the slope AT the inflexion point i 
  #A should be higher, relationship should asymptote out earlier and a greater stretch should be in an asymptotic form 

#Heterogenous communities (i.e. high ndvi, high elev values)
  #i as a coef should be higher (relative to what?) due to a longer period of Transient dominance in the lower scales pulling down the average, 
    #meaning i is further along in the x axis relative to the y 
  #k is again, difficult to predict 
  #A should be lower and shorter, as asymptotic form barely reached 

#what does this comparison look like? How do I split sites up via a threshold for hetero vs homogenous 
#to compare their avg coefs? 



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

########


devtools::load_all(path = 'C:/git/core-transient')

ndvi_data_raw <- get_bbs_gimms_ndvi()

ndvi_data_summer <- ndvi_data_raw %>%
  filter(!is.na(ndvi), month %in% c('may', 'jun', 'jul'), year > 1981) %>%
  group_by(site_id, year) %>%
  summarise(ndvi_sum = mean(ndvi)) %>%
  ungroup()
