#Spring Semester 2016 Additions
source("Gartland_Code.R")
#fix sourcing link of file 
# set working directory to where the climate files have been downloaded

setwd("C:/Users/GartlandM/Documents/Post-Grad/Research/GIS")

## Please download and install the following packages:
# maps, sp, rgdal, raster, maptools, rgeos
install.packages("raster")
install.packages("maps")
install.packages("sp")
install.packages("rgdal")
install.packages("maptools")
install.packages("rgeos")

library(raster)
library(maps)
library(sp)
library(rgdal)
library(maptools)
library(rgeos)
#read in the (typically) .bil file

tmean1_jan = raster('BIOCLIM_meanTemp/tmean1.bil')

plot(tmean1_jan)


#create a dummy lat-long file

sites = data.frame(longitude = c(-80, -90), latitude = c(40, 35))

head(sites)

points(sites$longitude, sites$latitude, col = 'red', pch = 16)

test = extract(tmean1_jan, sites)

test

#also still need to correct data using so refer to Jes Coyle's code first: 
# Read in single raster layer from january
tmean_jan = raster('BIOCLIM_meanTemp/tmean1.bil') # note scale off by factor of 10
plot(tmean_jan)

# Read in stack of layers from all 12 months
files = paste('BIOCLIM_meanTemp/tmean',1:12,'.bil', sep='')
tmean = stack(files) 
plot(tmean)

#above code works!!!

# Find MEAN across all months
meanT = calc(tmean, mean)
meanT
plot(meanT, col=mycol) #what is mycol? necessary to plot at all?

# Convert to actual temp
meanT = meanT/10 #done

bbc_lat_long$temp<-extract(meanT, sites)

#do the same for precip

#try to extract real data for bbc using bbc_lat_long file 
head(bbc_lat_long)
sites<-data.frame(longitude = bbc_lat_long$longitude, latitude = bbc_lat_long$latitude)
points(sites$longitude, sites$latitude, col= "red", pch=16)

test2 <- extract(tmean1_jan, sites)
test2
bbc_lat_long$temp<-extract(tmean1_jan, sites)
head(bbc_lat_long)
#precip?
precip<-raster("BIOCLIM_meanPrecip/prec_1.bil")
plot(precip)
points(sites$longitude, sites$latitude, col = 'red', pch = 16)
testP<-extract(precip, sites)
bbc_lat_long$precip<-extract(precip, sites)
#do for entire stack 

# Read in stack of layers from all 12 months
files2 = paste('BIOCLIM_meanPrecip/prec_',1:12,'.bil', sep='')
pmean = stack(files2) 
plot(pmean)
?plot
#above code works!!!

# Find MEAN across all months
meanP = calc(pmean, mean)
meanP #done, takes 45 mins to re-run 

bbc_lat_long$precip<-extract(meanP, sites)

#try to bring in ndvi veg indices similarly 
ndvimean<-raster("//bioark.ad.unc.edu/HurlbertLab/GIS/MODIS NDVI/Vegetation_Indices_may-aug_2000-2010.gri")
plot(ndvimean)
points(sites$longitude, sites$latitude, col = 'red', pch = 16)
test3 = extract(ndvimean, sites)
head(test3)
ndvimean<-ndvimean/10000
bbc_lat_long$ndvi<-extract(ndvimean, sites)
#rescaled by 10,000; celsius divided by 10 
#do elevation data too; process same for mean 
#mean elevation PLUS elevational range 
elevmean<-raster("alt.bil")
plot(elevmean)
plot(elevmean, xlim = c(-140, -60), ylim = c(25, 60))
points(sites$longitude, sites$latitude, col = 'red', pch = 16)
test4 = extract(elevmean, sites)
bbc_lat_long$elev<-extract(elevmean, sites)
head(test4)
#pull in radius elev data from Coyle folder 
elevrad<-raster("elevation_var_40km_radius.gri")
str(elevrad)
#need to re-project data points to match projection of elevation raster data 
#modify below code
elev_proj = "+proj=laea +lat_0=40.68 +lon_0=-92.925 +units=km +ellps=WGS84" # A string that defines the projection
tst = SpatialPoints(sites)
tst = SpatialPoints(sites, proj4string=CRS(elev_proj))
plot(elevrad)
points(tst)
tst = SpatialPoints(sites, proj4string=CRS("+proj=longlat +datum=WGS84"))
tst2 = spTransform(tst, CRS(elev_proj))
points(tst2, pch = 16, col = 'red')
?gBuffer
tst2_buff = gBuffer(tst2, width=40)
plot(tst2_buff)
tst2_buff = gBuffer(tst2, width=40)
plot(tst2_buff)
#extract data just like before with raster function 
test5 = extract(elevrad, tst2)
head(test5)
head(env_data) #bbs, not bbc data
#need to calc env z scores for bbc data 
#calc sd for each same as calc'd means 
#create forloop calcing z score for each bbc site and creating new output column attached to data 
#same as did for mindists 
#mean for a point - calc the mean of the means for all the data/sd of the means
#do I need to create a subset of unique bbc values or temp files? 
bbc_lat_long$zTemp_x = (bbc_lat_long$temp - mean(env_data$mat.mean)) / sd(env_data$mat.mean)
bbc_lat_long$zPrecip_x = (bbc_lat_long$precip - mean(env_data$ap.mean)) / sd(env_data$ap.mean)
bbc_lat_long$zElev_x = (bbc_lat_long$elev - mean(env_data$elev.mean)) / sd(env_data$elev.mean)
bbc_lat_long$zNDVI_x = (bbc_lat_long$ndvi - mean(env_data$sum.NDVI.mean)) / sd(env_data$sum.NDVI.mean)
bbc_zscores2<-write.csv(bbc_lat_long, file="bbc_zscores2.csv")
head(env_data)

#Euclidean distance calcs
#need to get BBS site z scores as well (zTempBBS, zPrecipBBS)
env_data$zTemp_y = (env_data$mat.mean - mean(env_data$mat.mean)) / sd(env_data$mat.mean)
env_data$zPrecip_y = (env_data$ap.mean - mean(env_data$ap.mean)) / sd(env_data$ap.mean)
env_data$zElev_y = (env_data$elev.mean - mean(env_data$elev.mean)) / sd(env_data$elev.mean)
env_data$zNDVI_y = (env_data$sum.NDVI.mean - mean(env_data$sum.NDVI.mean)) / sd(env_data$sum.NDVI.mean)
#merge based on geographic paried site file so that can link data up in first place 
#link bbc_lat_long to output (pair_dist is all <40km); link that file to env_data
link<-merge(bbc_lat_long, output, by.x="siteID", by.y="bbcsiteID")
head(link)
dim(link)
Euclid<-merge(link, env_data, by.x="bbsrouteID", by.y="stateroute")
head(Euclid)
dim(Euclid) #still only has 360 rows, should only have 53 columns, successful 
#merge paired to current BBC env data and then merge again with BBS env data 
#should still only be 360 rows 
#then for each sqrt of (zTempBBC - zTempBBS)
#subset first just to rows I want 
#x suffix = bbc derived env variables, y suffix = bbs derived env variables
Euclid<-subset(Euclid, select = c(bbsrouteID, siteID, latitude, longitude, temp, precip, ndvi, 
                                  elev, zTemp_x, zPrecip_x, zElev_x, zNDVI_x, minDist, 
                                  zTemp_y, zPrecip_y, zElev_y, zNDVI_y))
head(Euclid)
#### ---- Euc dist ---- #####
sq.dist.temp = (Euclid$zTemp_x - Euclid$zTemp_y)^2 #this is just the y2-y1 part, again, don't need to do a forloop 
sq.dist.precip = (Euclid$zPrecip_x - Euclid$zPrecip_y)^2
sq.dist.elev = (Euclid$zElev_x - Euclid$zElev_y)^2 
sq.dist.evi = (Euclid$zNDVI_x - Euclid$zNDVI_y)^2
Euclid$euc.dist = sqrt(sq.dist.temp + sq.dist.precip + sq.dist.elev + sq.dist.evi)
head(Euclid)
Euclid<-write.csv(Euclid, file="Euclid.csv")
Euclid<-read.csv("Euclid.csv", header = T)
###----create model that incorporates both geog and euclidean distance with discrepancies----####
#first merge Euclid data to table with the agreements/disagreements
#will reduce number of rows available 
Euclid_mod_data<-merge(Euclid, distance_agreement, by.x="siteID", by.y="bbcsiteID")
head(Euclid_mod_data)
dim(Euclid_mod_data) #127 rows 
#model incorporating both euclidean and geographic distance as variables 
Euclid_mod1<-lm(Core_Core~euc.dist+minDist.x, data=Euclid_mod_data)
Euclid_mod2<-lm(Core_Trans~euc.dist+minDist.x, data=Euclid_mod_data)
Euclid_mod3<-lm(Trans_Trans~euc.dist+minDist.x, data=Euclid_mod_data)
Euclid_mod4<-lm(Trans_Core~euc.dist+minDist.x, data=Euclid_mod_data)
summary(Euclid_mod1)
Euclid_fig<-boxplot(Euclid_mod_data[, 22:25], xlab="Dataset Agreement", ylab="Euclidean Distance")
#reminder: first part of designation refers to BBS half, second to the BBC half 
#do variance partitioning after model that includes both geog and euclid dist;
lm_sum_euc2<-lm(Core_Trans~euc.dist, data=Euclid_mod_data)
lm_sum_dist2<-lm(Core_Trans~minDist.x, data=Euclid_mod_data) 
#and then Euclid_mod2
summary(Euclid_mod2)$r.squared
summary(lm_sum_euc2)$r.squared
summary(lm_sum_dist2)$r.squared
a= summary(Euclid_mod2)$r.squared - summary(lm_sum_dist2)$r.squared
a
c= summary(Euclid_mod2)$r.squared - summary(lm_sum_euc2)$r.squared
c
b= summary(lm_sum_dist2)$r.squared - c
b
d= 1- summary(Euclid_mod2)$r.squared
d
#then try to look at unique variance explained by one or the other
#next step would be to use a landcover dataset where we characterize landcover 
#reference occ_dist_vs_scale R file in git repository folder in C drive 
#do at smaller scales for: 5, 10, 25, 50 stop data site pairings (geog AND euc dist)