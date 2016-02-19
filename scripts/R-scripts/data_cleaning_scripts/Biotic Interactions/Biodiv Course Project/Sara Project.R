#### ----All libs ----#####
library('sp')
library('gstat')
library('rgdal')
library("raster")

#### ----Temperature ----#####
tmean <- getData("worldclim", var = "tmean", res = 10)
#/Users/terrysnell/Desktop/Biodiv Course Project/tmean_10m_bil
tmin1 <- raster(paste(getwd(), "/wc10/tmin1.bil", sep = ""))
plot(tmin1)
newext <- drawExtent()  # click twice on the map to select the region of interest
tmin1.c <- crop(tmin1, newext)
plot(tmin1.c)
list.ras <- mixedsort(list.files(paste(getwd(), "/wc10/", sep = ""), full.names = T, pattern = ".bil"))
list.ras  # I have just collected a list of the files containing monthly temperature values
tmin.all <- stack(list.ras)
tmin.all <- tmin.all/10
tmin.all.c <- crop(tmin.c, newext)
plot(tmin.all.c)

# Read in stack of layers from all 12 months 
tmean <- getData("worldclim", var = "tmean", res = 10)
files<-paste('/Users/terrysnell/Desktop/Biodiv Course Project/tmean_10m_bil/tmean',1:12,'.bil',sep='')
tmeans<-stack(files)
#plot(tmeans)
#Calc means
mat = calc(tmeans, mean)
#Crop extent
#newext <- drawExtent()  
#tmeans.c <- crop(mat, newext)

#### ----Precip ----#####
# Read in stack of layers from all 12 months (doesn't work for these datasets!!)
prec <- getData("worldclim", var = "prec", res = 10)
pfiles<-paste('/Users/terrysnell/Desktop/Biodiv Course Project/prec_10m_bil/prec',1:12,'.bil',sep='')
pmeans<-stack(pfiles)
#plot(pmeans)
#Calc means
map = calc(pmeans, mean)
#Crop extent
#newext <- drawExtent()  
#pmeans.c <- crop(map, newext)

#### ----Elev----#####
#read in elevation data from world clim
elev <- getData("worldclim", var = "alt", res = 10)
alt_files<-paste('/Users/terrysnell/Desktop/Biodiv Course Project/alt_10m_bil', sep='')
#plot(altitude)

#newext <- drawExtent()  # click twice on the map to select the region of interest
#alt.new <- crop(altitude, newext)
#plot(alt.new)


#### ----NDVI ----#####
EVI <- read.csv("/Users/terrysnell/Desktop/Biodiv Course Project/All Env Data.csv", header = T)
fEVI <- merge(latlongs, EVI, by = "stateroute", all = F, sort=FALSE)

envtable <- read.csv("/Users/terrysnell/Desktop/Biodiv Course Project/All Env Data.csv", header = T)

####----Creating an environmental matrix ----####
occumatrix <- read.table("/Users/terrysnell/Desktop/Biodiv Course Project/Cleaned Data/site_sp_occupancy_matrix.csv", sep=",", header = T)
names(occumatrix)[names(occumatrix)=="X"] <- "stateroute"
route.locs = read.csv('/Users/terrysnell/Desktop/Biodiv Course Project/routes.csv')
route.locs$stateroute = 1000*route.locs$statenum + route.locs$Route

latlongs = subset(route.locs, select = c('stateroute', 'Lati', 'Longi'))
#latlongs.temp = extract(mat, route.sp)
route.sp = coordinates(latlongs[,3:2])
plot(route.sp)

#extracting temp out
finalmat = mat/10
temp <- raster::extract(finalmat, route.sp)
latlongs.temp

#extracting precip out
finalprecip <- raster::extract(map, route.sp)
latlongs.finalprecip

#extracting altitude out
elev <- raster::extract(altitude, route.sp)

#extracting EVI out
evi = subset(fEVI, select = c('stateroute', 'sum.EVI'))

#finalenv <- data.frame(stateroute = latlongs$stateroute, Temp = temp, Precip = finalprecip, Elev = elev, EVI = evi$sum.EVI)
finalenv <- subset(fEVI, select = c('stateroute', 'sum.EVI', 'elev.mean', 'mat', 'ap.mean')) #this is mean data for all vars

##### ---- BIRDS data ----#####
#Making data frame from wide to long in tidyr
library(tidyr)
tidyBirds <- gather(data = occumatrix, 
                     key = Species,
                     value = Occupancy, 
                     na.rm = TRUE,
                     X2881:X22860)

tidyBirds$Species = as.numeric(substr(tidyBirds$Species, 2, 
                                      nchar(as.character(tidyBirds$Species)))) #changed from X-sp to #sp
head(tidyBirds)

####---- Creating final data frame----####
#second arg in for loop has to be a vector
uniq.spp = unique(tidyBirds$Species, header = "Species")
birdsoutputm = c()
for (species in uniq.spp) {
  spec.routes <- tidyBirds[(tidyBirds$Species) == species, "stateroute"] #subset routes for each species (i) in tidybirds
  env.sub <- finalenv[finalenv$stateroute %in% spec.routes, ] #subset routes for each env in tidybirds
  #envmeans = as.vector(apply(env.sub[, 2:5], 2, mean))
  envsd = as.vector(apply(env.sub[, 2:5], 2, sd))
  
  birdsoutputm = rbind(birdsoutputm, c(species, envmeans, envsd))
  
}
birdsoutput = data.frame(birdsoutputm)
names(birdsoutput) = c("Species", "Mean.Temp", "Mean.Precip", "Mean.Elev", "Mean.EVI", "SD.Temp", "SD.Precip", "SD.Elev", "SD.EVI")

occubirds <- merge(birdsoutput, tidyBirds, by = "Species", all = FALSE, na.rm = T)
occuenv <- merge(finalenv, occubirds, by = "stateroute", all = F, na.rm = T)
occuenv <- na.omit(occuenv)

occuenv$zTemp <- ((occuenv$mat - occuenv$Mean.Temp) / occuenv$SD.Temp)
occuenv$zPrecip = (occuenv$ap.mean - occuenv$Mean.Precip) / occuenv$SD.Precip
occuenv$zElev = (occuenv$elev.mean - occuenv$Mean.Elev) / occuenv$SD.Elev
occuenv$zEVI = (occuenv$sum.EVI - occuenv$Mean.EVI) / occuenv$SD.EVI


#subset evi for 500 routes on environmental 
####---- linear model----####
for (species in uniq.spp) {
lmtemp <- lm(Occupancy ~ abs(zTemp), data = occuenv)
lmelev <- lm(Occupancy ~ abs(zElev), data = occuenv)
lmprecip <- lm(Occupancy ~ abs(zPrecip), data = occuenv)
lmevi <- lm(Occupancy ~ abs(zEVI), data = occuenv)
summary(lmtemp)
summary(lmelev)
summary(lmprecip)
summary(lmevi)
}

#linear model for each species

#  lmtemp <- lm(Occupancy ~ zTemp, data = occuenv)
#  lmelev <- lm(Occupancy ~ zElev, data = occuenv)
#  lmprecip <- lm(Occupancy ~ zPrecip, data = occuenv)
#  lmevi <- lm(Occupancy ~ zEVI, data = occuenv)
uniq2 <- unique(occuenv$Species)
beta = matrix(NA, nrow = length(uniq2), ncol = 4)
#colnames(beta) <- colnames(occuenv)
for(i in 1:length(uniq2)){ 
  spec <- subset(occuenv, Species == uniq2[i]) #subset routes for each species (i) in tidybirds
  lmtemp <- lm(Occupancy ~ abs(zTemp), data = spec)
  beta[i,1] = uniq2[i]
  beta[i,2] = summary(lmtemp)$coef[2,"Estimate"]
  beta[i,3] = summary(lmtemp)$coef[2,"Pr(>|t|)"]
  beta[i,4] = summary(lmtemp)$r.squared #output of summary distinct from lmtemp
}  


# Potential alternative for loop
uniq2 <- unique(occuenv$Species)
beta = c()
#colnames(beta) <- colnames(occuenv)
for(i in uniq2){ 
  spec <- subset(occuenv, Species == i) #subset routes for each species (i) in tidybirds
  lmtemp <- lm(Occupancy ~ abs(zTemp), data = spec)
  
  slope = summary(lmtemp)$coef[2,"Estimate"]
  p = summary(lmtemp)$coef[2,"Pr(>|t|)"]
  R2 = summary(lmtemp)$r.squared #output of summary distinct from lmtemp
  temp = c(i, slope, p, R2)
  beta = rbind(beta, temp)
}  


occuenv$euc.dist.temp = sqrt((occuenv$mat - occuenv$Mean.Temp) ^ 2)
occuenv$euc.dist.precip = sqrt((occuenv$ap.mean - occuenv$Mean.Precip) ^ 2)
occuenv$euc.dist.elev = sqrt((occuenv$elev.mean - occuenv$Mean.Elev) ^ 2)
occuenv$euc.dist.evi = sqrt((occuenv$sum.EVI - occuenv$Mean.EVI) ^ 2)  
#{sqrt(sum(occuenv$mat - occuenv$Mean.Temp) ^ 2) + sqrt(sum(occuenv$ap.mean - occuenv$Mean.Precip) ^ 2) +sqrt(sum(occuenv$elev.mean - occuenv$Mean.Elev) ^ 2) + sqrt(sum(occuenv$sum.EVI - occuenv$Mean.EVI) ^ 2)}

dist(occuenv$mat, method = "euclidean", diag = FALSE, upper = FALSE, p = 2) 

#convert from matrix to data frame, give col names


#lmelev <- lm(Occupancy ~ abs(zElev), data = occuenv)
#lmprecip <- lm(Occupancy ~ abs(zPrecip), data = occuenv)
#lmevi <- lm(Occupancy ~ abs(zEVI), data = occuenv)
#cbind(lmtemp$coefficients)

#assign value to a placeholder in beta
#test <- lapply(lmtemp$coefficients, abs)
#rbind(summary(lmtemp)$coefficients[2, 1])


#occubirds <- merge(route.locs, occumatrix, by.y = "stateroute")
#foo = c(); for (blahdyblah in uniq.spp) {foo = c(foo, paste("a", blahdyblah, sep = ""))}
#foo = c(); for (blahdyblah in 1:length(uniq.spp)) {foo = c(foo, paste("a", uniq.spp[blahdyblah], sep = ""))}


#MERGE EVERYTHING (see picture)
#after getting for loop for mean/std/var
#env vars merged by state route (what I did before)
#merge in species env means and vars (on species)
#calculate env dev using observed species mean and spec variance, calc from sets of vars
#relate occupancy and env dev


