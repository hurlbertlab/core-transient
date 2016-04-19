######################################
# BIOL 465 Research Project
# Sara Snell
# 11/18/15
# Climatic suitability and temporal occupancy in avian species
######################################

# Use this R script file to document how you proceed from 
# your raw data files to your processed (e.g. aggregated and/or merged) 
# data to analyses and plots. Use comments to create a road map for 
# everything that you will need to do. Some general steps that might 
# be useful are below, but be as specific as possible with respect to 
# which fields are being manipulated and what exactly is being 
# calculated when writing your comments.

### Set your working directory to the folder where all of your data and code are stored
#   E.g., setwd('C:/Users/hurlbert/Biol465/Project') on PC or
#         setwd('/Users/hurlbert/Biol465/Project') on Mac

setwd('/Users/terrysnell/Desktop/Biodiv Course Project')

### Read in raw data files
#   Mention briefly the source of these data files (e.g. provide link)
#   and what kind of data each file contains in comments

#### ----Temperature ----#####
#read in temperature data from world clim, stack data to get 1 MAT value
#temp/precip/elevation data from worldclim
tmean <- getData("worldclim", var = "tmean", res = 10)
tmin1 <- raster(paste(getwd(), "/wc10/tmin1.bil", sep = ""))
#plot(tmin1)

# Read in stack of layers from all 12 months 
tmean <- getData("worldclim", var = "tmean", res = 10)
files<-paste('/Users/terrysnell/Desktop/Biodiv Course Project/tmean_10m_bil/tmean',1:12,'.bil',sep='')
tmeans<-stack(files)
mat = calc(tmeans, mean)

#### ----Precip ----#####
#read in precip data from world clim, stack data to get 1 MAP value
# Read in stack of layers from all 12 months
prec <- getData("worldclim", var = "prec", res = 10)
pfiles<-paste('/Users/terrysnell/Desktop/Biodiv Course Project/prec_10m_bil/prec',1:12,'.bil',sep='')
pmeans<-stack(pfiles)

map = calc(pmeans, mean)

#### ----Elev ----#####
#read in elevation data from world clim
elev <- getData("worldclim", var = "alt", res = 10)
alt_files<-paste('/Users/terrysnell/Desktop/Biodiv Course Project/alt_10m_bil', sep='')

#### ----NDVI ----#####
#read in EVI data from Coyle et al 2013
EVI <- read.csv("/Users/terrysnell/Desktop/Biodiv Course Project/All Env Data.csv", header = T)
fEVI <- merge(latlongs, EVI, by = "stateroute", all = F, sort=FALSE)

#data table with all environmental variable means (use for this project only bc EVI wasn't working)
envtable <- read.csv("/Users/terrysnell/Desktop/Biodiv Course Project/All Env Data.csv", header = T)

### Simplify data (subset to the columns and/or rows of interest)

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

#mean data for all variables
finalenv <- subset(fEVI, select = c('stateroute', 'sum.EVI', 'elev.mean', 'mat', 'ap.mean')) #this is mean data for all vars

##### ---- BIRDS data ----#####
#Making BBS data frame from wide to long using tidyr
library(tidyr)
tidyBirds <- gather(data = occumatrix, 
                    key = Species,
                    value = Occupancy, 
                    na.rm = TRUE,
                    X2881:X22860)

tidyBirds$Species = as.numeric(substr(tidyBirds$Species, 2, 
                                      nchar(as.character(tidyBirds$Species)))) #changed from X-sp to #sp
head(tidyBirds)

### Calculate metrics of interest (e.g. mean abundance per species, or 
#   species richness per site, etc.)

####---- Creating final data frame----####
#For loop to calculate mean & standard dev environmental variables for each unique species
uniq.spp = unique(tidyBirds$Species, header = "Species")
birdsoutputm = c()
for (species in uniq.spp) {
  spec.routes <- tidyBirds[(tidyBirds$Species) == species, "stateroute"] #subset routes for each species (i) in tidybirds
  env.sub <- envtable[envtable$stateroute %in% spec.routes, ] #subset routes for each env in tidybirds
  envmeans = as.vector(apply(env.sub[, 2:5], 2, mean))
  envsd = as.vector(apply(env.sub[, 2:5], 2, sd))
  
  birdsoutputm = rbind(birdsoutputm, c(species, envmeans, envsd))
  
}
birdsoutput = data.frame(birdsoutputm)
names(birdsoutput) = c("Species", "Mean.Temp", "Mean.Precip", "Mean.Elev", "Mean.EVI", "SD.Temp", "SD.Precip", "SD.Elev", "SD.EVI")


### Combine relevant information from each of your two or more datasets
#   using merge()

#combining all information into one big data frame (species/occupancy/expected env variables/observed env variables)
occubirds <- merge(birdsoutput, tidyBirds, by = "Species", all = FALSE, na.rm = T)
occuenv <- merge(envtable, occubirds, by = "stateroute", all = F, na.rm = T)
occuenv <- na.omit(occuenv)

### Conduct analyses (linear regression, variance partitioning, etc)

#Calculating z scores for each environmnetal variable (observed mean - predicted mean/predicted SD)
occuenv$zTemp <- ((occuenv$mat - occuenv$Mean.Temp) / occuenv$SD.Temp)
occuenv$zPrecip = (occuenv$ap.mean - occuenv$Mean.Precip) / occuenv$SD.Precip
occuenv$zElev = (occuenv$elev.mean - occuenv$Mean.Elev) / occuenv$SD.Elev
occuenv$zEVI = (occuenv$sum.EVI - occuenv$Mean.EVI) / occuenv$SD.EVI

#subset evi for 500 routes on environmental 
####---- linear model----####
#creating linear models for each species and each environmental variable
#storing p, r2, slope in a matrix
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


#convert from matrix to data frame, give column names


#going to calculate euclidean distance for each environmental variable (euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2)))
#aggregate euclidean distance by species, take species-specific standard deviation
#divide euclidean distance by species-specific standard deviation


### Make plots
#going to make histograms to find outlier p/r2/slope for species
#going to plot each envrionmental variable slope vs. the others (6 pairings) 
#maybe another plot...TBD

#------------------------------------------------------------------------------#
# One basic test of your code is whether you can open a new RStudio window and #
# run the whole script without getting an error.                    #
#------------------------------------------------------------------------------#