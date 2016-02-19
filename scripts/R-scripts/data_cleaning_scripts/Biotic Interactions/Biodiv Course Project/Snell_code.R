######################################
# BIOL 465 Research Project
# Sara Snell
# 12/9/15
#Avian Temporal Occupancy and Climatic Suitability to Determine 
#Core and Transient Species Distributions in the United States
######################################

### Set working directory to the folder where all of your data and code are stored
setwd('/Users/terrysnell/Desktop/Biodiv Course Project')

#-------------for this project only used "All Env Data.csv" START ON LINE 49---------------------------------------#
###Temperature, precip, elevation data from WorldClim: http://www.worldclim.org/current
#### ----Temperature ----#####
#read in temperature data from world clim directly, stack data to get 1 MAT value
#temp/precip/elevation data from worldclim
library(raster)
tmean <- getData("worldclim", var = "tmean", res = 10)

# Read in stack of layers from all 12 months 
tmean <- getData("worldclim", var = "tmean", res = 10)
files<-paste('/Users/terrysnell/Desktop/Biodiv Course Project/tmean_10m_bil/tmean',1:12,'.bil',sep='')
tmeans<-stack(files)
mat = calc(tmeans, mean)
#plot(mat)

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
#read in EVI data from Coyle et al 2013, provided by Dr. Hurlbert
#data table with all environmental variable means (use for this project only bc EVI wasn't working)
EVI <- read.csv("/Users/terrysnell/Desktop/Biodiv Course Project/All Env Data.csv", header = T)
fEVI <- merge(latlongs, EVI, by = "stateroute", all = F, sort=FALSE)
evi = subset(fEVI, select = c('stateroute', 'sum.EVI'))

### Simplify data (subset to the columns and/or rows of interest) START HERE

#--------------------------------------------START HERE------------------------------------------------------------#

####----Creating an environmental matrix ----####
library(raster)
occumatrix <- read.table("/Users/terrysnell/Desktop/Biodiv Course Project/Cleaned Data/site_sp_occupancy_matrix.csv", sep=",", header = T)
names(occumatrix)[names(occumatrix)=="X"] <- "stateroute"
route.locs = read.csv('/Users/terrysnell/Desktop/Biodiv Course Project/routes.csv')
route.locs$stateroute = 1000*route.locs$statenum + route.locs$Route

latlongs = subset(route.locs, select = c('stateroute', 'Lati', 'Longi'))
route.sp = coordinates(latlongs[,3:2])
plot(route.sp)

#Read in ENV data set
envtable <- read.csv("/Users/terrysnell/Desktop/Biodiv Course Project/All Env Data.csv", header = T)

#mean data for all variables
finalenv <- subset(envtable, select = c('stateroute', 'sum.EVI', 'elev.mean', 'mat', 'ap.mean')) 

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

### Calculate metrics of interest
####---- Creating final data frame----####
#For loop to calculate mean & standard dev environmental variables for each unique species
uniq.spp = unique(tidyBirds$Species, header = "Species")
birdsoutputm = c()
for (species in uniq.spp) {
  spec.routes <- tidyBirds[(tidyBirds$Species) == species, "stateroute"] #subset routes for each species (i) in tidybirds
  env.sub <- envtable[envtable$stateroute %in% spec.routes, ] #subset routes for each env in tidybirds
  envmeans = as.vector(apply(env.sub[, c("mat", "ap","elev","sum.EVI")], 2, mean))
  envsd = as.vector(apply(env.sub[, c("mat", "ap","elev","sum.EVI")], 2, sd))
  
  birdsoutputm = rbind(birdsoutputm, c(species, envmeans, envsd))
  
}
birdsoutput = data.frame(birdsoutputm)
names(birdsoutput) = c("Species", "Mean.Temp", "Mean.Precip", "Mean.Elev", "Mean.EVI", "SD.Temp", "SD.Precip", "SD.Elev", "SD.EVI")

### Combine relevant information from each of your two or more datasets using merge()
#(species/occupancy/expected env variables/observed env variables)
occubirds <- merge(birdsoutput, tidyBirds, by = "Species", all = FALSE, na.rm = T)
occuenv <- merge(envtable, occubirds, by = "stateroute", all = F, na.rm = T)
occuenv <- na.omit(occuenv)

### Conduct analyses
#Calculating z scores for each environmnetal variable (observed mean - predicted mean/predicted SD)
occuenv$zTemp = (occuenv$mat - occuenv$Mean.Temp) / occuenv$SD.Temp
occuenv$zPrecip = (occuenv$ap.mean - occuenv$Mean.Precip) / occuenv$SD.Precip
occuenv$zElev = (occuenv$elev.mean - occuenv$Mean.Elev) / occuenv$SD.Elev
occuenv$zEVI = (occuenv$sum.EVI - occuenv$Mean.EVI) / occuenv$SD.EVI

#### ---- Euc dist ---- #####
#euclidean distance for each environmental variable (euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2)))
sq.dist.temp = (occuenv$mat - occuenv$Mean.Temp)^2
sq.dist.precip = (occuenv$ap.mean - occuenv$Mean.Precip)^2 
sq.dist.elev = (occuenv$elev.mean - occuenv$Mean.Elev)^2 
sq.dist.evi = (occuenv$sum.EVI - occuenv$Mean.EVI)^2
occuenv$euc.dist = sqrt(sq.dist.temp + sq.dist.precip + sq.dist.elev + sq.dist.evi)

#aggregate euclidean distance by species, take species-specific standard deviation
sorted.sd <- aggregate(occuenv$euc.dist, by = list(occuenv$Species), FUN = sd)
names(sorted.sd) = c("Species", "sp.sd")

#divide euclidean distance by species-specific standard deviation
final.analy.table <- merge(occuenv, sorted.sd, by = "Species")
occuenv$euc.dist.spp = occuenv$euc.dist/final.analy.table$sp.sd

#adding in common names to aid analysis
taxAOU <- read.table("/Users/terrysnell/Desktop/Biodiv Course Project/Tax_AOU_Alpha.csv", header = T, sep = ",", quote = "\"")
names(taxAOU) = c("Common.Name","Species","Code")

occuenv = merge(occuenv, taxAOU, by = "Species")

####---- linear model----####
#creating linear models for each species and each environmental variable
#storing p, r2, slope in a matrix
uniq2 <- unique(occuenv$Species)
beta = matrix(NA, nrow = length(uniq2), ncol = 19)

pdf('Occupancy_lm.pdf', height = 8, width = 10)
par(mfrow = c(3, 4))

for(i in 1:length(uniq2)){ 
  spec <- subset(occuenv, Species == uniq2[i]) #subset routes for each species (i) in tidybirds
  if(nrow(spec) > 4) {
  lmtemp <- lm(Occupancy ~ abs(zTemp), data = spec)
  lmelev <- lm(Occupancy ~ abs(zElev), data = spec)
  lmprecip <- lm(Occupancy ~ abs(zPrecip), data = spec)
  lmevi <- lm(Occupancy ~ abs(zEVI), data = spec)
  lmeuc.dist <- lm(Occupancy ~ abs(euc.dist.spp), data = spec)
  lmmulti <- lm(Occupancy ~ euc.dist.spp + zTemp+ zElev +zPrecip+ zEVI, data=spec)
  beta[i,1] = uniq2[i]
  beta[i,2] = summary(lmtemp)$coef[2,"Estimate"]
  beta[i,3] = summary(lmtemp)$coef[2,"Pr(>|t|)"]
  beta[i,4] = summary(lmtemp)$r.squared #output of summary distinct from lmtemp
  beta[i,5] = summary(lmelev)$coef[2,"Estimate"]
  beta[i,6] = summary(lmelev)$coef[2,"Pr(>|t|)"]
  beta[i,7] = summary(lmelev)$r.squared 
  beta[i,8] = summary(lmprecip)$coef[2,"Estimate"]
  beta[i,9] = summary(lmprecip)$coef[2,"Pr(>|t|)"]
  beta[i,10] = summary(lmprecip)$r.squared 
  beta[i,11] = summary(lmevi)$coef[2,"Estimate"]
  beta[i,12] = summary(lmevi)$coef[2,"Pr(>|t|)"]
  beta[i,13] = summary(lmevi)$r.squared 
  beta[i,14] = summary(lmeuc.dist)$coef[2,"Estimate"]
  beta[i,15] = summary(lmeuc.dist)$coef[2,"Pr(>|t|)"]
  beta[i,16] = summary(lmeuc.dist)$r.squared
  beta[i,17] = summary(lmmulti)$coef[2,"Estimate"] #off
  beta[i,18] = summary(lmmulti)$coef[2,"Pr(>|t|)"]
  beta[i,19] = summary(lmmulti)$adj.r.squared 

  #R2 position
  tempR2.pos = .8*(max(abs(spec$zTemp)) - min(abs(spec$zTemp))) + min(abs(spec$zTemp))
  elevR2.pos = .8*(max(abs(spec$zElev)) - min(abs(spec$zElev))) + min(abs(spec$zElev))
  precipR2.pos = .8*(max(abs(spec$zPrecip)) - min(abs(spec$zPrecip))) + min(abs(spec$zPrecip))
  eviR2.pos = .8*(max(abs(spec$zEVI)) - min(abs(spec$zEVI))) + min(abs(spec$zEVI))
  
  plot(abs(spec$zTemp), spec$Occupancy, pch = 16, xlab = "Temperature z-score", ylim = c(0,1), ylab = "Occupancy", main = spec$Common.Name[1], abline(lmtemp, col = "red"))
  text(x =tempR2.pos, y = .1, paste("R2 = ", round(beta[i,4], 2)), col = "red")
  plot(abs(spec$zElev), spec$Occupancy, pch = 16, xlab = "Elevation z-score", ylim = c(0,1), ylab = "Occupancy", main = spec$Common.Name[1], abline(lmelev, col = "red"))
  text(x = elevR2.pos, y = .1, paste("R2 = ", round(beta[i,7], 2)), col = "red")
  plot(abs(spec$zPrecip), spec$Occupancy, pch = 16, xlab = "Precipitation z-score", ylim = c(0,1), ylab = "Occupancy",main = spec$Common.Name[1], abline(lmprecip, col = "red"))
  text(x =precipR2.pos, y = .1, paste("R2 = ", round(beta[i,10], 2)), col = "red")
  plot(abs(spec$zEVI), spec$Occupancy, pch = 16, xlab = "EVI z-score", ylim = c(0,1), ylab = "Occupancy", main = spec$Common.Name[1], abline(lmevi, col = "red"))
  text(x = eviR2.pos, y = .1, paste("R2 = ", round(beta[i,13], 2)), col = "red")

  } else {
    beta[i, 1] = uniq2[i]
    beta[i, 2:19] = NA
  }
  
}

dev.off()    #closes plotting device, screen or connxn to a file

output.frame <- data.frame(beta) 
names(output.frame) = c("Species", "Temp.Est", "Temp.P", "Temp.R2", "Elev.Est", "Elev.P", "Elev.R2", "Precip.Est", "Precip.P", "Precip.R2", 
                        "EVI.Est", "EVI.P", "EVI.R2", "Euc.dist.Est", "Euc.dist.P", "Euc.dist.R2", "Multi.Est", "Multi.P", "Multi.R2")
analy <- merge(output.frame, taxAOU, by = "Species")
write.csv(analy, file = "BIOL465_final")

predictedtemp <- subset(analy, Temp.Est < 0)
predictedED <- subset(analy, Euc.dist.Est < 0)

####---- Plots ----####
#histograms to find outlier p/r2/slope for species
test = subset(output.frame, Temp.R2 != 1 & Elev.R2 != 1 & Precip.R2 != 1 & EVI.R2 != 1)
hist(test$Temp.R2, 10, main = "R Squared Distribution for Temperature", xlab = "Temperature R Squared")
hist(test$Elev.R2, 10, main = "R Squared Distribution for Elevation", xlab = "Elevation R Squared")
hist(test$Precip.R2, 10, main = "R Squared Distribution for Precipitation", xlab = "Precipitation R Squared")
hist(test$EVI.R2, 10, main = "R Squared Distribution for EVI", xlab = "EVI R Squared")
hist(test$Euc.dist.R2, 10, main = "R Squared Distribution for Eucldiean Distance", xlab = "ED R Squared")
hist(test$Multi.R2, 10, main = "R Squared Distribution for Multivariate Model", xlab = "Multivariate R Squared")


hist(test$Temp.Est, 10, main = "Slope Distribution for Temperature", xlab = "Temperature Slope")
abline(v = mean(test$Temp.Est), col = "red", lwd = 3)
hist(test$Elev.Est, 10, main = "Slope Distribution for Elevation", xlab = "Elevation Slope")
abline(v = mean(test$Elev.Est), col = "red", lwd = 3)
hist(test$Precip.Est, 10, main = "Slope Distribution for Precipitation", xlab = "Precipitation Slope")
abline(v = mean(test$Precip.Est), col = "red", lwd = 3)
hist(test$EVI.Est, 10, main = "Slope Distribution for EVI", xlab = "EVI Slope")
abline(v = mean(test$EVI.Est), col = "red", lwd = 3)
hist(test$Euc.dist.Est, 10, main = "Slope Distribution for Euclidean Distance", xlab = "ED Slope")
abline(v = mean(test$Euc.dist.Est), col = "red", lwd = 3)
#hist(output.frame$Multi.Est, main = "Slope Distribution for Multivariate Model", xlab = "Multivariate Slope")

#envrionmental variable slope vs. the others (6 pairings), did not end up using
#slope
test2 = subset(output.frame, Temp.R2 != 1 & Elev.R2 != 1 & Precip.R2 != 1 & EVI.R2 != 1)
plot(x=test2$Temp.Est, y=test2$Precip.Est, pch = 16,  col = "blue")
points(x=test2$Temp.Est, y=test2$Elev.Est, pch = 16, col = "gold")
points(x=test2$Temp.Est, y=test2$EVI.Est, pch = 16, col = "red")


plot(x=output.frame$Precip.Est, y=output.frame$EVI.Est, pch = 16, col = "blue")
plot(x=output.frame$Elev.Est, y=output.frame$EVI.Est, pch = 16, col = "blue")
plot(x=output.frame$Elev.Est, y=output.frame$Precip.Est, pch = 16, col = "blue")

#t tests
t.test(output.frame$Temp.Est,output.frame$Precip.Est,paired=TRUE) 
#p = 0.1108
#ES = 1.567293 
t.test(output.frame$Temp.Est,output.frame$Elev.Est,paired=TRUE) 
#p-value = 0.286
#ES = 0.9574938
t.test(output.frame$Temp.Est,output.frame$EVI.Est,paired=TRUE)
#p-value = 0.161
#ES = 1.046498 
t.test(output.frame$Precip.Est,output.frame$EVI.Est,paired=TRUE) 
#p-value = 0.3562
#ES =  -0.5207954 
t.test(output.frame$Elev.Est,output.frame$EVI.Est,paired=TRUE) 
#p-value = 0.3807
#ES = 0.08900375 
t.test(output.frame$Elev.Est,output.frame$Precip.Est,paired=TRUE) 
#p-value = 0.136
#ES = 0.6097991

#r
plot(x=output.frame$Temp.R2, y=output.frame$Precip.R2, pch = 16,  col = "blue")
par(new = T)
plot(x=output.frame$Temp.R2, y=output.frame$Elev.R2, pch = 16, col = "gold")
par(new = T)
plot(x=output.frame$Temp.R2, y=output.frame$EVI.R2, pch = 16, col = "red")
par(new = T)

plot(x=output.frame$Precip.R2, y=output.frame$EVI.R2, pch = 16, col = "blue")
plot(x=output.frame$Elev.R2, y=output.frame$EVI.R2, pch = 16, col = "blue")
plot(x=output.frame$Elev.R2, y=output.frame$Precip.R2, pch = 16, col = "blue")

#t tests
t.test(output.frame$Temp.R2,output.frame$Precip.R2,paired=TRUE) 
#p-value = 0.001983
t.test(output.frame$Temp.R2,output.frame$Elev.R2,paired=TRUE) 
#p-value = 0.001832
t.test(output.frame$Temp.R2,output.frame$EVI.R2,paired=TRUE)
#p-value = 1.183e-05
t.test(output.frame$Precip.R2,output.frame$EVI.R2,paired=TRUE) 
#p-value = 0.1322
t.test(output.frame$Elev.R2,output.frame$EVI.R2,paired=TRUE) 
#p-value = 0.07868
t.test(output.frame$Elev.R2,output.frame$Precip.R2,paired=TRUE) 
#p-value = 0.7133

test = subset(output.frame, Temp.R2 != 1 & Elev.R2 != 1 & Precip.R2 != 1 & EVI.R2 != 1)
plot(x=test$Temp.R2, y=test$Precip.R2, pch = 16,  col = "blue", xlab = " ", ylab = " ")
par(new = T)
plot(x=test$Temp.R2, y=test$Elev.R2, pch = 16, col = "gold", xlab = " ", ylab = " ")
par(new = T)
plot(x=test$Temp.R2, y=test$EVI.R2, pch = 16, col = "red", xlab = "Temperature R Squared", ylab = "Environmental R Squared")
par(new = F)

#calculating mean estimates
mean(output.frame[["Temp.Est"]], na.rm = T)
mean(output.frame[["Elev.Est"]], na.rm = T)
mean(output.frame[["Precip.Est"]], na.rm = T)
mean(output.frame[["EVI.Est"]], na.rm = T)
mean(output.frame[["Euc.dist.Est"]], na.rm = T)
#------------------------------------------------------------------------------#
# One basic test of your code is whether you can open a new RStudio window and #
# run the whole script without getting an error.                    #
#------------------------------------------------------------------------------#