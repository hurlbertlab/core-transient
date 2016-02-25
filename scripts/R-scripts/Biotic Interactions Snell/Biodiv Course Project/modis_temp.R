# Script for NCBG and PR MODIS greenup data and temperature data - from Summer 2015
# Tracie Hayes
# September 8, 2015

# See http://onlinelibrary.wiley.com/doi/10.1002/ece3.1273/full for information about MODIS data
# (Table 2 under Method)

# setwd('C:/git/caterpillars-count-analysis/modis-and-temp')

# Load required libraries
library(MODISTools)
library(raster)

library(maps) #don't know if I need all of these libraries
library(sp)
library(rgdal)
library(maptools)
library(rgeos)

# Format MODIS data
modis = data.frame(lat = c(35.898645, 35.809674), long = c(-79.031469, -78.716546))
modis$start.date = rep(2015, nrow(modis)) #not sure if these dates are formatted correctly
modis$end.date = rep(2015, nrow(modis))
modis$ID = c(1,2)

# Download MODIS data
MODISSubsets(LoadDat = modis, Products = 'MOD13Q1', 
             Bands = c('250m_16_days_EVI', '250m_16_days_pixel_reliability'), 
             Size = c(1,1))
bgmodis <- read.csv(list.files(pattern = ".asc")[1], header = FALSE, as.is = TRUE)
prmodis <- read.csv(list.files(pattern = ".asc")[2], header = FALSE, as.is = TRUE)
bgmodis$julianday <- as.numeric(substring(bgmodis$V8, 6,8))
#bgmodis$date <- strptime(bgmodis$julianday, "%m/%d/%Y")
prmodis$julianday <- as.numeric(substring(prmodis$V8, 6,8))

# Calculating average EVI across area (not taking into account pixel reliability)
# Botanical Garden:
tempbgevi = bgmodis[grep("EVI", bgmodis$V6),]
bgevi <- tempbgevi[11:91]
bgmean1 <- apply(bgevi, 1, mean)
bgmean2 <- bgmean1 / 10000
bgmean <- data.frame(julianday = tempbgevi$julianday, EVImean = bgmean2)
plot(bgmean$julianday, bgmean$EVImean, xlab = "Julian Day", ylab = "Mean EVI",
     col = 'blue', type = 'l')
# Prairie Ridge:
tempprevi = prmodis[grep("EVI", prmodis$V6),]
previ <- tempprevi[11:91]
prmean1 <- apply(previ, 1, mean)
prmean2 <- prmean1 / 10000
prmean <- data.frame(julianday = tempprevi$julianday, EVImean = prmean2)
plot(prmean$julianday, prmean$EVImean, xlab = "Julian Day", ylab = "Mean EVI",
     col = 'red', type = 'l')

# Taking into account pixel reliability?
# Botanical Garden:
tempbgpix = bgmodis[grep("reliability", bgmodis$V6),]
# All data is either 0 (Use with confidence) or 1 (Useful, but look at other QA info)
# 1s present on rows 19, 25-30 

# Plotting MODIS data


library(stats)
# Fitting a logistic curve to EVI data and using this to estimate greenup date:

# Prairie Ridge               
prmean$EVIdis = prmean$EVImean - min(prmean$EVImean)+.01
prlog = nls(EVIdis ~ SSlogis(julianday, Asym, xmid, scal), data = prmean)
par(mar=c(5, 4, 4, 4) + 0.1)
plot(prmean$julianday, prmean$EVImean, xlab = "Julian Day", ylab = "Mean EVI",
                             col = 'red', type = 'l', lwd = 3)
prmean$prEVIlog = predict(prlog)+min(prmean$EVImean)-.01
points(prmean$julianday, prmean$prEVIlog, col = 'red', lwd = 3, 
       lty = 'dashed', type = 'l')

# Botanical Garden
bgmean$EVIdis = bgmean$EVImean - min(bgmean$EVImean)+.01
bglog = nls(EVIdis ~ SSlogis(julianday, Asym, xmid, scal), data = bgmean)
par(mar=c(5, 4, 4, 4) + 0.1)
plot(bgmean$julianday, bgmean$EVImean, xlab = "Julian Day", ylab = "Mean EVI",
     col = 'blue', type = 'l', lwd = 3)
bgmean$bgEVIlog = predict(bglog)+min(bgmean$EVImean)-.01
points(bgmean$julianday, bgmean$bgEVIlog, col = 'blue', lwd = 3, 
       lty = 'dashed', type = 'l')


plot(prmean$julianday, predict(prlog)+min(prmean$EVImean)-.01, col = 'red', lwd = 3, 
       lty = 'dashed', type = 'l', ylim = c(0.15, 0.53), xlab = "Julian Day", ylab = "Mean EVI")
points(bgmean$julianday, predict(bglog)+min(bgmean$EVImean)-.01, col = 'blue', lwd = 3, 
       lty = 'dashed', type = 'l')
legend("topleft", c('BG mean EVI', 'PR mean EVI'), lwd = c(3,3), lty = c(2,2), col = c('blue', 'red'))

summary(prlog)
summary(bglog)


# Download and clean temperature data Prairie Ridge (Reedy Creek Weather Station)
prtemp1 = read.csv('pr_temp.csv') # Data retrieved from the past 180 days on Sept. 1, 2015, does not include Sept. 1 
prtemp1$julianday = c(64:243) # Add Julian Day for date conversion, do days match up?
prtemp1$date = as.Date(prtemp1$julianday, origin = '2014-12-31') # JD 1 is Jan. 1, 2015
prtemp1$avgmaxmin = (prtemp$maxtemp + prtemp$mintemp)/2
prtemp = prtemp1[,c(10,9,3,6,2,11)]
names(prtemp) = c('date', 'julianday', 'maxtemp', 'mintemp', 'avgtemp', 'avgmaxmin')

# Download and clean temperature data NC Botanical Garden
bgtemp1 = read.table('ncbg_temp.txt')
bgtemp2 = bgtemp1[,c(1, 4, 5)]
names(bgtemp2) = c('date', 'hitemp', 'lowtemp') # Each date has 23 temp values, one each hour except midnight
# For loops for determining highest of max values each day and lowest of min values each day:
datelist = unique(bgtemp2$date)
maxvec = vector(length = length(datelist))
for (i in 1:length(datelist)) {maxvec[i] = max(as.numeric(bgtemp2$hitemp[bgtemp2$date == datelist[i]]))}
minvec = vector(length = length(datelist))
for (i in 1:length(datelist)) {minvec[i] = min(as.numeric(bgtemp2$lowtemp[bgtemp2$date == datelist[i]]))}
#################
bgtemp = data.frame(date = datelist, julianday = c(1:244), maxtemp = ((maxvec - 32)*5/9), 
                    mintemp = ((minvec - 32)*5/9))
bgtemp$avgmaxmin = (bgtemp$maxtemp + bgtemp$mintemp)/2
plot(bgtemp$julianday, bgtemp$maxtemp)
plot(bgtemp$julianday, bgtemp$mintemp)

# Calculating Growing Degree Days (GDD), only using BG data because starts Jan. 1
# (and is very similar to PR data anyway)
pregdd <- bgtemp$avgmaxmin
pregdd[pregdd < 10] = 10
pregdd[pregdd > 30] = 30 # need to do more research about thresholds
pregdd1 <- pregdd - 10
gdd <- cumsum(pregdd1)
GDD <- data.frame(bgtemp$julianday, gdd)
names(GDD) <- c('julianday', 'GDD')
plot(GDD$julianday, GDD$GDD)

# Plotting everything together
par(mar=c(5, 4, 4, 4) + 0.1)
plot(bgmean$julianday, bgmean$EVImean, xlab = "Julian Day", ylab = "Mean EVI",
     col = 'blue', type = 'l', lwd = 3)
points(prmean$julianday, prmean$EVImean, xlab = "Julian Day", ylab = "Mean EVI",
     col = 'red', type = 'l', lwd = 3)

# Adding temperature data
par(new = T)
plot(bgtemp$julianday, bgtemp$maxtemp, col = 'blue', axes = FALSE, bty = "n", 
     xlab = "", ylab = "", type = 'l', xlim = c(0, 244), ylim = c(-5, 40))
par(new = T)
plot(prtemp$julianday, prtemp$maxtemp, col = 'red', axes = FALSE, bty = "n", 
     xlab = "", ylab = "", type = 'l', xlim = c(0, 244), ylim = c(-5, 40))
axis(side=4)
mtext("Maximum Temperature (C)", side=4, las = 0, line = 3)
legend("topleft", c('BG mean EVI', 'PR mean EVI', 'BG max temp', 'PR max temp'),
       lwd = c(3,3,1,1), lty = c(1,1,1,1), col = c('blue', 'red', 'blue', 'red'))

# Adding arth data to graph, see summary_functions.r and arth_analyses.r to run functions.
# BG = Bot garden julian day and mean density
# PRam = Prairie Ridge julian day and mean density
par(new = T)
plot(BG$julianday, BG$meanDensity, col = 'blue', axes = FALSE, bty = "n", 
     xlab = "", ylab = "", type = 'l', xlim = c(0, 244), ylim = c(0.4, 1.4))
par(new = T)
plot(PRam$julianday, PRam$meanDensity, col = 'red', axes = FALSE, bty = "n", 
     xlab = "", ylab = "", type = 'l', xlim = c(0, 244), ylim = c(0.4, 1.4))
axis(side=4)
mtext("Selected Arthropod Mean Density", side=4, las = 0, line = 3)
legend("topleft", c('BG mean EVI', 'PR mean EVI', 'BG arth density', 'PR arth density'),
       lwd = c(3,3,1,1), lty = c(1,1,1,1), col = c('blue', 'red', 'blue', 'red'))

# Merging for writing data
bgtempmerge = bgtemp[c(1,2,5)]
names(bgtempmerge) = c('date', 'julianday', 'bgavgtemp')
bgtempmerge$bgavgtemp = round(bgtempmerge$bgavgtemp, digits = 2)
prtempmerge = prtemp[c(2,6)]
names(prtempmerge) = c('julianday', 'pravgtemp')
bgEVImerge = bgmean[c(1,2,4)]
names(bgEVImerge) = c('julianday', 'bgEVI', 'bgEVIlog')
prEVImerge = prmean[c(1,2,4)]
names(prEVImerge) = c('julianday', 'prEVI', 'prEVIlog')
mergetemp = merge(bgtempmerge, prtempmerge, by = 'julianday', all = T)
mergeEVI = merge(bgEVImerge, prEVImerge, by = 'julianday', all = T)
premerge = merge(mergetemp, mergeEVI, by = 'julianday', all = T)
temp_EVI_GDD = merge(premerge, GDD, by = 'julianday', all = T)
setwd('c:/git/caterpillars-count-analysis/data')
write.csv(temp_EVI_GDD, file = "temp_EVI_GDD.csv")

# Change date to date class
bands$bandingDate <- as.Date(bands$bandingDate, '%m/%d/%Y')

CRS("+proj=laea +lat_0=40 +lon_0=-100") # lambert azimuthal equal area
