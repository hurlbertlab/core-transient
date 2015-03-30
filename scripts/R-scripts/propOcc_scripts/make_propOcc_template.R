################################################################################*
#  TEMPLATE TO MAKE THE PROPOCC AND SUMMARY TABLES
################################################################################*

# Source the core-transient functions and load required libraries and dataset:

library(stringr)
library(plyr)

source('scripts/R-scripts/core-transient_functions.R')

dataset = read.csv("data/formatted_datasets/dataset_223.csv")

dataFormattingTable = read.csv("Reference/data_formatting_table.csv")

dataFormattingTable = subset(dataFormattingTable, dataset_ID == 223)

#===============================================================================*
# ---- MAKE PROPORTIONAL OCCUPANCY AND DATA SUMMARY FRAMES ----
#===============================================================================*
# We have now formatted the dataset to the finest possible spatial and temporal
# grain, removed bad species, and added the dataset ID. It's now to make some
# scale decisions and determine the proportional occupancies.

#-------------------------------------------------------------------------------*
# ---- SITE DATA ----
#===============================================================================*
# What is the appropriate sampling grain for sites? We'll explore the data formatting table to see if there are any clues

# How many sites are there?

length(unique(dataset$site))

# Could this dataset involve spatial sampling at a grain below that of the site?

dataFormattingTable$spatial_scale_variable

# If no, you can move to the next section. If yes, you need to determine whether site designations are lat-long or nested sampling groups.

dataFormattingTable$LatLong_sites

#-------------------------------------------------------------------------------*
# ---- SITE SCALE: NESTED SAMPLING GROUPS
#-------------------------------------------------------------------------------*

getNestedSiteDataset = function(dataset, i){
  siteTable = read.table(text = as.character(dataset$site), sep = '_', stringsAsFactors = F)
  siteDefinition = dataFormattingTable$Raw_siteUnit
  siteUnitTable = read.table(text = as.character(siteDefinition), sep = '_', stringsAsFactors = F)
  outList = list(length = ncol(siteTable))
  for (i in 1:ncol(siteTable)){
    siteUnit = paste(as.character(siteUnitTable[1,1:i]), collapse = '_')
    if (siteUnit == siteUnitTable[,1]) {
      site = data.frame(siteTable[,1])} else {
      site = data.frame(factor(apply(siteTable[,1:i], 1, paste, collapse = '_')))
    } 
    names(site) = siteUnit
    if(names(site) == 'site') names(site) = 'site1'
    outList[[i]] = site
  }
  siteFrame = do.call(cbind, outList)
  nestedSiteData = cbind(dataset, siteFrame)
  return(list(nestedSiteData, names(siteFrame)))
}

getNestedTimeDataset = function(dataset){
  if(dataFormattingTable$spatial_scale_variable == 'Y') {
    dataset = getNestedSiteDataset(dataset)[[1]]}
  nestedSiteDataset$date = as.POSIXct(strptime(dataset$date, '%Y-%m-%d'))
  day = as.numeric(strftime(nestedSiteDataset$date, format = '%j'))
  week = trunc(day/7)+1
  biweek = trunc(week/2)+1
  month = as.numeric(format(nestedSiteDataset$date, '%m'))
  bimonth = trunc(month/2)+1
  season = ifelse(day < 80 |day >= 356, 1,
                  ifelse(day >= 80 & day < 172, 2,
                  ifelse(day >= 172 & day < 266, 3, 4)))
  subYearList = list(week, biweek, month, bimonth,season)
  names(subYearList) = c('week','biweek','month','bimonth','season')
  outList = list(length = length(subYearList))
  for(i in 1:length(subYearList)){
    outFrame = data.frame(paste(nestedSiteDataset$year, subYearList[[i]], sep = '_'))
    names(outFrame)  = paste('year', names(subYearList)[i], sep = '_')
    outList[[i]] = outFrame
  }
  subYearFrame = do.call(cbind, outList)
  return(cbind(nestedSiteDataset, subYearFrame))
}

getNestedDataset = function(dataset){
  if(dataFormattingTable$subannualTgrain == 'Y'){
    dataset = getNestedTimeDataset(dataset)
  } else {if(dataFormattingTable$spatial_scale_variable == T &
               dataFormattingTable$LatLong_sites != 'Y'){
    dataset = getNestedSiteDataset(dataset)
  }}
  return(dataset)
}

histFun = function(dataset){
  nestedDataset = getNestedDataset(dataset)
  timeGrains = c('date','year_week','year_biweek','year_month','year_bimonth','year_season','year')
  spatialGrains = getNestedSiteDataset(dataset)[[2]]
  par(mar=c(2,2,2,2))
  par(mfrow = c(length(timeGrains), length(spatialGrains)))
for(i in 1:length(timeGrains)){
  for(j in 1:length(spatialGrains)){
    d1 = data.frame(nestedDataset[,spatialGrains[[j]]],nestedDataset[,timeGrains[i]])
    names(d1) = c('site','time')
    d2 = ddply(d1, .(site), summarize, length(unique(time)))[,2] 
    hist(d2, xlab = 'Sampling events', main = paste(spatialGrains[j],timeGrains[i], sep ='_'),
         cex.main = .75,cex.axis = .5, col = 'gray')
  }
}
}


pdf('output/plots/exploringSiteSelection/hist_dataset223.pdf')
histFun(dataset)
dev.off()

####################################################################################################

timeGrains = c('date','year_week','year_biweek','year_month','year_bimonth','year_season','year')
spatialGrains = getNestedSiteDataset(dataset)[[2]]
test = getNestedDataset(dataset)
par(mar=c(2,2,2,2))
par(mfrow = c(length(timeGrains), length(spatialGrains)))
for(i in 1:length(timeGrains)){
  for(j in 1:length(spatialGrains)){
    t3 = data.frame(test[,spatialGrains[[j]]],test[,timeGrains[i]])
    names(t3) = c('site','time')
    t4 = ddply(t3, .(site), summarize, length(unique(time)))[,2] 
#     hist(t4, xlab = 'Sampling events', main = paste(spatialGrains[j],timeGrains[i], sep ='_'),
#          cex.main = .75,cex.axis = .5, col = 'gray')
t4o = t4[order(t4)]
cdf = sapply(t4o, function(x) sum(x > t4o))
plot(t4o, cdf, type ='l', main = paste(spatialGrains[j],timeGrains[i], sep ='_'), cex.main = .75)
abline(v = 5, lty='dashed')
  }}

####################################################################################################



nestedDataset = getNestedDataset(dataset)
timeGrains = c('date','year_week','year_biweek','year_month','year_bimonth','year_season','year')
spatialGrains = getNestedSiteDataset(dataset)[[2]]

wzMaker = function(i, threshold){
    spatialGrain = spatialGrains[i]
    nestedDataset$siteGrain = nestedDataset[,spatialGrain]
    
  # Subset to sites with a high enough species richness and year samples:
  
    siteSr_nTime = ddply(nestedDataset, .(siteGrain), summarize,
                       sr = length(unique(species)), 
                       nTime = length(unique(year)))
  
    goodSites = subset(siteSr_nTime, sr >= 10 & siteSr_nTime$nTime >= 5)$siteGrain
  
    d1 = nestedDataset[nestedDataset$siteGrain %in% goodSites,]

  # Get data frame of the number of spatial and temporal samples by site and year:
  
    spaceTime = ddply(d1,.(siteGrain, year), summarize,
                spatialSubsamples = length(unique(site)),
                temporalSubsamples = length(unique(date)))
  
    spaceTime = na.omit(spaceTime)
  
  # Get the value for w threshold:
    w = seq(min(spaceTime$spatialSubsamples), max(spaceTime$spatialSubsamples, by  = 1))
    siteYears = nrow(spaceTime)
    wFrame = data.frame(w)
    for(i in 1:length(w)) {
      wFrame[i,2] = nrow(subset(spaceTime, spatialSubsamples  <= w[i]))
      wFrame[i,3] = wFrame[i,2]/siteYears
    }
    names(wFrame)[2:3] = c('siteYears','propW')
    w = wFrame[which.min(abs(wFrame[,'propW'] - threshold)), 'w']

  # Get the value for the z threshold:
    z = seq(min(spaceTime$temporalSubsamples), max(spaceTime$temporalSubsamples, by  = 1))
    zFrame = data.frame(z)
    for(i in 1:length(z)){
      zFrame[i,2] = nrow(subset(spaceTime, temporalSubsamples <=z[i]))
      zFrame[i,3] = zFrame[i,2]/siteYears
    }
    names(zFrame)[2:3] = c('siteYears','propZ')
    z = zFrame[which.min(abs(zFrame[,'propZ'] - threshold)), 'z']
  
  # Output:
  
    outList = list(spatialGrain,wFrame,zFrame, w, z, spaceTime)
    names(outList) = c('spatialGrain', 'wFrame', 'zFrame','w', 'z', 'spaceTimeSamples')
    return(outList)
}

wzList = list(length = length(spatialGrains))
for(i in 1:length(spatialGrains)) wzList[[i]] = wzMaker(i, .8)
names(wzList) = spatialGrains


exploreWZ = function(i){
  wzListSub = wzList[[i]]
  nSiteYears = nrow(wzListSub$spaceTimeSamples)
  nSites = length(unique(wzListSub$spaceTimeSamples$siteGrain))
  w = wzList[[i]]$w
  spaceTimeSubW = subset(wzListSub$spaceTimeSamples, spatialSubsamples >= w)
  nSitesW = length(unique(spaceTimeSubW$siteGrain))
  siteYearsW = nrow(spaceTimeSubW)
  
  z = wzListSub$z
  spaceTimeSubZ = subset(wzListSub$spaceTimeSamples, temporalSubsamples >= z)
  nSitesZ= length(unique(spaceTimeSubZ$siteGrain))
  siteYearsZ = nrow(spaceTimeSubZ)
  
  spaceTimeSubWZ = subset(wzListSub$spaceTimeSamples, 
                          temporalSubsamples >= z &
                          spatialSubsamples >= w)
  nSitesWZ= length(unique(spaceTimeSubWZ$siteGrain))
  siteYearsWZ = nrow(spaceTimeSubZ)
  
  outList = list(nSiteYears, nSites, w, nSitesW, 
              siteYearsW, z, nSitesZ, siteYearsZ,
              nSitesWZ, siteYearsWZ)
  names(outList) = c('nSiteYears', 'nSites', 'w', 'nSitesW', 
                     'siteYearsW', 'z', 'nSitesZ', 'siteYearsZ',
                     'nSitesWZ', 'siteYearsWZ')
  return(outList)
}

exploreWZ(1)

plotWZ = function(i){
  par(mfrow = c(1, 2))
  plot(propW~w, data = wzList[[i]]$wFrame, 
       type = 'l', lwd = 2,
       xlab = 'Spatial subsamples',
       ylab = 'Proportion of siteYears',
       main = spatialGrains[i])
    abline(h = .8, lty = 2)
  plot(propZ~z, data = wzList[[i]]$zFrame, 
       type = 'l', lwd = 2,
       xlab = 'Temporal subsamples',
       ylab = 'Proportion of siteYears',
       main = spatialGrains[i])
    abline(h = .8, lty = 2)
}

for (i in 1:5) plotWZ(i)



names(wzList) = spatialGrains
  
# Plots of the number of temporal subsamples by the number of spatial subsamples

pdf('output/plots/exploringSiteSelection/spaceTimeSubsamples_d223.pdf', onefile = T)

plot.new()
par(mar = c(5,4,4,2))
par(mfrow = c(3,2))

for(i in 1:length(wzList)){
  spaceTimeSubSamples = wzList[[i]]$spaceTimeSubsamples
  plot(spaceTimeSubSamples$spatialSubsamples, spaceTimeSubSamples$temporalSubsamples, 
       xlab = 'Spatial subsamples', ylab = 'Temporal subsamples',
       pch = 19, col = 'darkgrey', main = wzList[[i]]$spatialGrain)
}
dev.off()

# Plots of the number of temporal subsamples by the number of spatial subsamples

wzScatterplotter = function(i, threshold){
  wzList = wzList[[i]]
  spatialGrain = wzList[[1]]
  site_wz = wzList[[2]]
  wz = wzList[[3]]
  wz$propsYw = wz$siteYears_w/nrow(site_wz)
  wz$propsYz = wz$siteYears_z/nrow(site_wz)
  
#   pdf(paste('output/plots/exploringSiteSelection/wz_scatterplots_', spatialGrain, '.pdf'))
  par(mfrow = c(2,2))
  
  plot(propsYw ~ w,  data = wz[order(w),], 
       type = 'l', col = 1, lwd = 1.5, ylim = c(0,1),
       ylab = 'Proportion of siteYears >= w', main = spatialGrain)
    abline(h = .8, lty = 2)

  plot(propsYz ~ z,  data = wz, # wz[order(z),],
       type = 'l', col = 1, lwd = 1.5, ylim = c(0,1),
       ylab = 'Proportion of siteYears >= z', main = spatialGrain)
      abline(h = .8, lty = 2)

  plot(siteYears_w ~ w,  data = wz[order(w),],
       type = 'l', col = 1, lwd = 1.5, 
       ylab = 'Number of siteYears >= w', main = spatialGrain)
        abline(h = .8*nrow(site_wz), lty = 2)

  plot(siteYears_z ~ z,  data = wz, #wz[order(z),],
       type = 'l', col = 1, lwd = 1.5, 
       ylab = 'Number of siteYears >= z', main = spatialGrain)
        abline(h = .8*nrow(site_wz), lty = 2)
}

pdf('output/plots/exploringSiteSelection/wzScatterplots_d223.pdf', onefile = T)

for(i in 1:length(spatialGrains)) wzScatterplotter(i, .8)

dev.off()

wzContourPlotter = function(i, threshold){
  wzList = wzList[[i]]
  spatialGrain = wzList[[1]]
  site_wz = wzList[[2]]
  wz = wzList[[3]]  
  print(contourplot(wz$propSiteYears_wz~ wz$w*wz$z, data=wz,
              xlab = 'Number of spatial subsamples',
              ylab = 'Number of temporal subsamples',
              main = paste(spatialGrain,'\nnSiteYears at threshold = ',
                           wz[which.min(abs(wz[,'propSiteYears_wz'] - threshold)),'siteYears_wz'],
                           '\nnSites at threshold = ',
                           wz[which.min(abs(wz[,'propSiteYears_wz'] - threshold)),'sites_wz'],
                           '\nw (nSpatialSubsamples) =',
                           wz[which.min(abs(wz[,'propSiteYears_wz'] - threshold)),'w'],
                           ', z (nTemporalSubsamples) = ',
                           wz[which.min(abs(wz[,'propSiteYears_wz'] - threshold)),'z'])))
}

pdf('output/plots/exploringSiteSelection/wz_contourplots_d223.pdf', onefile = T)

for(i in 1:length(spatialGrains)) wzContourPlotter(i, .8)

dev.off()




######################################################################

nestedSiteValidity = function(dataset, i){
  siteUnit = paste(as.character(siteUnitTable[1,1:i]), collapse = '_')
  dataset$year = getYear(dataset$date)
  if (siteUnit == siteUnitTable[,1]) {
    dataset$site = siteTable[,1]} else {
      dataset$site = factor(apply(siteTable[,1:i], 1, paste, collapse = '_'))
    } 
  siteSummary = ddply(dataset, .(site), summarize,
                      timeSamples = length(unique(year)), 
                      nSpecies = length(unique(species)))
  nSite = nrow(siteSummary)
  MEANnTime = mean(siteSummary$timeSamples)
  MINnTime = min(siteSummary$timeSamples)
  MAXnTime = max(siteSummary$timeSamples)
  STDEVnTime = sd(siteSummary$timeSamples) 
  nBadSiteTime = nrow(subset(siteSummary, timeSamples < 5))
  nBadSiteSpecies = nrow(subset(siteSummary, nSpecies < 10))
  nBadSites = nrow(subset(siteSummary, timeSamples < 5 | nSpecies < 10))
  propBadSiteTime = nBadSiteTime/nRecs
  propBadSiteSpecies = nBadSiteSpecies/nRecs
  propBadSites = nBadSiteSpecies/nRecs
  return(data.frame(siteUnit, nSite, 
                    MEANnTime, MINnTime, MAXnTime, STDEVnTime,
                    nBadSiteTime,nBadSiteSpecies,nBadSites,
                    propBadSiteTime, propBadSiteSpecies, propBadSites))
}

# Function to calculate site validity across scales for nested sites:

nestedSiteValiditySummary = function(dataset){
  siteUnit = dataFormattingTable$Raw_siteUnit
  siteUnitTable = read.table(text = as.character(siteUnit), sep = '_', stringsAsFactors = F)
  siteTable = read.table(text = as.character(dataset$site), sep = '_', stringsAsFactors = F)
  outList = list(length = ncol(siteTable))
  for (i in 1:ncol(siteTable)){
    outList[[i]] = nestedSiteValidity(dataset, i)
  }
  return(rbind.fill(outList))
}



# A good first pass is to look at the number of years and species per site:

nestedSiteValiditySummary(dataset)

nestedSiteHist = function(dataset, i){
  siteUnit = paste(as.character(siteUnitTable[1,1:i]), collapse = '_')
  dataset$year = getYear(dataset$date)
  if (siteUnit == siteUnitTable[,1]) {
    dataset$site = siteTable[,1]} else {
      dataset$site = factor(apply(siteTable[,1:i], 1, paste, collapse = '_'))
    } 
  siteSummary = ddply(dataset, .(site), summarize,
                      timeSamples = length(unique(year)), 
                      nSpecies = length(unique(species)))
  hist(siteSummary$timeSamples, col = 'gray')
}

# Function to make the spatial grain more course for a dataset. 

dSafe = dataset

dataset = dSafe

rescaleNestedDataset = function(dataset, scale){
  siteUnit = paste(as.character(siteUnitTable[1,1:scale]), collapse = '_')
  if (siteUnit == siteUnitTable[,1]){
    dataset$site = factor(siteTable[,1]) } else {
      dataset$site = factor(apply(siteTable[,1:scale], 1, paste, collapse = '_'))
    }
  dataset = ddply(dataset, .(datasetID, site, date, species), summarize,
                  count = sum(count))
  return(dataset)
}

# Make sure ot have a sense of the site levels first!

head(dataset$site)

dataset1 = rescaleNestedDataset(dataset, 4)

# Now let's remove the sites with inadequate sample sites:

badSites = subset(ddply(dataset1, .(site), summarize,
                  timeSamples = length(unique(getYear(date))), 
                  nSpecies = length(unique(species))),
                  nSpecies < 10 | timeSamples < 5)$site

dataset2 = dataset1[!dataset1$site %in% badSites,]

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE ANY SPATIAL GRAIN DECISIONS!

# Note: In many instances, site definition will be spatially explicit (e.g., 
# lats and longs). When this is the case, we may need to summarize the data to
# a courser precision (few decimal places). We can do so by using the 
# "round_any" function in Hadley Wickham's plyr package, specifying "floor" 
# as the rounding function.

#-------------------------------------------------------------------------------*
# ---- TIME DATA ----
#===============================================================================*
# We start by extracting year from the dataset. Year will now be our DEFAULT
# temporal grain. Decisions for finer temporal grains may be decided at a 
# later date.

# Change date column to year:

dataset$date = getYear(dataset$date)

# Change column name:

names(dataset)[3] = 'year'


#-------------------------------------------------------------------------------*
# ---- WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*

# And make our proportional occurence data frame:

write.csv(propOccFun(dataset), "data/propOcc_datasets/propOcc_223.csv", row.names = F)

# !GIT-ADD-COMMIT-PUSH propOcc!

# And make and write site summary dataset:

write.csv(siteSummaryFun(dataset), 'data/siteSummaries/siteSummary_223.csv', row.names = F)

# Note: Both the submodule and core-transient folder need to be pushed to, 
# in git bash:

# cd data
# git add formatted_datasets/dataset_208.csv
# git commit -m "added formatted dataset"
# git push
# cd ..
# git add data
# git commit -m "updated submodule with formatted dataset 208"
# git push

#-------------------------------------------------------------------------------*
# ---- EXPLORE YOUR DATASET SUMMARY INFO AND UPDATE THE DATA SOURCE TABLE  ----
#===============================================================================*

# !!!At this point, go to the data source table and provide:
#   -central lat and lon (if available, if so, LatLonFLAG = 0, if you couldn't do
#    it, add a flag of 1)
#   -spatial_grain columns (T through W)
#   -nRecs, nSites, nTime, nSpecies
#   -temporal_grain columns (AH to AK)
#   -Start and end year
#   -Any necessary notes
#   -flag any issues and put issue on github
#   -git-add-commit-push data_source_table.csv

dim(dataset)

length(unique(dataset$site))

length(unique(dataset$year))

length(unique(dataset$species))
