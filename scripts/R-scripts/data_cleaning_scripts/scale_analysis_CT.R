#  scale analysis function

# Load libraries:

library(stringr)
library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(MASS)
library(dplyr)
library(tidyr)
library(lme4)

# Source the functions file:
source('scripts/R-scripts/core-transient_functions.R')

getwd()
# Set your working directory to be in the home of the core-transient repository
# e.g., setwd('C:/git/core-transient')
# Min number of time samples required 
minNTime = 6

# Min number of species required
minSpRich = 10

# Ultimately, the largest number of spatial and 
# temporal subsamples will be chosen to characterize
# an assemblage such that at least this fraction
# of site-years will be represented.
topFractionSites = 0.5



dataformattingtable = read.csv('data_formatting_table.csv', header = T) 

datasetIDs = filter(dataformattingtable, spatial_scale_variable == 'Y',
                    format_flag == 1)$dataset_ID
datasetIDs = datasetIDs[datasetIDs  != 317] #dropped 317 bc ended up only being one spatial grain

summ = read.csv('output/tabular_data/core-transient_summary.csv', header=T)

grainlevels = c()
#function(datasetID, dataDescription) {
for(datasetID in datasetIDs){
  
  print(datasetID)
  
  dataset7 = read.csv(paste('data/formatted_datasets/dataset_', datasetID, '.csv', sep = ''))
  dataDescription = subset(read.csv("data_formatting_table.csv"),dataset_ID == datasetID)
  maxGrain = 1 ### this needs help.
  spatialgrains = dataDescription$Raw_siteUnit
  spatialgrains = as.character(spatialgrains)
  spatialgrains = unlist(strsplit(spatialgrains, '_'))
  spatialgrains = spatialgrains[length(spatialgrains):1] #reversing order to be from small to large not working
  #spatialgrains = c(spatialgrains, maxGrain)
  spatialgrain = c()
  grainLevel = 1
  for (sg in spatialgrains) {
    spatialgrain = paste(sg, spatialgrain, sep = "_")
    if (substr(spatialgrain, nchar(spatialgrain), nchar(spatialgrain)) == "_") {
      sGrain = substring(spatialgrain, 1, nchar(spatialgrain)-1)  
    } else {
      sGrain = spatialgrain
    }
    
    print(sGrain)

    tGrain = "year"
    if (nchar(as.character(dataset7$date[1])) > 4){ 
      dataset7$date = as.POSIXct(strptime(as.character(dataset7$date), format = "%Y-%m-%d"))
    }
  
    richnessYearsTest = richnessYearSubsetFun(dataset7, spatialGrain = sGrain, 
                                              temporalGrain = tGrain, 
                                              minNTime = minNTime, 
                                              minSpRich = minSpRich,
                                              dataDescription)
    if(class(richnessYearsTest) == "character"){
      goodSites = 0
    }else
    goodSites <- unique(richnessYearsTest$analysisSite)

    uniqueSites = unique(dataset7$site)
    fullGoodSites = c()
    for (s in goodSites) {
      tmp = as.character(uniqueSites[grepl(paste(s, "_", sep = ""), paste(uniqueSites, "_", sep = ""))])
      fullGoodSites = c(fullGoodSites, tmp)
    }
    
    dataset8 = subset(dataset7, site %in% fullGoodSites)
    
   if(goodSites == 0){
      subsettedData = dataset7
    }else
    subsettedData = subsetDataFun(dataset8, 
                                  datasetID, spatialGrain = sGrain, 
                                  temporalGrain = tGrain,
                                  minNTime = minNTime, minSpRich = minSpRich,
                                  proportionalThreshold = topFractionSites,
                                  dataDescription)

    writePropOccSiteSummary(subsettedData, spatialGrainAnalysis = TRUE, grainLevel = grainLevel)
    
    grainLevel = grainLevel + 1
    print(grainLevel)
    
    } # end of spatial grain loop
  grainlevels = rbind(grainlevels, c(datasetID, grainLevel-1))
} # end dataset loop
grainlevels = data.frame(grainlevels)
colnames(grainlevels) = c("datasetID", "NumGrains")
write.csv(grainlevels, "output/grainlevels.csv", row.names=FALSE)

# Merge all output files into 1 file
#grainlevels = read.csv("output/grainlevels.csv", header = TRUE)
files = list.files("data/spatialGrainAnalysis/propOcc_datasets")
bigfile = c()
#scale = c()
for(file in files){
  nfile= read.csv(paste("data/spatialGrainAnalysis/propOcc_datasets/", file, sep = ""))
  scale = substring(file, 18,last = 18)
  bigfile = rbind(bigfile, nfile)
  #scale=rbind(scale, unique(bigfile$datasetID))
}
bigfile=data.frame(bigfile)
#scale = data.frame(scale)

bigfile_taxa = merge(bigfile, dataformattingtable[,c('dataset_ID', 'taxa')], by.x = 'datasetID', by.y = "dataset_ID")
#biggile_scale= merge(bigfile, dataformattingtable[,c('dataset_ID', 'taxa')], )
  
write.csv(bigfile_taxa, "output/propOcc_w_taxa.csv", row.names=FALSE)

propOcc_w_taxa = read.csv("output/propOcc_w_taxa.csv", header = TRUE) # read in file if not running whole code

# rbind site_summary files
summfiles = list.files("data/spatialGrainAnalysis/siteSummaries")
allsummaries = c()
for(file in summfiles){
  nfile= read.csv(paste("data/spatialGrainAnalysis/siteSummaries/", file, sep = ""))
  nfile$scale = as.numeric(substring(file, 22,last = 22))
  nfile$site = as.factor(nfile$site)
  allsummaries = rbind(allsummaries, nfile)
}
allsummaries = data.frame(allsummaries)

# rbind propOcc files
propOccfiles = list.files("data/spatialGrainAnalysis/propOcc_datasets")
allpropOcc = c()
for(file in propOccfiles){
  nfile= read.csv(paste("data/spatialGrainAnalysis/propOcc_datasets/", file, sep = ""))
  nfile$scale = as.numeric(substring(file, 22,last = 22))
  nfile$site = as.factor(nfile$site)
  allpropOcc = rbind(allpropOcc, nfile)
}
allpropOcc = data.frame(allpropOcc)

# Summary statistics by datasetID/site, i.e. mean occupancy, % transient species (<=1/3)
summaries_taxa = merge(allsummaries, dataformattingtable[,c("dataset_ID","taxa","Raw_spatial_grain", "Raw_spatial_grain_unit")], by.x = 'datasetID', by.y = "dataset_ID")

write.csv(summaries_taxa, "output/summaries_grains_w_taxa.csv", row.names=FALSE)

summaries_taxa = read.csv("output/summaries_grains_w_taxa.csv", header = TRUE) # read in file if not running whole code

# merge in conversion table 
conversion_table = read.csv("output/conversion_table.csv", header =TRUE)

summaries_grains_w_taxa = merge(summaries_taxa, conversion_table, by.x = "Raw_spatial_grain_unit", by.y = "intl_units")

mean_occ_by_site = propOcc_w_taxa %>%
  group_by(datasetID, site) %>%
  dplyr::summarize(meanOcc = mean(propOcc), pctTrans = sum(propOcc <= 1/3)/n())

occ_taxa = merge(mean_occ_by_site, summaries_grains_w_taxa, by = c("datasetID", "site"))

# Calculating number of core, trans, and total spp for each dataset/site combo
propOcc_demog = merge(propOcc_w_taxa, occ_taxa, by =  c("datasetID", "site"))

propOcc_w_taxa$spptally = 1

totalspp = propOcc_w_taxa %>% group_by(datasetID, site) %>% tally(spptally)

numCT= propOcc_w_taxa %>% group_by(datasetID, site) %>%  dplyr::summarize(numTrans = sum(propOcc <= 1/3), numCore=sum(propOcc > 2/3))

spptotals = merge(totalspp, numCT, by= c("datasetID", "site"))
  
# for each dset - the propocc as response and the # of grain levels, community size, and random effect of taxa would be the predictor variables
mod1 = lmer(meanOcc ~ meanAbundance * (1|taxa), data=occ_taxa)
summary(mod1)
