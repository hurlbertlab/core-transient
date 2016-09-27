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

for(datasetID in datasetIDs){
  dataset7 = read.csv(paste('data/formatted_datasets/dataset_', datasetID, '.csv', sep = ''))
  #propOcc= read.csv(paste("data/spatialGrainAnalysis/propOcc_datasets/", file, sep = ""))
  #sitsum = 
  
  sites = unique(dataset7$site) 
  sitesplit = strsplit(as.character(sites), '_')
  sitesplit = data.frame(sitesplit)
  if(length(unique(unlist(sitesplit[1,]))>1)){
    #calc occ at coarse scale, rbind to propocc, set scale = to prev scale + 1
    # need to try with dataset other than 207!
    
    tGrain = 'year'
    
    dataset7$maxsite = 1
    sGrain = dataset7$maxsite
    
    richnessYearsTest = richnessYearSubsetFun(dataset7, spatialGrain = sGrain, 
                                              temporalGrain = tGrain, 
                                              minNTime = minNTime, 
                                              minSpRich = minSpRich,
                                              dataDescription)


    goodSites = unique(richnessYearsTest$analysisSite)
    length(goodSites)
    
    uniqueSites = unique(dataset7$site)
    fullGoodSites = c()
    for (s in goodSites) {
      tmp = as.character(uniqueSites[grepl(paste(s, "_", sep = ""), paste(uniqueSites, "_", sep = ""))])
      fullGoodSites = c(fullGoodSites, tmp)
    }
    
    dataset8 = subset(dataset7, site %in% fullGoodSites)

    subsettedData = subsetDataFun(dataset8, 
                                  datasetID, 
                                  spatialGrain = sGrain, 
                                  temporalGrain = tGrain,
                                  minNTime = minNTime, minSpRich = minSpRich,
                                  proportionalThreshold = topFractionSites,
                                  dataDescription)

    
    writePropOccSiteSummary(subsettedData, spatialGrainAnalysis = TRUE)
  }
}






