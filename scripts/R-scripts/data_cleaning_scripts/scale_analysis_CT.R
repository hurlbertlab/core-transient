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

summ = read.csv('output/tabular_data/core-transient_summary.csv', header=T)

grainlevels = c()
#function(datasetID, dataDescription) {
for(datasetID in datasetIDs){
  
  print(datasetID)
  
  dataset7 = read.csv(paste('data/formatted_datasets/dataset_', datasetID, '.csv', sep = ''))
  dataDescription = subset(read.csv("data_formatting_table.csv"),dataset_ID == datasetID)
  spatialgrains = dataDescription$Raw_siteUnit
  spatialgrains = as.character(spatialgrains)
  spatialgrains = unlist(strsplit(spatialgrains, '_'))
  spatialgrain = c()
  grainLevel = 1
  for (sg in spatialgrains) {
    spatialgrain = paste(spatialgrain, sg, sep = "_")
    sGrain = substring(spatialgrain, 2)
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
      subsettedData = dataset8
    }else
    subsettedData = subsetDataFun(dataset8, 
                                  datasetID, spatialGrain = sGrain, 
                                  temporalGrain = tGrain,
                                  minNTime = minNTime, minSpRich = minSpRich,
                                  proportionalThreshold = topFractionSites,
                                  dataDescription)

    if(goodSites == 0){
      subsettedData = dataset8
    }else
      writePropOccSiteSummary(subsettedData, spatialGrainAnalysis = TRUE, grainLevel = grainLevel)

    grainLevel = grainLevel + 1
    print(grainLevel)
    
    } # end of spatial grain loop
  grainlevels = rbind(grainlevels, c(datasetID, grainLevel))
} # end dataset loop
grainlevels = data.frame(grainlevels)
colnames(grainlevels) = c("datasetID", "NumGrains")
write.csv(grainlevels, "output/grainlevels.csv", row.names=FALSE)

# Merge all output files into 1 file
#grainlevels = read.csv("output/grainlevels.csv", header = TRUE)
files = list.files("data/spatialGrainAnalysis/propOcc_datasets")
bigfile = c()
for(file in files){
  nfile= read.csv(paste("data/spatialGrainAnalysis/propOcc_datasets/", file, sep = ""))
  bigfile = rbind(bigfile, nfile)
}
bigfile=data.frame(bigfile)

bigfile_taxa = merge(bigfile, dataformattingtable[,c('dataset_ID', 'taxa')], by.x = 'datasetID', by.y = "dataset_ID")

write.csv(bigfile_taxa, "output/all_grains_w_taxa.csv", row.names=FALSE)


