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


#function(datasetID, dataDescription) {
for(datasetID in datasetIDs){

  print(datasetID)

  dataset7 = read.csv(paste('data/formatted_datasets/dataset_', datasetID, '.csv', sep = ''))
  dataDescription = subset(read.csv("data_formatting_table.csv"),dataset_ID == datasetID)
  spatialgrains = dataDescription$Raw_siteUnit
  spatialgrains = as.character(spatialgrains)
  spatialgrains = unlist(strsplit(spatialgrains, '_'))
  spatialgrain = c()
  for (sg in spatialgrains) {
    spatialgrain = c(spatialgrain, paste(spatialgrain, sg, sep = "_"))
    spatialgrains = substring(spatialgrain, 2)
    print(spatialgrains)
    sGrain = sg
    tGrain = "year"
    if (nchar(as.character(dataset7$date[1])) > 4){ ###### ISSUE
      dataset7$date = as.POSIXct(strptime(as.character(dataset7$date), format = "%Y-%m-%d"))
    }
  }
  ## dataset210 is erroring bc one of the sub-spatial grains is erroring at getNestedSiteDataset (plot)
  # need tryCatch to cycle through only viable sGrains
  #tyCatch
  #richTest = tryCatch({  
  richnessYearsTest = richnessYearSubsetFun(dataset7, spatialGrain = sGrain, 
                                            temporalGrain = tGrain, 
                                            minNTime = minNTime, 
                                            minSpRich = minSpRich,
                                            dataDescription)
    
 # error=function(cond){
   # message(paste("Error in richnessYearsTest$analysisSite : 
  #$ operator is invalid for atomic vectors"))
    
 
  #if (richnessYearsTest == 'No acceptable sites, rethink site definitions or temporal scale'){ 
  #  goodSites <- NA
 
  
  # else 
   goodSites <- unique(richnessYearsTest$analysisSite)
   # print(length(goodSites))
 # if (length(goodSites) == 0){ 
  #  goodSites <- NA
 # } 

  #else goodSites <- unique(richnessYearsTest$analysisSite)
    
    if (!is.na(goodSites)){
    uniqueSites = unique(dataset7$site)
    
    }
    fullGoodSites = c()
    for (s in goodSites) {
      tmp = as.character(uniqueSites[grepl(paste(s, "_", sep = ""), paste(uniqueSites, "_", sep = ""))])
      fullGoodSites = c(fullGoodSites, tmp)
    }
    
    dataset8 = subset(dataset7, site %in% fullGoodSites)
    
    subsettedData = subsetDataFun(dataset8, 
                                  datasetID, spatialGrain = sGrain, 
                                  temporalGrain = tGrain,
                                  minNTime = minNTime, minSpRich = minSpRich,
                                  proportionalThreshold = topFractionSites,
                                  dataDescription)
    
    # Output will get written to spatialGrainAnalysis folder
    writePropOccSiteSummary(subsettedData, spatialGrainAnalysis = TRUE)
    
    # save datasetID, s, length(goodSites)
    #goodsites = data.frame(datasetID, sGrain, length(goodSites))
    scale_df = rbind(goodsites, data.frame(datasetID, sGrain, length(goodSites)))
    }
    else 
    # if it doesn't work (i.e. error, no good sites):
    
    # save dataset ID, NA, NA
    scale_df = data.frame(datasetID, NA, NA)
    write.csv(scale_df, paste("data/spatialGrainAnalysis/siteSummaries/dataset_", datasetID, sGrain,'.csv', sep = ''))
    # END tryCatch
})

### need the trycatch for a dataset that doesnt work
  richnessTest = tryCatch( 
    
    {
      suppressWarnings(fitdistr(richnessYearsTest, "beta", list(shape1 = 2, shape2 = 2), lower = c(1e-10, 1e-10))) 
    },
    error = function(cond) {
      message(paste("Error in fitdistr; trying new starting values")) 
      tryCatch(
        {
          suppressWarnings(fitdistr(occs, "beta", list(shape1 = 3, shape2 = 3), lower = c(1e-10, 1e-10))) 
        },
        error = function(cond) {
          list(estimate = c(NA, NA)) 
        },
        warning = function(cond) {
          message()
        })
    },
    warning = function(cond) {
      message(cond) 
    }
  )
  
    if (a == 'warning') {
      return_value <- 'myDivide warning result'
      warning("myDivide warning message")
    } else if (a == 'error') {
      return_value <- 'myDivide error result'
      stop("myDivide error message")
    } else {
      return_value = d / as.numeric(a)
    }
    return(return_value)
  }
  else return(datadescription$Raw_spatial_grain)
  }}

# return data frame w occ data at that scale
write.csv(scale_analysis, "scale_analysis.csv", row.names = FALSE)




