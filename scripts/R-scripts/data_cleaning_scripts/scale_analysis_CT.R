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
datasetIDs = dataformattingtable$dataset_ID[dataformattingtable$format_flag == 1]
summ = read.csv('output/tabular_data/core-transient_summary.csv', header=T)

function(datasetID, dataDescription) {
  dataset_ID = 274
  datasetID = 274
dataset7 = read.csv(paste('data/formatted_datasets/dataset_', datasetID, '.csv', sep = ''))

dataDescription = subset(read.csv("data_formatting_table.csv"),dataset_ID == datasetID)

if (as.character(spatial_scale_variable) == 'Y'){
  spatialgrains = dataDescription$Raw_siteUnit
  spatialgrains = as.character(spatialgrains)
  spatialgrains = unlist(strsplit(spatialgrains, '_'))
 
  
spatial_grain = c()
  
  for (s in spatialgrains) {
    sGrain = s
    print(sGrain)
    richnessYearsTest = richnessYearSubsetFun(dataset7, spatialGrain = sGrain, 
                                              temporalGrain = tGrain, 
                                              minNTime = minNTime, 
                                              minSpRich = minSpRich,
                                              dataDescription)
    
    spatial_grain = rbind(spatial_grain,richnessYearsTest)
  }
  spatial_grain = data.frame(spatial_grain)
  
  

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


###################################################################### TBD
#Number of unique sites meeting criteria
goodSites = unique(richnessYearsTest$analysisSite)
length(goodSites)

# Now subset dataset7 to just those goodSites as defined. This is tricky though
# because assuming Sgrain is not the finest resolution, we will need to use
# grep to match site names that begin with the string in goodSites.
# The reason to do this is that sites which don't meet the criteria (e.g. not
# enough years of data) may also have low sampling intensity that constrains
# the subsampling level of the well sampled sites.

uniqueSites = unique(dataset7$site)
fullGoodSites = c()
for (s in goodSites) {
  tmp = as.character(uniqueSites[grepl(paste(s, "_", sep = ""), paste(uniqueSites, "_", sep = ""))])
  fullGoodSites = c(fullGoodSites, tmp)
}

dataset8 = subset(dataset7, site %in% fullGoodSites)

# Once we've settled on spatial and temporal grains that pass our test above,
# we then need to 1) figure out what levels of spatial and temporal subsampling
# we should use to characterize that analysis grain, and 2) subset the
# formatted dataset down to that standardized level of subsampling.

# For example, if some sites had 20 spatial subsamples (e.g. quads) per year while
# others had only 16, or 10, we would identify the level of subsampling that 
# at least 'topFractionSites' of sites met (with a default of 50%). We would 
# discard "poorly subsampled" sites (based on this criterion) from further analysis. 
# For the "well-sampled" sites, the function below randomly samples the 
# appropriate number of subsamples for each year or site,
# and bases the characterization of the community in that site-year based on
# the aggregate of those standardized subsamples.

subsettedData = subsetDataFun(dataset8, 
                              datasetID, 
                              spatialGrain = sGrain, 
                              temporalGrain = tGrain,
                              minNTime = minNTime, minSpRich = minSpRich,
                              proportionalThreshold = topFractionSites,
                              dataDescription)
# Take a look at the propOcc:

head(propOccFun(subsettedData))

hist(propOccFun(subsettedData)$propOcc)

# Take a look at the site summary frame:

siteSummaryFun(subsettedData)

# If everything looks good, write the files:

writePropOccSiteSummary(subsettedData)

# Update Data Formatting Table with summary stats of the formatted,
# properly subsetted dataset
dataFormattingTable = dataFormattingTableUpdateFinished(datasetID, subsettedData)

# Add any final notes about the dataset that might be of interest:
dataFormattingTable[,'General_notes'] = 
  dataFormattingTableFieldUpdate(datasetID, 'General_notes', 
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 )

# And write the final data formatting table:

write.csv(dataFormattingTable, 'data_formatting_table.csv', row.names = F)

# Remove all objects except for functions from the environment:

rm(list = setdiff(ls(), lsf.str()))

