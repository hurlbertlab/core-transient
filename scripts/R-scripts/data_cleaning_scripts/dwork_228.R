# Load reshape2 library:

library(reshape2)

# Set read and write directories:

in_dir = 'raw_datasets/dataset_228RAW'
out_dir = 'formatted_datasets'
  
# Get data:
# Note: each file represents one site from the Hubbard Brook study.

hb = read.csv(file.path(in_dir,'hb_bird.txt'))
mk = read.csv(file.path(in_dir,'mk_bird.txt'))
rp = read.csv(file.path(in_dir,'rp_bird.txt'))
sm = read.csv(file.path(in_dir,'sm_bird.txt'))

reshape.fun = function(sampling.site, site.code){
  # Remove X's from the year column:
    names(sampling.site) = gsub('X', '',names(sampling.site))
  # Melt from wide to long format:
    dClean = melt(sampling.site, id.vars = 'Bird.Species')
  # Set column names:
    names(dClean) = c('species','year','count')
  # Remove rows of summary data:
    dClean = dClean[dClean$species!='Total  (all Species)' &
                  dClean$species!= 'Number of Species',]
  # Replace "t" (trace?) values with 0's:
    dClean$count = factor(gsub('t', 0, dClean$count))
  # There is some r's in there connected with numbers, remove them:
    dClean$count = gsub('r', '', dClean$count)
  # Convert count and year data to numeric:
    dClean$count = as.numeric(dClean$count)
    dClean$year = as.numeric(levels(dClean$year))[as.integer(dClean$year)]
  # Add a site column (and arrange as the first column):
    site.name = paste('d228',site.code, sep ='_')
    dClean$site = factor(rep(site.name, length(dClean$year)))
    dClean = dClean[,c(4,1:3)]
  # Remove 0's:
    dClean = dClean[dClean$count>0,]
  # Remove NA's:
    dClean = na.omit(dClean)
    dClean
   }

d228 = rbind(reshape.fun(hb,'hb'),reshape.fun(mk,'mk'),
      reshape.fun(rp,'rp'),reshape.fun(sm,'sm'))

# Add a dataset ID column for matching with metadata

d228$datasetID = rep(228, length(d228[,1]))

# Rearrange the columns"

d228 = d228[,c(5,1:4)]

# Write to file:

write.csv(d228, file.path(out_dir,'dataset_228.csv'))

###################################################################################*
# ---- END DATA FORMATTING. START PROPOCC AND DATA SUMMARY ----
###################################################################################*
# We have now formatted the dataset to the finest possible spatial and temporal grain, removed bad species, and added the dataset ID. It's now to make some scale decisions and determine the proportional occupancies.

# Load additional required libraries and dataset:

library(dplyr)
library(tidyr)

datasetID = ds

# Get formatted dataset:

dataset = read.csv(paste("data/formatted_datasets/dataset_",
                         datasetID, ".csv", sep =''))

# Have a look at the dimensions of the dataset and number of sites:

dim(dataset)
length(unique(dataset$site))
length(unique(dataset$date))
head(dataset)

# Get the data formatting table for that dataset:

dataFormattingTable = subset(read.csv("data_formatting_table.csv"),
                             dataset_ID == datasetID)

# Check relevant table values:

dataFormattingTable$LatLong_sites

dataFormattingTable$spatial_scale_variable

dataFormattingTable$Raw_siteUnit

dataFormattingTable$subannualTgrain

# We'll start with the function "richnessYearSubsetFun". This will subset the data to sites with an adequate number of years of sampling and species richness. If there are no adequate years, the function will return a custom error message.

richnessYearsTest = richnessYearSubsetFun(dataset, spatialGrain = 'site_block_plot', 
                                          temporalGrain = 'year', 
                                          minNTime = 10, minSpRich = 10)

head(richnessYearsTest)
dim(richnessYearsTest) ; dim(dataset)
length(unique(richnessYearsTest$analysisSite))

# All looks okay, so we'll now get the subsetted data (w and z and sites with adequate richness and time samples):

subsettedData = subsetDataFun(dataset, datasetID, spatialGrain = 'site_block_plot', temporalGrain = 'year',
                              minNTime = 10, minSpRich = 10,
                              proportionalThreshold = .5)

# Take a look at the propOcc:

head(propOccFun(subsettedData))

hist(propOccFun(subsettedData)$propOcc)

# Take a look at the site summary frame:

siteSummaryFun(subsettedData)

# If everything looks good, write the files:

writePropOccSiteSummary(subsettedData)

# Remove all objects except for functions from the environment:

rm(list = setdiff(ls(), lsf.str()))

