# Eastern Wood bird community data

# Load libraries:

  library(reshape)

# Set read and write directories:

in_dir = 'raw_datasets'
out_dir = 'formatted_datasets'

# Get data: 

d226 = read.csv(file.path(in_dir,'dataset_226.csv'))

# Melt from wide to long format:

dClean = melt(d226, id.vars = 'Species')

# Set column names:

names(dClean) = c('species','year','count')
  
# Remove X's from the year column and convert year to numeric:
  
dClean$year = as.numeric(gsub('X', '',dClean$year))
  
# Add a site column:

dClean$site = factor(rep('d226_ew', length(dClean$year)))

# Add a dataset ID column for matching with metadata
  
dClean$datasetID = rep(226, length(dClean[,1]))
    
# Rearrange the columns"
  
dClean = dClean[,c(5,4,1:3)]
  
# Remove NA's:

dClean = na.omit(dClean)

# Remove 0's:

d226 = dClean[dClean$count>0,]

# Not all species were samples in 1949, so these records need to be excluded:

d226 = d226[d226$year!=1949,]

# Write to the formatted data folder:

write.csv(d226, file.path(out_dir,'dataset_226.csv'), row.names = F)
  
# Remove objects from the global environment

rm(list = ls())
