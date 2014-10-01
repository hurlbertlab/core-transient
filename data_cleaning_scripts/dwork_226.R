setwd('/Users/bsevans/Desktop/core-transient-datasets/raw_datasets')

d226 = read.csv('dataset_226.csv')

# Remove X's from the year column:
  names(d226) = gsub('X', '',names(d226))

# Melt from wide to long format:

  dClean = melt(d226, id.vars = 'Species')

# Set column names:

  names(dClean) = c('species','year','count')

# Convert year data to numeric:

  dClean$year = as.numeric(levels(dClean$year))[as.integer(dClean$year)]

# Add a site column (and arrange as the first column):

  dClean$site = factor(rep('d226_ew', length(dClean$year)))
  dClean = dClean[,c(4,1:3)]

# Remove NA's:

  dClean = na.omit(dClean)

# Remove 0's:

  d226 = dClean[dClean$count>0,]

# Write to the formatted data folder:

setwd('/Users/bsevans/Desktop/core-transient-datasets/formatted_datasets')

write.csv(d226, 'dataset_226.csv', row.names = F)
