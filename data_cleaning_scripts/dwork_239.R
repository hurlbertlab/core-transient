# Dataset 239: Phytoplankton data from cruises. 

# Add libraries:

library(plyr)
library(reshape)

# Set read and write directories:

in_dir = 'raw_datasets'
out_dir = 'formatted_datasets'

# These datasets include spatial sample data (t1) and collection data (t3)

d239t1 = read.csv(file.path(in_dir,'/dataset_239RAW/dataset_239_table1.csv'))
d239t3 = read.csv(file.path(in_dir,'/dataset_239RAW/dataset_239_table3.csv'))

# Change date to year:

year = as.numeric(format(as.Date(d239t1$Date, format = '%d-%m-%Y'), '%Y'))
d239t1[,3] = year

# Function to set the resolution for the creation of spatial sites:

res.set = function(resolution){
  Lat = round_any(d239t1$Lat, resolution)
  Lon = round_any(d239t1$Lon, resolution)
  d239t1$site = paste(Lat,Lon, sep = '')
  d239t1
}

# Testing mechanism ... in order to satisfy the # of years condition for the analysis
# we need to ensure that a given resolution yields a certain number of years (our limit is 5 years):

test.fun = function(resolution){
  t = res.set(resolution)
  num.years = function(x) length(unique(x))
  t1 = aggregate(Date~site, data = t, num.years)
  t1[t1[,2]>4,]
}

test.fun(1)

# If we explore a number of resolutions above, we can see that there is only one site in which
# the number of years is 5 or more for an incredible range of resolutions. This means that, for 
# our purpose, only site 50-4, at one degree resolution can truly be used.
# Note: despite previous concerns of samples from varying depths, it appears that all samples
# within this subset are from surface waters.

# Subset to only the spatial location with an adequate number of temporal samples.

d239t1 = res.set(1)

d239t1 = d239t1[d239t1$site == '50-4',]

# Subset the community data to only records from that location:

d239t3b = d239t3[d239t3$X  %in% d239t1$SampleID == T,]

# Switch from wide to long format:

# Melt from wide to long format:

dmelt = melt(d239t3b, id.vars = 'X')

# Remove zeros

d239 = dmelt[dmelt$value>0,]

# Add year by merging based on SampleID:

d239 = merge(d239, d239t1, by.x = 'X', by.y = 'SampleID', all = T)

# Create datasetID and site fields: 

datasetID = rep(239, length(d239[,1]))
site = rep('d239_1', length(d239[,1]))

# Remove columns and arrange/name in keeping with other datasets:

d239 = data.frame(datasetID, site, d239[,2],d239[,5],d239[,3])
colnames(d239) = c('datasetID','site','species','year','count')

# Write to file:

write.csv(d239, file.path(out_dir,'dataset_239.csv'), row.names = F)

# Remove objects from the global environment

rm(list = ls())
