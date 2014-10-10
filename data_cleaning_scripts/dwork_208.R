# Dataset 208: Insect data from Kellogg Biological Station sticky traps

# Add libraries:

library(plyr)

# Set read and write directories:

in_dir = 'raw_datasets'
out_dir = 'formatted_datasets'

# Get data:

d208 = read.csv(file.path(in_dir,'dataset_208.csv'))

# Remove first row (empty):

d208 = d208[-1,]

# Make "decimal" year (compile montly samples)

year = as.numeric(format(as.Date(d208$sample_record_date, format = '%Y-%m-%d'), '%Y'))

month = as.numeric(format(as.Date(d208$sample_record_date, format = '%Y-%m-%d'), '%m'))

year = year + .1*month

# Make site field by concatenating treatment, replicate, and station fields:

site = paste(d208$treatment, d208$replicate, d208$station, sep = '-')

site = paste('dwork208', site, sep ='_')

# Make dataset ID field:

datasetID = rep(208, length(site))

d208 = data.frame(datasetID, site, d208$species, year, d208$adults)

names(d208)[c(3,5)] = c('species','count')

# Remove 0's

d208 = d208[d208$count>0,]

