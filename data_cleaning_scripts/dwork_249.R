# Dataset 249: Fish in a lake

# Set read and write directories:

in_dir = 'raw_datasets'
out_dir = 'formatted_datasets'

d249 = read.csv(file.path(in_dir,'dataset_249.csv'))

# For each, I will calculate the capture by effort:

d249$count = d249$total_caught/d249$effort

# Use the maximum count for a given year, site, and species:

d249.1 = aggregate(count~lakeid + spname + year4, d249, max)

# Add datasetID column:

d249.1$datasetID = rep(249, length(d249.1[,1]))

# Include dataset name in site column:

d249.1$site = paste('d249',d249.1$lakeid, sep ='_')

# Reduce frame to necessary fields and change order of fields:

d249.1 = d249.1[,c(5:6,2:4)]

# Change column names to standard format:

names(d249.1)[3:4] = c('species','year')

# Write to csv:

write.csv(d249.1, file.path(out_dir,'dataset_249.csv'), row.names = F)

# Remove objects from the global environment

rm(list = ls())

