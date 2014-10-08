# Dataset 241: Benthic organisms in kelp forest surrounding San Nicolas Island

# Add libraries:

library(plyr)

# Set read and write directories:

in_dir = 'raw_datasets'
out_dir = 'formatted_datasets'

d241 = read.csv(file.path(in_dir,'dataset_241.csv'))

# Convert the date column to an R formatted date:

d241$record_date = strptime(d241$record_date, '%m/ %d/ %y')

# Add a sampling year line (summarize by year):

d241$year = as.numeric(format(d241$record_date, '%Y'))

# Calculate the maximum density for a given year,species, and site:

d241.1 = aggregate(density~station + speciescode + year, d241, max)

# Add datasetID column:

d241.1$datasetID = rep(241, length(d241.1[,1]))

# Include dataset name in station column:

d241.1$station = paste('d241',d241.1$station, sep ='_')

# Change order of fields

d241.1 = d241.1[,c(5,1:4)]

# Rename site and species fields:

names(d241.1)[2:3] = c('site','species')

# Write to csv:

write.csv(d241.1, file.path(out_dir,'dataset_241.csv'), row.names = F)

# Remove objects from the global environment

rm(list = ls())

