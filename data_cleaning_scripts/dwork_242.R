# Dataset 242: Adult benthic fish in kelp forest surrounding San Nicolas Island

# Set read and write directories:

in_dir = 'raw_datasets'
out_dir = 'formatted_datasets'

d242 = read.csv(file.path(in_dir,'dataset_242.csv'))

# Convert the date column to an R formatted date:

d242$record_date = strptime(d242$record_date, '%m/ %d/ %y')

# Add a sampling year line (summarize by year):

d242$year = as.numeric(format(d242$record_date, '%Y'))

# Calculate the maximum density for a given year,species, and site:

d242.1 = aggregate(adultdensity~station + speciescode + year, d242, max)

# Remove density values of 0:

d242.1 = d242.1[d242.1$adultdensity > 0,]

# Add datasetID column:

d242.1$datasetID = rep(242, length(d242.1[,1]))

# Include dataset name in station column:

d242.1$station = paste('d242',d242.1$station, sep ='_')

# Change order of fields

d242.1 = d242.1[,c(5,1:4)]

# Rename site and species fields:

names(d242.1)[c(2:3,5)] = c('site','species','count')

# Write to csv:

write.csv(d242.1, file.path(out_dir,'dataset_242.csv'), row.names = F)

# Remove objects from the global environment

rm(list = ls())

