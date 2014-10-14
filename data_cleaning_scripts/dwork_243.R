# Dataset 243: Adult mid-water fish in kelp forest surrounding San Nicolas Island

# Set read and write directories:

in_dir = 'raw_datasets'
out_dir = 'formatted_datasets'

d243 = read.csv(file.path(in_dir,'dataset_243.csv'))

# Convert the date column to an R formatted date:

d243$record_date = strptime(d243$record_date, '%m/ %d/ %y')

# Add a sampling year line (summarize by year):

d243$year = as.numeric(format(d243$record_date, '%Y'))

# Calculate the maximum density for a given year,species, and site:

d243.1 = aggregate(adultdensity~station + speciescode + year, d243, max)

# Remove density values of 0:

d243.1 = d243.1[d243.1$adultdensity > 0,]

# Add datasetID column:

d243.1$datasetID = rep(243, length(d243.1[,1]))

# Include dataset name in station column:

d243.1$station = paste('d243',d243.1$station, sep ='_')

# Change order of fields

d243.1 = d243.1[,c(5,1:4)]

# Rename site and species fields:

names(d243.1)[2:3] = c('site','species')

# Write to csv:

write.csv(d243.1, file.path(out_dir,'dataset_243.csv'), row.names = F)

# Remove objects from the global environment

rm(list = ls())

