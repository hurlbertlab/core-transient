# Dataset 232: Sevilleta small mammals

# Load libraries:

library(plyr)

# Set read and write directories:

in_dir = 'raw_datasets'
out_dir = 'formatted_datasets'

# Get data:

d232 = read.csv(file.path(in_dir,'dataset_232.csv'))

# Create a data frame of the count of individuals for a given 
# location and sampling event:

d232.1 = ddply(d232, .(location,year, season, night,species), 
  summarise, count = length(species))

# Create a data frame of the maximum count of individuals 
# for a given sampling event within a season.

d232.2 = ddply(d232.1,.(location,year,season,species),
               summarise, count = max(count))

# Combine the year and season columns:

year = as.numeric(paste(d232.2$year,d232.2$season, sep ='.'))

# Arrange the fields in the same order as other datasets:

d232 = data.frame(d232.2[,1],d232.2[,4],year,d232.2[,5])
names(d232) = c('site','species','year','count')

# Write to file:

write.csv(d232, file.path(out_dir,'dataset_232.csv'))

# Remove objects from the global environment

rm(list = ls())


