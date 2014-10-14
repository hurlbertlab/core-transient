# Dataset 252: Fish in a streams (Cowheeta)

# Load libraries:

library(plyr)

# Set read and write directories:

in_dir = 'raw_datasets'
out_dir = 'formatted_datasets'

d252 = read.csv(file.path(in_dir,'dataset_252.csv'))

# Data were collected every two months. Including month as a decimal to year

d252$year = d252$year + .1*d252$month

# NA's are coded as -888, remove these values (using species field)

d252 = d252[d252$species!='-888',]

# Because codes are used, it's safest to concatenate the order, family, genus, and species:

d252$species = paste(d252$sporder,d252$family,d252$genus,d252$species, sep ='-')

# Subset to the necessary columns and provide appropriate names:

d252 = d252[,c(5,11,2,12)]

names(d252) = c('site','species','year', 'count')

# Calculate the count by site, species, and year:

d252.1 = ddply(d252,.(site,species,year),
               summarise, count = sum(count))

# Add datasetID column:

d252.1$datasetID = rep(252, length(d252.1[,1]))

# Include dataset name in site column:

d252.1$site = paste('d252',d252.1$site, sep ='_')

# Change order of fields:

d252.1 = d252.1[,c(5,1:4)]

# Write to csv:

write.csv(d252.1, file.path(out_dir,'dataset_252.csv'), row.names = F)

# Remove objects from the global environment

rm(list = ls())

