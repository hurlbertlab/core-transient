# Dataset 250: Fish in a streams (Cowheeta)

# Load libraries:

library(plyr)

# Set read and write directories:

in_dir = 'raw_datasets'
out_dir = 'formatted_datasets'

d250 = read.csv(file.path(in_dir,'dataset_250.csv'))

# Data were sorted into fall and spring, so I will use this as the date subset (spring = .1, fall = .2)

season = .1*ifelse(d250$month<8, 1, 2)

# Add season to the year column:

d250$year = d250$year + season

# Subset to the necessary columns and provide appropriate names:

d250 = d250[,c(6,5,2)]

names(d250) = c('site','species','year')

# Calculate the count by site, species, and year:

d250.1 = ddply(d250,.(site,species,year),
               summarise, count = length(species))

# Add datasetID column:

d250.1$datasetID = rep(250, length(d250.1[,1]))

# Include dataset name in site column:

d250.1$site = paste('d250',d250.1$site, sep ='_')

# Change order of fields:

d250.1 = d250.1[,c(5,1:4)]

# Write to csv:

write.csv(d250.1, file.path(out_dir,'dataset_250.csv'), row.names = F)

# Remove objects from the global environment

rm(list = ls())

