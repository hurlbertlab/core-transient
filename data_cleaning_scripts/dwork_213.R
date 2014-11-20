# Dataset 232: Sevilleta small mammals

# Load libraries:

library(plyr)

# Set read and write directories:

in_dir = 'raw_datasets'
out_dir = 'formatted_datasets'

# Get data:

d213 = read.csv(file.path(in_dir,'dataset_213.csv'))

# Year and plot data are in one string. Extract year as the last two characters:
# Note: The function below is necessary because nchar is not equivalent across
# plot years.

substrRight <- function(x, n){
  x = as.character(x)
  substr(x, nchar(x[1])-n+1, nchar(x[1]))
}

d213$year = as.numeric(paste('19',substrRight(d213$plotyear, 2), sep =''))

# Extract plot data (code removes the last two (year) characters):

d213$plot = factor(substr(as.character(d213$plotyear), 1, nchar(as.character(d213$plotyear))-2))

# Can't count "Bare ground" as a species ... remove it:

d213 = d213[d213$sp!='Bare ground',]

# Remove "Short grass" as as species:

d213 = d213[d213$species!='Short grass',]

# Create a data frame of the count of individuals for a given 
# location and sampling event (note, count column is basal area):

d213.1 = ddply(d213, .(plot,species,year), 
               summarise, count = max(area))

# Add a dataset ID column for matching with metadata

d213.1$datasetID = rep(213,1, length(d213.1[,1]))

# Rearrange the columns"

d213.1 = d213.1[,c(5,1:4)]

# Make names equivalent to other datasets:

names(d213.1) = c('datasetID','site','species','year','count')


# Write to file:

write.csv(d213.1, file.path(out_dir,'dataset_213.csv'), row.names = F)

# Remove objects from the global environment

rm(list = ls())

