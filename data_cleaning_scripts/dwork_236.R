# Dataset 236: Chile small mammals

# Add libraries:

library(plyr)

# Set read and write directories:

in_dir = 'raw_datasets'
out_dir = 'formatted_datasets'

d236 = read.csv(file.path(in_dir,'dataset_236.csv'))

# Extract time (year) from the date string:

d236$year = as.numeric(substr(as.character(d236$mo), 1, 4))

# Create a "site" column, the sites are the 20 experimental grids

site = paste('d236_',d236$gr, sep = '')

# Make initial frame (necessary columns, not summarized):

df1 = data.frame(site, species = d236$sp, year = d236$year)

# Create a data frame of the count of individuals for a given sampling event:

df2 = ddply(df1, .(site, species, year), 
            summarise, count = length(species))

# Add a dataset ID column:

d236 = cbind(datasetID = rep(236, length(df2[,1])), df2)

# Write to file:

write.csv(d236, file.path(out_dir,'dataset_236.csv'), row.names = F)

# Remove objects from the global environment

rm(list = ls())
