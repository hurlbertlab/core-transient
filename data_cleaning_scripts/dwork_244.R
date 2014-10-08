# Dataset 244: Benthic organisms in the kelp forests of the Channel Islands

# Set read and write directories:

in_dir = 'raw_datasets'
out_dir = 'formatted_datasets'

d244 = read.csv(file.path(in_dir,'dataset_244.csv'))

# Remove NA's

d244 = na.omit(d244)

# Calculate the maximum density for a given year,species, and site:

d244.1 = aggregate(densitymean~site + species + year, d244, max)

# Remove density values of 0:

d244.1 = d244.1[d244.1$densitymean > 0,]

# Add datasetID column:

d244.1$datasetID = rep(244, length(d244.1[,1]))

# Include dataset name in site column:

d244.1$site = paste('d244',d244.1$site, sep ='_')

# Change order of fields

d244.1 = d244.1[,c(5,1:4)]

# Rename site and species fields:

names(d244.1)[5] = 'density'

# Write to csv:

write.csv(d244.1, file.path(out_dir,'dataset_244.csv'), row.names = F)

# Remove objects from the global environment

rm(list = ls())

