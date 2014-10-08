# Dataset 246: Benthic fish in the kelp forests of the Channel Islands

# Set read and write directories:

in_dir = 'raw_datasets'
out_dir = 'formatted_datasets'

d246 = read.csv(file.path(in_dir,'dataset_246.csv'))

# Remove observations for which both the abundance and count columns are NA:

d246 = na.omit(d246)

# The species data unfortunately include age, requires splitting

species = strsplit(as.character(d246$scientificname), split = ',')

d246$species = do.call(rbind, species)[,1]

# To make things easier to see, I will re-arrange the columns now:

d246 = d246[,c(2,10,3,9)]

# For abundance frame, use the maximum count for a given year, site, and species:

d246.1 = aggregate(count~site + species + year, d246abund, max)

# Add datasetID column:

d246.1$datasetID = rep(246, length(d246.1[,1]))

# Include dataset name in site column:

d246.1$site = paste('d246',d246.1$site, sep ='_')

# Change order of fields

d246.1 = d246.1[,c(5,1:4)]

# Write to csv:

write.csv(d246.1, file.path(out_dir,'dataset_246.csv'), row.names = F)

# Remove objects from the global environment

rm(list = ls())

