# Dataset 232: Sevilleta small mammals

# Download libraries:

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
