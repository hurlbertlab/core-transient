# This script calculates the proportional occupancy of species across years. It will
# be sourced by other scripts.

#----------------------------------------------------------------------------------*
# ---- Libraries ----
#==================================================================================*

library(plyr)

#----------------------------------------------------------------------------------*
# ---- Get data ----
#==================================================================================*

# Set read and write directories:

in_dir = 'formatted_datasets'
out_dir = 'output'

# Get summary table:

summary.table = read.csv('data_source_table.csv')

# Gather all files in directory:

datasets = list.files(in_dir, pattern="*.csv", full.names=T)
data.list = lapply(datasets, read.csv)

# The following function and line of code will change field names from density
# to count to ensure equivalent names (for rbinding):

name.changer = function(x){
  if (names(x)[5] == 'density') names(x)[5] = 'count'
  x
}

data.list = lapply(data.list, name.changer)

# Bind the list into a single dataframe that includes all datasets:

d = rbind.fill(data.list)

#----------------------------------------------------------------------------------*
# ---- Function create proportion of years by species data frame ----
#==================================================================================*

prop.by.year = function(dataset, site){
  d = d[d$datasetID == dataset & d$site == site,] # Subsets data by dataset & site
  sp = unique(d$species)        # Generates a species list
  years = sort(unique(d$year))
  yrs = length(years)  # Generates a list of years
  # For loop to calculate the proportion of years a species has been observed:
  prop.yrs = numeric()
  for (i in 1:length(sp)){                        
    prop.yrs[i] = length(unique(d[d$species == sp[i],'year']))/yrs
  }
  prop.df = data.frame(sp,prop.yrs)  # Dataframe of species and proportion of years
  list.out = list(prop.df,yrs)
  names(list.out) = c('prop.df','years')
  return(list.out)
}


