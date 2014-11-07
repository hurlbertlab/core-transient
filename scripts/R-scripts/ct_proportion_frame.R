# This script calculates the proportional occupancy of species across time units
# in the study. The output is a dataframe that contains the proportional occupancy
# by species and site (name = prop.df) and a dataframe containing the number of 
# time units per site (name = Ntime.df). These dataframes will be sourced by other 
# scripts.

#----------------------------------------------------------------------------------*
# ---- Libraries ----
#==================================================================================*

library(plyr)

#----------------------------------------------------------------------------------*
# ---- Get data ----
#==================================================================================*

# Set read and write directories:

in_dir = 'formatted_datasets'

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
# ---- Calculate the number of time samples per site ----
#==================================================================================*

n.timeFun = function(dataset, site){
  d = d[d$datasetID == dataset & d$site == site,] # Subsets data by dataset & site
  years = length(unique(d$year))
  data.frame(dataset, site = site, nt = years)
} 

#----------------------------------------------------------------------------------*
# ---- Function to create proportion of occurences species and time data frame ----
#==================================================================================*

prop.t.fun = function(dataset, site){
  d = d[d$datasetID == dataset & d$site == site,] # Subsets data by dataset & site
  sp = unique(d$species)        # Generates a species list
  n.t = n.timeFun(dataset, site)[,3]
  # For loop to calculate the proportion of years a species has been observed:
  occ = numeric()
  for (i in 1:length(sp)){                        
    occ[i] = length(unique(d[d$species == sp[i],'year']))/n.t
  }
  prop.df = data.frame(dataset, site, sp,occ)  # Dataframe of species and proportion of years
  return(prop.df)
}

sites = unique(d$site)
dID = numeric()
props.df = list()  # dataframe of 
nTime.df = list()

for(i in 1:length(sites)){
  dID[i] = unique(d[d$site == sites[i],'datasetID'])
  props.df[[i]] = prop.t.fun(dID[i],sites[i])
  nTime.df[[i]] = n.timeFun(dID[i],sites[i])
}

# Turn lists into data frames:

prop.df = rbind.fill(props.df)
n.time =  rbind.fill(nTime.df)

# Write files

write.csv(prop.df, 'output/prop.df.csv', row.names = F)
write.csv(n.time, 'output/Ntime.df.csv', row.names = F)





