# This script calculates the proportional occupancy of species across time units
# in the study. The output is a dataframe that contains the proportional occupancy
# by species and site (name = prop.df) and a dataframe containing the number of 
# time units per site (name = Ntime.df). These dataframes will be sourced by other 
# scripts.

library(plyr)

#----------------------------------------------------------------------------------*
# ---- FUNCTIONS ----
#==================================================================================*


# The following function and line of code will change field names from density
# to count to ensure equivalent names (for rbinding):

name.changer = function(x){
  if (names(x)[5] == 'density') names(x)[5] = 'count'
  x
}

# ---- Calculate the number of time samples per site ----

n.timeFun = function(d, site){
  d = d[d$site == site,] # Subsets data by site
  years = length(unique(d$year))
  dataset = rep(unique(d$datasetID), length(years))
  data.frame(dataset, site = site, nt = years)
} 

# ---- Function to create proportion of occurences species and time data frame ----

prop.t.fun = function(d, site){
  d = d[d$site == site,] # Subsets data by site
  sp = unique(d$species)        # Generates a species list
  dataset = rep(unique(d$datasetID), length(sp))
  n.t = n.timeFun(d, site)[,3]
  # For loop to calculate the proportion of years a species has been observed:
  occ = numeric()
  for (i in 1:length(sp)){                        
    occ[i] = length(unique(d[d$species == sp[i],'year']))/n.t
  }
  prop.df = data.frame(dataset, site, sp,occ)  # Dataframe of species and proportion of years
  return(prop.df)
}

# ---- Wrapper function to output data ----

data.prep.wrapper = function(i){
  d = read.csv(datasets[i])
  d = name.changer(d)
  sites = unique(d$site)
  dID = numeric()
  props.df = list()  
  nTime.df = list()
  for(j in 1:length(sites)){
    dID[j] = unique(d[d$site == sites[j],'datasetID'])
    props.df[[j]] = prop.t.fun(d, sites[j])
    nTime.df[[j]] = n.timeFun(d,sites[j])
  }
  rm(d)
  props.df = rbind.fill(props.df)
  nTime.df = rbind.fill(nTime.df)
  out.list = list(props.df, nTime.df)
  names(out.list) = c('prop.df', 'nTime.df')
  return(out.list)
}

test = data.prep.wrapper(1)

#----------------------------------------------------------------------------------*
# ---- Set-up ----
#==================================================================================*

# Libraries:

library(plyr)

# Set read and write directories:

in_dir = 'formatted_datasets'

# Gather all files in directory:

datasets = list.files(in_dir, pattern="*.csv", full.names=T)

out.list = list()  
for(i in 1:length(datasets)) out.list[[i]] = data.prep.wrapper(i)

prop.list = list()
n.time.list = list()

for(i in 1:length(datasets)) prop.list[[i]] = out.list[[i]][[1]]
for(i in 1:length(datasets)) n.time.list[[i]] = out.list[[i]][[2]]

# Turn lists into data frames:

prop.df = rbind.fill(prop.list)
n.time =  rbind.fill(n.time.list)

# Write files

write.csv(prop.df, 'output/prop.df.csv', row.names = F)
write.csv(n.time, 'output/Ntime.df.csv', row.names = F)