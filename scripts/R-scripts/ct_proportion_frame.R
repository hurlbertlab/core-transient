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
  years = length(unique(d$year))
  dataset = rep(unique(d$datasetID), length(years))
  data.frame(dataset, site = site, nt = years)
} 

# ---- Function to create proportion of occurences species and time data frame ----

prop.t.fun = function(d, site){
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

# ---- Wrapper function to output prop.df ----

prop.df.maker = function(i){
  d = read.csv(datasets[i])
  d = name.changer(d)
  d = d[d$count > 0,]
  sites = unique(d$site)
  d1 = list()
  prop.list = list()  
  for(j in 1:length(sites)){
    d1[[j]] =  d[d$site == sites[j],]
    prop.list[[j]] = prop.t.fun(d1[[j]], sites[j])
  } 
  rm(list =  c('d','d1'))
  return(rbind.fill(prop.list))
}

# ---- Wrapper function to output nTime.df ----

nTime.maker = function(i){
  d = read.csv(datasets[i])
  d = name.changer(d)
  d = d[d$count > 0,]
  sites = unique(d$site)
  d1 = list()
  props.df = list()  
  nTime.list = list()
  for(j in 1:length(sites)){
    d1[[j]] =  d[d$site == sites[j],] 
    nTime.list[[j]] = n.timeFun(d1[[j]],sites[j])
  } 
  rm(list =  c('d','d1'))
  return(rbind.fill(nTime.list))
}

#----------------------------------------------------------------------------------*
# ---- Set-up ----
#==================================================================================*

# Libraries:

library(plyr)

# Set read and write directories:

in_dir = 'formatted_datasets'

# Gather all files in directory:

datasets = list.files(in_dir, pattern="*.csv", full.names=T)

prop.list = list()  
nTime.list = list()

for(i in 1:length(datasets)) prop.list[[i]] = prop.df.maker(i)

for(i in 1:length(datasets)) nTime.list[[i]] =nTime.maker(i)

# Turn lists into data frames:

prop.df = rbind.fill(prop.list)
nTime =  rbind.fill(nTime.list)

# Write files

write.csv(prop.df, 'output/prop.df.csv', row.names = F)
write.csv(n.time, 'output/nTime.df.csv', row.names = F)