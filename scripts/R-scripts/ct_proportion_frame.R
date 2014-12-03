# This script calculates the proportional occupancy of species across time units
# in the study. The output is a dataframe that contains the proportional occupancy
# by species and site (name = prop.df) and a dataframe containing the number of 
# time units per site (name = Ntime.df). These dataframes will be sourced by other 
# scripts.

#----------------------------------------------------------------------------------*
# ---- FUNCTIONS ----
#==================================================================================*

# ---- Function to prepare data for analysis ----

prep.fun = function(datasets, i){
    d = read.csv(datasets[i])
    if (names(d)[5] == 'density') names(d)[5] = 'count'
    d = d[d$count > 0,]
    return(d)
  }

# ---- Calculate proportion of occurences for a given species ----

occfun = function(sp) length(unique(d[d$species == sp,4]))/length(unique(d$year))


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
  prop.df = data.frame(dataset, site, sp,occ, row.names = NULL) 
  return(prop.df)
}

# ---- Wrapper function to output prop.df ----

prop.df.maker = function(datasets, i){
  d = prep.fun(datasets, i)
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
  d = prep.fun(datasets, i)
  sites = unique(d$site)
  years = numeric()
  nTime.list = list()
  for(j in 1:length(sites)){
    years[j] =  length(unique(d[d$site == sites[j],4])) 
  } 
  return(data.frame(datasetID = rep(unique(d$datasetID), length(years)),
                    site = sites, years))
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