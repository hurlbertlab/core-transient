# This script calculates the proportional occupancy of species across time units
# in the study. The output is a dataframe that contains the proportional occupancy
# by species and site (name = occProp) and a dataframe containing the number of 
# time units per site (name = Ntime). These dataframes will be sourced by other 
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
  d = d[d$site == site,]        # Subset data to a given site
  sp = unique(d$species)        # Generates a species list
  n.t = length(unique(d$year))  # Number of years
  dataset = rep(unique(d$datasetID), length(sp))
  # For loop to calculate the proportion of years a species has been observed:
  occ = numeric()
  for (i in 1:length(sp)){                        
    occ[i] = length(unique(d[d$species == sp[i],'year']))/n.t
  }
  occProp = data.frame(dataset, site, sp,occ, row.names = NULL) 
  return(occProp)
}

# ---- Wrapper function to output occProp ----

occProp.maker = function(datasets, i){
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

nTime.maker = function(datasets, i){
  d = prep.fun(datasets, i)
  sites = unique(d$site)
  years = numeric()
  nTime.list = list()
  for(j in 1:length(sites)){
    years[j] =  length(unique(d[d$site == sites[j],4])) 
  } 
  return(data.frame(datasetID = rep(unique(d$datasetID), length(years)),
                    site = sites, nt = years))
  }

# ---- Write function ----
# Runs functions to create occProp and nTime .csv files:

get.outsFun = function(datasets){
  require(plyr)
  # For loops run proportional and nTime functions:
    prop.list = list()  
    nTime.list = list() 
    for(i in 1:length(datasets)) prop.list[[i]] = occProp.maker(datasets, i)  
    for(i in 1:length(datasets)) nTime.list[[i]] = nTime.maker(datasets, i) 
  # Turn lists into data frames:
    occProp = rbind.fill(prop.list)
    nTime =  rbind.fill(nTime.list)
  # Return list with occProp and nTime frames:
    out.list = list(occProp, nTime)
    names(out.list) = c('occProp','nTime')
    return(out.list)
}

# ---- Replace function ----

# Writes the proportional and nTime frames for data for datasets
# that are already in the proportional dataframe file.

proc.replaceFun = function(datasetID){
  # Get the dataset names in the formatted files directory:
    dataset = paste('formatted_datasets/dataset_',datasetID,'.csv', sep ='')
    data = read.csv(dataset)
  # Get existing data:
    occProp = read.csv('output/occProp.csv')
    nTime = read.csv('output/nTime.csv')
  # Remove the data for that datasetID:
    occProp = occProp[occProp$dataset!=datasetID,]
    nTime = nTime[nTime$datasetID!=datasetID,]
  # Return datasets to run or print up-to-date message:
      # Get new processed data:
      new.dfs = get.outsFun(dataset)
      # Bind with previously processed data:
      occProp = rbind(occProp, new.dfs[[1]])
      nTime = rbind(nTime, new.dfs[[2]])
      # Sort by dataset
      occProp = occProp[order(occProp$dataset),]
      nTime = nTime[order(nTime$datasetID),]
      # Write to file:
      write.csv(occProp, 'output/occProp.csv', row.names = F)
      write.csv(nTime, 'output/nTime.csv', row.names = F)
}

# ---- Switch function ----
# Writes the proportional and nTime frames for data not already in the 
# occProp file.

proc.newFun = function(){
  # Get the dataset names in the formatted files directory:
    in_dir = 'formatted_datasets'
    datasets = list.files(in_dir, pattern="*.csv", full.names=T)
  # Get existing data:
    occProp = read.csv('output/occProp.csv')
    nTime = read.csv('output/nTime.csv')
  # Extract the dataset paths for the existing data:
    y0 = unique(as.character(occProp$dataset))
    y = paste('formatted_datasets/dataset_',y0,'.csv', sep = '')
  # Find datasets not in the occProp file:
    datasets = datasets[!datasets %in% y]
  # Return datasets to run or print up-to-date message:
    if (length(datasets[!datasets %in% y]) == 0) {
    print("Up-to-date")} else {
      # Get new processed data:
        new.dfs = get.outsFun(datasets)
      # Bind with previously processed data:
        occProp = rbind(occProp, new.dfs[[1]])
        nTime = rbind(nTime, new.dfs[[2]])
      # Sort by dataset
        occProp = occProp[order(occProp$dataset),]
        nTime = nTime[order(nTime$datasetID),]
      # Write to file:
        write.csv(occProp, 'output/occProp.csv', row.names = F)
        write.csv(nTime, 'output/nTime.csv', row.names = F)
      }
}

#----------------------------------------------------------------------------------*
# ---- Make occProp and nTime csv files ----
#==================================================================================*
# THIS SECTION IS COMMENTED OUT SO THIS FILE CAN BE SOURCED WITHOUT RUNNING
# THESE LINES!!! This is to be run only if writing the occProp and nTime files for
# the first time!
# 
# # Set read and write directories:
# 
# in_dir = 'formatted_datasets'
# 
# # Gather all files in directory:
# 
# datasets = list.files(in_dir, pattern="*.csv", full.names=T)
# 
# outs = get.outsFun(datasets)
# 
# # Write files
# 
# write.csv(outs[[1]], 'output/occProp.csv', row.names = F)
# write.csv(outs[[2]], 'output/nTime.csv', row.names = F)

#----------------------------------------------------------------------------------*
# ---- Append occProp and nTime csv files with new data ----
#==================================================================================*

proc.newFun()

