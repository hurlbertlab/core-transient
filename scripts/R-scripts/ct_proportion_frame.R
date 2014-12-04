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

nTime.maker = function(datasets, i){
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

# ---- Write function ----
# Runs functions to create prop.df and nTime .csv files:

get.outsFun = function(datasets){
  require(plyr)
  # For loops run proportional and nTime functions:
    prop.list = list()  
    nTime.list = list() 
    for(i in 1:length(datasets)) prop.list[[i]] = prop.df.maker(datasets, i)  
    for(i in 1:length(datasets)) nTime.list[[i]] = nTime.maker(datasets, i) 
  # Turn lists into data frames:
    prop.df = rbind.fill(prop.list)
    nTime =  rbind.fill(nTime.list)
  # Return list with prop.df and nTime frames:
    out.list = list(prop.df, nTime)
    names(out.list) = c('prop.df','nTime')
    return(out.list)
}

# ---- Switch function ----
# Writes the proportional and nTime frames for data not already in the 
# prop.df file.

proc.newFun = function(){
  # Get the dataset names in the formatted files directory:
  datasets = list.files(in_dir, pattern="*.csv", full.names=T)
  # Get existing data:
  prop.df = read.csv('output/prop.df.csv')
  prop.df = prop.df[-1,]
  nTime = read.csv('output/nTime.df.csv')
  # Extract the dataset paths for the existing data:
  y0 = unique(as.character(prop.df$dataset))
  y = paste('formatted_datasets/dataset_',y0, '.csv', sep = '')
  # Find datasets not in the prop.df file:
    datasets = datasets[!datasets %in% y]
  # Return datasets to run or print up-to-date message:
    if (length(datasets[!datasets %in% y]) == 0) {
    print("Up-to-date")} else {
      # Get new processed data:
        new.dfs = get.outsFun(datasets)
      # Bind with previously processed data:
        prop.df = rbind(prop.df, new.dfs[[1]])
        nTime = rbind(nTime, new.dfs[[2]])
      # Write to file:
        write.csv(prop.df, 'output/prop.df.csv', row.names = F)
        write.csv(nTime, 'output/nTime.df.csv', row.names = F)
      # Return list with prop.df and nTime frames:
        out.list = list(prop.df, nTime)
        names(out.list) = c('prop.df','nTime')
        return(out.list)}
  }

#----------------------------------------------------------------------------------*
# ---- Make prop.df and nTime csv files ----
#==================================================================================*
# WARNING! This is if you are writing the prop.df and nTime files for the first 
# time only!

# Set read and write directories:

in_dir = 'formatted_datasets'

# Gather all files in directory:

datasets = list.files(in_dir, pattern="*.csv", full.names=T)

# !!! IF this is the first run (no existing prop.df or nTime frames, run writeFun).
# Be careful! This might take awhile!

outs = get.outsFun(datasets)

# Write files

write.csv(outs[[1]], 'output/prop.df.csv', row.names = F)
write.csv(outs[[2]], 'output/nTime.df.csv', row.names = F)

#----------------------------------------------------------------------------------*
# ---- Append prop.df and nTime csv files with new data ----
#==================================================================================*

outs = proc.newFun()


