#This script calculates occupancy distributions and core and occasional species richness
#for BBS route with continuous data from 1996 - 2010.  It averages across values calculated for all possible 
#windows of size t (from 1:15) within the date range

###################################################################################*
# ---- SET-UP ----
###################################################################################*

#----------------------------------------------------------------------------------*
# ---- Libraries ----
#==================================================================================*

library(plyr)

#----------------------------------------------------------------------------------*
# ---- Get data ----
#==================================================================================*

# Set read and write directories:

in_dir = 'formatted_datasets'

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
