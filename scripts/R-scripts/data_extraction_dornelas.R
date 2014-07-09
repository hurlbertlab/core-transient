# DATA EXTRACTION SCRIPT FOR DORNELAS ET AL. 2014
#
# GOAL: This script subsets the data provided by Maria Dornelas into studies that fit our 
# sampling criteria (>= 5 years and >= 10 species), excludes files that can be obtained using
# the EcoData Retriever, separates the datasets by study, and writes the files.

#=========================================================================================
# SET-UP
#-----------------------------------------------------------------------------------------

# Libraries:

library(plyr)

# Working directory:

setwd('/home/bsevans/Desktop')

# Get data:

t = read.csv('TSforAllen.csv')

#=========================================================================================
# SUBSET THE DATA
#-----------------------------------------------------------------------------------------

# Count the number of years and species for each study:

counts = ddply(t, c('ID'), function(t)c(length(unique(t$Year)), length(unique(t$Species))))

# Subset the counts file to those with >= 5 yrs and >= 10 species:

counts = counts[counts[,2]>4&counts[,3]>9,]

# Subset the Dornelas dataframe to only those files that fit the minimum sample criteria:

t2 = match_df(t,counts, on = 'ID')

# Remove files that can be obtained using Ecodata Retriever:

retrieverIDs = c(18,45,54,56,57,58,59,194)

t = t2[!t2$ID %in% retrieverIDs,]

#=========================================================================================
# WRITE THE DATA AS SEPARATE DATASETS
#-----------------------------------------------------------------------------------------
 
# Dataset writer function function:

ds.writer = function(id) {
  setwd('/home/bsevans/core-transient-datasets')
  ds = t[t$ID == id,]
  ds.name = paste('dataset_',id,'.csv',sep='')
  write.csv(ds, ds.name,row.names = F)
}

IDs = unique(t$ID)
for(i in 1:length(IDs)){
  dswriter(IDs[i])
}

