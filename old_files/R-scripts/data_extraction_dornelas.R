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

counts = ddply(t, 'ID', function(t)c(length(unique(t$Year)), length(unique(t$Species))))

# Subset the counts file to those with >= 5 yrs and >= 10 species:

counts = counts[counts[,2]>4&counts[,3]>9,]

# Subset the Dornelas dataframe to only those files that fit the minimum sample criteria:

t2 = match_df(t,counts, on = 'ID')

# Remove files that can be obtained using Ecodata Retriever:

retrieverIDs = c(18,44,45,54,56,57,58,59,194)

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

#=========================================================================================
# MAKE SITE DATA
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# Lats and Lons
#-----------------------------------------------------------------------------------------
# Lats and lons for Dornelas datasets are embedded in some of the sample ID's. I need to 
# determine which datasets contain LL's and extract them.

# Get just the ID and Sample I columns

sites = t[,c(2,4)]

# Get the unique sampling locations (i.e., remove redundancies)

sites = unique(sites)

# Return the first row and ID, and SampleID columns only for
# each dataset:

sites.check.ll = aggregate(t$SampleID, by = list(t$ID), function(x) x[1])
names(sites.check.ll) = c('ID','SampleID')

# The "by-hand" part. Check to see which datasets have lat-lons (take notes!)

sites.check.ll

# Make a list of the the IDs without latlon info:

no.ll = c(33,39,41,42,44,46,47,60,69,70,71,72,78,85,86,99,110,112,128,143,164,171,173,
          195,196,197,198,200)

# Split the datasets into those that have and those that don't have lat-lons:

sites.ll = sites[!sites$ID %in% no.ll,]
sites.no.ll = sites[sites$ID %in% no.ll,]

# Split the SampleID column to extracts lats and lons:

outsplit = strsplit(as.character(sites.ll$SampleID),'\\_')

# Extract lats and lons and add to the table:
# Warning, this for loop takes a while to run!


lat = numeric()
lon = numeric()
for (i in 1:length(outsplit)){
  lat[i] = as.numeric(tail(outsplit[[i]], n = 2)[1])
  lon[i] = as.numeric(tail(outsplit[[i]], n = 1))
}

sites.ll$lat = lat
sites.ll$lon = lon


#=========================================================================================
# WRITE THE DATA AS SEPARATE DATASETS
#-----------------------------------------------------------------------------------------
 
head(sites.ll)
