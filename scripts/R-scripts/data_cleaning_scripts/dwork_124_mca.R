# Formatting Dataset 124: OBIS Marine Inverts

#-------------------------------------------------------------------------------*
# ---- SET-UP ----
#===============================================================================*

# Load libraries:

library(stringr)
library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(MASS)


# Source the functions file:

getwd()
setwd('C:/Users/auriemma//core-transient')
source('scripts/R-scripts/core-transient_functions.R')

# Get data. First specify the dataset number ('ds') you are working with.

ds = 124 

list.files('data/raw_datasets')

dataset = read.csv(paste('data/raw_datasets/dataset_', ds, '.csv', sep = ''))

dataFormattingTable = read.csv('Reference/data_formatting_table.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*

head(dataset)
str(dataset)
summary(dataset)
tail(dataset)

# Remove column called 'X'

dataset1 = dataset[,-1]
head(dataset1)

# Change ID column to 'datasetID'

names(dataset1)[1] = 'datasetID'

# Change name of site column

names(dataset1)[3] = 'site'

head(dataset1)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE! 
# Are the ONLY site identifiers the latitude and longitude of the observation or 
# sample? (I.e., there are no site names or site IDs or other designations) Y/N

dataFormattingTable[,'LatLong_sites'] = 
  dataFormattingTableFieldUpdate(ds, 'LatLong_sites','Y') 

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*
# Explore

head(dataset1)
length(unique(dataset1$site))
summary(dataset1)

# Check all sites

levels(dataset1$site)

# Sites are listed with a name (USA_Massachusetts or USA_Massachusetts_North_America_Atlantic) followed by a lat_long. Data with North_America_Atlantic also includes a number before the lat_long. Can get rid of the redundant 'USA_Massachusetts' so that at least some of the data will be just lat_longs.

dataset2 = dataset1
dataset2$site = str_sub(dataset2$site, start = 21)

# Check

head(dataset2,100)
unique(dataset2$site)

# All looks good; sites are now either just lat_long or read as 'rth_America_Atlantic_number_lat_long

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

# !DATA FORMATTING TABLE UPDATE! 

# Raw_siteUnit. 

dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(ds, 'Raw_siteUnit','lat_long or site number and lat_long') 

# spatial_scale_variable. Is a site potentially nested (e.g., plot within a quad or decimal lat longs that could be scaled up)? Y/N

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(ds, 'spatial_scale_variable', 'N') 

# Notes_siteFormat.

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_siteFormat', "site fields were cut down using substring to delete the redundant site name USA_Massachusetts which was present in every site. Remaining are the sites as just lat_longs and also sites with rth_America_Atlantic followed by possibly a site number, then a lat_long")

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*

# Explore

length(unique(dataset2$Species))
# 654 unique species accounted for

head(dataset2)

# Upper and lower case could present an issue, so make new species column with all uppercase

dataset2$species = toupper(dataset2$Species)
head(dataset2)
length(unique(dataset2$species))

# no change in number of uniques, so case wasn't an issue

# Check all species to look for removable data

class(dataset2$species)
dataset2$species = as.factor(dataset2$species)

levels(dataset2$species)

# Several species have repeats but followed by a typo of a symbol character (Â) and a space. Can be treated as typo and removed from the dataset.  

# Trying functions seemed to work to remove these characters, but probably not the most efficient method. 
           
spTest = levels(dataset2$species)
spTest1 = str_trim(spTest)
head(spTest1)
spTest2 = gsub("Â", "", spTest1)
head(spTest2)
length(unique(spTest))
length(unique(spTest2))

# Apply it to the dataset

dataset3 = dataset2
dataset3$species = str_trim(dataset3$species)
unique(dataset3$species)
  
  # This worked to remove the trailing space on these typo species

# Now to remove the symbol character (Â)

dataset3$species = gsub("Â", "", dataset3$species)

# Check to see if it worked

length(unique(dataset2$species))
length(unique(dataset3$species))

# Removed 35 typo species from the dataset
# Reset vectors

dataset3$species = as.factor(dataset3$species)

# Check all species again

levels(dataset3$species)
head(dataset3)

# Remove old species column

dataset3 = dataset3[,-4]
head(dataset3)

# All good after check

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Column M. Notes_spFormat. Provide a THOROUGH description of any changes made
# to the species field, including why any species were removed.

dataFormattingTable[,'Notes_spFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_spFormat', "several species were removed because they were repeated in the dataset due to a symbol and extra space; treated as a typo. typos were removed from the dataset individually, first by the space then by the symbol. started with 654 uniques, now have 619.")

