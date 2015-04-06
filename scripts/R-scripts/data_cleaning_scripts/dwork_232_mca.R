# Formatting Dataset 232: Sevilleta small mammals

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
setwd('C:/Users/auriemma/core-transient/')
source('scripts/R-scripts/core-transient_functions.R')

# Get data. First specify the dataset number ('ds') you are working with.

ds = 232 

list.files('data/raw_datasets')

dataset = read.csv(paste('data/raw_datasets/dataset_', ds, '.csv', sep = ''))

dataFormattingTable = read.csv('Reference/data_formatting_table.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*

head(dataset)
summary(dataset)
str(dataset)

# Remove fields not needed

names(dataset)

unusedFields = c(7,9,10,11,12,13)

dataset1 = dataset[,-unusedFields]

head(dataset1)

# Explore more

head(dataset1, 20)

summary(dataset1)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE! 
# Are the ONLY site identifiers the latitude and longitude of the observation or 
# sample? (I.e., there are no site names or site IDs or other designations) Y/N

dataFormattingTable[,'LatLong_sites'] = 
  dataFormattingTableFieldUpdate(ds, 'LatLong_sites','N') 

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*

# Sites are nested in this dataset.  Sites are listed, where there are separate webs and individual traps within the webs

site = paste(dataset1$location, dataset1$web, dataset1$trap, sep = "_")

# comparing the site fields in the dataset with the new vector of sites:

head(site)

# Looks good, now add site to dataset and remove old site fields

dataset2 = dataset1

dataset2$site = factor(site)

dataset2 = dataset2[,-c(2,5,6)]
head(dataset2)

# All good

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

# !DATA FORMATTING TABLE UPDATE! 

# Raw_siteUnit. 

dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(ds, 'Raw_siteUnit', 'location_web_trap') 

# spatial_scale_variable. Is a site potentially nested (e.g., plot within a quad or decimal lat longs that could be scaled up)? Y/N

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(ds, 'spatial_scale_variable','Y')

# Notes_siteFormat. Use this field to THOROUGHLY describe any changes made to the site field during formatting.

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_siteFormat', 'site fields concatenated. after looking through metadata descriptions, it seems that location_web_trap describes the order of nested sites from small to large.')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*

# Look at the individual species present:

levels(dataset2$species) 

# Species is coded with four letter codes, but found Sevilleta species coding 
#here:  https://knb.ecoinformatics.org/knb/metacat?action=read&qformat=knb&sessionid=&docid=knb-lter-sev.8&displaymodule=attributedomain&entitytype=dataTable&entityindex=1&attributeindex=8.

# "dime" species repeated because space at end, so remove spaces

dataset3 = dataset2
dataset3$species = str_trim(dataset3$species)

# Reset factor levels

dataset3$species = factor(dataset3$species)

# Check
levels(dataset3$species)
length(unique(dataset3$species))
length(unique(dataset2$species))

# All good, no bad species to remove

# Look at the head of the dataset to ensure everything is correct:

head(dataset3)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Column M. Notes_spFormat. Provide a THOROUGH description of any changes made
# to the species field, including why any species were removed.

dataFormattingTable[,'Notes_spFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_spFormat', 'Species is coded with four letter codes, but found Sevilleta species coding here:  http://sev.lternet.edu/data/sev-008/4786. No bad species removed because all accounted for in this source. Only change made was remove extra space after one species')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*

names(dataset3)
names(dataset)

# No actual count field in this dataset

head(dataset3, 40)

# Table the dataframe by the appropriate fields to get frequencies of species per time sample per site. After checking metadata it seems that night is not a necessary time component.  So it doesn't need to be considered for this dataset.

dataset_count = data.frame(table(dataset3[,c('species','year','season','site')]))

# Get rid of the zeros

dataset_count = dataset_count[dataset_count$Freq!=0, ]

# Check

head(dataset_count, 30)
tail(dataset_count,20)
summary(dataset_count)

# Seems to have worked, make Freq the count field

dataset5 = dataset_count

names(dataset5)[6] = 'count'

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE COUNT DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

dataFormattingTable[,'countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'countFormat', 'count')

dataFormattingTable[,'Notes_countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_countFormat', 'no count field in this dataset, so created a dataframe using table to get frequency of species per time sample per site.')

#-------------------------------------------------------------------------------*
# ---- FORMAT TIME DATA ----
#===============================================================================*

head(dataset5)

# Year, season and night are the fields representing time.

dataset6 = dataset5
head(dataset6)

# Season needs to be incorporated into the year.  Metadata says that coding for season is as follows: 1 = Spring, 2 = Summer, 3 = Fall


