# Cleaning dataset 234: Powdermill Mammals

#-------------------------------------------------------------------------------*
# ---- SET-UP ----
#===============================================================================*
# # Load libraries:

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

# Get data

ds = 234

list.files('data/raw_datasets')

dataset = read.csv(paste('data/raw_datasets/dataset_', ds, '.csv', sep = ''))

dataFormattingTable = read.csv('Reference/data_formatting_table.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*
names(dataset)
head(dataset)
tail(dataset)
str(dataset)

# Remove unwanted columns

# Lots of them, so list ones that will be used instead of unused
  # List used fields
usedFields = c(3,6,7)

  # Remove
dataset1 = dataset[,usedFields]

# Check
head(dataset1)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*
# View summary of fields in the dataset:

summary(dataset1)
head(dataset1)

# Make 'site' object

site = dataset1$quadr
head(site)

# Add new column to dataset

dataset2 = dataset1

dataset2$site = factor(site)
head(dataset2)

# Remove old one

dataset2 = dataset2[,-c(3)]

# Check
head(dataset2)

# Check for number of sites sampled per year

uniqQuadPerYear = data.frame(table(unique(dataset[, c('year', 'quadr')])$year))

uniqQuadPerYear

# Note the minor difference in number of sites sampled each year

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

# !DATA FORMATTING TABLE UPDATE!

dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(ds, 'Raw_siteUnit', 'quadr')

# Sites are different quadrats within a 1 ha grid, sp can be reduced to the whole grid

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(ds, 'spatial_scale_variable','Y')

# Notes_siteFormat.

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_siteFormat', 'sites are quadrats within a 1 ha plot.  No changes made to site data. Site data was checked for variation in number of sites sampled per year.  Varied from 99 sites to 105 sites, a negligible differece, so was ignored. There may be blank site names and other misnamed sites, but this can be ignored')


#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*

# Look at the individual species present:

head(dataset2)
levels(dataset2$species)

# Species are coded but metadata found does not discuss species coding.  There are some obvious items to remove from the dataset, though:

bad_sp = c("","?")

# Remove the bad species from dataset
dataset3 = dataset2[!dataset2$species %in% bad_sp,]

# Reset factor levels
dataset3$species = factor(dataset3$species)

# Check
levels(dataset3$species)
head(dataset3)

dim(dataset2)
dim(dataset3)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Column M. Notes_spFormat.
dataFormattingTable[,'Notes_spFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_spFormat', 'no info in metadata about species coding but not many different species so little chance for error. Only items removed were blanks and items labeled with a question mark')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*
names(dataset3)
head(dataset3, 30)

# Leaves just species, date, and site columns

# Species are accounted for several times at one time sample, so extract count data using table function

dataset_count = data.frame(table(dataset3[,c('species','date','site')]))
dataset_count = dataset_count[dataset_count$Freq!=0, ]
head(dataset_count,30)
tail(dataset_count,30)

# Freq value is the number of each species at each site and each time sample

# Change dataset to new dataframe

dataset4 = dataset_count

# Change count column name

names(dataset4)[4] = 'count'

# check all

head(dataset4)
summary(dataset4)

# All looks good

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE COUNT DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Possible values for countFormat field are density, cover, and count.
dataFormattingTable[,'countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'countFormat', 'count')

dataFormattingTable[,'Notes_countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_countFormat', 'data was extracted by creating table and getting frequency of each species at each time sample per each site.')

#-------------------------------------------------------------------------------*
# ---- FORMAT TIME DATA ----
#===============================================================================*
# This dataset gives date without any separation betw values
# So, need to separate values using substring, then put back together to make date object

dataset5 = dataset4

# Substring year, month, and day

year = str_sub(dataset5$date, end = 4)

month = str_sub(dataset5$date, start = 5, end = 6)

day = str_sub(dataset5$date, start = 7, end = 8)

# Paste values back together

date = paste(month, day, year, sep = '/')

# Change to date format

date1 = strptime(date, '%m/%d/%Y')

class(date1)
summary(date1)

# Add new date field to dataset

dataset6 = dataset5

dataset6$date = date1

# Check
head(dataset6, 20)
tail(dataset6)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATE DATA WERE MODIFIED!

# !DATA FORMATTING TABLE UPDATE!

# Notes_timeFormat.

dataFormattingTable[,'Notes_timeFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_timeFormat',  'temporal data provided as dates, but with no separation betw year, month, day values. Separated the values, pasted back together, then converted to date object.  Then added the date object to the dataset.')

# subannualTgrain. After exploring the time data, was this dataset sampled at a sub-annual temporal grain? Y/N

dataFormattingTable[,'subannualTgrain'] = 
  dataFormattingTableFieldUpdate(ds, 'subannualTgrain', 'Y')

