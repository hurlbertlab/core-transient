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
  # List unused fields
unusedFields = c(1,2,4,5,8,9,11,12,13,14,15,16,17)

  # Remove
dataset1 = dataset[,-unusedFields]

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

dataset2 = dataset2[,-c(3)]

# Check
head(dataset2)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

# !DATA FORMATTING TABLE UPDATE!

dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(ds, 'Raw_siteUnit', 'quadr')

# Site is probably a site listed as a letter and quadrant within listed as number
dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(ds, 'spatial_scale_variable',
                                 
                                 'Y')

# Notes_siteFormat.

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_siteFormat',                                   
                                 'sites are 1 ha plots lettered and trapping stations separated into 10 by 10 grids which are numbered.  No changes made to site data')


#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*

# Look at the individual species present:

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

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!