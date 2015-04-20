# Formatting Dataset 33: Marine Phytoplankton

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

ds = 33 

dataset = read.csv(paste('data/raw_datasets/dataset_', ds, '.csv', sep = ''))

dataFormattingTable = read.csv('Reference/data_formatting_table.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*

names(dataset)
head(dataset)
summary(dataset)

# Remove unused field labeled 'X'

dataset1 = dataset[,-1]
head(dataset1)

# Change SampleID field to 'site'

names(dataset1)[3] = 'site'

# Change 'ID' field to 'datasetID'

names(dataset1)[1] = 'datasetID'

# Change species field to 'species'

names(dataset1)[4] = 'species'

# Check 

head(dataset1)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

