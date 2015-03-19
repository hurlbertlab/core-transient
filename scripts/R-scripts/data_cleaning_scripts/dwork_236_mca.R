Cleaning dataset 236:  Chile small mammals

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

source('scripts/R-scripts/core-transient_functions.R')

# Get data. First specify the dataset number ('ds') you are working with.

ds = 236 

list.files('data/raw_datasets')

dataset = read.csv(paste('data/raw_datasets/dataset_', ds, '.csv', sep = ''))

dataFormattingTable = read.csv('Reference/data_formatting_table.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*

names(dataset)

dim(dataset)

str(dataset)

head(dataset)

summary(dataset)

# Remove some unwanted fields
# Look through metadata to see which fields mean what
  # List unused
unusedFields = c(1,3,4,8,9,11,12)

  # What does sst, rst, st, f stand for??
