# Cleaning dataset 173 OBIS Inverts

# Set Up
  # Load libraries and source functions
setwd("C:/Users/auriemma/core-transient/")

library(plyr)
library(stringr)
source("scripts/R-scripts/core-transient_functions.R")

# Get data
d = read.csv("data/raw_datasets/dataset_173.csv")

# Explore dataset
dim(d)
names(d)
str(d)
head(d)
summary(d)

# Remove unwanted columns
d = d[,-c(1)]
head(d)

# Rename columns to appropriate names
names(d) = c('datasetID','year','site','species','count')
names(d)
head(d, 15)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*