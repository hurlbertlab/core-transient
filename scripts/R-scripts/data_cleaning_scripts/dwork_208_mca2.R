# Formatting dataset 208: Landis insects

# set correct directory
getwd()
setwd('C:/git/core-transient')

# Library packages and source functions
library(plyr)
library(stringr)
source('scripts/R-scripts/core-transient_functions.R')

# Get data
d = read.csv('data/raw_datasets/dataset_208.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*

names(d)
str(d)
head(d)
# Remove unwanted columns
d = d[,-c(1,7,8,11)]
head(d)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# Find number unique species
length(unique(d$species))
unique(d$species)

# Remove blanks and other bad species
d1 = d
badspp = c('something else','')
d1 = d1[!d1$species %in% badspp,]
dim(d1)
dim(d)
unique(d1$species)

d = d1
