# Formatting Dataset 97 ArcOD Plankton

#-------------------------------------------------------------------------------*
# ---- SET-UP ----
#===============================================================================*
# Load libraries:

library(stringr)
library(plyr)

# Source the functions file:

setwd('C:/Users/auriemma/core-transient/')
source('scripts/R-scripts/core-transient_functions.R')

# Get data:

d = read.csv('data/raw_datasets/dataset_97.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*

head(d)
names(d)
str(d)

# Remove unwanted columns
d1 = d[,-c(1,2)]
head(d1)

  # Change d1 back to d
d = d1
summary(d)
