Cleaning dataset 236:  Chile small mammals

#-------------------------------------------------------------------------------*
# ---- SET-UP ----
#===============================================================================*

# Source the functions file:
setwd('C:/Users/auriemma/core-transient/')
source('scripts/R-scripts/core-transient_functions.R')

# Get data:

getwd()

list.files('data/raw_datasets')

dataset = read.csv('data/raw_datasets/dataset_236.csv')

#===============================================================================*
# MAKE FORMATTED DATASET
#===============================================================================*

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*
# View field names:

names(dataset)

dim(dataset)

str(dataset)

head(dataset)

summary(dataset)

# Remove some unwanted fields
dataset = dataset[,-c(1,3,4,8,9,11,12,16)]
head(dataset)


#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*
# Explore potential site columns
summary(dataset$gr)
unique(dataset$gr)
unique(dataset$row)
unique(dataset$col)

# Remove 'row' and 'col' fields
dataset = dataset[,-c(3,4)]
head(dataset)

# 'gr' column is going to be the site (grid)
# Change to factor
class(dataset$gr)
dataset$gr = factor(as.character(dataset$gr))
unique(dataset$gr)
levels(dataset$gr)

# Change name
names(dataset)[2] = "site"
head(dataset)


