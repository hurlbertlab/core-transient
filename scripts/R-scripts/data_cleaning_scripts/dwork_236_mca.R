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

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# Look at the individual species present:

sp = dataset$sp
class(dataset$sp)
levels(sp)

length(unique(sp))
# Only 14 species present in this dataset
# No bad species present

unique(dataset$sp)

# Change name
names(dataset)[3]= "species"
head(dataset)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT TIME DATA ----
#===============================================================================*
# Explore mo column
summary(dataset$mo)
length(unique(dataset$mo))
# Date is structured as datenth no space

# Extract date and reformat
date = dataset$mo
head(date)

# Substring month from date
library(stringr)

  #Change to character and dataframe
date = data.frame(as.character(date))
head(date)

  # Substring out year and month
date$month = substring(date$as.character.date., 5)
head(date)
date$year = substring(date$as.character.date., 1,4)
head(date)

# Remove old column
date = date[,-1]
head(date)

# Because we're going to need a day column to satisfy date format
# add a column of just '01's, just as a day 1

date$day = rep(as.character(01), nrow(date))
summary(date)
head(date)

# Paste year and month together
date$date = paste(date$month, date$day, date$year, sep = "/")
head(date)

# Extract as date format
date1= strptime(date$date,'%m/%d/%Y')
class(date1)
head(date1)
summary(date1)

# Add new date to dataset
dataset$date = date1
head(dataset)

# Remove old column
dataset = dataset[,-1]

# Double check over
head(dataset)

# All good

