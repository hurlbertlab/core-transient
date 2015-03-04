# Cleaning dataset 234: Powdermill Mammals

#-------------------------------------------------------------------------------*
# ---- SET-UP ----
#===============================================================================*
# Source the functions file:

source('scripts/R-scripts/core-transient_functions.R')

# Get data:

getwd()

list.files('data/raw_datasets')

dataset = read.csv('data/raw_datasets/dataset_234.csv')

#===============================================================================*
# MAKE FORMATTED DATASET
#===============================================================================*

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*
names(dataset)
head(dataset)
tail(dataset)

# Remove unwanted columns
dataset1 = dataset[,-c(1,2,4,5,8,9,11,12,13,14,15,16,17)]
head(dataset1)

dataset = dataset1

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*
# View summary of fields in the dataset:

summary(dataset)
head(dataset)

# How many sites are there
length(unique(dataset$quadr))
# 124 unique sites

# View sites
unique(dataset$quadr)

# Remove bad sites 
badsites = c("?", "0")
dataset1 = dataset[!dataset$quadr %in% badsites,]
unique(dataset1$quadr)

# See how many sites removed
dim(dataset1)
dim(dataset)

# All good, revert back from dataset1 to dataset
dataset = dataset1

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT TIME DATA ----
#===============================================================================*
# Explore dates
head(dataset)

# Date is lumped altogether in one string
# Need to substring day and month from date column

# Create separate dataframe

date = data.frame(as.character(dataset$date))
head(date)

# Substring day
date$day = str_sub(date$as.character.dataset.date., start = -2)
head(date)

# Substring month
date$month = str_sub(date$as.character.dataset.date., start = 5, end = -3)
head(date)

# Now year
date$year = str_sub(date$as.character.dataset.date., end = 4)
head(date)

# Paste day month year back together
date1 = paste(date$month, date$day, date$year, sep = "/")
head(date1)

# Add date1 as date object to dataset
dataset$date = strptime(date1, "%m/%d/%Y")
class(dataset$date)
unique(dataset$date)
head(dataset)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*

# THIS IS ALL OLD CODE NEEDS REWORKING

# Create a data frame of the count of individuals for a given sampling event:

df2 = ddply(df1, .(site, year, species, date), 
               summarise, count = length(species))

# Create a data frame of the maximum count of individuals 
# for a given sampling event within a season.

df3 = ddply(df2,.(site,year,species),
               summarise, count = max(count))

# Arrange the fields in the same order as other datasets:

df4 = data.frame(df3[,1],df3[,3],df3[,2],df3[,4])
names(df4) = c('site','species','year','count')

# Add a dataset ID column for matching with metadata

df4$datasetID = rep(234, length(df4[,1]))

# Rearrange the columns"

d234 = df4[,c(5,1:4)]

# Write to file:

write.csv(d234, file.path(out_dir,'dataset_234.csv'), row.names = F)

# Remove objects from the global environment

rm(list = ls())
