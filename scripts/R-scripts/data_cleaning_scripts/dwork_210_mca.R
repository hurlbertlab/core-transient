# Formatting dataset 210: Cedar Creek LTER Plants

#-------------------------------------------------------------------------------*
# ---- SET-UP ----
#===============================================================================*

# Load libraries:

library(stringr)
library(plyr)

# Source the functions file:

source('scripts/R-scripts/core-transient_functions.R')

# Get data:

getwd()

list.files('data/raw_datasets')

d = read.csv('data/raw_datasets/dataset_210.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*

head(d)
dim(d)
str(d)

# Remove columns not needed
d1 = d[,-c(1,2,6,7,8,9)]
head(d1)
summary(d1)

# Revert back to d
d = d1

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*

# 2 different fields associated with site data: "field" and "plot"
# Explore
class(d$field)
class(d$plot)
unique(d$field)
unique(d$plot)

# After checking for data to remove, there is none, so no removals
# Change plot to factor
d1 = d
d1$plot = factor(d1$plot)
levels(d$plot)

# Concatenate the two site columns to create new 'site' column
d1$site = paste(d1$field, d1$plot, sep = '_')
head(d1)
unique(d1$site)

# All looks good, so remove old site columns and change back to d
d = d1[,-c(2,3)]
head(d)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# explore
class(d$species)
length(unique(d$species))
#  259 unique species names

# Capitalize all to check for any case errors
sp = toupper(d$species)
length(unique(sp))

  # No case errors, so use original species column

# Look through unique species to find unwanted species names
levels(d$species)

# Several species names to be removed
badsp = c("Miscellaneous forb", "Miscellaneous grasses", "Miscellaneous grasses 2", "Miscellaneous herb", 
          "Miscellaneous herbs", "Miscellaneous legumes", "Miscellaneous litter", "Miscellaneous rushes", 
          "Miscellaneous sedges", "Miscellaneous sp.", "Miscellaneous woody plants","Forb seedlings","Mosses & lichens",
          "Pine needles")

# Remove species
d1 = d[!d$species %in% badsp,]
length(unique(d1$species))
unique(d1$species)

# Check nrows
nrow(d)
nrow(d1)

d = d1

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT TIME DATA ----
#===============================================================================*