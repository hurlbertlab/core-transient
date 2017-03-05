################################################################################*
#  DATA FORMATTING TEMPLATE
################################################################################*
#
# Dataset name: Mammal abundance indices in the northern portion of the Great Basin
# Dataset source (link): http://esapubs.org/archive/ecol/E086/172/metadata.htm
# Formatted by: Allen Hurlbert
#
# Start by opening the data formatting table (data_formatting_table.csv). 
# Datasets to be worked on will have a 'format_flag' of 0.

# Flag codes are as follows:
  # 0 = not currently worked on
  # 1 = formatting complete
  # 2 = formatting in process
  # 3 = formatting halted, issue
  # 4 = data unavailable
  # 5 = data insufficient for generating occupancy data

# NOTE: All changes to the data formatting table will be done in R! 
# Do not make changes directly to this table, this will create conflicting versions.

# YOU WILL NEED TO ENTER DATASET-SPECIFIC INFO IN EVERY LINE OF CODE PRECEDED
# BY "#--! PROVIDE INFO !--#". 

# YOU SHOULD RUN, BUT NOT OTHERWISE MODIFY, ALL OTHER LINES OF CODE.

#-------------------------------------------------------------------------------*
# ---- SET-UP ----
#===============================================================================*

# This script is best viewed in RStudio. I like to reduced the size of my window
# to roughly the width of the section lines (as above). Additionally, ensure 
# that your global options are set to soft-wrap by selecting:
# Tools/Global Options .../Code Editing/Soft-wrap R source files

# Load libraries:

library(stringr)
library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(MASS)


# Source the functions file:

getwd()

# Set your working directory to be in the home of the core-transient repository
# e.g., setwd('C:/git/core-transient')

source('scripts/R-scripts/core-transient_functions.R')

# Get data. First specify the dataset number ('datasetID') you are working with.

#--! PROVIDE INFO !--#
datasetID = 307 

list.files('data/raw_datasets')

dataset = read.csv(paste('data/raw_datasets/dataset_', datasetID, '.csv', sep = ''))

dataFormattingTable = read.csv('data_formatting_table.csv')

# Make sure the original name of the raw data file is saved in the data formatting table.
# NOT, for example, 'rawdataset_255.csv', but the filename as originally downloaded.
# Check the data source link (available in the table, and hopefully posted above) if
# the data is available online. If the data come from a published paper and there is
# no file that was downloaded, enter "NA".

dataFormattingTable[,'Raw_datafile_name'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Raw_datafile_name',  
                                 
#--! PROVIDE INFO !--#
  'Mammal_abundance_indices.txt') 



########################################################
# ANALYSIS CRITERIA                                    #  
########################################################

# Min number of time samples required 
minNTime = 6

# Min number of species required
minSpRich = 10

# Ultimately, the largest number of spatial and 
# temporal subsamples will be chosen to characterize
# an assemblage such that at least this fraction
# of site-years will be represented.
topFractionSites = 0.5

#######################################################

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*
# Here, you are predominantly interested in getting to know the dataset, and 
# determine what the fields represent and  which fields are relavent.

# View field names:

names(dataset)

# View how many records and fields:

dim(dataset)

# View the structure of the dataset:


# View first 6 rows of the dataset:

head(dataset)

# Here, we can see that there are some fields that we won't use. These might be
# fields describing weather, observer ID's, or duplicate information like year
# or month when there is already a complete date column.

dataFormattingTable[,'format_flag'] = 
  dataFormattingTableFieldUpdate(datasetID, 'format_flag', 
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 5)

# Flag codes are as follows:
# 0 = not currently worked on
# 1 = formatting complete
# 2 = formatting in process
# 3 = formatting halted, issue
# 4 = data unavailable
# 5 = data insufficient for generating occupancy data

# Add any final notes about the dataset that might be of interest:
dataFormattingTable[,'General_notes'] = 
  dataFormattingTableFieldUpdate(datasetID, 'General_notes', 
                                 
                                 #--! PROVIDE INFO !--#                                 
  "This dataset provides 'indices of abundance of selected mammals' including coyote, lagomorphs and 8 species of rodents. First, the combination of coyote with rodents is questionable for characterizing an ecological assemblage which puts the dataset at or below the richness threshold. Also, the use of the term SELECT implies that data on additional, transient, species are not included.")

# And write the final data formatting table:

write.csv(dataFormattingTable, 'data_formatting_table.csv', row.names = F)

# Remove all objects except for functions from the environment:

rm(list = setdiff(ls(), lsf.str()))

