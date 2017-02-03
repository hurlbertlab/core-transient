################################################################################*
#  DATA FORMATTING TEMPLATE
################################################################################*
#
# Dataset name: PISCO: Intertidal: MARINe Core Surveys: Species Counts
# Dataset source (link): doi:10.6085/AA/pisco_intertidal.81.7 accessed on DataOne
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
datasetID = 283 

list.files('data/raw_datasets')

dataset = read.csv(paste('data/raw_datasets/dataset_', datasetID, '.csv', sep = ''))

dataFormattingTable = read.csv('data_formatting_table.csv')

# Make sure the original name of the raw data file is saved in the data formatting table.

dataFormattingTable[,'Raw_datafile_name'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Raw_datafile_name',  
                                 
#--! PROVIDE INFO !--#
  'pisco_intertidal.91.3.data') 



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

# Subset to the Motile Invertebrate Monitoring (MethodCode=="MI", 926895/933000 records)
dataset1a = subset(dataset, MethodCode == "MI")



# Here, we can see that there are some fields that we won't use. These might be
# fields describing weather, observer ID's, or duplicate information like year
# or month when there is already a complete date column.

# If all fields will be used, then set unusedFieldNames = ""

names(dataset)

#--! PROVIDE INFO !--#
unusedFieldNames = c('GroupCode', 'SampleSeason', 'SamplingSeasonCode', 'SampleYear',
                     'SeasonSeq', 'TargetSpecies', 'SeaStarColor', 'CountQualifier','Comments')

dataset1 = dataset1a[, !names(dataset) %in% unusedFieldNames]

# Note that I've given a new name here "dataset1", this is to ensure that 
# we don't have to go back to square 1 if we've miscoded anything.

# A quick check of both the metadata and number of records per species shows that
# this dataset is probably unsuitable since they are focused primarily on 
# characterizing trends in core, abundant species, so the dataset by design
# will not have transients:

# "Species Count Methods - Overview
# Species Count sampling is conducted by recording the number of a specific organism found in a permanent plot at # a given site. For some species, size and color are also recorded. The type of plot and number of replicates 
# monitored is dependent on the target species. Algae and invertebrate species counts are recorded. Please note  # that these data do not reflect all species present in a given area."
table(dataset1$SixLetterCode)


# ABORT

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


dataFormattingTable[,'General_notes'] = 
  dataFormattingTableFieldUpdate(datasetID, 'General_notes', 
                                 
                                 #--! PROVIDE INFO !--#                                 
  'Dataset seems to be focused on a fixed set of species, so will not have info on transients')

# And write the final data formatting table:

write.csv(dataFormattingTable, 'data_formatting_table.csv', row.names = F)

# Remove all objects except for functions from the environment:

rm(list = setdiff(ls(), lsf.str()))

