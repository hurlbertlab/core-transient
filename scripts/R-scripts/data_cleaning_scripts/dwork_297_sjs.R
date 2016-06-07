################################################################################*
#
# Dataset name: Marsh Ecology Research Program (MERP): Activity trap data (1980-1989)
# Dataset source (link): https://knb.ecoinformatics.org/#view/doi:10.5063/AA/bowdish.819.26
# Formatted by:  Allen Hurlbert
#
# Invertebrate sampling occurred in the controlled MERP cells on the Delta Marsh. Sampling stations within each cell were stratified based on dominant vegetation cover in order to increase sampling efficiency, as this reflects bottom contours. The number of sampling stations used per cover type and the number of samples taken per site was determined by using the calculations of Downing (1979) based on expected invertebrate densities (Murkin et al., 1982). During the baseline, deep flooding, and drawdown phases (1980-84), there were 2 sampling sites established for each cover type in each cell, both experimental and control. During the re-flooding phase (1985-89), two water depth (30 cm 'shallow' and 60 cm 'deep') strata were also determined for every cover type in the experimental cells, while the control cells (11 and 12) were not differentiated by strata and only one sample was taken at each site.
#
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
datasetID = 297 

list.files('data/raw_datasets')

#dataset = read.csv(paste('data/raw_datasets/dataset_', datasetID, '.csv', sep = ''))

dataFormattingTable = read.csv('data_formatting_table.csv')

# Make sure the original name of the raw data file is saved in the data formatting table.
# NOT, for example, 'rawdataset_255.csv', but the filename as originally downloaded.
# Check the data source link (available in the table, and hopefully posted above) if
# the data is available online. If the data come from a published paper and there is
# no file that was downloaded, enter "NA".

dataFormattingTable[,'Raw_datafile_name'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Raw_datafile_name',  
                                 
#--! PROVIDE INFO !--#
  'merp_inverts_at1980-1982_data.txt, merp_inverts_at1985-1989_data.txt') 

# As we've now successfully created the formatted dataset, we will now update 
# the format flag field. 

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
  'Time series spans major disturbances (drawdown, flooding), so unsuitable for our purposes.')


# And write the final data formatting table:

write.csv(dataFormattingTable, 'data_formatting_table.csv', row.names = F)

# Remove all objects except for functions from the environment:

rm(list = setdiff(ls(), lsf.str()))

