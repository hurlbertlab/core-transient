################################################################################*
#
# Dataset name: Long-Term Community Dynamics of Small Landbirds with and Without 
# Exposure to Extensive Disturbance from Military Training Activities
# Dataset source (link): http://link.springer.com/article/10.1007/s00267-009-9421-6/fulltext.html
# Formatted by: Sara Snell 

# Occupancy data (not counts or raw survey data) provided for two sites:
# Ft. Riley and Konza Prairie

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
datasetID = 270

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
  'extracted from Rivers et al. 2010 Env Mgmt') 


# What is the spatial grain of the finest sampling scale? For example, this might be
# a 0.25 m2 quadrat, or a 5 m transect, or a 50 ml water sample.
# The mean transect length at Konza was 12,222 m, and at Ft Riley 5982 m ; 
# take an average and multiply times 200 m transect width

dataFormattingTable[,'Raw_spatial_grain'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Raw_spatial_grain',  
                                 
                                 #--! PROVIDE INFO !--#
                                 1820400) 

dataFormattingTable[,'Raw_spatial_grain_unit'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Raw_spatial_grain_unit',  
                                 
                                 #--! PROVIDE INFO !--#
                                 'm2') 


dataFormattingTable[,'LatLong_sites'] = 
  dataFormattingTableFieldUpdate(datasetID, 'LatLong_sites',  
                                 
                                 #--! PROVIDE INFO !--#
                                 'N') 

dataFormattingTable[,'Notes_timeFormat'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Notes_timeFormat', 
                                 
                                 #--! PROVIDE INFO !--#
                                 'Annual counts')


# subannualTgrain. After exploring the time data, was this dataset sampled at a 
#   sub-annual temporal grain? Y/N

dataFormattingTable[,'subannualTgrain'] = 
  dataFormattingTableFieldUpdate(datasetID, 'subannualTgrain', 
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 'N')

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(datasetID, 'spatial_scale_variable',
                                 
                                 #--! PROVIDE INFO !--#
                                 'N')

# Notes_siteFormat. Use this field to THOROUGHLY describe any changes made to the 
# site field during formatting.

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Notes_siteFormat', 
                                 
                                 #--! PROVIDE INFO !--#
                                 'Linear transects of width 200 m at each of two sites')

dataFormattingTable[,'countFormat'] = 
  dataFormattingTableFieldUpdate(datasetID, 'countFormat',  
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 'NA')

dataFormattingTable[,'Notes_countFormat'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Notes_countFormat', 
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 'Raw occupancy data, not individual counts, provided.')

dataFormattingTable[,'Notes_spFormat'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Notes_spFormat',  
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 'No bad species names or typos')


dataFormattingTable[,'format_flag'] = 
  dataFormattingTableFieldUpdate(datasetID, 'format_flag', 
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 1)

# Flag codes are as follows:
# 0 = not currently worked on
# 1 = formatting complete
# 2 = formatting in process
# 3 = formatting halted, issue
# 4 = data unavailable
# 5 = data insufficient for generating occupancy data


dataset1 = data.frame(datasetID = datasetID, 
                      site = c(rep('Ft Riley', nrow(dataset)), rep('Konza Prairie', nrow(dataset))),
                      species = rep(dataset$Species, 2),
                      propOcc = c(dataset$FR_occ, dataset$KP_occ))

dataset2 = dataset1[dataset1$propOcc > 0,]

write.csv(dataset2, paste('data/propOcc_datasets/propOcc_', datasetID, '.csv', sep = ''),
          row.names = F)

##########################################
# Site summary

# Note mean abundance is not provided, but based on the number of years in
# which species with relative abundance of 0.001 were observed, we roughly estimate
# average abundance per year.

# E.g., at KP, species with average relative abundance 0.001 were observed on 2-6 years.
# This means at a minimum, 6 total individuals corresponds to a relative abundance of
# 0.001, which implies 6,000 individuals over the 11-year sampling period, or 
# 545 individuals per year on average.

# At FR: A species with relative abundance of 0.001 was observed in 5 years, implying 5,000
# total individuals and so 454 individuals per year on average.

summary = data.frame(datasetID = rep(datasetID, 2), 
                     site = c('Ft Riley', 'Konza Prairie'), 
                     spRich = c(sum(dataset2$site == 'Ft Riley'), # should be 52
                                sum(dataset2$site == 'Konza Prairie')), #should be 55
                     nTime = rep(11, 2),
                     meanAbundance = c(454, 545))

write.csv(summary, paste('data/siteSummaries/siteSummary_', datasetID, '.csv', sep = ''),
          row.names = F)
#########################################


dataFormattingTable[,'Raw_nSpecies'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Raw_nSpecies', 
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 length(unique(dataset2$species)))


dataFormattingTable[,'Formatted_nSpecies'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Formatted_nSpecies', 
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 length(unique(dataset2$species)))

dataFormattingTable[,'Raw_Mean_Individuals_perSiteYear'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Raw_Mean_Individuals_perSiteYear', 
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 500)

dataFormattingTable[,'Formatted_Mean_Individuals_perSiteYear'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Formatted_Mean_Individuals_perSiteYear', 
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 500)

dataFormattingTable[,'General_notes'] = 
  dataFormattingTableFieldUpdate(datasetID, 'General_notes', 
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 'Temporal occupancy data for 62 species at 2 sites from Table 1')

# And write the final data formatting table:

write.csv(dataFormattingTable, 'data_formatting_table.csv', row.names = F)


# Remove all objects except for functions from the environment:
rm(list = setdiff(ls(), lsf.str()))


