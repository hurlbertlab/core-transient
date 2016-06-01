################################################################################*
# Data cleaning script for dataset 257, Costello and Myers 1996, amphipods
#                                          
#
# http://www.sciencedirect.com/science/article/pii/0022098196000305
#
# Data are extracted from published figures in the form of counts of number
# of species occurring with different frequencies (temporal occupancy).

# As such there is no formatted dataset, and not all fields in the data formatting
# table are available.

# This script only writes propOcc_XX.csv and siteSummary_XX.csv files. See the
# data_formatting_template.R for a complete example of cleaning a more complete
# raw dataset.

# Data used are from the "Monthly" series corresponding to scrub bads 

datasetID = 257

dataset = read.csv(paste('data/raw_datasets/dataset_', datasetID, '.csv', sep = ''))

dataFormattingTable = read.csv('data_formatting_table.csv')

dataFormattingTable[,'Raw_datafile_name'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Raw_datafile_name',  
                                 
                                 #--! PROVIDE INFO !--#
                                 'extracted from Costello & Myers 1996') 

# What is the spatial grain of the finest sampling scale? For example, this might be
# a 0.25 m2 quadrat, or a 5 m transect, or a 50 ml water sample.
# "The artificial substrata were small balls of plastic mesh commercially available as
# kitchen scouring pads. They varied little in size, so it was assumed that sample size
# effects were negligible; weight 28.02 g, length 12.6 cm, width 11.6 cm."

dataFormattingTable[,'Raw_spatial_grain'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Raw_spatial_grain',  
                                 
                                 #--! PROVIDE INFO !--#
                                 146) 

dataFormattingTable[,'Raw_spatial_grain_unit'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Raw_spatial_grain_unit',  
                                 
                                 #--! PROVIDE INFO !--#
                                 'cm2') 


dataFormattingTable[,'LatLong_sites'] = 
  dataFormattingTableFieldUpdate(datasetID, 'LatLong_sites',  
                                 
                                 #--! PROVIDE INFO !--#
                                 'N') 

dataFormattingTable[,'Notes_timeFormat'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Notes_timeFormat', 
                                 
                                 #--! PROVIDE INFO !--#
                                 'Monthly samples.')


# subannualTgrain. After exploring the time data, was this dataset sampled at a 
#   sub-annual temporal grain? Y/N

dataFormattingTable[,'subannualTgrain'] = 
  dataFormattingTableFieldUpdate(datasetID, 'subannualTgrain', 
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 'Y')

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(datasetID, 'spatial_scale_variable',
                                 
                                 #--! PROVIDE INFO !--#
                                 'N')

# Notes_siteFormat. Use this field to THOROUGHLY describe any changes made to the 
# site field during formatting.

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Notes_siteFormat', 
                                 
                                 #--! PROVIDE INFO !--#
                                 'A site is a single plastic mesh kitchen scouring pad.')

dataFormattingTable[,'countFormat'] = 
  dataFormattingTableFieldUpdate(datasetID, 'countFormat',  
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 'presence')

dataFormattingTable[,'Notes_countFormat'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Notes_countFormat', 
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 'Raw occupancy data, not individual counts, provided.')

dataFormattingTable[,'Notes_spFormat'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Notes_spFormat',  
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 'No species names, just total number of species at each temporal occupancy level.')


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


############################################

# Function takes a dataset with two columns: numYrs = the number of years
# (or other temporal unit) that a species was observed to be present, and
# numSpp = the number of species observed with that level of temporal frequency.
repeatOccs = function(data) {
  occs = c()
  for (i in data$numYrs) { 
    occs = c(occs, rep(i/max(data$numYrs), data$numSpp[data$numYrs == i]))
  }
  return(occs)
}

dataset1 = data.frame(datasetID = datasetID, 
                      site = 1, 
                      species = 1:sum(dataset$numSpp),
                      propOcc = repeatOccs(dataset))

write.csv(dataset1, paste('data/propOcc_datasets/propOcc_', datasetID, '.csv', sep = ''),
          row.names = F)

##########################################
# Site summary

meanN = 6187/12 #report of 6187 individuals over 12 month dataset

summary = data.frame(datasetID = datasetID, 
                     site = 1, 
                     spRich = sum(dataset$numSpp),
                     nTime = max(dataset$numYrs),
                     meanAbundance = meanN)

write.csv(summary, paste('data/siteSummaries/siteSummary_', datasetID, '.csv', sep = ''),
          row.names = F)
#########################################


dataFormattingTable[,'Raw_nSpecies'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Raw_nSpecies', 
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 summary$spRich)


dataFormattingTable[,'Formatted_nSpecies'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Formatted_nSpecies', 
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 summary$spRich)

dataFormattingTable[,'Raw_Mean_Individuals_perSiteYear'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Raw_Mean_Individuals_perSiteYear', 
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 meanN)

dataFormattingTable[,'Formatted_Mean_Individuals_perSiteYear'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Formatted_Mean_Individuals_perSiteYear', 
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 meanN)

dataFormattingTable[,'General_notes'] = 
  dataFormattingTableFieldUpdate(datasetID, 'General_notes', 
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 'only monthly presence-absence info available')

# And write the final data formatting table:

write.csv(dataFormattingTable, 'data_formatting_table.csv', row.names = F)


# Remove all objects except for functions from the environment:
rm(list = setdiff(ls(), lsf.str()))

