################################################################################*
#
# Dataset name: Saskatoon and Lethbridge moths
# Dataset source (link): https://www.jstor.org/stable/1930989
# Formatted by: Allen Hurlbert and Sara Snell

# reformatting the original moth time series dataset from Preston 1948

datasetID = 319

dataFormattingTable = read.csv('data_formatting_table.csv')

dataFormattingTable[,'Raw_datafile_name'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Raw_datafile_name',  
                                 
                                 #--! PROVIDE INFO !--#
                                 'extracted from Preston 1948') 

# What is the spatial grain of the finest sampling scale? 

# A light trap. Assume it samples from a radius of 50 m? 2500*pi = 

dataFormattingTable[,'Raw_spatial_grain'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Raw_spatial_grain',  
                                 
                                 #--! PROVIDE INFO !--#
                                 7854) 

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
                                 'Catch data summarized annually')


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
                                 'Two distinct sample sites')

dataFormattingTable[,'countFormat'] = 
  dataFormattingTableFieldUpdate(datasetID, 'countFormat',  
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 '')

dataFormattingTable[,'Notes_countFormat'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Notes_countFormat', 
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 'Annual counts')

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

saskatoon = c(rep(1/22, 49), rep(2/22, 29), rep(3/22, 9), rep(4/22, 16), rep(5/22, 8), rep(6/22, 10),
              rep(7/22, 12), rep(8/22, 6), rep(9/22, 11), rep(10/22, 8), rep(11/22, 11), rep(12/22, 15),
              rep(13/22, 3), rep(14/22, 6), rep(15/22, 6), rep(16/22, 11), rep(17/22, 8), rep(18/22, 1),
              rep(19/22, 7), rep(20/22, 7), rep(21/22, 6), rep(22/22, 38))

lethbridge = c(rep(1/22, 45), rep(2/22, 21), rep(3/22, 19), rep(4/22, 13), rep(5/22, 12), rep(6/22, 10),
               rep(7/22, 8), rep(8/22, 9), rep(9/22, 9), rep(10/22, 17), rep(11/22, 11), rep(12/22, 5),
               rep(13/22, 7), rep(14/22, 7), rep(15/22, 7), rep(16/22, 6), rep(17/22, 8), rep(18/22, 15),
               rep(19/22, 16), rep(20/22, 16), rep(21/22, 15), rep(22/22, 15))


occ = data.frame(datasetID = datasetID, site = c(rep("Saskatoon", length(saskatoon)), 
                                                 rep("Lethbridge", length(lethbridge))),
                 species = c(1:length(saskatoon), 1:length(lethbridge)),
                 propOcc = c(saskatoon, lethbridge))


write.csv(occ, paste('data/propOcc_datasets/propOcc_', datasetID, '.csv', sep = ''),
          row.names = F)



##########################################
# Site summary

summary = data.frame(datasetID = datasetID, 
                     site = c("Saskatoon", "Lethbridge"),
                     spRich = c(length(saskatoon), length(lethbridge)),
                     nTime = 22,
                     meanAbundance = NA)

write.csv(summary, paste('data/siteSummaries/siteSummary_', datasetID, '.csv', sep = ''),
          row.names = F)


#########################################


dataFormattingTable[,'Raw_nSpecies'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Raw_nSpecies', 
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 max(summary$spRich))


dataFormattingTable[,'Formatted_nSpecies'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Formatted_nSpecies', 
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 max(summary$spRich))

dataFormattingTable[,'Raw_Mean_Individuals_perSiteYear'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Raw_Mean_Individuals_perSiteYear', 
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 NA)

dataFormattingTable[,'Formatted_Mean_Individuals_perSiteYear'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Formatted_Mean_Individuals_perSiteYear', 
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 NA)

dataFormattingTable[,'General_notes'] = 
  dataFormattingTableFieldUpdate(datasetID, 'General_notes', 
                                 
                                 #--! PROVIDE INFO !--#                                 
                                 'Only frequency of occupancy levels provided')

# And write the final data formatting table:

write.csv(dataFormattingTable, 'data_formatting_table.csv', row.names = F)


# Remove all objects except for functions from the environment:
rm(list = setdiff(ls(), lsf.str()))



