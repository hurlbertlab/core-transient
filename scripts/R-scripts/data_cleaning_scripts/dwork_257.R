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
