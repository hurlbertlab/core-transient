# This script formats Dornelas datasets into the same format as the others.

#----------------------------------------------------------------------------------*
# ---- Set-up ----
#==================================================================================*

# Libraries:

library(plyr)

# Set read and write directories:

datasets = list.files('raw_datasets/dornelas', pattern="*.csv", full.names=T)
data.list = lapply(datasets, read.csv)

test = data.list[[60]]

dornelas.formatter = function(i){
  df = data.list[[i]]
  # Remove X column and put order of formatted data:
  df = df[,c(2,4:5,3,6)] 
  # Maintain site naming convention:
  df[,2] = paste('d',df[,1], '_', df[,2],sep = '') 
  # Set column names:
  names(df) = c('datasetID','site','species','year','count')
  # Write to file:
  write.csv(df, paste('formatted_datasets/dataset_', unique(df$datasetID),
                      '.csv', sep = ''), row.names =F)
}

for (i in 1:length(data.list)) dornelas.fomatter(i)

