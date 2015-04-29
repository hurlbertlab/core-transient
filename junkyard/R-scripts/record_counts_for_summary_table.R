# Get the summary data table:

outSummary = read.csv('data_source_table.csv')

# Filling in the data table:

# Function to get the summary stats for a given record:

rec_summary = function(datasetID){
  rec_loc = paste('formatted_datasets/dataset_',
                  datasetID,'.csv', sep='')
  d = read.csv(rec_loc)
  nRecs = dim(d)[1]
  nSites = length(unique(d$site))
  nTime = length(unique(d$year))
  nSp = length(unique(d$species))
  df = data.frame(dataset = datasetID, nRecs, nSites, nTime, nSp)
  return(df)
}

# To get the summary stats across formatted datasets:

# Gather the formatted datasets:

datasets = list.files('formatted_datasets', pattern="*.csv", full.names=T)

rec_summari = function(i){
  d = read.csv(datasets[i])
  nRecs = dim(d)[1]
  dataset = unique(d$dataset)
  nSites = length(unique(d$site))
  nTime = length(unique(d$year))
  nSp = length(unique(d$species))
  df = data.frame(dataset, nRecs, nSites, nTime, nSp)
  return(df)
}

outList = list()
for(i in 1:length(datasets)) outList[[i]] = rec_summari(i)

# Make into a dataframe and sort:

outDf = rbind.fill(outList)

# Get dataframes of just matching and non-matchings summary records:

outSummary_match = outSummary[which(outSummary$dataset_ID %in% outDf$dataset),]

outSummary_match[,17:20] = outDf[,2:5]

# Bind with a dataframe of non-matching records:

outSummary_nomatch = outSummary[-which(outSummary$dataset_ID %in% outDf$dataset),]

outS = rbind(outSummary_nomatch, outSummary_match)

# Order by record ID:

outS = outS[order(outS$dataset_ID),]

# Write to file:

write.csv(outS, 'data_source_table.csv', row.names = F)

  





