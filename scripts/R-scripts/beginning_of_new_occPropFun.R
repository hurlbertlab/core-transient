d = read.csv('data/formatted_datasets/dataset_249.csv')

propOccFun = function(dataset){
  siteNames = unique(dataset$site)
  occPropOutList = list(length = length(siteNames))
  # For each of the sites in the dataset ...
  for(i in 1:length(siteNames)){
    # Subset to calculate occupancy by site:
      dataSite = subset(d, site == siteNames[i])        
      sp = factor(unique(dataSite$species))
      datasetNum = rep(unique(dataSite$datasetID), length(sp))
    # For each species ...
    # Calculate the proportion of years a species has been observed:
      occPropSp = numeric(length = length(sp))
      for (j in 1:length(sp)){
        dataSiteSp = subset(dataSite, species == as.character(sp[j]))
        occPropSp[j] = length(unique(dataSiteSp$year))/length(unique(dataSite$year))
      }
    occPropOutList[[i]] = data.frame(datasetNum, siteNames[i], sp, occPropSp, row.names = NULL)
  }
  occPropDf = rbind.fill(occPropOutList)
    return(occPropDf)
}

t2 = propOccFun(d)

siteEliminatorFun = function(occPropDf){
  occPropDfSubSp = 
}
