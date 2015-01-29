d = read.csv('data/formatted_datasets/dataset_249.csv')

occFun = function(dataSite, sp){
  dataSiteSp = subset(dataSite, species == sp)
  numSpYrs = length(unique(dataSiteSp$year))
  numSiteYrs = length(unique(dataSite$year))
  propOcc = numSpYrs/numSiteYrs
  return(propOcc)
  }

propOccFun = function(dataset, siteName){
  siteNames = unique(dataset$site)
  outList = list(length = length(siteNames))
  for(i in 1:length(siteNames)){
    # Data subsets for calculate occupancy by site:
      dataSite = subset(d, site == siteNames[i])        
      sp = factor(unique(dataSite$species))
      datasetNum = rep(unique(dataSite$datasetID), length(sp))
    # For loop to calculate the proportion of years a species has been observed:
      occPropSp = numeric(length = length(sp))
      for (j in 1:length(sp)){                        
        occPropSp[j] = occFun(dataSite, as.character(sp[j]))
      }
    occProp = data.frame(datasetNum, site, sp, occPropSp, row.names = NULL)
  }
    return(occProp)
}
