library(dplyr)

#' Get table of species abundances
#' 
get_abund_data  = function(datasetIDs){
  datasetIDs = datasetIDs$dataset_ID
  dataset_path = 'data/standardized_datasets/'
  abund_data = data.frame()
  for (dataset in datasetIDs){
    filename = paste('dataset_', dataset, '.csv', sep = '')
    print(paste("Loading:", filename))
    site_data = read.csv(file.path(dataset_path, filename), stringsAsFactors = FALSE, fileEncoding = 'latin1')
    abund_data = rbind(abund_data, site_data)
  }
  # Strip zeros which are included to document a sampling event occurred
  abund_data = abund_data[abund_data$count != 0,]
  return(abund_data)
}

#' Get table of species proportional occupancies
#' 
get_propocc_data  = function(datasetIDs){
  datasetIDs = datasetIDs$dataset_ID
  dataset_path = 'data/propOcc_datasets/'
  propocc_data = data.frame()
  for (dataset in datasetIDs){
    filename = paste('propOcc_', dataset, '.csv', sep = '')
    print(paste("Loading:", filename))
    site_data = read.csv(file.path(dataset_path, filename), stringsAsFactors = FALSE, fileEncoding = 'latin1')
    propocc_data = rbind(propocc_data, site_data)
  }
  return(propocc_data)
}

#' Calculate turnover (1 - Jaccard similarity) which is the number of species
#' unique to either species list divided by the total number of unique species
turnover = function(splist1, splist2) {
  tot_uniq_sp = length(unique(c(splist1, splist2)))
  shared_sp = length(splist1) + length(splist2) - tot_uniq_sp
  Jturnover = 1 - shared_sp/tot_uniq_sp
  return(Jturnover)
}




#' Get list of dataset IDS for datasets that meet criteria for analysis
#' 
dataformattingtable = read.csv('data_formatting_table.csv')
datasetIDs = dataformattingtable %>%
  filter(format_flag == 1, dataset_ID != 1, 
         countFormat %in% c('count', 'cover', 'density', 'abundance', 'presence', 'biomass')) %>% 
  select(dataset_ID)

abund_data = get_abund_data(datasetIDs)
propocc_data = get_propocc_data(datasetIDs)
all_data = left_join(abund_data, propocc_data, by = c('datasetID', 'site', 'species'))


turnover_output = data.frame()
for (dataset in datasetIDs[,1]) {
  subdata = subset(all_data, datasetID == dataset)
  sites = unique(subdata$site)
  print(paste("Calculating turnover: dataset", dataset))
  for (site in sites) {
    sitedata = subdata[subdata$site == site,]
    notrans = sitedata[sitedata$propOcc > 1/3,]
    years = as.numeric(unique(sitedata$year))
    TJs = c()
    TJ_notrans = c()
    for (year in years[1:(length(years)-1)]) {
      comm1 = unique(sitedata$species[sitedata$year == year])
      comm2 = unique(sitedata$species[sitedata$year == year + 1])
      T_J = turnover(comm1, comm2)
      
      comm1_noT = unique(notrans$species[notrans$year == year])
      comm2_noT = unique(notrans$species[notrans$year == year + 1])
      T_J_notran = turnover(comm1_noT, comm2_noT)
      
      TJs = c(TJs, T_J)
      TJ_notrans = c(TJ_notrans, T_J_notran)
    }
    temp_output = data.frame(datasetID = dataset, site = site, TJ = mean(TJs),
                             TJnotrans = mean(TJ_notrans))
    turnover_output = rbind(turnover_output, temp_output)
  }
}

write.csv(turnover_output, "output/tabular_data/temporal_turnover.csv")
