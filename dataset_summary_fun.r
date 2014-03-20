# Function for pulling out summary stats and plots for core-transient datasets

ct_summary = function(dataset, siteID, spID, timeID) {
  
  data = unique(dataset[, c(siteID, spID, timeID)])
  
  # Mean and cumulative richness per site
  site.rich = data.frame(table(data[, c(spID, timeID)]))
  site.rich = site.rich[site.rich$Freq != 0, ]
}