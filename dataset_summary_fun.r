# Function for pulling out summary stats and plots for core-transient datasets

ct_summary = function(dataset, siteID, spID, timeID) {
  
  data = unique(dataset[, c(siteID, spID, timeID)])
  
  num.sites = length(unique(dataset[, siteID]))
  num.spp = length(unique(dataset[, spID]))
  num.times = length(unique(dataset[, timeID]))
  
  # Mean richness per site
  site.yr.rich = data.frame(table(data[, c(timeID, siteID)]))
  site.yr.rich = site.yr.rich[site.yr.rich$Freq != 0, ]
  mean.site.rich = aggregate(site.yr.rich$Freq, by = list(site.yr.rich$stateroute), mean)
  names(mean.site.rich) = c(siteID, 'Richness')
  
  # Time samples per site
  site.times = unique(dataset[, c(siteID, timeID)])
  site.time.count = data.frame(table(site.times[, siteID]))
  names(site.time.count) = c(siteID, 'Times')
  
  # Number of sites and number of years per species
  sp.sites = unique(dataset[, c(siteID, spID)])
  sp.site.count = data.frame(table(sp.sites[, spID]))
  sp.years = unique(dataset[, c(spID, timeID)])
  sp.year.count = data.frame(table(sp.years[, spID]))
  sp.count = merge(sp.site.count, sp.year.count, by = 'Var1', all = T)
  names(sp.count) = c(spID, 'NumSites', 'NumTimes')
}