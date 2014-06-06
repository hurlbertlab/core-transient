# This code gathers the data from the core-transient-datasets site, calculates summary information for
# the data.

ct_summary = function(dataset, storageFormat, siteID, spID, timeID) {
  # Get data from the core-transient-datasets GitHub site:
    if (storageFormat == 'sql') 
      {addsqlcodehere_see_package_RODBC}
    else 
      {url = 'https://raw.githubusercontent.com/hurlbertlab/core-transient-datasets/master/'
       df = read.csv(paste(url, dataset, sep ='')) }
  # Basic summary information for dataset:
    data = unique(df[, c(siteID, spID, timeID)])
    num.sites = length(unique(df[, siteID]))
    num.spp = length(unique(df[, spID]))
    num.times = length(unique(df[, timeID]))
    summary.frame = data.frame(dataset, num.sites,num.spp,num.times)
      colnames(summary.frame) = c('dataset','num.sites','num.spp','num.times')
  # Mean richness per site:
    site.yr.rich = data.frame(table(data[, c(timeID, siteID)]))
    site.yr.rich = site.yr.rich[site.yr.rich$Freq != 0, ]
    mean.site.rich = aggregate(site.yr.rich$Freq, by = list(site.yr.rich$stateroute), mean)
      names(mean.site.rich) = c(siteID, 'Richness')
  # Time samples per site
    site.times = unique(data[, c(siteID, timeID)])
    site.time.count = data.frame(table(site.times[, siteID]))
    names(site.time.count) = c(siteID, 'Times')
  # Number of sites and number of years per species
    sp.sites = unique(data[, c(siteID, spID)])
    sp.site.count = data.frame(table(sp.sites[, spID]))
    sp.years = unique(data[, c(spID, timeID)])
    sp.year.count = data.frame(table(sp.years[, spID]))
    sp.count = merge(sp.site.count, sp.year.count, by = 'Var1', all = T)
    names(sp.count) = c(spID, 'NumSites', 'NumTimes')
  # Ouput list:
    outList = list(data,summary.frame, site.yr.rich, 
                 mean.site.rich, site.time.count, sp.count)
    names(outList) = c('data','summary.frame','site.yr.rich',
                     'mean.site.rich','site.time.count','sp.count')
    outList
  }

#===============================
# As an example of the function:
#===============================

funTest = ct_summary('test_dataset.csv','csv', 'stateroute','Aou','Year')

names(funTest)

#-----------------
# Example output:
#-----------------

# Summary frame (1 line), that I picture could be rbinded across datasets:

funTest[['summary.frame']]

# Richness by site and year:

funTest[['site.yr.rich']]

funTest[['mean.site.rich']]

# How many times was each site counted?

funTest[['site.time.count']]

# The data for the c-t analysis:

funTest[['sp.count']]

##############################################################################
# END ... THOUGHTS?
##############################################################################

