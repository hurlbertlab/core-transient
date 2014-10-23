#This script calculates occupancy distributions and core and occasional species richness


###################################################################################*
# ---- SET-UP ----
###################################################################################*

#----------------------------------------------------------------------------------*
# ---- Libraries ----
#==================================================================================*

library(plyr)
library(ggplot2)

#----------------------------------------------------------------------------------*
# ---- Get data ----
#==================================================================================*

# Set read and write directories:

in_dir = 'formatted_datasets'

# Get summary table:

summary.table = read.csv('data_source_table.csv')

# Gather all files in directory:

datasets = list.files(in_dir, pattern="*.csv", full.names=T)
data.list = lapply(datasets, read.csv)

# The following function and line of code will change field names from density
# to count to ensure equivalent names (for rbinding):

name.changer = function(x){
  if (names(x)[5] == 'density') names(x)[5] = 'count'
  x
}

data.list = lapply(data.list, name.changer)

# Bind the list into a single dataframe that includes all datasets:

d = rbind.fill(data.list)

#----------------------------------------------------------------------------------*
# ---- Functions for calculating bimodality ----
#==================================================================================*
# Note: bimodality is the fraction of species occurring at either end of occupancy
# distribution

bimod = function(occs,lo=1/3, hi=2/3) {
  return(sum(occs <= lo | occs>hi)/length(occs))
  }

bimodality = function(occs) {
  maxvar = var(c(rep(min(occs),floor(length(occs)/2)),
                 rep(max(occs),ceiling(length(occs)/2))))
  return(var(occs)/maxvar)
}

#----------------------------------------------------------------------------------*
# ---- Function to return core-transient summary analysis ----
#==================================================================================*

coreTrans = function(dataset, site, threshold){
  d = d[d$datasetID == dataset & d$site == site,] # Subsets data by dataset & site
  sp = unique(d$species)        # Generates a species list
  years = sort(unique(d$year))
  yrs = length(years)  # Generates a list of years
  # For loop to calculate the proportion of years a species has been observed:
    prop.yrs = numeric()
    for (i in 1:length(sp)){                        
      prop.yrs[i] = length(unique(d[d$species == sp[i],'year']))/yrs
    }
    d1 = data.frame(sp,prop.yrs)  # Dataframe of species and proportion of years
  # Subset into core and transient:
    core.thresh = 1 - threshold   # Threshold for core species
    trans.thresh = threshold      # Threshold for transient species
    sp.core = d1[d1$prop.yrs>=core.thresh,]
    sp.trans = d1[d1$prop.yrs<=trans.thresh,]
  # Assign species to core or transient status:
    d1$CT = ifelse(d1$prop.yrs>=core.thresh,'core',
                  ifelse(d1$prop.yrs<=trans.thresh,'transient',NA))
  # Merge with the original data frame:
    d = merge(d, d1, by.x = 'species', by.y = 'sp')[,-6]
  # Function to calculate richness indices and output it as a 1-row dataframe:
    rich.indis = function(dataset, site, threshold){
      rich.total = length(unique(d[,1]))  
      rich.core = length(unique(d[d$CT=='core',1]))
      rich.trans = length(unique(d[d$CT=='transient',1]))
      prop.core = rich.core/rich.total
      prop.trans = rich.trans/rich.total
      summary.out = summary.table[summary.table[,1] == dataset,c(9,11)]
      out = data.frame(dataset, site, threshold,summary.out[,1], summary.out[,2],
                       rich.total, rich.core, rich.trans, 
                      prop.core, prop.trans)
      names(out) = c('datasetID','site','threshold','system','taxa',
                     'total_richness','core_richness','trans_richness',
                     'prop_core','prop_trans')
      out
      }
  # Calculate richness indices across years for the study:
    r.across.years = rich.indis(dataset, site, threshold)
  # Graphical output: 
    # Histogram across years
      site.histogram = ggplot(d1, aes(x=prop.yrs)) + 
        geom_histogram(aes(y=..density..), binwidth=.1,
                       colour="black", fill="gray") + 
        geom_density(alpha=.2, fill="blue") + 
        labs(title= paste('Proportional density:',site),
             x = 'Proportion of years', y = 'Density of species/year') +
        theme(axis.text = element_text(size=14, color = 'black'),
              axis.title = element_text(size=18),
              title = element_text(size=22),
              axis.line = element_line(colour = "black"),
              panel.background = element_blank())
  # Output
    list(r.across.years, site.histogram)
}


# Summary table across all sites and datasets:

sites = unique(d$site)
dID = numeric()
out.frame = list()

for(i in 1:length(sites)){
    dID[i] = unique(d[d$site == sites[i],'datasetID'])
    out.frame[[i]] = coreTrans(dID[i],sites[i],.33)[[1]]
  }

out.frame = rbind.fill(out.frame)

# Graphical output across all sites and datasets:

out.plots = list()

for(i in 1:length(sites)){
  dID[i] = unique(d[d$site == sites[i],'datasetID'])
  out.plots[[i]] = coreTrans(dID[i],sites[i],.33)[[2]]
}


# Summary data by dataset:

out.by.datasets = ddply(out.frame, .(datasetID), summarize, 
      mean.prop.core = mean(prop_core), 
      se.prop.core = sd(prop_core)/sqrt(length(prop_core)),
      mean.prop.core = mean(prop_trans), 
      se.prop.core = sd(prop_trans)/sqrt(length(prop_trans)))

# Output:

out.frame

out.by.datasets




