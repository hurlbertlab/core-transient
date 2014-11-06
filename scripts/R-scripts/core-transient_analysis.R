#This script calculates occupancy distributions and core and occasional species richness

#----------------------------------------------------------------------------------*
# ---- Libraries ----
#==================================================================================*

library(plyr)

#----------------------------------------------------------------------------------*
# ---- Get data ----
#==================================================================================*

# Set read and write directories:

in_dir = 'formatted_datasets'
out_dir = 'output'

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
# ---- Function create proportion of years by species data frame ----
#==================================================================================*

prop.by.year = function(dataset, site){
  d = d[d$datasetID == dataset & d$site == site,] # Subsets data by dataset & site
  sp = unique(d$species)        # Generates a species list
  years = sort(unique(d$year))
  yrs = length(years)  # Generates a list of years
  # For loop to calculate the proportion of years a species has been observed:
  prop.yrs = numeric()
  for (i in 1:length(sp)){                        
    prop.yrs[i] = length(unique(d[d$species == sp[i],'year']))/yrs
  }
  prop.df = data.frame(sp,prop.yrs)  # Dataframe of species and proportion of years
    list.out = list(prop.df,yrs)
      names(list.out) = c('prop.df','years')
  return(list.out)
  }

#----------------------------------------------------------------------------------*
# ---- Functions for calculating bimodality ----
#==================================================================================*
# Note: bimodality is the fraction of species occurring at either end of occupancy
# distribution

bimodality = function(occs, yrs) {
  maxvar = var(c(rep(1/yrs,floor(length(occs)/2)),
                 rep(1,ceiling(length(occs)/2))))
  return(var(occs)/maxvar)
  }

#----------------------------------------------------------------------------------*
# ---- Function for fitting the beta distribution ----
#==================================================================================*
# Note: From meeting with Allen, 10/30/14, not currently tested within this script.

fitbeta = function(dataID, not1) {
  occs = out.list[[1]][[dataID]]$prop.yrs
  occs[occs == 1] = not1
  shape.params = fitdistr(occs, "beta", list(shape1 = 2, shape2 = 2))
  return(as.vector(shape.params$estimate))
  }

#----------------------------------------------------------------------------------*
# ---- Function to generate output summary dataset ----
#==================================================================================*
# Note the output of this function is a one line data frame.

ctSummary = function(dataset, site, prop.df, yrs, threshold){
  # Calculate bimodality of the dataset and site:
    bimodal = bimodality(prop.df$prop.yrs, yrs)
  # Subset into core and transient:
    core.thresh = 1 - threshold   # Threshold for core species
    trans.thresh = threshold      # Threshold for transient species
    sp.core = prop.df[prop.df$prop.yrs>=core.thresh,]
    sp.trans = prop.df[prop.df$prop.yrs<=trans.thresh,]
  # Assign species to core or transient status:
    prop.df$CT = ifelse(prop.df$prop.yrs>=core.thresh,'core',
                 ifelse(prop.df$prop.yrs<=trans.thresh,'transient',NA))
  # Merge with the original data frame:
    d = merge(d, prop.df, by.x = 'species', by.y = 'sp')[,-6]
  # Calculate richness indices:
    rich.total = length(unique(d[,1]))  
    rich.core = length(unique(d[d$CT=='core',1]))
    rich.trans = length(unique(d[d$CT=='transient',1]))
    prop.core = rich.core/rich.total
    prop.trans = rich.trans/rich.total
    summary.out = summary.table[summary.table[,1] == dataset,c(9,11)]
  # Output
    out = data.frame(dataset, site, threshold,summary.out[,1], summary.out[,2], yrs,
              rich.total, rich.core, rich.trans, 
              prop.core, prop.trans, bimodal, mean(prop.df$prop.yrs))
    names(out) = c('datasetID','site','threshold','system','taxa', 'yrs',
                    'total_richness','core_richness','trans_richness',
                    'prop_core','prop_trans', 'bimodality','mean')
    return(out)
    }

#----------------------------------------------------------------------------------*
# ---- Function to return core-transient summary analysis ----
#==================================================================================*

coreTrans = function(dataset, site, threshold){
  # Extract proportional data frame:
    prop.list = prop.by.year(dataset, site)
    prop.df = prop.list[['prop.df']]
    yrs = prop.list[['years']]
  # Summary table output:
    outSummary = ctSummary(dataset, site, prop.df, yrs, threshold)
  # Output
    list(prop.df,outSummary)
  }

#----------------------------------------------------------------------------------*
# ---- Generate output across sites  ----
#==================================================================================*

# Generate output lists across sites + datasets:

sites = unique(d$site)
dID = numeric()
out.raw = list()  # dataframe of 
out.frame = list()

for(i in 1:length(sites)){
  dID[i] = unique(d[d$site == sites[i],'datasetID'])
  out.raw[[i]] = coreTrans(dID[i],sites[i],.33)[[1]]
  out.frame[[i]] = coreTrans(dID[i],sites[i],.33)[[2]]
}

# Make the output summary frame into a single dataframe:

out.frame = rbind.fill(out.frame)

# Bind outputs into a single list:

out.list = list(out.raw, out.frame)
  names(out.list) = c('raw_output', 'summary_output')
  

# Write the tabular summary data output:

write.csv(out.frame, paste(out_dir,'/tabular_data/out_frame.csv', sep = ''), row.names = F)




