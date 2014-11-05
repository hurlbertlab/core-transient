#This script calculates occupancy distributions and core and occasional species richness


###################################################################################*
# ---- SET-UP ----
###################################################################################*

#----------------------------------------------------------------------------------*
# ---- Libraries ----
#==================================================================================*

library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)

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
# ---- Function to make core-transient histogram  ----
#==================================================================================*

ct.hist = function(prop.df, outSummary) {
  # Set breaks and band width for the histogram:
  bw = (max(prop.df$prop.yrs)-min(prop.df$prop.yrs))/10
  brks = seq(min(prop.df$prop.yrs), max(prop.df$prop.yrs),bw)
  # Plot labels:
  main = paste('Site ', outSummary[,2], paste('(', outSummary[,4],', ', outSummary[,5],')', sep = ''))
  sub = bquote(b == .(round(outSummary[,12], 2)) ~ '    '~
                 mu == .(round(outSummary[,13],2)) ~ '    '~
                 t == .(outSummary[,6]))
  # Plot data: 
  ggplot(prop.df, aes(x=prop.yrs)) +
    # Add histogram:
    geom_histogram(aes(y = ..density..), breaks = brks, right = F,
                   fill = 'gray', color = 1) +
    # Add density:
    geom_density(alpha=.2, fill="blue") + 
    # Add labels:
    xlab('Proportion of temporal samples') + ylab('Density') + 
    ggtitle(bquote(atop(.(main), atop(.(sub))))) +
    # Add themes:
    theme(axis.text = element_text(size=14, color = 1),
          axis.title.x = element_text(vjust = -1),
          axis.title.y = element_text(vjust = 2),
          title = element_text(size=18, vjust = -1),
          axis.line = element_line(colour = "black"),
          panel.background = element_blank(),
          plot.margin = unit(c(.5,.5,1.5,1), "lines"))
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
  # Graphical output: 
    siteHistogram = ct.hist(prop.df, outSummary)
  # Output
    list(prop.df,outSummary, siteHistogram)
  }

#----------------------------------------------------------------------------------*
# ---- Generate output across sites  ----
#==================================================================================*

# Generate output lists across sites + datasets:

sites = unique(d$site)
dID = numeric()
out.raw = list()  # dataframe of 
out.frame = list()
out.plots = list()

for(i in 1:length(sites)){
  dID[i] = unique(d[d$site == sites[i],'datasetID'])
  out.raw[[i]] = coreTrans(dID[i],sites[i],.33)[[1]]
  out.frame[[i]] = coreTrans(dID[i],sites[i],.33)[[2]]
  out.plots[[i]] = coreTrans(dID[i],sites[i],.33)[[3]]
}

# Make the output summary frame into a single dataframe:

out.frame = rbind.fill(out.frame)

# Bind outputs into a single list:

out.list = list(out.raw, out.frame, out.plots)
  names(out.list) = c('raw_output', 'summary_output','plots')
  
# Add site names to the plot outs

names(out.list[[3]]) = out.frame$site

out.frame$site[1]

# Summary data by dataset:

out.by.datasets = ddply(out.frame, .(datasetID), summarize, 
      mean.prop.core = mean(prop_core), 
      se.prop.core = sd(prop_core)/sqrt(length(prop_core)),
      mean.prop.core = mean(prop_trans), 
      se.prop.core = sd(prop_trans)/sqrt(length(prop_trans)))

# Output:

# Write the tabular summary data output:

write.csv(out.frame, paste(out_dir,'/tabular_data/out_frame.csv', sep = ''), row.names = F)

# Write all of the plot outs:


plot.fun = function(i){
  out_name = paste('/plots/histogram_',out.frame$site[i],'.pdf', sep ='')
  out = paste(out_dir, out_name, sep = '')
  pdf(out, width = 6.5, height = 5.5)
    plot(out.list[[3]][[i]])
  dev.off()
}

for (i in 1:length(out.list[[3]])){
  plot.fun(i)
}




