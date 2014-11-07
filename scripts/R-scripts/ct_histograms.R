# This script creates histograms of each site with density lines, bimodality index,
# mean, and number of time samples.

#----------------------------------------------------------------------------------*
# ---- Set-up ----
#==================================================================================*

# Get files:

prop.df = read.csv('output/prop.df.csv')
nTime = read.csv('output/Ntime.df.csv')
outSummary = read.csv('data_source_table.csv')

# Source core-transient functions:

source('scripts/R-scripts/tokeshi_function.R')

library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)

#----------------------------------------------------------------------------------*
# ---- Dashboard: Generate plot output  ----
#==================================================================================*

# Histogram for a given site, and threshold:

ct.hist('d226_ew', .33)

# Generate output lists across sites + datasets:

sites = unique(prop.df$site)

# Plot across sites, output as one pdf: 

out.plots = list()
for(i in 1:length(sites)){
  out.plots[[i]] = ct.hist(sites[i],.33)
}

pdf('output/plots/CT_histograms.pdf', 
    width = 6.5, height = 5.5, onefile = T)
  out.plots
dev.off()

