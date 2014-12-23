###################################################################################*
# ---- CORE-TRANSIENT SUMMARY DASHBOARD ----
###################################################################################*
# This file is used as a "dashboard" to observe / produce summary output from core-
# transient analyses. Output includes summary tables and plots. Functions are
# located in the core-transient_functions.R source file.

proc.replaceFun(78)

#----------------------------------------------------------------------------------*
# ---- Set-up ----
#==================================================================================*
# The source script that checks for new datasets and adds/writes them to the prop 
# and nTime frames if necessary:

source('scripts/R-scripts/ct_proportion_frame.R')

# Get files:

occProp = read.csv('output/occProp.csv')
nTime = read.csv('output/nTime.csv')
outSummary = read.csv('data_source_table.csv')

# Source core-transient functions:

source('scripts/R-scripts/core-transient_functions.R')

# Load libraries:

library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(MASS)

#==================================================================================*
#  ---- SUMMARY TABLE OUTPUT ----
#==================================================================================*

#----------------------------------------------------------------------------------*
# ---- Sampling summary ----
#----------------------------------------------------------------------------------*

# Calculate the summary statistics (all summary data with the exception of
# bimodality across sites for sites that meet our sampling criteria). Input is 
# the core-transient threshold. The proportional dataframe must be loaded into the
# environment above.

site = unique(occProp$site)
# site = site[50:51]
threshold = 1/3
out.list = list()

for (i in 1:length(site)){
  out.list[[i]] = summaryStats(site[i], threshold)
}

ss = rbind.fill(out.list)

# Remove sites with < 10 species and < 5 time intervals:

samplingSummary  = ss[ss$rich.total >= 10 & ss$nTime >= 5,]

samplingSummary = na.omit(samplingSummary)

# Something is wrong here! showing lots of NA's. Not sure the cause, will have to
# explore this tomorrow (it is an improvement over the last summary file -- which
# contained only 157 records -- this one contains 474 records)

# Subset the nTime frame to include just the appropriate samples:

sites2 = data.frame(site = samplingSummary$site)
nTime = merge(nTime, sites2, all = F)
nTime$site = factor(nTime$site)

#----------------------------------------------------------------------------------*
# ---- Core-transient summary table ----
#----------------------------------------------------------------------------------*
# Input is the cut-ff for core- and transient designation and the number of reps
# used to calculate the p-value. The proportional dataframe must be loaded into the
# environment above and the sampling summary code above must be run prior to 
# running this script.

site = factor(samplingSummary$site)
threshold = 1/3
reps = 1000

out.list = list()

for (i in 1:length(site)){
  tryCatch({
    out.list[[i]] =  ctSummary(site[i], threshold, reps)
  }, error=function(e){
    cat('ERROR for site',site[i],':', conditionMessage(e), '\n')
    })
}

ct = rbind.fill(out.list)

# ---- Write core-transient summary table to file ----

write.csv(ct, 'output/tabular_data/core-transient_summary.csv', row.names = F)

#----------------------------------------------------------------------------------*
# ---- Mode table ----
#----------------------------------------------------------------------------------*
# Input is the cut-ff for core- and transient designation and the number of reps
# used to calculate the p-value. The proportional dataframe must be loaded into the
# environment and the sampling summary code must be run prior to running this script.
# WARNING: This takes a very long time to run!

site = factor(samplingSummary$site)
threshold = 1/3
reps = 1000

out.list = list()
   
for (i in 1:length(site)){
  tryCatch({
  out.list[[i]] =  mode.summary(site[i], reps)
  }, error = function(e){
    cat('ERROR for site',site[i],':', conditionMessage(e), '\n')
  })
}

modeSummary = rbind.fill(out.list)

write.csv(modeSummary, 'output/tabular_data/ct_mode_summary.csv', row.names = F)

#==================================================================================*
# ---- PLOTS ----
#==================================================================================*
# Plot outputs include:
# 1. Core-transient histogram

#----------------------------------------------------------------------------------*
# ---- Core-transient histogram  ----
#----------------------------------------------------------------------------------*
# This script (ultimately) creates a single pdf with all histograms of sites in 
# which the number of samples is adequate for core-transient analyses. The
# data loading code at the head of this document must be run prior to executing 
# this script. It is NOT necessary to to run dataset summary table codes.

# Get core-transient summary table (if not already in environment):

ct = read.csv('output/tabular_data/core-transient_summary.csv')

# Core-transient histogram for a given site and cut-off (example is Eastern Wood):

ct.hist('d226_ew')

# Run a for loop to create plots for each site (output as list, only runs
# when the proportion of core sites are < 1):

# Create a vector of sites with < 1 proportion of core species:

site = ct[ct$prop.core < 1,'site']

out.plots = list()
  for(i in site){
    tryCatch({
      out.plots[[i]] = ct.hist(i)},
      error = function(e){
        cat('ERROR for site',site[i],':', conditionMessage(e), '\n')
      })
  }

# Write plots to file:

pdf('output/plots/CT_histograms.pdf', 
    width = 6.5, height = 5.5, onefile = T)
out.plots
dev.off()
