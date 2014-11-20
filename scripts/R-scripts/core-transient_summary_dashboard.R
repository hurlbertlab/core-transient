###################################################################################*
# ---- CORE-TRANSIENT SUMMARY DASHBOARD ----
###################################################################################*
# This file is used as a "dashboard" to observe / produce summary output from core-
# transient analyses. Output includes summary tables and plots. Functions are
# located in the core-transient_functions.R source file.

#----------------------------------------------------------------------------------*
# ---- Set-up ----
#==================================================================================*

# If a dataset was changed or added, the script that creates the proportional and
# nTime dataframes must be rerun:

source('scripts/R-scripts/ct_proportion_frame.R')

# Get files:

prop.df = read.csv('output/prop.df.csv')
nTime = read.csv('output/Ntime.df.csv')
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

site = unique(prop.df$site)
threshold = 1/3
out.list = list()

for (i in site){
  out.list[[i]] = sampling(i, threshold)
}

ss = rbind.fill(out.list)

# Remove sites with < 10 species and < 5 time intervals:

samplingSummary = ss[ss$rich.total > 10 & ss$nTime > 5,]

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

for (i in site){
  out.list[[i]] =  ctSummary(i, threshold, reps)
}

ct = rbind.fill(out.list)

# ---- Write core-transient summary table to file ----

write.csv(ct, 'output/tabular_data/core-transient_summary.csv', row.names = F)

#----------------------------------------------------------------------------------*
# ---- Mode table ----
#----------------------------------------------------------------------------------*
# Input is the cut-ff for core- and transient designation and the number of reps
# used to calculate the p-value. The proportional dataframe must be loaded into the
# environment above and the sampling summary code above must be run prior to 
# running this script.

site = factor(samplingSummary$site)
threshold = 1/3
reps = 1000

out.list = list()

for (i in site){
  out.list[[i]] =  mode.summary(i, reps)
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
    out.plots[[i]] = ct.hist(i)
  }

# Write plots to file:

pdf('output/plots/CT_histograms.pdf', 
    width = 6.5, height = 5.5, onefile = T)
out.plots
dev.off()

