###################################################################################*
# ---- CORE-TRANSIENT SUMMARY DASHBOARD ----
###################################################################################*
# This file is used as a dashboard to observe / produce summary output from core-
# transient analyses. Output includes summary tables and plots. Functions are
# located in the core-transient_functions.R source file.

#----------------------------------------------------------------------------------*
# ---- Set-up ----
#==================================================================================*

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

#----------------------------------------------------------------------------------*
#  ---- SUMMARY TABLE OUTPUT ----
#==================================================================================*

#p.bimodal('d246_25',1000)

# ---- Core-transient summary table ----
# Input is the cut-ff for core- and transient designation

ct = coreTrans(1/3)

# ---- Write core-transient summary table to file ----

write.csv(ct, 'output/tabular_data/core-transient_summary.csv', row.names = F)

#----------------------------------------------------------------------------------*
# ---- PLOTS ----
#==================================================================================*
# Plot outputs include:
# 1. Core-transient histogram

#----------------------------------------------------------------------------------*
# ---- Core-transient histogram  ----
#----------------------------------------------------------------------------------*

# Create a vector of sites:

sites = unique(prop.df$site)

# Core-transient histogram for a given site and cut-off (example is Eastern Wood):

ct.hist('d226_ew',10)

# Run a for loop to create plots for each site (output as list):
# NOTE! THERE ARE WARNINGS HERE ... EXPLORE WHY!!!

out.plots = list()
  for(i in sites){
    out.plots[[i]] = ct.hist(i,.33)
  }

# Write plots to file:

pdf('output/plots/CT_histograms.pdf', 
    width = 6.5, height = 5.5, onefile = T)
out.plots
dev.off()

