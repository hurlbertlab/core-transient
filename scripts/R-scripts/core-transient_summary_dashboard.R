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

# site = ct[ct$prop.core < 1,'site']
site = ct[,'site']

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

#==================================================================================*
# ---- SUMMARIZING BY TAXA AND SYSTEM (TERRESTRIAL, AQUATIC, MARINE)  ----
#==================================================================================*

# Get ct file, replacing site with a numeric vector (just for simplifying):

ct = read.csv('output/tabular_data/core-transient_summary.csv')

ct$site = seq(1, length(ct$site),1)

# Some functions:

se = function(x) sd(x)/sqrt(length(x))

#----------------------------------------------------------------------------------*
# ----  Method 1: Considering site as the sampling unit ----
#==================================================================================*

propCoreSys = ddply(ct, .(system), summarize, 
              meanPropCore = mean(prop.core), sePropCore = se(prop.core),
              meanPropTrans = mean(prop.trans), sePropTrans = se(prop.trans),
              meanMu = mean(mu), seMu = se(mu),
              meanBimodal = mean(bimodal), seBimodal = se(bimodal),
              meanAlpha = mean(alpha), seAlpha = se(alpha),
              meanBeta = mean(beta), seBeta = se(beta))

propCoreTaxa = ddply(ct, .(taxa), summarize, 
                    meanPropCore = mean(prop.core), sePropCore = se(prop.core),
                    meanPropTrans = mean(prop.trans), sePropTrans = se(prop.trans),
                    meanMu = mean(mu), seMu = se(mu),
                    meanBimodal = mean(bimodal), seBimodal = se(bimodal),
                    meanAlpha = mean(alpha), seAlpha = se(alpha),
                    meanBeta = mean(beta), seBeta = se(beta))

propCoreSys = cbind(variable = rep('system', length(propCoreSys[,1])),
                    group = propCoreSys[,1],
                    propCoreSys[,2:length(propCoreSys)])

propCoreTaxa = cbind(variable = rep('taxa', length(propCoreTaxa[,1])),
                                   group = propCoreTaxa[,1],
                    propCoreTaxa[,2:length(propCoreTaxa)])

summary_SysTaxa = rbind(propCoreSys, propCoreTaxa)

write.csv(rbind(propCoreSys, propCoreTaxa),
          'output/tabular_data/summary_by_SysTaxa.csv', row.names = F)

#----------------------------------------------------------------------------------*
# ---- Method 2: Considering species observations as the sampling unit ----
#==================================================================================*

ct = read.csv('output/tabular_data/core-transient_summary.csv')

ctSub = ct[,c(1:5)]

occSysTaxa = merge(ctSub, occProp, by = 'site',all = T)
occSysTaxa = na.omit(occSysTaxa)

#----------------------------------------------------------------------------------*
# ---- A little bit of exploration ... time effect? ----
#----------------------------------------------------------------------------------*
# Plot of species occurence by the number of time intervals

ggplot(occSysTaxa, aes(x = nTime, y = occ, color = taxa, shape = system)) +
  geom_point()+
  scale_shape_manual(values=c(15,16,17))+
  xlab('Number of time intervals')+
  ylab('Proportion of occurences')+
  ggtitle('Occurences by time\n(points = species at a given site, n = 16303)')+
  theme(axis.text = element_text(size=14, color = 1),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 1),
        title = element_text(size=16, vjust = 3),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        plot.margin = unit(c(1.5,.5,1.5,1), "lines"))

# How much of the variation at a given site is explained by time (i.e., do sp occurences
# become more dispersed with increased time samples?)?

ost2 = ddply(occSysTaxa, .(site, system, taxa, nTime), 
             summarize, dOcc = mean(abs(.5-occ)))

ggplot(ost2, aes(x = nTime, y = dOcc, color = taxa, shape = system)) +
  geom_point()+
  scale_shape_manual(values=c(15,16,17))+
  xlab('Number of time intervals')+
  ylab('Site-level bimodality')+
  ggtitle('Bimodality by time\n(points = sites, n = 539)')+
  theme(axis.text = element_text(size=14, color = 1),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 1),
        title = element_text(size=16, vjust = 3),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        plot.margin = unit(c(1.5,.5,1.5,1), "lines"))

# How much can the existence of core or transient species be explained by the
# number of time intervals?

coreOrTransFun = function(x) length(x)/length(x)

ostCT = ddply(occSysTaxa, .(site, system, taxa, nTime), summarize, 
      ct = length(occ[occ>=2/3|occ<=1/3])/length(occ))

ostC = ddply(occSysTaxa, .(site, system, taxa, nTime), summarize, 
              ct = length(occ[occ>=2/3])/length(occ))

ostT = ddply(occSysTaxa, .(site, system, taxa, nTime), summarize, 
              ct = length(occ[occ<=1/3])/length(occ))

# How much can bimodality  be explained by the
# number of time intervals?

bimodSysTax = ddply(ct, .(site, system, taxa, nTime), 
                    summarize, bm = mean(bimodal))

plot(bimodal~nTime, data = ct)

ggplot(ct, aes(x = nTime, y = bimodal, color = taxa, shape = system)) +
  geom_point()+
  scale_shape_manual(values=c(15,16,17))+
  xlab('Number of time intervals')+
  ylab('Mean absolute difference between\n0.5 and proportional occurence')+
  ggtitle('Variation in occurences by time\n(points = sites, n = 539)')+
  theme(axis.text = element_text(size=14, color = 1),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 1),
        title = element_text(size=16, vjust = 3),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        plot.margin = unit(c(1.5,.5,1.5,1), "lines"))

summary(lm(bimodal~nTime, data = ct))

#----------------------------------------------------------------------------------*
# ---- Plotting bimodality by system and taxa ----
#----------------------------------------------------------------------------------*

# Bimodality by system:

bimodSys = ddply(ct, .(system), summarize, 
                 mean_bimod = mean(bimodal),
                 sd_bimod = sd(bimodal),
                 se_bimod = se(bimodal),
                 n_sites = length(bimodal))

# Bimodality by taxa:

bimodTaxa = ddply(ct, .(taxa), summarize, 
                 mean_bimod = mean(bimodal),
                 sd_bimod = sd(bimodal),
                 se_bimod = se(bimodal),
                 n_sites = length(bimodal))

# Plot bimodality by system

ggplot(bimodSys, aes(x = system, y = mean_bimod)) +
  geom_point(size = 5)+
  geom_errorbar(aes(ymin = mean_bimod - se_bimod,
                    ymax = mean_bimod + se_bimod),
                    width = .15) + 
  ylim(0.2,.65)+
  xlab('System')+
  ylab('Bimodality')+
  ggtitle(bquote(atop('Bimodality by system',
          'Sites: Aquatic = 11 Marine = 444 Terrestrial = 84')))+
  theme(axis.text = element_text(size=14, color = 1),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 1),
        title = element_text(size=14, vjust = 3),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        panel.grid.major = element_line(size = .5, color = 'gray90'),
        panel.grid.minor = element_line(size = .25, color = 'gray90'),
        plot.margin = unit(c(1.5,.5,1.5,1), "lines"))



