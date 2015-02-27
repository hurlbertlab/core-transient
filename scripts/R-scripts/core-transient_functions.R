###################################################################################*
# CORE-TRANSIENT FUNCTIONS                                                         *
###################################################################################*
# This script contains all of the functions used in the analyses that summarize
# core-transient data by site (and across sites). It is divided in 3 parts:
#   1. Bimodality summary statistics
#   2. Summary output tables for a given site and across sites
#   3. Plot output

#==================================================================================*
# ---- LOAD LIBRARIES ----
#==================================================================================*

# Load libraries:

library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(MASS)

#==================================================================================*
# ---- GENERAL FUNCTIONS ----
#==================================================================================*

# Standard error:

se = function(x) sd(x)/sqrt(length(x))

#==================================================================================*
# ---- FUNCTIONS for proportional occurance and site summary data frames  ----
#==================================================================================*

# The following function is used to create and explore and extract the species 
# richness and number of time samples for a site.

siteSummaryFun = function(dataset){
  ddply(dataset, .(datasetID, site), summarize, 
        spRich = length(unique(species)), 
        nTime = length(unique(year)))
}

# The following function writes the proportional occurence data
# frame on sites.

propOccFun = function(dataset){
  spTime = ddply(dataset, .(datasetID, site, species), summarize, 
                 spTime = length(unique(year)))
  siteTime = ddply(dataset, .(datasetID, site), summarize, 
                   siteTime = length(unique(year)))
  propOcc = merge(spTime, siteTime)
  propOcc$propOcc = propOcc$spTime / propOcc$siteTime
  return(propOcc[,-c(4:5)])
}

#==================================================================================*
# ---- BASIC DATASET LOADING AND SUMMARIZING ----
#==================================================================================*

# The following function reads in the data and returns a list of the proportional 
# occurence data frame, the site summary (sp richness and number of time samples
# for a given site), system, and taxa:

getDataList = function(datasetID){
  propOcc = read.csv(paste('data/propOcc_datasets/propOcc_', 
                           datasetID, '.csv', sep = ''))
  siteSummary = read.csv(paste('data/siteSummaries/siteSummary_', 
                               datasetID, '.csv',  sep = ''))
  metaData = subset(read.csv('data_source_table.csv'),
                    dataset_ID == datasetID)
  system = metaData$system
  taxa = metaData$taxa
  return(list(propOcc = propOcc, siteSummary = siteSummary, 
              system = system, taxa = taxa))
}

summaryStats = function(datasetID, siteValue, threshold){
  # Get data:
  dataList = getDataList(datasetID)
  
  propOcc = subset(dataList[[1]], site == siteValue)$propOcc
  siteSummary = subset(dataList[[2]], site == siteValue)$propOcc
  siteMetadata = subset(metadata, )
  
  d = occProp[as.character(occProp$site) == site,]
  nTime = nTime[as.character(nTime$site) == site,'nt']
  dst = outSummary[outSummary$dataset_ID == dataset,]
  # Calculate richness indices:
  richTotal = siteSummary$spRich
  richCore = length(propOcc[propOcc >= 1 - threshold])
  richTrans = length(propOcc[propOcc <= threshold])
  # Calculate proportional occurrences:
  propCore = richCore/richTotal
  propTrans = richTrans/richTotal
  mu = mean(propOcc)
  # Output 1-row dataset:
  return(data.frame(dataset, site, system = dst$system, taxa = dst$taxa, nTime,
                    rich.total, rich.core, rich.trans, prop.core, prop.trans, mu))
}


#==================================================================================*
# ---- BIMODALILITY ----
#==================================================================================*
# NOTE: For these functions to run, occProp, Ntime, and outSummary frames must
# already be loaded and the "Sampling summary" lines of code MUST be run in the 
# dashboard!
#
# Functions:
# - bimodality: Calculates the bimodality metric developed by Allen and Ethan. 
#     Inputs: Site
#     Outputs: A single numeric bimodality value
#
# - random.bimodality: The bimodality for a random sample of the dataset.
#     Inputs: Site
#     Outputs: A single numeric bimodality value
#
# - p.bimodal: Randomization test for bimodality. Runs n-reps of the random.
#     bimodality function and compares the actual bimodality with the 
#     distribution of random values.
#     Inputs: Site, number of reps
#     Outputs: A single numeric p-value.
#
# - occs.scaled: Scales occupancy from [0,1] to (0,1) -- because beta distribution
#     inputs must not contain 0's or 1's.
#     Inputs: Site
#     Outputs: A numeric vector of scaled occupancy values.
# 
# - fitbeta: Calculates the shape parameters for a fitted beta distribution.
#     Inputs: Site
#     Outputs: A vector of shape parameters (alpha and beta).
# 
#----------------------------------------------------------------------------------*
# ---- Function for calculating bimodality ----
#==================================================================================*
# Note 1: Bimodality is the fraction of species occurring at either end of 
# occupancy distribution. We use a randomization approach to test whether the 
# distribution is significantly bimodal.
# Note 2: To run this function the number of time samples for the site (nt) needs
# to be specified. This is done so in the wrapper summary table function.

# True bimodality for a given site (or random sample of occurrences at a site)


bimodality = function(site) {
  nTime = subset(siteSummaryFun(dataset), site = site)$nTime
  occs = propOcc$propOcc
  maxvar = var(c(rep(1/nTime,floor(length(occs)/2)),
                 rep(1,ceiling(length(occs)/2))))
  return(var(occs)/maxvar)
}

# Random sample of occurences for a given site (to be used in randomization, below):

random.occs = function(site){
  nt = nTime[as.character(nTime$site) == site,'nt']
  occs = occProp[as.character(occProp$site) == site,'occ']
  t1 = data.frame(table(occs))                      # Occurence prop and frequency
  occ = data.frame(occs = seq(1/nt, 1, length = nt)) # Possible occurence props
  t2 = merge(occ, t1, all.x = T)  # Occurence by possible proportions
  t2[is.na(t2[,2]),2]<-0                            # Replace NA's with zeros
  # Reassign bin values randomly and add to frame:
  new.freq = sample(t2$Freq, length(t2[,1]))
  t3 = data.frame(t2[,1], new.freq)
  # Create new occurence vector:
  r.occs=unlist(apply(t3, 1, function(x) rep(x[1], x[2])))
  return(as.vector(r.occs))
}

# Randomization test for bimodality:

p.bimodal = function(site, reps){
  nt = nTime[as.character(nTime$site) == site,'nt']
  actual.bimod = bimodality(occProp[as.character(occProp$site) == site,'occ'], site)
  # For loop to get random bimodality values
  r.bimod = numeric(length = reps)
  for (i in 1:reps){
    r.bimod[i] = bimodality(random.occs(site), site)
  }
  # Calculate the p-value (proportion of sites with higher bimodality than the
  # actual bimodality value):
  sum(r.bimod >= actual.bimod)/(reps + 1)
}

#----------------------------------------------------------------------------------*
# ---- Function for fitting the beta distribution ----
#==================================================================================*
# Required packages = MASS

# Scale occupancy from [0,1] to (0,1) following Smithson and Verkuilen 2006
# Note: See supplemental at
# http://supp.apa.org/psycarticles/supplemental/met_11_1_54/met_11_1_54_supp.html

occs.scaled = function(site){
  x = occProp[as.character(occProp$site) == site,'occ']
  n = length(x)
  s = .5
  (x*(n-1)+s)/n
}

# Fit beta distribution:

fitBeta = function(site) {
  if (bimodality(occProp[as.character(occProp$site) == site,'occ'], site)!= 0)
  {occs  = occs.scaled(site)
  shape.params = suppressWarnings(fitdistr(occs, "beta",
                                  list(shape1 = 2, shape2 = 2)))
  return(as.vector(shape.params$estimate))
  } else c(NA, NA)
}

#==================================================================================*
# ---- CORE-TRANSIENT MODE STATISTICS ----
#==================================================================================*

# Proportion of samples that are core or transient:

mode.prop = function(occs, mode) {
  if (mode == 'core') length(occs[occs >= 1-threshold])/length(occs)
    else length(occs[occs <= threshold])/length(occs)
}

# Randomization test for a given mode (is the proportion of samples in core or
# transient greater than we would expect by random chance):

p.mode = function(site, mode, reps){
    actual.prop = mode.prop(occProp[as.character(occProp$site) == site,'occ'], mode)
  # For loop to get random frequncies in the mode:
    r.props = numeric(length = reps)
    for (i in 1:reps){
      r.props[i] = mode.prop(random.occs(site), mode)
    }
  # Calculate the p-value (proportion of sites with higher frequency than the
  # actual bimodality value):
  p.mode = sum(r.props >= actual.prop)/(reps + 1)
  return(data.frame(actual.prop, p.mode))
}

mode.summary = function(site, reps){
  # Summary stats for core and transient modes:
    core.prop = p.mode(site, 'core', reps)
    trans.prop = p.mode(site, 'trans', reps)
 # Bind output into dataframe:
    df.out = data.frame(site, threshold, cbind(core.prop, trans.prop))
    names(df.out)[3:6] = c('core.prop', 'coreP','trans.prop','transP')
    return(df.out)
}

#==================================================================================*
# ---- DATASET SUMMARY FUNCTIONS ----
#==================================================================================*
# NOTE: For these functions to run, occProp, Ntime, and outSummary frames must
# already be loaded!
#
# Functions:
# - summaryStats: Produces summary sampling data for one site. 
#     Inputs: Site and the threshold value for core and transient designation.
#     Outputs: A one-row dataframe with dataset ID, site ID, threshold used,
#       the system, taxa, # of time samples, total, core, and transient richness
#       proportion of core and transient species, and the average proportion of 
#       occurance across species.
#
# - ctSummary: A partial-wrapper function that runs and compiles bimodality test
#     statistics across sites and adds it to the sampling summary frame above.
#     Inputs: Site and the threshold value for core and transient designation.
#     Outputs: A one-row dataframe with the summary output above and bimodality
#     (Allen + Ethan formula), randomization-derived p-value, and the alpha and
#     beta shape parameters for the beta distibution.
#
#----------------------------------------------------------------------------------*
# ---- Function to generate summary of sampling ----
#==================================================================================*



summaryStats = function(site, threshold){
  # Get data:
  dataset = as.numeric(gsub('_','',substr(site,2,4)))
  d = occProp[as.character(occProp$site) == site,]
  nTime = nTime[as.character(nTime$site) == site,'nt']
  dst = outSummary[outSummary$dataset_ID == dataset,]
  # Calculate richness indices:
  rich.total = length(d[,1])
  rich.core = length(d[d$occ>=1-threshold,1])
  rich.trans = length(d[d$occ<=threshold,1])
  # Calculate proportional occurrences:
  prop.core = rich.core/rich.total
  prop.trans = rich.trans/rich.total
  mu = mean(d$occ)
  # Output 1-row dataset:
  return(data.frame(dataset, site, system = dst$system, taxa = dst$taxa, nTime,
                    rich.total, rich.core, rich.trans, prop.core, prop.trans, mu))
}

#----------------------------------------------------------------------------------*
# ---- Function to generate output summary dataset ----
#==================================================================================*

ctSummary = function(site, threshold, reps){
  # Get data for a given site:
    d = occProp[as.character(occProp$site) == site,]
  # Sampling summary for site:
    samplingSummary = summaryStats(site, threshold)
    nt = samplingSummary[as.character(samplingSummary$site) == site,'nTime']  
  # Calculate bimodality of the dataset and site:
    bimodal = bimodality(d$occ, site)
    bimodal.p = p.bimodal(site, reps)
  # Calculate the alpha and beta shape parameters for the beta disatribution:
    fB = fitBeta(site)
  # Make a bimodality dataframe:
    bimodality.df = data.frame(bimodal, bimodal.p, alpha = fB[1], beta = fB[2])
  # Output
    return(cbind(samplingSummary, bimodality.df))
  }

#==================================================================================*
# ---- PLOT FUNCTIONS ----
#==================================================================================*
# NOTE: For these functions to run, occProp, Ntime, and outSummary frames must
# already be loaded!

#----------------------------------------------------------------------------------*
# ---- Custom themes ----
#==================================================================================*

# Theme for plot with no background grid:

theme_CT_NoGrid = function(base_size = 12) {
  theme(
    axis.text.x = element_text(size=14, color = 'black',vjust = 1, hjust = .5),
    axis.text.y = element_text(size=12, color = 'black', hjust = 1),
    axis.title.x = element_text(size = 18, vjust = -1),
    axis.title.y = element_text(size = 18, vjust = 1.5),
    title = element_text(size=16, vjust = 1),
    legend.title=element_blank(),
    axis.line = element_line(color = 'black'),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(2,.5,1.5,.5), 'lines'))
}

theme_CT_Grid = function(base_size = 12) {
  theme(axis.text = element_text(size=14, color = 'black'),
        axis.title.x = element_text(size = 18, vjust = -1),
        axis.title.y = element_text(size = 18, vjust = 1),
        title = element_text(size=18, vjust = -0.5),
        axis.line = element_line(colour = 'black'),
        panel.background = element_blank(),
        panel.grid.major = element_line(size = .5, color = 'gray90'),
        panel.grid.minor = element_line(size = .25, color = 'gray90'),
        plot.margin = unit(c(0,.5,1.5,.5), 'lines'))
}

#----------------------------------------------------------------------------------*
# ---- Function to make core-transient histogram  ----
#==================================================================================*
# This function creates a ct histogram for one site:

ct.hist = function(site) {
  # Get data, subset to a given site:
      occProp = occProp[as.character(occProp$site) == site,]
      ct = ct[as.character(ct$site) == site, ]
  # Plot labels:
    main = paste('Site ', site, paste('(',  as.character(ct$system),
                   ', ', as.character(ct$taxa),')', sep = ''))
    sub = bquote(b ~ '=' ~ .(round(ct$bimodal, 2)) ~ '    '~
                   P['b'] ~ '=' ~ .(round(ct$bimodal.p, 3)) ~ '    '~
                   mu ~ '=' ~ .(round(ct$mu, 2)) ~ '    '~
                   t ~ '=' ~ .(ct$nTime))
    sub2 = bquote(alpha ~ '=' ~ .(round(ct$alpha, 3)) ~ '    '~
                   beta ~ '=' ~ .(round(ct$beta, 3)))
  # Set band width, breaks and possible values of x for the histogram:
    bw = 1/nTime#(max(occProp$occ)-min(occProp$occ))/10
    brks = seq(min(occProp$occ), max(occProp$occ),bw)
    x = seq(1/ct$nTime,1-1/ct$nTime, .01)
    beta.df = data.frame(x = x, y = dbeta(x, ct$alpha, ct$beta))
  # Plot data: 
    out.plot = ggplot(occProp, aes(x=occ)) +
      geom_histogram(aes(y = ..density..), binwidth = bw, breaks = brks, right = F,
                     fill = 'gray', color = 1) +
      xlim(1/nTime, 1) +
      geom_line(data = beta.df, aes(x = x, y = y), color = 'red') +
      # stat_function(fun = function(x) dbeta(x, ct$alpha, ct$beta), color = 'red') +
      # Add labels:
      xlab('Proportion of temporal samples') + ylab('Density') + 
      ggtitle(bquote(atop(.(main), atop(.(sub), atop(.(sub2)))))) +
      # Add themes:
      theme(axis.text = element_text(size=14, color = 1),
            axis.title.x = element_text(vjust = -1),
            axis.title.y = element_text(vjust = 2),
            title = element_text(size=16, vjust = -1),
            axis.line = element_line(colour = "black"),
            panel.background = element_blank(),
            plot.margin = unit(c(.5,.5,1.5,1), "lines"))
    return(out.plot)
  }
