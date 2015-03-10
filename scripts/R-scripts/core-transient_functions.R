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

library(stringr)
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
# ---- FUNCTIONS for proportional occurrence and site summary data frames  ----
#==================================================================================*

# Function to change date object to year:

getYear = function(date){
  if (class(date)[1] == 'factor') date = as.POSIXlt(date)
  return(as.numeric(format(date, '%Y')))
}

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
# ---- GET DATA ----
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


bimodalityFun = function(propOcc_or_RandomPropOcc, nTime){
  occs = propOcc_or_RandomPropOcc
  maxvar = var(c(rep(1/nTime,floor(length(occs)/2)),
                 rep(1,ceiling(length(occs)/2))))
  return(var(occs)/maxvar)
}

# Random sample of occurences for a given site (to be used in randomization, below):

randomOccsFun = function(propOcc, nTime){
  # Generate a table (data frame) of occProps and frequencies:
  occPropTable = data.frame(table(propOcc))
  # Create a data frame of possible occProps:
  occPropDummyTable = data.frame(propOcc = seq(1/nTime, 1, by = 1/nTime))
  # Merge the two data frames:
  combinedTable = merge(occPropDummyTable, occPropTable, all.x = T)
  combinedTable[is.na(combinedTable[,2]),2]<-0                            # Replace NA's with zeros
  # Reassign bin values randomly and add to frame:
  newFreq = sample(combinedTable$Freq, length(combinedTable[,1]))
  randomTable = data.frame(combinedTable[,1], newFreq)
  randomOccs=unlist(apply(randomTable, 1, function(x) rep(x[1], x[2])))
  return(as.vector(randomOccs))
}

# Randomization test for bimodality:

pBimodalFun = function(propOcc,nTime, reps){
  actualBimod = bimodalityFun(propOcc, nTime)
  # For loop to get random bimodality values
  randomBimod = numeric(length = reps)
  for (i in 1:reps){
    randomBimod[i] = bimodalityFun(randomOccsFun(propOcc, nTime), nTime)
  }
  # Calculate the p-value (proportion of sites with higher bimodality than the
  # actual bimodality value):
  sum(randomBimod >= actualBimod)/(reps + 1)
}

#----------------------------------------------------------------------------------*
# ---- Function for fitting the beta distribution ----
#==================================================================================*
# Required packages = MASS

# Scale occupancy from [0,1] to (0,1) following Smithson and Verkuilen 2006
# Note: See supplemental at
# http://supp.apa.org/psycarticles/supplemental/met_11_1_54/met_11_1_54_supp.html

occsScaledFun = function(occProp){
  x = occProp# [as.character(occProp$site) == site,'occ']
  n = length(x)
  s = .5
  (x*(n-1)+s)/n
}

# Fit beta distribution:

fitBeta = function(occProp, nTime) {
  if (bimodalityFun(occProp,nTime)!= 0)
  {occs  = occsScaledFun(occProp)
  shape.params = suppressWarnings(fitdistr(occs, "beta",
                                  list(shape1 = 2, shape2 = 2)))
  return(as.vector(shape.params$estimate))
  } else c(NA, NA)
}

#==================================================================================*
# ---- CORE-TRANSIENT MODE STATISTICS ----
#==================================================================================*

# Proportion of samples that are core or transient:

modeProp = function(propOcc, mode, threshold) {
  if (mode == 'core') length(propOcc[propOcc >= 1-propOcc])/length(propOcc)
    else length(propOcc[propOcc <= threshold])/length(propOcc)
}

# Randomization test for a given mode (is the proportion of samples in core or
# transient greater than we would expect by random chance):

pModeFun = function(propOcc, nTime, mode, threshold, reps){
    actualProp = modeProp(propOcc, mode, threshold)
  # For loop to get random frequncies in the mode:
    randomProps = numeric(length = reps)
    for (i in 1:reps){
      randomProps[i] = modeProp(randomOccsFun(propOcc, nTime), mode, threshold)
    }
  # Calculate the p-value (proportion of sites with higher frequency than the
  # actual bimodality value):
  pVal = sum(randomProps >= actualProp)/(reps + 1)
  return(pVal)
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

# Summary stats for all sites in a dataset:

summaryStatsFun = function(datasetID, threshold, reps){
  # Get data:
  dataList = getDataList(datasetID)
  sites  = dataList$siteSummary$site
  # Get summary stats for each site:
  outList = list(length = length(sites))
  for(i in 1:length(sites)){
    propOcc = subset(dataList$propOcc, site == sites[i])$propOcc
    siteSummary = subset(dataList$siteSummary, site == sites[i])
    nTime = siteSummary$nTime
    spRichTotal = siteSummary$spRich
    spRichCore = length(propOcc[propOcc >= 1 - threshold])
    spRichTrans = length(propOcc[propOcc <= threshold])
    propCore = spRichCore/spRichTotal
    propCore_pVal = pModeFun(propOcc, nTime, 'core', threshold, reps)
    propTrans = spRichTrans/spRichTotal
    propTrans_pVal = pModeFun(propOcc, nTime, 'trans', threshold, reps)
    mu = mean(propOcc)
    bimodality = bimodalityFun(propOcc, nTime)
    pBimodal = pBimodalFun(propOcc, nTime, reps)
    betaParms = fitBeta(propOcc, nTime)
      alpha = betaParms[1]
      beta = betaParms[2]   
    outList[[i]] = data.frame(datasetID, site = sites[i],
                              system = dataList$system, taxa = dataList$taxa,
                              nTime, spRichTotal, spRichCore, spRichTrans,
                              propCore, propCore_pVal,  propTrans, propTrans_pVal,
                              mu, bimodality, pBimodal, alpha, beta)
  }
  return(rbind.fill(outList))
}

#----------------------------------------------------------------------------------*
# ---- MAKE SUMMARY STATS OF ANY NEW PROPOCC FILES ----
#==================================================================================*

addNewSummariesFun = function(threshold, reps){
  currentSummaryData = read.csv('output/tabular_data/core-transient_summary.csv')
  currentDatasetIDs = unique(currentSummaryData$datasetID)
  propOcc_datasets = list.files('data/propOcc_datasets')
  propOccDatasetIDs = read.table(text = 
                  as.character(read.table(text = propOcc_datasets,
                  sep =c('_'))[,2]),sep ='.')[,1]
  newDatasetIDs = propOccDatasetIDs[!propOccDatasetIDs %in% currentDatasetIDs]
  # For loop to extract summary stats for new datasetIDs
  outList = list(length = length(newDatasetIDs))
  for(i in 1:length(newDatasetIDs)){
    outList[[i]] = summaryStatsFun(newDatasetIDs[i], threshold, reps)
  }
  newSummaryData = rbind.fill(outList)
  updatedSummaryData = rbind(currentSummaryData, newSummaryData)
  return(updatedSummaryData[order(datasetID),])
}

###################################################################################*
# ---- UPDATE COMPLETE TO THIS POINT ----
###################################################################################*

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
