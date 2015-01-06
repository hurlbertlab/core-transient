###################################################################################*
  # ---- CORE-TRANSIENT TABULAR SUMMARY OUTPUT ----
###################################################################################*
# This file is used as a "dashboard" to observe / produce tabular summary output 
# from core-transient analyses. Functions are located in the 
# core-transient_functions.R source file.

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

#----------------------------------------------------------------------------------*
# ----  Counting the number of individuals per site ----
#==================================================================================*

ct = read.csv('output/tabular_data/core-transient_summary.csv')
datasets = unique(ct$dataset)
  
getCounts = function(i){
    dataset = paste('formatted_datasets/dataset_',datasets[i],'.csv', sep ='')
    d = read.csv(dataset)
    if (names(d)[5] == 'density') names(d)[5] = 'count'
    d = d[d$count > 0,]
    out = ddply(d, .(site), summarize, nIndividuals = sum(count))
    return(out)
}
    


get.outsFun = function(datasets){
    require(plyr)
    # For loops run proportional and nTime functions:
    prop.list = list()  
    nTime.list = list() 
    for(i in 1:length(datasets)) prop.list[[i]] = occProp.maker(datasets, i)  
    for(i in 1:length(datasets)) nTime.list[[i]] = nTime.maker(datasets, i) 
    # Turn lists into data frames:
    occProp = rbind.fill(prop.list)
    nTime =  rbind.fill(nTime.list)
    # Return list with occProp and nTime frames:
    out.list = list(occProp, nTime)
    names(out.list) = c('occProp','nTime')
    return(out.list)
  }

  
proc.countsDatasetFun = function(datasetID){
    # Get the dataset names in the formatted files directory:
    dataset = paste('formatted_datasets/dataset_',datasetID,'.csv', sep ='')
    data = read.csv(dataset)
    # Get new processed data:
    new.dfs = get.outsFun(dataset)
    # Bind with previously processed data:
    occProp = rbind(occProp, new.dfs[[1]])
    nTime = rbind(nTime, new.dfs[[2]])
    # Sort by dataset
    occProp = occProp[order(occProp$dataset),]
    nTime = nTime[order(nTime$datasetID),]
    # Write to file:
    write.csv(occProp, 'output/occProp.csv', row.names = F)
    write.csv(nTime, 'output/nTime.csv', row.names = F)
  }
  
  

#----------------------------------------------------------------------------------*
# ----  Summarizing outputs with site as the sampling unit ----
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
