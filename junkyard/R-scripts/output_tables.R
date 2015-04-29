###################################################################################*
# ---- CORE-TRANSIENT TABULAR SUMMARY OUTPUT ----
###################################################################################*  
# This file is used as a "dashboard" to observe / produce tabular summary output 
# from core-transient analyses. Functions are located in the 
# core-transient_functions.R source file.

#----------------------------------------------------------------------------------*
# ---- Set-up ----
#==================================================================================*

# Source functions:

source('scripts/R-scripts/core-transient_functions.R')

# Get files:

occPropList = paste('data/propOcc_datasets/',
                    list.files('data/propOcc_datasets'), sep ='')

siteSummaryList = paste('data/siteSummaries/',
                    list.files('data/siteSummaries'), sep ='')

# #----------------------------------------------------------------------------------*
# # ---- Sampling summary ----
# #==================================================================================*
#   
# # Calculate the summary statistics (all summary data with the exception of
# # bimodality across sites for sites that meet our sampling criteria). Input is 
# # the core-transient threshold. 
# 
# site = unique(occProp$site)
# # site = site[50:51]
# threshold = 1/3
# out.list = list()
# 
# for (i in 1:length(site)){
#   out.list[[i]] = summaryStats(site[i], threshold)
# }
# 
# ss = rbind.fill(out.list)

#----------------------------------------------------------------------------------*
# ---- Core-transient summary table ----
#----------------------------------------------------------------------------------*

# Get the summary data for datasets that have already been summarized:

currentSummaryData = read.csv('output/tabular_data/core-transient_summary.csv')

# Create a vector of the already summarized datasets:

currentDatasetIDs = unique(currentSummaryData$datasetID)

# Get a vector of the datset ID's in the propOcc folder:

datasetIDs = as.numeric(sapply(strsplit(occPropList, '\\_|\\.'),'[[',3))

# Subset the ID's to those not already processed:

datasetIDs[!datasetIDs %in% currentDatasetIDs]
# Generate an empty list for summary table output:

outList = list(length = length(datasetIDs))

# Loop across available propOcc datasets:
# Note: SummaryStatsFun(datasetID, treshold, reps)

for(i in 1:length(datasetIDs)){
  outList[[i]] = summaryStatsFun(datasetIDs[i], 1/3,1)
}

# Bind list output into a single dataframe:

summaryStats = rbind.fill(outList)

# Write file:

write.csv(summaryStats, 'output/tabular_data/core-transient_summary.csv', row.names = F)

test = read.csv('output/tabular_data/core-transient_summary.csv')

###################################################################################*
# ---- UPDATED TO THIS POINT ----
###################################################################################*

#----------------------------------------------------------------------------------*
# ----  Counting the number of individuals per site ----
#==================================================================================*

# Get summary table file and collect dataset names within the formatted dataset
# directory:

ct = read.csv('output/tabular_data/core-transient_summary.csv')

filenames <- list.files('formatted_datasets', pattern="*.csv", full.names=TRUE)
  
# Function to get the number of individuals for sites in a given dataset:
  
getCounts = function(i){
    d = read.csv(filenames[i])
    if (names(d)[5] == 'density') names(d)[5] = 'count'
    d = d[d$count > 0,]
    out = ddply(d, .(site), summarize, nIndividuals = sum(count))
    return(out)
}

# Function to calculate individuals per site across datasets:
  
countsFun = function(){
  outList = list()
    for(i in 1:length(datasets)){
        outList[[i]] = getCounts(i)
    }
  rbind.fill(outList)
}
  
# Modify the ct table to include count-per-site data:
  
ct = merge(ct, countsFun(), all = F)

# Write modified core-transient summary table:
  
write.csv(ct, 'output/tabular_data/core-transient_summary.csv', row.names = F)

#----------------------------------------------------------------------------------*
# ----  Summarizing outputs with site as the sampling unit ----
#==================================================================================*

ct = read.csv('output/tabular_data/core-transient_summary.csv')
  
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
