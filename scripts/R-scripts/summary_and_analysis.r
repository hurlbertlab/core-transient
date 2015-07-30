###############################################
# Code for running core-transient analysis
# and data summaries over all formatted datasets.
#
# Input files are named propOcc_XXX.csv where
# XXX is the dataset ID.

source('scripts/R-scripts/core-transient_functions.R')

# Maximum occupancy of transient species
# (and hence the minimum occupancy of core species is 1 - threshold)
threshold = 1/3

# Number of replicates for randomization tests
reps = 999

##################################################################

# If running summaries for the first time (or wanting to start
# anew because all formatted datasets have changed) and a
# 'core-transient_summary.csv' file does not exist yet in the
# output/tabular_data folder, or if you just want to get summary
# stats for one or a few datasets into R, run this section

# Specify here the datasetIDs and then run the code below.
datasetIDs = c()

summaries = c()
for (d in datasetIDs) {
  newsumm = summaryStatsFun(d, threshold, reps)
  summaries = rbind(summaries, newsumm)
}

write.csv(summaries, 'output/tabular_data/core-transient_summary.csv', 
          row.names = F)

##################################################################

##################################################################

# If running summaries for the newly updated or created formatted
# datasets to be appended to the existing 'core-transient_summary.csv'
# file, then run this section.

# If you do not want to re-write the existing file, set write = FALSE.

# Also, this command can be used instead of the section above to
# create the 'core-transient_summary.csv' file from scratch for all
# datasets with formatted data.

addNewSummariesFun(threshold, reps, write = TRUE)


#####################

# Plotting summary results across datasets for Core-Transient analysis

summ = read.csv('output/tabular_data/core-transient_summary.csv', header=T)
summ$taxa = factor(summ$taxa)
summ$system = factor(summ$system)
dsets = unique(summ[, c('datasetID', 'system','taxa')])


colors8 = c(rgb(98/255, 83/255, 108/255),
            rgb(125/255, 73/255, 67/255),
            rgb(120/255, 21/255, 21/255),
            rgb(171/255, 167/255, 46/255),
            rgb(186/255, 103/255, 30/255),
            rgb(0, 54/255, 117/255),
            rgb(29/255, 106/255, 155/255),
            rgb(86/255, 141/255, 27/255))

par(mfrow = c(2, 2), mar = c(1,1,1,1))
pie(table(dsets$system), main = paste("By dataset (n = ", nrow(dsets), ")", sep = ""))
pie(table(summ$system), main = paste("By site (n = ", nrow(summ), ")", sep = ""))
pie(table(dsets$taxa), col = colors8)
pie(table(summ$taxa), col = colors8)