###############################################
# Code for running core-transient analysis
# and data summaries over all formatted datasets.
#
# Input files are named propOcc_XXX.csv where
# XXX is the dataset ID.

setwd("C:/git/core-transient")

library(lme4)
library(plyr) # for core-transient functions
library(ggplot2)
library(tidyr)
library(maps)
library(gridExtra)
library(RColorBrewer)
library(sp)
library(rgdal)
library(raster)
library(dplyr)
library(digest)


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
dataformattingtable = read.csv('data_formatting_table.csv', header = T) 

datasetIDs = dataformattingtable$dataset_ID[dataformattingtable$format_flag == 1]

# BBS (dataset 1) will be analyzed separately for now.
datasetIDs = datasetIDs[!datasetIDs %in% c(1)]

# Other datasets will need to be excluded depending on the particular analysis:
#  --only datasets with countFormat %in% c('count', 'abundance') can be used
#    for species abundance distribution analysis
#  --only datasets with multiple hierarchical spatial scales can be used
#    for the scale analysis
#  --datasets without propOcc values for individual species can only be used
#    in summaries of proportion transient vs core (e.g. d319, d257)
#    (although I think these should get weeded out based on the first criterion)

summaries = c()
for (d in datasetIDs) {
  newsumm = summaryStatsFun(d, threshold, reps)
  summaries = rbind(summaries, newsumm)
  print(d)
}

write.csv(summaries, 'output/tabular_data/core-transient_summary.csv', 
          row.names = T)

##################################################################

# If running summaries for the newly updated or created formatted
# datasets to be appended to the existing 'core-transient_summary.csv'
# file, then run this section.

# If you do not want to re-write the existing file, set write = FALSE.

# Also, this command can be used instead of the section above to
# create the 'core-transient_summary.csv' file from scratch for all
# datasets with formatted data.

# summ = addNewSummariesFun(threshold, reps, write = TRUE)

# Plotting summary results across datasets for Core-Transient analysis
summ = read.csv('output/tabular_data/core-transient_summary.csv', header=T)
summ$taxa = factor(summ$taxa)
summ$taxa[summ$taxa == "Reptile"] <- NA
summ$system[summ$system == "Aquatic"] <- "Freshwater"
summ$system = factor(summ$system)
summ = na.omit(summ)
summ1 =  subset(summ, !datasetID %in% c(1, 99, 85, 90, 91, 92, 97, 124)) # excluding BBS to include below-scale route info
summ1.5 = summ1[, c("datasetID","site","system","taxa","propCore", "propTrans", "meanAbundance")]
write.csv(summ1.5, "output/tabular_data/summ1.5.csv", row.names = FALSE)

# read in pre formatted bbs below dataset
bbs_summ2 = read.csv("data/BBS/bbs_below_summ2.csv", header = TRUE)
bbs_summ50 = bbs_summ2[grep("-50", as.character(bbs_summ2$site)),]
  
summ2 = rbind(bbs_summ50,summ1.5)
write.csv(summ2, "output/tabular_data/summ2.csv", row.names = FALSE)
dsets = unique(summ2[, c('datasetID', 'system','taxa')])

taxorder = c('Bird', 'Plant', 'Mammal', 'Fish', 'Invertebrate', 'Benthos', 'Plankton')

dsetsBySystem = data.frame(table(dsets$system))
dsetsBySystem =  dsetsBySystem[order(-dsetsBySystem$Freq),]
dsetsByTaxa = table(dsets$taxa)
sitesBySystem = data.frame(table(summ2$system))
sitesBySystem =  sitesBySystem[order(-sitesBySystem$Freq),]
sitesByTaxa = table(summ2$taxa)

colors7 = c(colors()[552], # plankton
            rgb(29/255, 106/255, 155/255), #bird
            colors()[144], # invert
            colors()[139], # plant
            colors()[551], #mammal
            colors()[17], #benthos
            colors()[637]) #fish

            

symbols7 = c(16, 18, 167, 15, 17, 1, 3) 

taxcolors = data.frame(taxa = unique(summ$taxa), color = colors7, pch = symbols7)

taxcolors$abbrev = taxcolors$taxa
taxcolors$abbrev = gsub("Benthos", 'Be', taxcolors$abbrev)
taxcolors$abbrev = gsub("Bird", 'Bi', taxcolors$abbrev)
taxcolors$abbrev = gsub("Fish", 'F', taxcolors$abbrev)
taxcolors$abbrev = gsub("Invertebrate", 'I', taxcolors$abbrev)
taxcolors$abbrev = gsub("Mammal", 'M', taxcolors$abbrev)
taxcolors$abbrev = gsub("Plankton", 'Pn', taxcolors$abbrev)
taxcolors$abbrev = gsub("Plant", 'Pt', taxcolors$abbrev)

write.csv(taxcolors, "output/tabular_data/taxcolors.csv", row.names = FALSE)

# Create data summary figure as a 6-panel PDF
pdf('output/plots/data_summary_hists.pdf', height = 8, width = 10)
par(mfrow = c(3, 2), mar = c(4,4,1.2,1.2), cex = 1.25, oma = c(0,0,0,0), las = 1,
    cex.lab = 1)
b1=barplot(dsetsBySystem$Freq, col = c('burlywood','skyblue', 'navy'), xaxt = "n",cex.names = 1) 
mtext("Datasets", 2, cex = 1.25, las = 0, line = 2.5)
title(outer=FALSE,adj=1,main="A",cex.main=1.5,col="black",font=2,line=-0.1)
barplot(log10(sitesBySystem$Freq), col = c('burlywood','skyblue', 'navy'), cex.names = 1, 
        xaxt = "n",yaxt = "n", ylim = c(0,4)) 
axis(side = 2, 0:3,labels=c("1","10","100","1000"))
mtext(expression("Assemblages"), 2, cex = 1.25, las = 0, line = 3.5) # log[10]*
title(outer=FALSE,adj=1,main="B",cex.main=1.5,col="black",font=2,line=-0.1)
bar1 = barplot(dsetsByTaxa[taxorder], xaxt = "n", axisnames = F,
               col = as.character(taxcolors$color[match(taxorder, taxcolors$taxa)]))

mtext("Datasets", 2, cex = 1.25, las = 0, line = 2.5)
title(outer=FALSE,adj=1,main="C",cex.main=1.5,col="black",font=2,line=-0.1)
bar2 = barplot(log10(sitesByTaxa[taxorder]), axes = F, axisnames = F, ylim = c(0,4),
               col = as.character(taxcolors$color[match(taxorder, taxcolors$taxa)]))
axis(2, 0:3,labels=c("1","10","100","1000"))
mtext(expression("Assemblages"), 2, cex = 1.25, las = 0, line = 3.5)
title(outer=FALSE,adj=0.95,main="D",cex.main=1.5,col="black",font=2,line=-0.1)

# species richness in community and number of study years
summ1$taxa <-droplevels(summ1$taxa, exclude = c("","All","Amphibian", "Reptile"))
summ1.col = merge(summ1, taxcolors, by = "taxa")
summ1.col$taxa <- factor(summ1.col$taxa,
                         levels = c('Bird','Plant','Mammal','Fish','Invertebrate','Benthos','Plankton'),ordered = TRUE)
rankedtaxorder = c('Bird','Mammal','Plankton','Benthos','Invertebrate','Plant','Fish')

bar1 = boxplot(summ1.col$spRichTotal~summ1.col$taxa, cex.axis =1, frame.plot = FALSE,  col = as.character(summ1.col$color[match(taxorder, summ1.col$taxa)]), axes = FALSE, ylim = c(0, 160)) 
axis(side = 2) 
mtext(expression("Species Richness"), 2, cex = 1.25, las = 0, line = 2.5)
title(outer=FALSE,adj=1,main="E",cex.main=1.5,col="black",font=2,line=-0.1)
bar2 = boxplot(summ1.col$nTime~summ1.col$taxa, xaxt = "n", frame.plot = FALSE, cex.axis =1,col = as.character(summ1.col$color[match(taxorder, summ1.col$taxa)]))
mtext(expression("Years"), 2, cex = 1.25, las = 0, line = 2.5)
title(outer=FALSE,adj=1,main="F",cex.main=1.5,col="black",font=2,line=-0.1)

dev.off()

