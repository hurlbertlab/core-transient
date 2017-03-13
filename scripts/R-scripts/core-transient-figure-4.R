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
library(merTools)
library(digest)


source('scripts/R-scripts/core-transient_functions.R')

# Specify here the datasetIDs and then run the code below.
dataformattingtable = read.csv('data_formatting_table.csv', header = T) 

datasetIDs = dataformattingtable$dataset_ID[dataformattingtable$format_flag == 1]

# BBS (dataset 1) will be analyzed separately for now.
datasetIDs = datasetIDs[!datasetIDs %in% c(1)]

##### Boxplots showing distribution of core and transient species by taxon #####
# Read in datasets
taxcolors = read.csv("output/tabular_data/taxcolors.csv", header = TRUE)
occ_taxa = read.csv("output/tabular_data/occ_taxa.csv", header = TRUE)
areamerge = read.csv("output/tabular_data/areamerge.csv", header = TRUE)
bbs_spRich = read.csv("data/BBS/bbs_abun4_spRich.csv", header = TRUE)
transrich = read.csv("output/tabular_data/transrich.csv", header = TRUE)
minustransrich = read.csv("output/tabular_data/minustransrich.csv", header = TRUE)

occ_merge = occ_taxa[,c("datasetID", "site","taxa", "meanAbundance", "pctTrans","pctCore","pctNeither","scale", "spRich")]
bbs_occ = rbind(bbs_spRich,occ_merge)

#### Figure 4c ####
turnover = read.csv("output/tabular_data/temporal_turnover.csv", header = TRUE)
turnover_taxa = merge(turnover,dataformattingtable[,c("dataset_ID", "taxa")], by.x = "datasetID", by.y = "dataset_ID")
turnover_col = merge(turnover_taxa, taxcolors, by = "taxa")

turnover_col$taxa = factor(turnover_col$taxa,
                                levels = c('Invertebrate','Fish','Plankton','Mammal','Plant','Bird','Benthos'),ordered = TRUE)
colscale = c("gold2","turquoise2", "red", "purple4","forestgreen","#1D6A9B", "azure4")


m <- ggplot(turnover_col, aes(x = TJ, y = TJnotrans))
m + geom_abline(intercept = 0,slope = 1, lwd =1.5,linetype="dashed")+geom_point(aes(colour = taxa), size = 6) + xlab("Transients Slope") + ylab("Without Transients Slope") + scale_colour_manual(breaks = turnover_col$taxa,values = colscale) + theme(axis.text.x=element_text(size=24),axis.text.y=element_text(size=24),axis.title.x=element_text(size=32),axis.title.y=element_text(size=32,angle=90,vjust = 2))+ theme_classic()
ggsave(file="C:/Git/core-transient/output/plots/spturnover_4c.pdf", height = 10, width = 15)


##### Figure 4d ##### only scaled vars
minustransrich$minustrans = minustransrich$n

bbs_occ_trans = merge(bbs_occ, transrich, by = c("datasetID", "site", "scale"), all.x = TRUE)
bbs_occ_trans = merge(bbs_occ_trans, minustransrich[, c("datasetID", "site", "scale", "minustrans")], by = c("datasetID", "site", "scale"), all.x = TRUE)

bbs_occ_trans_area = merge(areamerge[,c("datasetID", "site", "area")], bbs_occ_trans, by = c("datasetID", "site"))

scaleIDs = unique(bbs_occ_trans_area$datasetID)

scaleIDs = scaleIDs[! scaleIDs %in% c(225,248,254, 282,291)] # 248 tbd

slopes = c()
for(id in scaleIDs){
  print(id)
  plotsub = subset(bbs_occ_trans_area,datasetID == id) 
  taxa = as.character(unique(plotsub$taxa))
  mod.t = lm(log10(plotsub$spRich) ~ log10(plotsub$area))
  mod.t.slope = summary(mod.t)$coef[2,"Estimate"]
  mod.n= lm(log10(plotsub$minustrans) ~ log10(plotsub$area))
  mod.n.slope = summary(mod.n)$coef[2,"Estimate"]
  print(mod.n.slope)
  taxcolor = subset(taxcolors, taxa == as.character(plotsub$taxa)[1])
  slopes = rbind(slopes, c(mod.t.slope, mod.n.slope, taxa))
}
colnames(slopes) = c("spRich_slope","minustrans_slope", "taxa")
plot_relationship = merge(slopes, taxcolors, by = "taxa")
plot_relationship$spRich_slope = as.numeric(as.character(plot_relationship$spRich_slope))
plot_relationship$minustrans_slope = as.numeric(as.character(plot_relationship$minustrans_slope))


plot_relationship$taxa = factor(plot_relationship$taxa,
                                levels = c('Invertebrate','Fish','Plankton','Mammal','Plant','Bird','Benthos'),ordered = TRUE)
colscale = c("gold2","turquoise2", "red", "purple4","forestgreen","#1D6A9B", "azure4")

p <- ggplot(plot_relationship, aes(x = spRich_slope, y = minustrans_slope))
p + geom_abline(intercept = 0,slope = 1, lwd =1.5,linetype="dashed")+geom_point(aes(colour = taxa), size = 6) + xlab("Transients Slope") + ylab("Without Transients Slope") + scale_colour_manual(breaks = plot_relationship$taxa,values = colscale) + theme(axis.text.x=element_text(size=24),axis.text.y=element_text(size=24),axis.title.x=element_text(size=32),axis.title.y=element_text(size=32,angle=90,vjust = 2))+ theme_classic()
ggsave(file="C:/Git/core-transient/output/plots/sparea_4d.pdf", height = 10, width = 15)



