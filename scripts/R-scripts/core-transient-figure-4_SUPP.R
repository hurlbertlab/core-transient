###############################################
# Code for running core-transient analysis
# and data summaries over all formatted datasets.
#
# Input files are named propOcc_XXX.csv where
# XXX is the dataset ID.

# setwd("C:/git/core-transient")

library(lme4)
library(plyr) # for core-transient functions
library(ggplot2)
library(merTools)
library(tidyr)
library(maps)
library(gridExtra)
library(RColorBrewer)
library(sp)
library(rgdal)
library(raster)
library(dplyr)
library(digest)
library(Hmisc)


source('scripts/R-scripts/core-transient_functions.R')

# Specify here the datasetIDs and then run the code below.
dataformattingtable = read.csv('data_formatting_table.csv', header = T) 

datasetIDs = dataformattingtable$dataset_ID[dataformattingtable$format_flag == 1]

# BBS (dataset 1) will be analyzed separately for now.
# datasetIDs = datasetIDs[!datasetIDs %in% c(1)]

#################### FIG 3 WITH LOWER THRESHOLDS ######################### 
occ_taxa25=read.csv("output/tabular_data/occ_taxa_25.csv",header=TRUE)
occ_taxa25$pctTrans25 = occ_taxa25$pctTrans
occ_taxa10=read.csv("output/tabular_data/occ_taxa_10.csv",header=TRUE)
occ_taxa10$pctTrans10 = occ_taxa10$pctTrans
occ_taxa = merge(occ_taxa25, occ_taxa10[,c("datasetID", "site", "pctTrans10")], by = c("datasetID", "site"))

summ25 = read.csv("output/tabular_data/core-transient_summary_25.csv", header = TRUE)
summ10 = read.csv("output/tabular_data/core-transient_summary_10.csv", header = TRUE)

colors7 = c(colors()[552], # plankton
            rgb(29/255, 106/255, 155/255), #bird
            colors()[144], # invert
            colors()[139], # plant
            colors()[551], #mammal
            colors()[17], #benthos
            colors()[637]) #fish



symbols7 = c(16, 18, 167, 15, 17, 1, 3) 

taxcolors = read.csv("output/tabular_data/taxcolors.csv", header = TRUE)
scaleIDs = filter(dataformattingtable, spatial_scale_variable == 'Y',
                  format_flag == 1)$dataset_ID

bbs_abun = read.csv("data/BBS/bbs_allscales33.csv", header=TRUE)

# read in bbs abundance data
bbs_focal_occs_pctTrans = read.csv("data/BBS/bbs_focal_occs_pctTrans_Site.csv", header = TRUE)
bbs_area2 = right_join(bbs_abun[, c("site", "area")], bbs_focal_occs_pctTrans, by = "site")
bbs_area2 = bbs_area2[!duplicated(bbs_area2), ]
bbs_area2$pctTrans25 = bbs_area2$propTrans25
bbs_area2$site = as.factor(bbs_area2$site)
bbs_area2  = bbs_area2[, c("datasetID", "site", "taxa", "pctTrans25", "area")]
bbs_area2$area = bbs_area2$area * 1000000

area = read.csv("output/tabular_data/scaled_areas_3_2.csv", header = TRUE)

#### Fig 3a Area #####
areamerge.5 = merge(occ_taxa25[,c("datasetID", "site", "pctTrans25")], area, by = c("datasetID", "site"))
areamerge  = areamerge.5[, c("datasetID", "site", "taxa", "pctTrans25", "area")]

areamerge = rbind(bbs_area2, areamerge)
areamerge = na.omit(areamerge)

areamerge_mod = areamerge[! areamerge$datasetID %in% c(207, 210, 217, 218, 222, 223, 225, 241,258, 282, 322, 280, 248, 254, 279, 291),]

bbs_spRich25 = merge(bbs_abun, bbs_focal_occs_pctTrans[,c("site", "propTrans25")], by = "site")  
bbs_spRich25$pctTrans25 = bbs_spRich25$propTrans25
bbs_spRich25 = bbs_spRich25[, c("datasetID", "site", "taxa",  "meanAbundance", "pctTrans25")]
#### Figures 3a-3c panel plot #####
scaleIDs = filter(dataformattingtable, spatial_scale_variable == 'Y',
                  format_flag == 1)$dataset_ID 
scaleIDs = scaleIDs[! scaleIDs %in% c(207, 210, 217, 218, 222, 223, 225, 241,258,274, 282, 322, 280, 248, 254, 279, 291)]  # waiting on data for 248

occ_merge = occ_taxa25[,c("datasetID", "site","taxa", "meanAbundance", "pctTrans25")]
bbs_occ = rbind(occ_merge, bbs_spRich25)
bbs_occ = bbs_occ[!bbs_occ$site %in% c("53800-5-6", "53800-25-2"),]
#### Fig 3c/d predicted model ####
bbs_occ_pred = bbs_occ[!bbs_occ$datasetID %in% c(207, 210, 217, 218, 222, 223, 225, 238, 241, 258, 282, 322, 280,317),]
bbs_occ_pred = na.omit(bbs_occ_pred)
bbs_occ_pred_mod = bbs_occ_pred[! bbs_occ_pred$datasetID %in% c(207, 210, 217, 218, 222, 223, 225, 241,258, 282, 322, 280, 248, 254, 279, 291),]

mod3c = lmer(pctTrans25~log10(meanAbundance) * taxa + (log10(meanAbundance)|datasetID), data=bbs_occ_pred)
summary(mod3c)
r.squaredGLMM(mod3c)
occ_sub_pred = data.frame(datasetID = 999, taxa = unique(bbs_occ_pred$taxa), meanAbundance =  102) # 102 is median abun for data frame (median(bbs_occ_pred$meanAbundance))
# to test: test = filter(occ_sub_pred, taxa == "Invertebrate")
predmod3c = merTools::predictInterval(mod3c, occ_sub_pred, n.sims=1000)

# matching by predicted output vals based on occ_sub_pred
predmod3c$taxa = c("Invertebrate","Plant","Mammal","Fish","Bird", "Plankton", "Benthos") 
# write.csv(predmod3c, "output/tabular_data/predmod3c.csv", row.names = FALSE)

predmod = merge(predmod3c, taxcolors, by = "taxa")
predmod$taxorder = c(1,4,6,3,2,5,7)

# 3d
ecosys = merge(bbs_occ_pred, dataformattingtable[,c("dataset_ID", "system")], by.y = "dataset_ID", by.x = "datasetID")
mod3d = lmer(pctTrans25~log10(meanAbundance) * system + (log10(meanAbundance)|datasetID), data=ecosys)
summary(mod3d)
occ_pred_3d = data.frame(datasetID = 999, system = unique(ecosys$system), meanAbundance =  102) # 102 is median abun for data frame (median(bbs_occ_pred$meanAbundance))
predmod3d = merTools::predictInterval(mod3d, occ_pred_3d, n.sims=1000)
predmod3d$order = c(1,2,3)

#### panel plot ####
areamerge_mod = areamerge[! areamerge$datasetID %in% c(207, 210, 217, 218, 222, 223, 225, 241,258, 282, 322, 280, 248, 254, 279, 291),]

areaModel = lmer(pctTrans25 ~ log10(area) * taxa + (log10(area) | datasetID), data = areamerge_mod)
r.squaredGLMM(areaModel)

summary(lm(pctTrans25 ~ log10(area), data = areamerge_mod))

bbs_occ_pred_mod = bbs_occ_pred[! bbs_occ_pred$datasetID %in% c(207, 210, 217, 218, 222, 223, 225, 241,258, 282, 322, 280, 248, 254, 279, 291),]

# r and pseudo r2 10%
abunModel = lmer(pctTrans25 ~ log10(meanAbundance) * taxa + (log10(meanAbundance) | datasetID), data = bbs_occ_pred_mod)
r.squaredGLMM(abunModel)

summary(lm(pctTrans25 ~ log10(meanAbundance), data = bbs_occ_pred_mod))

dats = areamerge %>% 
  group_by(datasetID, taxa) %>% 
  dplyr::summarize(minA = min(area), maxA = max(area))
dats = data.frame(dats)

minA  = dplyr::select(dats, datasetID, taxa, minA) %>% dplyr::rename(area = minA)
maxA  = dplyr::select(dats, datasetID, taxa, maxA) %>% dplyr::rename(area = maxA)

dats$minApred <- merTools::predictInterval(areaModel, minA)$fit
dats$maxApred <- merTools::predictInterval(areaModel, maxA)$fit
dats$taxa <- NULL

area_plot = merge(areamerge, dats, by = "datasetID")

pdf('output/plots/3a_3d_25SUPP.pdf', height = 10, width = 12)
par(mfrow = c(2, 2), mar = c(5,5,1,1), cex = 1, oma = c(0,0,0,0), las = 1)
palette(colors7)

# A
plot(NA, xlim = c(-2, 8), ylim = c(0,1), xlab = expression("log"[10]*" Area (m"^2*")"), 
     ylab = "% Transients", cex.lab = 2, frame.plot=FALSE, xaxt = "n", yaxt = "n", 
     mgp = c(3.25,1,0))
axis(1, cex.axis =  1.5)
axis(2, cex.axis =  1.5)
b1 = for(id in scaleIDs){
  print(id)
  plotsub = subset(area_plot,datasetID == id)
  taxa = as.character(unique(plotsub$taxa))
  taxcolor = subset(taxcolors, taxa == as.character(plotsub$taxa)[1])
  segments(log10(plotsub$minA), plotsub$minApred, log10(plotsub$maxA), plotsub$maxApred, col = as.character(taxcolor$color), lwd = 4)
  par(new=TRUE)
}
segments(6.702913, range(area_plot$minApred)[1],-1.397940, range(area_plot$maxApred)[2], col = "black", lwd = 4) # range(log10(area_plot$maxA))
title(outer=FALSE,adj=0.02,main="A",cex.main=2,col="black",font=2,line=-1)
par(new= FALSE)

dats = bbs_occ_pred %>% 
  group_by(datasetID, taxa) %>% 
  dplyr::summarize(minAb = min(meanAbundance), maxAb = max(meanAbundance))
dats = data.frame(dats)

minAb  = dplyr::select(dats, datasetID, taxa, minAb) %>% dplyr::rename(meanAbundance = minAb)
maxAb  = dplyr::select(dats, datasetID, taxa, maxAb) %>% dplyr::rename(meanAbundance = maxAb)

dats$minAbpred <- merTools::predictInterval(abunModel, minAb)$fit
dats$maxAbpred <- merTools::predictInterval(abunModel, maxAb)$fit
dats$taxa = NULL

bbs_occ = merge(bbs_occ_pred, dats, by = "datasetID")

plot(NA, xlim = c(0, 7), ylim = c(0,1), col = as.character(taxcolor$color), xlab = expression("log"[10]*" Community Size"), ylab = "% Transients", cex.lab = 2,frame.plot=FALSE, yaxt = "n", xaxt = "n", mgp = c(3.25,1,0))
axis(1, cex.axis =  1.5)
axis(2, cex.axis =  1.5)
b2 = for(id in scaleIDs){
  print(id)
  plotsub = subset(bbs_occ,datasetID == id)
  taxa = as.character(unique(plotsub$taxa))
  taxcolor = subset(taxcolors, taxa == as.character(plotsub$taxa)[1])
  segments(log10(plotsub$minAb), plotsub$minAbpred, log10(plotsub$maxAb), plotsub$maxAbpred, col = as.character(taxcolor$color), lwd = 4)
  par(new=TRUE)
}
segments(6.470117, range(bbs_occ$minAbpred)[1],0.5246248, range(bbs_occ$maxAbpred)[2], col = "black", lwd = 4)
abline(v = log10(102), lty = 'dotted', lwd = 2) 
title(outer=FALSE,adj=0.02,main="B",cex.main=2,col="black",font=2,line=-1)
par(new= FALSE)
legend('topright', legend = as.character(taxcolors$taxa), lty=1,lwd=3,col = as.character(taxcolors$color), cex = 1.5, bty = "n")
par(new = FALSE)

b3 = barplot(predmod$fit[predmod$taxorder], cex.names = 2,col = c(colors()[17], "gold2", "turquoise2","red","forestgreen","purple4","#1D6A9B"), ylim = c(0, 1.05))
Hmisc::errbar(c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9), predmod$fit[predmod$taxorder], predmod$upr[predmod$taxorder], predmod$lwr[predmod$taxorder], add= TRUE, lwd = 1.25, pch = 3)
mtext("25% Transients", 2, cex = 2, las = 0, line = 2.5)
title(outer=FALSE,adj=0.02,main="C",cex.main=2,col="black",font=2,line=-1)

b4 = barplot(predmod3d$fit[predmod3d$order], cex.names = 2,col = c('burlywood','skyblue','navy'), ylim = c(0, 0.8))
Hmisc::errbar(c(0.7, 1.9, 3.1), predmod3d$fit[predmod3d$order], predmod3d$upr[predmod3d$order], predmod3d$lwr[predmod3d$order], add= TRUE, lwd = 1.25, pch = 3)
mtext("25% Transients", 2, cex = 2, las = 0, line = 2.5)
title(outer=FALSE,adj=0.02,main="D",cex.main=2,col="black",font=2,line=-1)
dev.off()

##### 10 pct trans ######
areamerge.25 = merge(occ_taxa10[,c("datasetID", "site", "pctTrans10")], area, by = c("datasetID", "site"))
areamerge.5  = areamerge.25[, c("datasetID", "site", "taxa", "pctTrans10", "area")]
areamerge = areamerge.5[! areamerge.5$datasetID %in% c(207, 210, 217, 218, 222, 223, 225, 241,258, 282, 322, 280, 248, 254, 279, 291),]

#### Figures 3a-3c panel plot #####
scaleIDs = filter(dataformattingtable, spatial_scale_variable == 'Y',
                  format_flag == 1)$dataset_ID 
scaleIDs = scaleIDs[! scaleIDs %in% c(207, 210, 217, 218, 222, 223, 225, 241,258,274, 282, 322, 280, 248, 254, 279, 291)]  
# bbs_area = tidyr::separate(bbs_area, site, c("site", "subsite"), sep = "-")
bbs_focal_occs_pctTrans = read.csv("data/BBS/bbs_focal_occs_pctTrans_Site.csv", header = TRUE)
bbs_area2 = merge(bbs_abun[, c("site", "area", "meanAbundance")], bbs_focal_occs_pctTrans, by = "site")
bbs_area2$pctTrans10 = bbs_area2$propTrans10
bbs_area2$site = as.factor(bbs_area2$site)
bbs_area2$area = bbs_area2$area * 1000000
bbs_area3  = bbs_area2[, c("datasetID", "site", "taxa", "pctTrans10", "area")]

#### Fig 3a Area #####
area = read.csv("output/tabular_data/scaled_areas_3_2.csv", header = TRUE)

areamerge = rbind(bbs_area3, areamerge)
areamerge = na.omit(areamerge)

bbs_spRich10 = bbs_area2[, c("datasetID", "site", "taxa",  "meanAbundance", "pctTrans10")]

occ_merge = occ_taxa10[,c("datasetID", "site","taxa", "meanAbundance", "pctTrans10")]
bbs_occ = rbind(occ_merge, bbs_spRich10)
bbs_occ = bbs_occ[!bbs_occ$site %in% c("53800-5-6", "53800-25-2"),]
#### Fig 3c/d predicted model ####
bbs_occ_pred.5 = bbs_occ[!bbs_occ$datasetID %in% c(207, 210, 217, 218, 222, 223, 225, 238, 241, 258, 282, 322, 280,317),]
bbs_occ_pred.5 = na.omit(bbs_occ_pred.5)
bbs_occ_pred = bbs_occ_pred.5[! bbs_occ_pred.5$datasetID %in% c(207, 210, 217, 218, 222, 223, 225, 241,258, 282, 322, 280, 248, 254, 279, 291),]

mod3c = lmer(pctTrans10~log10(meanAbundance) * taxa + (log10(meanAbundance) | datasetID), data=bbs_occ_pred)
summary(mod3c)
occ_sub_pred = data.frame(datasetID = 999, taxa = unique(bbs_occ_pred$taxa), meanAbundance =  102) # 102 is median abun for data frame (median(bbs_occ_pred$meanAbundance))
# to test: test = filter(occ_sub_pred, taxa == "Invertebrate")
predmod3c = merTools::predictInterval(mod3c, occ_sub_pred, n.sims=1000)

# matching by predicted output vals based on occ_sub_pred
predmod3c$taxa = c("Invertebrate","Plant","Mammal","Fish","Bird", "Plankton", "Benthos") 
# write.csv(predmod3c, "output/tabular_data/predmod3c.csv", row.names = FALSE)

predmod = merge(predmod3c, taxcolors, by = "taxa")
predmod$taxorder = c(1,4,3,6,7,5,2)

# 3d
ecosys = merge(bbs_occ_pred, dataformattingtable[,c("dataset_ID", "system")], by.y = "dataset_ID", by.x = "datasetID")
mod3d = lmer(pctTrans10~log10(meanAbundance) * system + (log10(meanAbundance) | datasetID), data=ecosys)
summary(mod3d)
occ_pred_3d = data.frame(datasetID = 999, system = unique(ecosys$system), meanAbundance =  102) # 102 is median abun for data frame (median(bbs_occ_pred$meanAbundance))
predmod3d = merTools::predictInterval(mod3d, occ_pred_3d, n.sims=1000)
predmod3d$order = c(1:3)


#### panel plot ####
area_plot = data.frame()
pdf('output/plots/3a_3d_10SUPP.pdf', height = 10, width = 12)
par(mfrow = c(2, 2), mar = c(5,5,1,1), cex = 1, oma = c(0,0,0,0), las = 1)
palette(colors7)

# r and pseudo r2 10%
areaModel = lmer(pctTrans10 ~ log10(area) * taxa + (log10(area) | datasetID), data = areamerge)
r.squaredGLMM(areaModel)

summary(lm(pctTrans10 ~ log10(area), data = areamerge))


dats = areamerge %>% 
  group_by(datasetID, taxa) %>% 
  dplyr::summarize(minA = min(area), maxA = max(area))
dats = data.frame(dats)

minA  = dplyr::select(dats, datasetID, taxa, minA) %>% dplyr::rename(area = minA)
maxA  = dplyr::select(dats, datasetID, taxa, maxA) %>% dplyr::rename(area = maxA)

dats$minApred <- merTools::predictInterval(areaModel, minA)$fit
dats$maxApred <- merTools::predictInterval(areaModel, maxA)$fit
dats$taxa <- NULL

area_plot = merge(areamerge, dats, by = "datasetID")

# A
plot(NA, xlim = c(-2, 8), ylim = c(0,1), xlab = expression("log"[10]*" Area (m"^2*")"), 
     ylab = "% Transients", cex.lab = 2, frame.plot=FALSE, xaxt = "n", yaxt = "n", 
     mgp = c(3.25,1,0))
axis(1, cex.axis =  1.5)
axis(2, cex.axis =  1.5)
b1 = for(id in scaleIDs){
  print(id)
  plotsub = subset(area_plot,datasetID == id)
  taxa = as.character(unique(plotsub$taxa))
  taxcolor = subset(taxcolors, taxa == as.character(plotsub$taxa)[1])
  segments(log10(plotsub$minA), plotsub$minApred, log10(plotsub$maxA), plotsub$maxApred, col = as.character(taxcolor$color), lwd = 4)
  par(new=TRUE)
}
segments(6.702913, range(area_plot$minApred)[1],-1.397940, range(area_plot$maxApred)[2], col = "black", lwd = 4) # range(log10(area_plot$maxA))
title(outer=FALSE,adj=0.02,main="A",cex.main=2,col="black",font=2,line=-1)
par(new= FALSE)


abunModel = lmer(pctTrans10 ~ log10(meanAbundance) * taxa + (log10(meanAbundance) | datasetID), data = bbs_occ)
r.squaredGLMM(abunModel)
summary(lm(pctTrans10 ~ log10(meanAbundance), data = bbs_occ))


dats = bbs_occ %>% 
  group_by(datasetID, taxa) %>% 
  dplyr::summarize(minAb = min(meanAbundance), maxAb = max(meanAbundance))
dats = data.frame(dats)

minAb  = dplyr::select(dats, datasetID, taxa, minAb) %>% dplyr::rename(meanAbundance = minAb)
maxAb  = dplyr::select(dats, datasetID, taxa, maxAb) %>% dplyr::rename(meanAbundance = maxAb)

dats$minAbpred <- merTools::predictInterval(abunModel, minAb)$fit
dats$maxAbpred <- merTools::predictInterval(abunModel, maxAb)$fit
dats$taxa = NULL

bbs_occ = merge(bbs_occ, dats, by = "datasetID")

plot(NA, xlim = c(0, 7), ylim = c(0,1), col = as.character(taxcolor$color), xlab = expression("log"[10]*" Community Size"), ylab = "% Transients", cex.lab = 2,frame.plot=FALSE, yaxt = "n", xaxt = "n", mgp = c(3.25,1,0))
axis(1, cex.axis =  1.5)
axis(2, cex.axis =  1.5)
b2 = for(id in scaleIDs){
  print(id)
  plotsub = subset(bbs_occ,datasetID == id)
  taxa = as.character(unique(plotsub$taxa))
  taxcolor = subset(taxcolors, taxa == as.character(plotsub$taxa)[1])
  segments(log10(plotsub$minAb), plotsub$minAbpred, log10(plotsub$maxAb), plotsub$maxAbpred, col = as.character(taxcolor$color), lwd = 4)
  par(new=TRUE)
}
segments(6.470117, range(bbs_occ$minAbpred)[1],0.5246248, range(bbs_occ$maxAbpred)[2], col = "black", lwd = 4)
abline(v = log10(102), lty = 'dotted', lwd = 2) 
title(outer=FALSE,adj=0.02,main="B",cex.main=2,col="black",font=2,line=-1)
par(new= FALSE)
legend('topright', legend = as.character(taxcolors$taxa), lty=1,lwd=3,col = as.character(taxcolors$color), cex = 1.5, bty = "n")
par(new = FALSE)

b3 = barplot(predmod$fit[predmod$taxorder], cex.names = 2,col = c(colors()[17],"gold2", "turquoise2","red","forestgreen","purple4","#1D6A9B"), ylim = c(0, 1))
Hmisc::errbar(c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9), predmod$fit[predmod$taxorder], predmod$upr[predmod$taxorder], predmod$lwr[predmod$taxorder], add= TRUE, lwd = 1.25, pch = 3)
mtext("10% Transients", 2, cex = 2, las = 0, line = 2.5)
title(outer=FALSE,adj=0.02,main="C",cex.main=2,col="black",font=2,line=-1)

b4 = barplot(predmod3d$fit[predmod3d$order], cex.names = 2,col = c('burlywood','skyblue','navy'), ylim = c(0, 0.8))
Hmisc::errbar(c(0.7, 1.9, 3.1), predmod3d$fit[predmod3d$order], predmod3d$upr[predmod3d$order], predmod3d$lwr[predmod3d$order], add= TRUE, lwd = 1.25, pch = 3)
mtext("10% Transients", 2, cex = 2, las = 0, line = 2.5)
title(outer=FALSE,adj=0.02,main="D",cex.main=2,col="black",font=2,line=-1)
dev.off()

dev.off()

