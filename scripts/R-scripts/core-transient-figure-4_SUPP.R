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
# subsetting to only count ids 
scaleIDs = scaleIDs[! scaleIDs %in% c(207, 210, 217, 218, 222, 223, 210, 238, 241,258, 282, 322, 280,317)]
bbs_abun = read.csv("data/BBS/bbs_abun_occ.csv", header=TRUE)

# read in bbs abundance data
bbs_area = read.csv("data/BBS/bbs_area.csv", header = TRUE)

#### Fig 3a Area #####
area = read.csv("output/tabular_data/scaled_areas_3_2.csv", header = TRUE)

areamerge.5 = merge(occ_taxa25[,c("datasetID", "site", "pctTrans25")], area, by = c("datasetID", "site"))
areamerge  = areamerge.5[, c("datasetID", "site", "taxa", "pctTrans25", "area")]

# areamerge = rbind(bbs_area, areamerge)
# areamerge = na.omit(areamerge)

#### Figures 3a-3c panel plot #####
scaleIDs = filter(dataformattingtable, spatial_scale_variable == 'Y',
                  format_flag == 1)$dataset_ID 
scaleIDs = scaleIDs[! scaleIDs %in% c(1, 207, 210, 217, 218, 222, 223, 225, 241,258,274, 282, 322, 280, 248, 254, 279, 291)]  # waiting on data for 248
bbs_spRich = read.csv("data/BBS/bbs_abun4_spRich.csv", header = TRUE)
occ_merge = occ_taxa25[,c("datasetID", "site","taxa", "meanAbundance", "pctTrans25","pctCore","pctNeither","scale", "spRich")]
bbs_occ = occ_merge

#### Fig 3c/d predicted model ####
bbs_occ_pred = bbs_occ[!bbs_occ$datasetID %in% c(207, 210, 217, 218, 222, 223, 225, 238, 241, 258, 282, 322, 280,317),]
bbs_occ_pred = na.omit(bbs_occ_pred)
mod3c = lmer(pctTrans25~(1|datasetID) * taxa * log10(meanAbundance), data=bbs_occ_pred)
summary(mod3c)
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
mod3d = lmer(pctTrans25~(1|datasetID) * system * log10(as.numeric(meanAbundance)), data=ecosys)
summary(mod3d)
occ_pred_3d = data.frame(datasetID = 999, system = unique(ecosys$system), meanAbundance =  102) # 102 is median abun for data frame (median(bbs_occ_pred$meanAbundance))
predmod3d = merTools::predictInterval(mod3d, occ_pred_3d, n.sims=1000)
predmod3d$order = c(1,2,3)

#### panel plot ####
area_plot = data.frame()
pdf('output/plots/3a_3d_25SUPP.pdf', height = 10, width = 12)
par(mfrow = c(2, 2), mar = c(5,5,1,1), cex = 1, oma = c(0,0,0,0), las = 1)
palette(colors7)

plot(NA, xlim = c(-2, 7), ylim = c(0,1), col = as.character(taxcolor$color), xlab = expression("log"[10]*" Area"), ylab = "% Transients", cex.lab = 2,frame.plot=FALSE)
b1 = for(id in scaleIDs){
  print(id)
  plotsub = subset(areamerge,datasetID == id)
  print(plotsub$taxa)
  taxa = as.character(unique(plotsub$taxa))
  mod3 = lm(plotsub$pctTrans25 ~ log10(plotsub$area))
  mod3.slope = summary(mod3)$coef[2,"Estimate"]
  mod3.coef1 = summary(mod3$coef[1])
  xnew = range(log10(plotsub$area))
  xhat <- predict(mod3, newdata = data.frame((xnew)))
  xhats = range(xhat)
  print(xhats)
  taxcolor = subset(taxcolors, taxa == as.character(plotsub$taxa)[1])
  y=summary(mod3)$coef[1] + (xhats)*summary(mod3)$coef[2]
  area_plot  = rbind(area_plot , c(id, xhats, mod3.slope,mod3.coef1,y,taxa))
  lines(log10(plotsub$area), fitted(mod3), col=as.character(taxcolor$color),lwd=4)
  par(new=TRUE)
}
title(outer=FALSE,adj=0.02,main="A",cex.main=2,col="black",font=2,line=-1)
par(new= FALSE)

plot(NA, xlim = c(0, 7), ylim = c(0,1), col = as.character(taxcolor$color), xlab = expression("log"[10]*" Community Size"), ylab = "% Transients", cex.lab = 2,frame.plot=FALSE)
b2 = for(id in unique(areamerge$datasetID)){
  print(id)
  plotsub = subset(bbs_occ,datasetID == id)
  mod3 = lm(plotsub$pctTrans25 ~ log10(plotsub$meanAbundance))
  xnew = range(log10(plotsub$meanAbundance))
  xhat <- predict(mod3, newdata = data.frame((xnew)))
  xhats = range(xhat)
  print(xhats)
  taxcolor = subset(taxcolors, taxa == as.character(plotsub$taxa)[1])
  y=summary(mod3)$coef[1] + (xhats)*summary(mod3)$coef[2]
  lines(log10(plotsub$meanAbundance), fitted(mod3), col=as.character(taxcolor$color),lwd=4)
  par(new=TRUE)
}
abline(v = log10(102), lty = 'dotted', lwd = 2) 
par(new=TRUE)
title(outer=FALSE,adj=0.02,main="B",cex.main=2,col="black",font=2,line=-1)
legend('topright', legend = as.character(taxcolors$taxa), lty=1,lwd=3,col = as.character(taxcolors$color), cex = 1.25, bty = "n")
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

areamerge.5 = merge(occ_taxa10[,c("datasetID", "site", "pctTrans10")], area, by = c("datasetID", "site"))
areamerge  = areamerge.5[, c("datasetID", "site", "taxa", "pctTrans10", "area")]


#### Figures 3a-3c panel plot #####
scaleIDs = filter(dataformattingtable, spatial_scale_variable == 'Y',
                  format_flag == 1)$dataset_ID 
scaleIDs = scaleIDs[! scaleIDs %in% c(1, 207, 210, 217, 218, 222, 223, 225, 241,258,274, 282, 322, 280, 248, 254, 279, 291)]  # waiting on data for 248
bbs_spRich = read.csv("data/BBS/bbs_abun4_spRich.csv", header = TRUE)
occ_merge = occ_taxa10[,c("datasetID", "site","taxa", "meanAbundance", "pctTrans10","pctCore","pctNeither","scale", "spRich")]
bbs_occ = occ_merge

#### Fig 3c/d predicted model ####
bbs_occ_pred = bbs_occ[!bbs_occ$datasetID %in% c(207, 210, 217, 218, 222, 223, 225, 238, 241, 258, 282, 322, 280,317),]
bbs_occ_pred = na.omit(bbs_occ_pred)
mod3c = lmer(pctTrans10~(1|datasetID) * taxa * log10(meanAbundance), data=bbs_occ_pred)
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
mod3d = lmer(pctTrans10~(1|datasetID) * system * log10(as.numeric(meanAbundance)), data=ecosys)
summary(mod3d)
occ_pred_3d = data.frame(datasetID = 999, system = unique(ecosys$system), meanAbundance =  102) # 102 is median abun for data frame (median(bbs_occ_pred$meanAbundance))
predmod3d = merTools::predictInterval(mod3d, occ_pred_3d, n.sims=1000)
predmod3d$order = c(1:3)

#### panel plot ####
area_plot = data.frame()
pdf('output/plots/3a_3d_10SUPP.pdf', height = 10, width = 12)
par(mfrow = c(2, 2), mar = c(5,5,1,1), cex = 1, oma = c(0,0,0,0), las = 1)
palette(colors7)


plot(NA, xlim = c(-2, 7), ylim = c(0,1), col = as.character(taxcolor$color), xlab = expression("log"[10]*" Area"), ylab = "% Transients", cex.lab = 2,frame.plot=FALSE)
b1 = for(id in scaleIDs){
  print(id)
  plotsub = subset(areamerge,datasetID == id)
  taxa = as.character(unique(plotsub$taxa))
  mod3 = lm(plotsub$pctTrans10 ~ log10(plotsub$area))
  mod3.slope = summary(mod3)$coef[2,"Estimate"]
  mod3.coef1 = summary(mod3$coef[1])
  xnew = range(log10(plotsub$area))
  xhat <- predict(mod3, newdata = data.frame((xnew)))
  xhats = range(xhat)
  print(xhats)
  taxcolor = subset(taxcolors, taxa == as.character(plotsub$taxa)[1])
  y=summary(mod3)$coef[1] + (xhats)*summary(mod3)$coef[2]
  area_plot  = rbind(area_plot , c(id, xhats, mod3.slope,mod3.coef1,y,taxa))
  lines(log10(plotsub$area), fitted(mod3), col=as.character(taxcolor$color),lwd=4)
  par(new=TRUE)
}
title(outer=FALSE,adj=0.02,main="A",cex.main=2,col="black",font=2,line=-1)
par(new= FALSE)

plot(NA, xlim = c(0, 7), ylim = c(0,1), col = as.character(taxcolor$color), xlab = expression("log"[10]*" Community Size"), ylab = "% Transients", cex.lab = 2,frame.plot=FALSE)
b2 = for(id in unique(areamerge$datasetID)){
  print(id)
  plotsub = subset(bbs_occ,datasetID == id)
  mod3 = lm(plotsub$pctTrans10 ~ log10(plotsub$meanAbundance))
  xnew = range(log10(plotsub$meanAbundance))
  xhat <- predict(mod3, newdata = data.frame((xnew)))
  xhats = range(xhat)
  print(xhats)
  taxcolor = subset(taxcolors, taxa == as.character(plotsub$taxa)[1])
  y=summary(mod3)$coef[1] + (xhats)*summary(mod3)$coef[2]
  lines(log10(plotsub$meanAbundance), fitted(mod3), col=as.character(taxcolor$color),lwd=4)
  par(new=TRUE)
}
abline(v = log10(102), lty = 'dotted', lwd = 2) 
par(new=TRUE)
title(outer=FALSE,adj=0.02,main="B",cex.main=2,col="black",font=2,line=-1)
legend('topright', legend = as.character(taxcolors$taxa), lty=1,lwd=3,col = as.character(taxcolors$color), cex = 1.25, bty = "n")
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

