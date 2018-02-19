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
library(piecewiseSEM)
library(MuMIn)

source('scripts/R-scripts/core-transient_functions.R')

# Specify here the datasetIDs and then run the code below.
dataformattingtable = read.csv('data_formatting_table.csv', header = T) 

datasetIDs = dataformattingtable$dataset_ID[dataformattingtable$format_flag == 1]

# BBS (dataset 1) will be analyzed separately for now.
datasetIDs = datasetIDs[!datasetIDs %in% c(1)]

#################### FIG 4 ######################### 
occ_taxa=read.csv("output/tabular_data/occ_taxa.csv",header=TRUE)

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
scaleIDs = scaleIDs[! scaleIDs %in% c(207, 210, 217, 218, 222, 223, 225, 238, 241,258, 282, 322, 280,317)]
bbs_abun = read.csv("data/BBS/bbs_allscales33.csv", header=TRUE)
bbs_abun$pctTrans = bbs_abun$propTrans
#### Fig 4a Area #####
area = read.csv("output/tabular_data/scaled_areas_3_2.csv", header = TRUE)

areamerge.5 = merge(occ_taxa[,c("datasetID", "site", "pctTrans")], area, by = c("datasetID", "site"), na.rm = TRUE)
areamerge1  = areamerge.5 [, c("datasetID", "site", "taxa", "pctTrans", "area")]

# read in bbs abundance data
bbs_area = bbs_abun[, c("datasetID", "site", "taxa", "pctTrans", "area")]
areamerge = rbind(bbs_area,areamerge1)
# write.csv(areamerge, "output/tabular_data/areamerge.csv", row.names = FALSE)

#### Figures 4a-4c panel plot #####
scaleIDs = filter(dataformattingtable, spatial_scale_variable == 'Y',
                  format_flag == 1)$dataset_ID 
scaleIDs = scaleIDs[! scaleIDs %in% c(207, 210, 217, 218, 222, 223, 225, 241,258, 282, 322, 280, 248, 254, 279, 291)]  # waiting on data for 248
bbs_abun$pctCore = bbs_abun$propCore
bbs_spRich = bbs_abun[,c("datasetID","site","taxa", "meanAbundance", "pctTrans","pctCore")]
occ_merge = occ_taxa[,c("datasetID", "site","taxa", "meanAbundance", "pctTrans","pctCore")]
bbs_occ = rbind(bbs_spRich,occ_merge)
bbs_occ = bbs_occ[!bbs_occ$site %in% c("53800-5-6", "53800-25-2"),]

#### Fig 4c/d predicted model ####
bbs_occ_pred = bbs_occ[!bbs_occ$datasetID %in% c(207, 210, 217, 218, 222, 223, 225, 238, 241, 248, 258, 282, 322, 280,317),]

mod4c = lmer(pctTrans ~ log10(meanAbundance) * taxa + (log10(meanAbundance)|datasetID), data = bbs_occ_pred)
summary(mod4c)
occ_sub_pred = data.frame(datasetID = 999, taxa = unique(bbs_occ_pred$taxa), meanAbundance =  102) # 102 is median abun for data frame (median(bbs_occ_pred$meanAbundance))
# to test: test = filter(occ_sub_pred, taxa == "Invertebrate")
predmod4c = merTools::predictInterval(mod4c, occ_sub_pred, n.sims=1000)

# matching by predicted output vals based on occ_sub_pred
predmod4c$taxa = c("Bird","Invertebrate", "Plant", "Mammal","Fish", "Plankton", "Benthos") 
# write.csv(predmod4c, "output/tabular_data/predmod4c.csv", row.names = FALSE)

predmod = merge(predmod4c, taxcolors, by = "taxa")

lm.hsd = lm(fit ~ taxa, data= predmod) #Tukeys HSD
summary(aov(fit ~ taxa, data= predmod), test = "Chisq")
agricolae::HSD.test(lm.hsd, "taxa")
predmod$order = c(1,4,3,6,7,5,2)

# 4d
ecosys = merge(bbs_occ_pred, dataformattingtable[,c("dataset_ID", "system")], by.y = "dataset_ID", by.x = "datasetID")
mod4d = lmer(pctTrans ~ log10(meanAbundance) * system + (log10(meanAbundance)|datasetID), data=ecosys)
summary(mod4d)
occ_pred_4d = data.frame(datasetID = 999, system = unique(ecosys$system), meanAbundance =  102) # 102 is median abun for data frame (median(bbs_occ_pred$meanAbundance))
predmod4d = merTools::predictInterval(mod4d, occ_pred_4d, n.sims=1000)
predmod4d$order = c(1:3)


# pseudo r2 area
bbs_occ_area = merge(bbs_occ_pred, areamerge[,c("datasetID", "site", "area")], by = c("datasetID", "site"))
bbs_occ_area = na.omit(bbs_occ_area)
mod4a = lmer(pctTrans ~ log10(area) * taxa + (log10(area)|datasetID), data=bbs_occ_area)
r.squaredGLMM(mod4a)
mod_a = lm(bbs_occ_area$pctTrans~predict(mod4a))
summary(mod_a)
rsquared(mod4a, aicc = FALSE)

# pseudo r2 abun
mod4b = lmer(pctTrans ~ log10(meanAbundance) * taxa + (log10(meanAbundance)|datasetID), data = bbs_occ_area)
mod_r = lm(bbs_occ_area$pctTrans~predict(mod4b))
summary(mod_r)
rsquared(mod4b, aicc = FALSE)

# R2 area
modar = lm(pctTrans~log10(area), data=bbs_occ_area)
summary(modar)

mod6 = lm(pctTrans~log10(meanAbundance), data=bbs_occ_area)
summary(mod6)
# The marginal R squared values are those associated with your fixed effects, 
# the conditional ones are those of your fixed effects plus the random effects. 
# Usually we will be interested in the marginal effects.
# https://ecologyforacrowdedplanet.wordpress.com/2013/08/27/r-squared-in-mixed-models-the-easy-way/


#### panel plot ####
area_plot = data.frame()
areamerge_fig = read.csv("output/tabular_data/areafig.csv", header = TRUE)
area.5 = merge(occ_taxa[,c("datasetID", "site", "pctTrans")], areamerge_fig, by = c("datasetID", "site"), na.rm = TRUE)
area.5  = area.5 [, c("datasetID", "site", "taxa", "pctTrans", "area")]
areamerge_fig = rbind(bbs_area,area.5)
# areamerge %>%
#  dplyr::filter(datasetID == c(277, 280, 314)) %>%
#  summarise(volchange = area^(2/3))
  
  
pdf('output/plots/4a_4d.pdf', height = 10, width = 14)
par(mfrow = c(2, 2), mar = c(5,5,1,1), cex = 1, oma = c(0,0,0,0), las = 1)
palette(colors7)

areamerge_fig = na.omit(areamerge_fig)
all = lm(areamerge_fig$pctTrans ~ log10(areamerge_fig$area))
xnew = range(log10(areamerge_fig$area))
xhat <- predict(all, newdata = data.frame((xnew)))
xhats = range(xhat)
lower = range(xhat)[1]
upper = range(xhat)[2]


plot(NA, xlim = c(-2, 8), ylim = c(0,1), xlab = expression("log"[10]*" Area (m"^2*")"), 
     ylab = "% Transients", cex.lab = 2, frame.plot=FALSE, xaxt = "n", yaxt = "n", 
     mgp = c(3.25,1,0))
axis(1, cex.axis =  1.5)
axis(2, cex.axis =  1.5)
b1 = for(id in scaleIDs){
  print(id)
  plotsub = subset(areamerge_fig,datasetID == id)
  taxa = as.character(unique(plotsub$taxa))
  mod4 = lm(plotsub$pctTrans ~ log10(plotsub$area))
  mod4.slope = summary(mod4)$coef[2,"Estimate"]
  mod4.coef1 = summary(mod4$coef[1])[3]
  xnew = range(log10(plotsub$area))
  xhat <- predict(mod4, newdata = data.frame((xnew)))
  xhats = range(xhat)
  lower = range(xhat)[1]
  upper = range(xhat)[2]
  print(xhats)
  taxcolor = subset(taxcolors, taxa == as.character(plotsub$taxa)[1])
  y= summary(mod4)$coef[1]+ (xhats)*summary(mod4)$coef[2]
  area_plot  = rbind(area_plot , c(id, lower,upper, mod4.slope,taxa))
   lines(log10(plotsub$area), fitted(mod4), col=as.character(taxcolor$color),lwd=4)
   # points(log10(plotsub$area), plotsub$pctTrans)
  par(new=TRUE)
}
lines(log10(areamerge_fig$area), fitted(all), col="black", lwd=3)
title(outer=FALSE,adj=0.02,main="A",cex.main=2,col="black",font=2,line=-1)
par(new= FALSE)


bbs_occ = na.omit(bbs_occ)
occ_all = lm(bbs_occ$pctTrans ~ log10(bbs_occ$meanAbundance))
xnew = range(log10(bbs_occ$meanAbundance))
xhat <- predict(occ_all, newdata = data.frame((xnew)))
xhats = range(xhat)

plot(NA, xlim = c(0, 7), ylim = c(0,1), col = as.character(taxcolor$color), xlab = expression("log"[10]*" Community Size"), ylab = "% Transients", cex.lab = 2,frame.plot=FALSE, yaxt = "n", xaxt = "n", mgp = c(3.25,1,0))
axis(1, cex.axis =  1.5)
axis(2, cex.axis =  1.5)
b2 = for(id in scaleIDs){
  print(id)
  plotsub = subset(bbs_occ,datasetID == id)
  mod4 = lm(plotsub$pctTrans ~ log10(plotsub$meanAbundance))
  xnew = range(log10(plotsub$meanAbundance))
  xhat <- predict(mod4, newdata = data.frame((xnew)))
  xhats = range(xhat)
  print(xhats)
  taxcolor = subset(taxcolors, taxa == as.character(plotsub$taxa)[1])
  y=summary(mod4)$coef[1] + (xhats)*summary(mod4)$coef[2]
  lines(log10(plotsub$meanAbundance), fitted(mod4), col=as.character(taxcolor$color),lwd=4)
  # points(log10(plotsub$meanAbundance), plotsub$pctTrans)
  par(new=TRUE)
}
abline(v = log10(102), lty = 'dotted', lwd = 2) 
par(new=TRUE)
title(outer=FALSE,adj=0.02,main="B",cex.main=2,col="black",font=2,line=-1)
lines(log10(bbs_occ$meanAbundance), fitted(occ_all), col="black",lwd=3)
legend('topright', legend = as.character(taxcolors$taxa), lty=1,lwd=3,col = as.character(taxcolors$color), cex = 1.5, bty = "n")
par(new = FALSE)

b4 = barplot(predmod$fit[predmod$order], cex.names = 2,col = c(colors()[17],"gold2", "turquoise2","red","forestgreen","purple4","#1D6A9B"), ylim = c(0, 1.1), yaxt = "n")
axis(2, cex.axis = 1.5)
Hmisc::errbar(c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9), predmod$fit[predmod$order], predmod$upr[predmod$order], predmod$lwr[predmod$order], add= TRUE, lwd = 1.25, pch = 3)
mtext("% Transients", 2, cex = 2, las = 0, line = 3, mgp = c(3.25,1,0))
title(outer=FALSE,adj=0.02,main="C",cex.main=2,col="black",font=2,line=-1)

b4 = barplot(predmod4d$fit[predmod4d$order], cex.names = 1.5,col = c('burlywood','skyblue','navy'), ylim = c(0, 0.8), yaxt = "n")
axis(2, cex.axis = 1.5)
Hmisc::errbar(c(0.7, 1.9, 3.1), predmod4d$fit[predmod4d$order], predmod4d$upr[predmod4d$order], predmod4d$lwr[predmod4d$order], add= TRUE, lwd = 1.25, pch = 3)
mtext("% Transients", 2, cex = 2, las = 0, line = 3, mgp = c(3.25,1,0))
title(outer=FALSE,adj=0.02,main="D",cex.main=2,col="black",font=2,line=-1)
dev.off()

dev.off()

colnames(area_plot) = c("id","xlow","xhigh","slope", "taxa")
area_plot = data.frame(area_plot)
area_plot$datasetID = as.numeric(area_plot$id)
area_plot$xlow = as.numeric(area_plot$xlow)
area_plot$xhigh = as.numeric(area_plot$xhigh)
area_plot$slope = as.numeric(area_plot$slope)
write.csv(area_plot, "output/tabular_data/fig_4a_output.csv", row.names =FALSE)




###### panel plot pred model ######
pdf('output/plots/4a_4d_pred.pdf', height = 10, width = 14)
par(mfrow = c(2, 2), mar = c(5,5,1,1), cex = 1, oma = c(0,0,0,0), las = 1)
palette(colors7)

areamerge_fig = na.omit(areamerge_fig)
all = lmer(pctTrans ~ log10(area) * taxa + (log10(area) | datasetID), data = bbs_occ_area)

xnew = range(log10(areamerge_fig$area))
xhat <- predict(all, newdata = data.frame((xnew)))
xhats = range(xhat)
lower = range(xhat)[1]
upper = range(xhat)[2]


plot(NA, xlim = c(-2, 8), ylim = c(0,1), xlab = expression("log"[10]*" Area (m"^2*")"), 
     ylab = "% Transients", cex.lab = 2, frame.plot=FALSE, xaxt = "n", yaxt = "n", 
     mgp = c(3.25,1,0))
axis(1, cex.axis =  1.5)
axis(2, cex.axis =  1.5)
b1 = for(id in scaleIDs){
  print(id)
  plotsub = subset(areamerge_fig,datasetID == id)
  taxa = as.character(unique(plotsub$taxa))
  mod4 = lmer(pctTrans ~ log10(area) * taxa + (log10(area) | datasetID), data = plotsub)
  mod4.slope = summary(mod4)$coef[2,"Estimate"]
  mod4.coef1 = summary(mod4$coef[1])[3]
  xnew = range(log10(plotsub$area))
  xhat <- predict(mod4, newdata = data.frame((xnew)))
  xhats = range(xhat)
  lower = range(xhat)[1]
  upper = range(xhat)[2]
  print(xhats)
  taxcolor = subset(taxcolors, taxa == as.character(plotsub$taxa)[1])
  y= summary(mod4)$coef[1]+ (xhats)*summary(mod4)$coef[2]
  area_plot  = rbind(area_plot , c(id, lower,upper, mod4.slope,taxa))
  lines(log10(plotsub$area), fitted(mod4), col=as.character(taxcolor$color),lwd=4)
  # points(log10(plotsub$area), plotsub$pctTrans)
  par(new=TRUE)
}
lines(log10(areamerge_fig$area), fitted(all), col="black", lwd=3)
title(outer=FALSE,adj=0.02,main="A",cex.main=2,col="black",font=2,line=-1)
par(new= FALSE)


bbs_occ = na.omit(bbs_occ)
occ_all = lm(bbs_occ$pctTrans ~ log10(bbs_occ$meanAbundance))
xnew = range(log10(bbs_occ$meanAbundance))
xhat <- predict(occ_all, newdata = data.frame((xnew)))
xhats = range(xhat)

plot(NA, xlim = c(0, 7), ylim = c(0,1), col = as.character(taxcolor$color), xlab = expression("log"[10]*" Community Size"), ylab = "% Transients", cex.lab = 2,frame.plot=FALSE, yaxt = "n", xaxt = "n", mgp = c(3.25,1,0))
axis(1, cex.axis =  1.5)
axis(2, cex.axis =  1.5)
b2 = for(id in scaleIDs){
  print(id)
  plotsub = subset(bbs_occ,datasetID == id)
  mod4 = lm(plotsub$pctTrans ~ log10(plotsub$meanAbundance))
  xnew = range(log10(plotsub$meanAbundance))
  xhat <- predict(mod4, newdata = data.frame((xnew)))
  xhats = range(xhat)
  print(xhats)
  taxcolor = subset(taxcolors, taxa == as.character(plotsub$taxa)[1])
  y=summary(mod4)$coef[1] + (xhats)*summary(mod4)$coef[2]
  lines(log10(plotsub$meanAbundance), fitted(mod4), col=as.character(taxcolor$color),lwd=4)
  # points(log10(plotsub$meanAbundance), plotsub$pctTrans)
  par(new=TRUE)
}
abline(v = log10(102), lty = 'dotted', lwd = 2) 
par(new=TRUE)
title(outer=FALSE,adj=0.02,main="B",cex.main=2,col="black",font=2,line=-1)
lines(log10(bbs_occ$meanAbundance), fitted(occ_all), col="black",lwd=3)
legend('topright', legend = as.character(taxcolors$taxa), lty=1,lwd=3,col = as.character(taxcolors$color), cex = 1.5, bty = "n")
par(new = FALSE)

b4 = barplot(predmod$fit[predmod$order], cex.names = 2,col = c(colors()[17],"gold2", "turquoise2","red","forestgreen","purple4","#1D6A9B"), ylim = c(0, 1.1), yaxt = "n")
axis(2, cex.axis = 1.5)
Hmisc::errbar(c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9), predmod$fit[predmod$order], predmod$upr[predmod$order], predmod$lwr[predmod$order], add= TRUE, lwd = 1.25, pch = 3)
mtext("% Transients", 2, cex = 2, las = 0, line = 3, mgp = c(3.25,1,0))
title(outer=FALSE,adj=0.02,main="C",cex.main=2,col="black",font=2,line=-1)

b4 = barplot(predmod4d$fit[predmod4d$order], cex.names = 1.5,col = c('burlywood','skyblue','navy'), ylim = c(0, 0.8), yaxt = "n")
axis(2, cex.axis = 1.5)
Hmisc::errbar(c(0.7, 1.9, 3.1), predmod4d$fit[predmod4d$order], predmod4d$upr[predmod4d$order], predmod4d$lwr[predmod4d$order], add= TRUE, lwd = 1.25, pch = 3)
mtext("% Transients", 2, cex = 2, las = 0, line = 3, mgp = c(3.25,1,0))
title(outer=FALSE,adj=0.02,main="D",cex.main=2,col="black",font=2,line=-1)
dev.off()

dev.off()

###### ######

# pseudo r2
mod4 = lm(bbs_occ$pctTrans ~ log10(bbs_occ$meanAbundance))
bbs_r = na.omit(bbs_occ)
mod4_r = lm(bbs_r$pctTrans~predict(mod4))
summary(mod4_r)

#### Figure 4b transients and scale ####
pdf('output/plots/4b_sara_scale_transient_reg.pdf', height = 6, width = 7.5)
par(mfrow = c(1, 1), mar = c(6, 6, 1, 1), mgp = c(4, 1, 0), 
    cex.axis = 1.5, cex.lab = 2, las = 1)
palette(colors7)

occ_merge = occ_taxa[,c("datasetID", "site","taxa", "meanAbundance", "pctTrans","pctCore")]
bbs_occ = rbind(bbs_spRich,occ_merge)

for(id in scaleIDs){
  print(id)
  plotsub = subset(bbs_occ,datasetID == id)
  mod4 = lm(plotsub$pctTrans ~ log10(plotsub$meanAbundance))
  xnew = range(log10(plotsub$meanAbundance))
  xhat <- predict(mod4, newdata = data.frame((xnew)))
  xhats = range(xhat)
  print(xhats)
  taxcolor = subset(taxcolors, taxa == as.character(plotsub$taxa)[1])
  y=summary(mod4)$coef[1] + (xhats)*summary(mod4)$coef[2]
  plot(NA, xlim = c(0, 7), ylim = c(0,1), col = as.character(taxcolor$color), xlab = expression("Log"[10]*" Community Size"), ylab = "Proportion transient species", cex = 1.5)
  lines(log10(plotsub$meanAbundance), fitted(mod4), col=as.character(taxcolor$color),lwd=5)
  par(new=TRUE)
}
par(new=TRUE)
legend('topright', legend = as.character(taxcolors$taxa), lty=1,lwd=3,col = as.character(taxcolors$color), cex = 1.35)
L = legend('topright', legend = as.character(taxcolors$taxa), lty=1,lwd=3,col = as.character(taxcolors$color), cex = 1.35)
dev.off()

#### Supplemental core and scale ####
pdf('output/plots/supp_sara_scale_core_reg.pdf', height = 6, width = 7.5)
par(mfrow = c(1, 1), mar = c(6, 6, 1, 1), mgp = c(4, 1, 0), 
    cex.axis = 1.5, cex.lab = 2, las = 1)
palette(colors7)
for(id in scaleIDs){
  print(id)
  plotsub = subset(bbs_occ,datasetID == id)
  mod4 = lm((1-plotsub$pctTrans) ~ log10(plotsub$meanAbundance))
  xnew=range(log10(plotsub$meanAbundance))
  xhat <- predict(mod4, newdata = data.frame((xnew)))
  xhats = range(xhat)
  print(xhats)
  taxcolor=subset(taxcolors, taxa == as.character(plotsub$taxa)[1])
  y=summary(mod4)$coef[1] + (xhats)*summary(mod4)$coef[2]
  plot(NA, xlim = c(0, 7), ylim = c(0,1), col = as.character(taxcolor$color), xlab = expression("Log"[10]*" Community Size"), ylab = "% Core", cex = 1.5)
  lines(log10(plotsub$meanAbundance), fitted(mod4), col=as.character(taxcolor$color),lwd=5)
  par(new=TRUE)
}
segments(0,  0, x1 = 5.607, y1 = 1, col = rgb(29/255, 106/255, 155/255), lwd=5)
par(new=TRUE)
dev.off()
