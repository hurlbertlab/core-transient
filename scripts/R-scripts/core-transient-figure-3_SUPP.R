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
# alt_trans = read.csv("output/tabular_data/alt_trans_thresholds.csv", header =TRUE)

occ_alt = merge(occ_taxa, alt_trans[,c("datasetID", "site","propTrans33", "propTrans25", "propTrans10")], all = TRUE)

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
bbs_abun = read.csv("data/BBS/bbs_abun_occ.csv", header=TRUE)

bbs_area = merge(bbs_abun, alt_trans, by.x = "stateroute", by.y = "site")
bbs_area$site = bbs_area$stateroute
bbs_area = bbs_area [, c("datasetID", "site", "taxa", "propTrans25", "area")]
#### Fig 3a Area #####
area = read.csv("output/tabular_data/scaled_areas_3_2.csv", header = TRUE)

areamerge.5 = merge(alt_trans[,c("datasetID", "site", "propTrans25")], area, by = c("datasetID", "site"))
areamerge  = areamerge.5 [, c("datasetID", "site", "taxa", "propTrans25", "area")]

areamerge = rbind(bbs_area, areamerge)
#### Figures 3a-3c panel plot #####
scaleIDs = filter(dataformattingtable, spatial_scale_variable == 'Y',
                  format_flag == 1)$dataset_ID 
scaleIDs = scaleIDs[! scaleIDs %in% c(207, 210, 217, 218, 222, 223, 225, 241,258, 282, 322, 280, 248, 254, 291)]  # waiting on data for 248
bbs_spRich = read.csv("data/BBS/bbs_abun4_spRich.csv", header = TRUE)
occ_merge = occ_alt[,c("datasetID", "site","taxa", "meanAbundance", "propTrans25","pctCore","pctNeither","scale", "spRich")]
bbs_occ = occ_merge

#### Fig 3c/d predicted model ####
bbs_occ_pred = bbs_occ[!bbs_occ$datasetID %in% c(207, 210, 217, 218, 222, 223, 225, 238, 241, 258, 282, 322, 280,317),]
bbs_occ_pred = na.omit(bbs_occ_pred)
mod3c = lmer(propTrans25~(1|datasetID) * taxa * log10(meanAbundance), data=bbs_occ_pred)
summary(mod3c)
occ_sub_pred = data.frame(datasetID = 999, taxa = unique(bbs_occ_pred$taxa), meanAbundance =  102) # 102 is median abun for data frame (median(bbs_occ_pred$meanAbundance))
# to test: test = filter(occ_sub_pred, taxa == "Invertebrate")
predmod3c = merTools::predictInterval(mod3c, occ_sub_pred, n.sims=1000)

# matching by predicted output vals based on occ_sub_pred
predmod3c$taxa = c("Mammal","Fish","Invertebrate","Bird", "Plankton") 
# write.csv(predmod3c, "output/tabular_data/predmod3c.csv", row.names = FALSE)

predmod = merge(predmod3c, taxcolors, by = "taxa")
predmod$taxorder = c(3,2,5,4,1)

# 3d
ecosys = merge(bbs_occ_pred, dataformattingtable[,c("dataset_ID", "system")], by.y = "dataset_ID", by.x = "datasetID")
mod3d = lmer(propTrans25~(1|datasetID) * system * log10(as.numeric(meanAbundance)), data=ecosys)
summary(mod3d)
occ_pred_3d = data.frame(datasetID = 999, system = unique(ecosys$system), meanAbundance =  102) # 102 is median abun for data frame (median(bbs_occ_pred$meanAbundance))
predmod3d = merTools::predictInterval(mod3d, occ_pred_3d, n.sims=1000)
predmod3d$order = c(1,3,2)
  
#### panel plot ####
area_plot = data.frame()
pdf('output/plots/3a_3d_25SUPP.pdf', height = 10, width = 12)
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 1, 1), cex = 1, oma = c(0,0,0,0), las = 1)
palette(colors7)

plot(NA, xlim = c(-2, 7), ylim = c(0,1), col = as.character(taxcolor$color), xlab = expression("Log"[10]*" Area"), ylab = "% Transients", cex.lab = 1.5,frame.plot=FALSE)
b1 = for(id in scaleIDs){
  print(id)
  plotsub = subset(areamerge,datasetID == id)
  taxa = as.character(unique(plotsub$taxa))
  mod3 = lm(plotsub$propTrans25 ~ log10(plotsub$area))
  mod3.slope = summary(mod3)$coef[2,"Estimate"]
  mod3.coef1 = summary(mod3$coef[1])
  xnew = range(log10(plotsub$area))
  xhat <- predict(mod3, newdata = data.frame((xnew)))
  xhats = range(xhat)
  print(xhats)
  taxcolor = subset(taxcolors, taxa == as.character(plotsub$taxa)[1])
  area_plot  = rbind(area_plot , c(id, xhats, mod3.slope,mod3.coef1,y,taxa))
  y=summary(mod3)$coef[1] + (xhats)*summary(mod3)$coef[2]
   lines(log10(plotsub$area), fitted(mod3), col=as.character(taxcolor$color),lwd=4)
  par(new=TRUE)
}
title(outer=FALSE,adj=0.02,main="A",cex.main=1.5,col="black",font=2,line=-1)
par(new= FALSE)

plot(NA, xlim = c(0, 7), ylim = c(0,1), col = as.character(taxcolor$color), xlab = expression("Log"[10]*" Community Size"), ylab = "% Transients", cex.lab = 1.5,frame.plot=FALSE)
b2 = for(id in scaleIDs){
  print(id)
  plotsub = subset(bbs_occ,datasetID == id)
  mod3 = lm(plotsub$propTrans25 ~ log10(plotsub$meanAbundance))
  xnew = range(log10(plotsub$meanAbundance))
  xhat <- predict(mod3, newdata = data.frame((xnew)))
  xhats = range(xhat)
  print(xhats)
  taxcolor = subset(taxcolors, taxa == as.character(plotsub$taxa)[1])
  y=summary(mod3)$coef[1] + (xhats)*summary(mod3)$coef[2]
  lines(log10(plotsub$meanAbundance), fitted(mod3), col=as.character(taxcolor$color),lwd=4)
  par(new=TRUE)
}
par(new=TRUE)
title(outer=FALSE,adj=0.02,main="B",cex.main=1.5,col="black",font=2,line=-1)
legend('topright', legend = as.character(taxcolors$taxa), lty=1,lwd=3,col = as.character(taxcolors$color), cex = 1.25, bty = "n")
par(new = FALSE)

b3 = barplot(predmod$fit[predmod$taxorder], cex.names = 1.5,col = c("gold2", "turquoise2","red","purple4","#1D6A9B"), ylim = c(0, 3))
Hmisc::errbar(c(0.7, 1.9, 3.1, 4.3, 5.5), predmod$fit[predmod$taxorder], predmod$upr[predmod$taxorder], predmod$lwr[predmod$taxorder], add= TRUE, lwd = 1.25, pch = 3)
mtext("% Transients", 2, cex = 1.5, las = 0, line = 2.5)
title(outer=FALSE,adj=0.02,main="C",cex.main=1.5,col="black",font=2,line=-1)

b4 = barplot(predmod3d$fit[predmod3d$order], cex.names = 1.5,col = c('burlywood','navy','skyblue'), ylim = c(0, 0.8))
Hmisc::errbar(c(0.7, 1.9, 3.1), predmod3d$fit[predmod3d$order], predmod3d$upr[predmod3d$order], predmod3d$lwr[predmod3d$order], add= TRUE, lwd = 1.25, pch = 3)
mtext("% Transients", 2, cex = 1.5, las = 0, line = 2.5)
title(outer=FALSE,adj=0.02,main="D",cex.main=1.5,col="black",font=2,line=-1)
dev.off()

dev.off()

colnames(area_plot) = c("id","xlow","xhigh","slope", "taxa")
area_plot = data.frame(area_plot)
area_plot$datasetID = as.numeric(area_plot$id)
area_plot$xlow = as.numeric(area_plot$xlow)
area_plot$xhigh = as.numeric(area_plot$xhigh)
area_plot$slope = as.numeric(area_plot$slope)
write.csv(area_plot, "output/tabular_data/fig_3a_output.csv", row.names =FALSE)


# ggplot not happening
#area_plot$taxa = factor(area_plot$taxa, levels = c('Invertebrate','Fish','Plankton','Mammal','Plant','Bird','Benthos'),ordered = TRUE)
#colscale = c("gold2","turquoise2", "red", "purple4","forestgreen","#1D6A9B", "azure4")

#p <- ggplot(area_plot, aes(x = log10(area), y = slope))
#p + geom_abline(intercept = 0,slope = 1, lwd =1.5,linetype="dashed") + geom_point(aes(colour = taxa), size = 6) + xlab("Species Richness") + ylab("Species Richness Without Transients") + scale_colour_manual(breaks = plot_relationship$taxa,values = colscale) + theme(axis.text.x=element_text(size=24),axis.text.y=element_text(size=24),axis.title.x=element_text(size=32),axis.title.y=element_text(size=32,angle=90,vjust = 2))+ theme_classic()
#ggsave(file="C:/Git/core-transient/output/plots/sparea_4c.pdf", height = 10, width = 15)

#### Figure 3b transients and scale ####
pdf('output/plots/3b_sara_scale_transient_reg.pdf', height = 6, width = 7.5)
par(mfrow = c(1, 1), mar = c(6, 6, 1, 1), mgp = c(4, 1, 0), 
    cex.axis = 1.5, cex.lab = 2, las = 1)
palette(colors7)

bbs_spRich = read.csv("data/BBS/bbs_abun4_spRich.csv", header = TRUE)
occ_merge = occ_alt[,c("datasetID", "site","taxa", "meanAbundance", "propTrans25","pctCore","pctNeither","scale", "spRich")]
bbs_occ = rbind(bbs_spRich,occ_merge)

for(id in scaleIDs){
  print(id)
  plotsub = subset(bbs_occ,datasetID == id)
  mod3 = lm(plotsub$propTrans25 ~ log10(plotsub$meanAbundance))
  xnew = range(log10(plotsub$meanAbundance))
  xhat <- predict(mod3, newdata = data.frame((xnew)))
  xhats = range(xhat)
  print(xhats)
  taxcolor = subset(taxcolors, taxa == as.character(plotsub$taxa)[1])
  y=summary(mod3)$coef[1] + (xhats)*summary(mod3)$coef[2]
  plot(NA, xlim = c(0, 7), ylim = c(0,1), col = as.character(taxcolor$color), xlab = expression("Log"[10]*" Community Size"), ylab = "% Transients", cex = 1.5)
  lines(log10(plotsub$meanAbundance), fitted(mod3), col=as.character(taxcolor$color),lwd=5)
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
  mod3 = lm((1-plotsub$propTrans25) ~ log10(plotsub$meanAbundance))
  xnew=range(log10(plotsub$meanAbundance))
  xhat <- predict(mod3, newdata = data.frame((xnew)))
  xhats = range(xhat)
  print(xhats)
  taxcolor=subset(taxcolors, taxa == as.character(plotsub$taxa)[1])
  y=summary(mod3)$coef[1] + (xhats)*summary(mod3)$coef[2]
  plot(NA, xlim = c(0, 7), ylim = c(0,1), col = as.character(taxcolor$color), xlab = expression("Log"[10]*" Community Size"), ylab = "% Core", cex = 1.5)
  lines(log10(plotsub$meanAbundance), fitted(mod3), col=as.character(taxcolor$color),lwd=5)
  par(new=TRUE)
}
segments(0,  0, x1 = 5.607, y1 = 1, col = rgb(29/255, 106/255, 155/255), lwd=5)
par(new=TRUE)
dev.off()

##### old ggplot #####
p <- ggplot(predmod, aes(x = factor(abbrev), y = fit, fill=factor(predmod$taxa)))
p +geom_bar(stat = "identity", fill = levels(colscale))+ theme_classic() + geom_errorbar(ymin = predmod$lwr, ymax= predmod$upr, width=0.2) + xlab("") + ylab("Proportion of Species") + ylim(0, 1) + theme(axis.ticks.x=element_blank(),axis.text.x=element_blank(),axis.text.y=element_text(size=30),axis.title.x=element_text(size=30),axis.title.y=element_text(size=24,angle=90,vjust = 2))+guides(fill=guide_legend(title="",keywidth = 2, keyheight = 1)) 

ggsave(file="C:/Git/core-transient/output/plots/3c_predmod.pdf", height = 10, width = 15)