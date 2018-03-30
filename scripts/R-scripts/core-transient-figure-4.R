###############################################
# Code for running core-transient analysis
# and data summaries over all formatted datasets.
#
# Input files are named propOcc_XXX.csv where
# XXX is the dataset ID.

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
# convert km2 to m2
bbs_abun$area = bbs_abun$area * 1000000
#### Fig 4a Area #####
area = read.csv("output/tabular_data/scaled_areas_3_2.csv", header = TRUE)

areamerge.5 = merge(occ_taxa[,c("datasetID", "site", "pctTrans")], area, by = c("datasetID", "site"), na.rm = TRUE)
areamerge.5$area = areamerge.5$area
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
mod4a = lmer(pctTrans ~ log10(area) * taxa + (log10(area)|datasetID), data=bbs_occ_area)
r.squaredGLMM(mod4a)

coefs <- data.frame(coef(summary(mod4a)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))

# R2 area
modar = lm(pctTrans~log10(area), data=bbs_occ_area)
summary(modar)

mod6 = lm(pctTrans~log10(meanAbundance), data=bbs_occ_area)
summary(mod6)

# pseudo r2 abun
mod4b = lmer(pctTrans ~ log10(meanAbundance) * taxa + (log10(meanAbundance)|datasetID), data = bbs_occ_area)
rsquared(mod4b, aicc = FALSE)

coefs <- data.frame(coef(summary(mod4b)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))

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
  
pdf('output/plots/4a_4d.pdf', height = 10, width = 14)
par(mfrow = c(2, 2), mar = c(5,5,1,1), cex = 1, oma = c(0,0,0,0), las = 1)
palette(colors7)

areamerge_fig = subset(areamerge_fig, datasetID %in% scaleIDs)
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


bbs_occ = subset(bbs_occ, datasetID %in% scaleIDs)
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

areaModel = lmer(pctTrans ~ log10(area) * taxa + (log10(area) | datasetID), data = bbs_occ_area)

dats = bbs_occ_area %>% 
  group_by(datasetID, taxa) %>% 
  dplyr::summarize(minA = min(area), maxA = max(area), minAb = min(meanAbundance), maxAb = max(meanAbundance))
dats = data.frame(dats)

minA  = dplyr::select(dats, datasetID, taxa, minA) %>% dplyr::rename(area = minA)
maxA  = dplyr::select(dats, datasetID, taxa, maxA) %>% dplyr::rename(area = maxA)

dats$minApred <- merTools::predictInterval(areaModel, minA)$fit
dats$maxApred <- merTools::predictInterval(areaModel, maxA)$fit
dats$taxa <- NULL

area_plot = merge(bbs_occ_area, dats, by = "datasetID")

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
  segments(log10(plotsub$minA), plotsub$minApred, x1 = log10(plotsub$maxA), plotsub$maxApred, col = as.character(taxcolor$color), lwd = 4)
  par(new=TRUE)
}
segments(6.7029128, range(area_plot$minApred)[1],-1.346787, range(area_plot$maxApred)[2], col = "black", lwd = 4) # range(log10(area_plot$maxA))
title(outer=FALSE,adj=0.02,main="A",cex.main=2,col="black",font=2,line=-1)
par(new= FALSE)

# B
abunModel = lmer(pctTrans ~ log10(meanAbundance) * taxa + (log10(meanAbundance) | datasetID), data = bbs_occ_area)

dats = bbs_occ_area %>% 
  group_by(datasetID, taxa) %>% 
  dplyr::summarize(minA = min(area), maxA = max(area), minAb = min(meanAbundance), maxAb = max(meanAbundance))
dats = data.frame(dats)

minAb  = dplyr::select(dats, datasetID, taxa, minAb) %>% dplyr::rename(meanAbundance = minAb)
maxAb  = dplyr::select(dats, datasetID, taxa, maxAb) %>% dplyr::rename(meanAbundance = maxAb)

dats$minAbpred <- merTools::predictInterval(abunModel, minAb)$fit
dats$maxAbpred <- merTools::predictInterval(abunModel, maxAb)$fit
dats$taxa = NULL

bbs_occ = merge(bbs_occ_area, dats, by = "datasetID")

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
segments(6.470117, range(bbs_occ$minAbpred)[1],0.243038, range(bbs_occ$maxAbpred)[2], col = "black", lwd = 4)
abline(v = log10(102), lty = 'dotted', lwd = 2) 
title(outer=FALSE,adj=0.02,main="B",cex.main=2,col="black",font=2,line=-1)
par(new= FALSE)
legend('topright', legend = as.character(taxcolors$taxa), lty=1,lwd=3,col = as.character(taxcolors$color), cex = 1.5, bty = "n")
par(new = FALSE)


b3 = barplot(predmod$fit[predmod$order], cex.names = 2,col = c(colors()[17],"gold2", "turquoise2","red","forestgreen","purple4","#1D6A9B"), ylim = c(0, 1.1), yaxt = "n")
axis(2, cex.axis = 1.5)
Hmisc::errbar(c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9), predmod$fit[predmod$order], predmod$upr[predmod$order], predmod$lwr[predmod$order], add= TRUE, lwd = 1.25, pch = 3)
mtext("% Transients", 2, cex = 2, las = 0, line = 3, mgp = c(3.25,1,0))
title(outer=FALSE,adj=0.02,main="C",cex.main=2,col="black",font=2,line=-1)

b4 = barplot(predmod4d$fit[predmod4d$order], cex.names = 1.5,col = c('burlywood','skyblue','navy'), ylim = c(0, 1), yaxt = "n")
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


####### elev heterogeneity model ################
latlongs = read.csv("data/latlongs/latlongs.csv", header =TRUE)
latlongs = filter(latlongs, datasetID != 1)
latlongs = filter(latlongs, taxa != "Fish")

bbs_latlong = read.csv("data/latlongs/bbs_2000_2014_latlongs.csv", header = TRUE)
bbs_latlong$datasetID = 1
bbs_latlong$taxa = "Bird"
bbs_latlong$Lon = bbs_latlong$Longi
bbs_latlong$Lat = bbs_latlong$Lati
bbs_latlong$site = as.factor(bbs_latlong$stateroute)
bbs_latlong = bbs_latlong[, c("datasetID", "taxa", "site", "Lat", "Lon")]
all_latlongs = rbind(latlongs, bbs_latlong)
all_latlongs = na.omit(all_latlongs)

# Makes routes into a spatialPointsDataframe
coordinates(all_latlongs)=c('Lon','Lat')
projection(all_latlongs) = CRS("+proj=longlat +ellps=WGS84")
prj.string <- CRS("+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km")
# "+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km"
# Transforms routes to an equal-area projection - see previously defined prj.string
routes.laea = spTransform(all_latlongs, CRS("+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km"))

##### extracting elevation data ####
# A function that draws a circle of radius r around a point: p (x,y)
RADIUS = 5

make.cir = function(p,r){
  points=c()
  for(i in 1:360){
    theta = i*2*pi/360
    y = p[2] + r*cos(theta)
    x = p[1] + r*sin(theta)
    points = rbind(points,c(x,y))
  }
  points=rbind(points,points[1,])
  circle=Polygon(points,hole=F)
  circle
}

routes.laea@data$dId_site = paste(routes.laea@data$datasetID, routes.laea@data$site, sep = "_")
routes.laea@data$unique = 1:1077


#Draw circles around all routes 
circs = sapply(1:nrow(routes.laea@data), function(x){
  circ =  make.cir(routes.laea@coords[x,],RADIUS)
  circ = Polygons(list(circ),ID=routes.laea$unique[x]) 
}
)

circs.sp = SpatialPolygons(circs, proj4string=CRS("+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km"))

# Check that circle locations look right
# plot(circs.sp, add = TRUE)

# read in elevation raster at 1 km resolution
elev <- raster("Z:/GIS/DEM/sdat_10003_1_20170424_102000103.tif")
NorthAm = readOGR("Z:/GIS/geography", "continent")
NorthAm2 = spTransform(NorthAm, CRS("+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km"))

# plot(elevNA2), plot(NorthAm2)

clip<-function(raster,shape) {
  a1_crop<-crop(raster,shape)
  step1<-rasterize(shape,a1_crop)
  a1_crop*step1}


elevNA2 = projectRaster(elev, crs = prj.string) #UNMASKED!
elevNA3 <- raster::mask(elev, NorthAm2)

test = clip(elev, NorthAm2)

elev.point = raster::extract(elevNA3, routes.laea)
elev.mean = raster::extract(elevNA3, circs.sp, fun = mean, na.rm=T)
elev.var = raster::extract(elevNA3, circs.sp, fun = var, na.rm=T)

env_elev = data.frame(unique = routes.laea@data$unique, elev.point = elev.point, elev.mean = elev.mean, elev.var = elev.var)


lat_scale_elev = merge(routes.laea, env_elev, by = c("unique")) # checked to make sure order lined up, d/n seem to be another way to merge since DID keeps getting lost
lat_scale_elev = data.frame(lat_scale_elev)

lat_scale_rich = merge(lat_scale_elev, summ[,c("datasetID","site", "meanAbundance")], by = c("datasetID", "site"), all.x = TRUE)
#  "spRichTrans", 
# write.csv(lat_scale_rich, "output/tabular_data/lat_scale_rich_3_30.csv", row.names = F)
lat_scale = read.csv("output/tabular_data/lat_scale_rich_5km.csv", header = TRUE, stringsAsFactors = FALSE)

lat_scale_rich_taxa = filter(lat_scale, datasetID == 1) %>% separate(., site, c("stateroute", "level", "number"), sep = "-") %>% filter(., level == 50)
lat_scale_rich_taxa$site = lat_scale_rich_taxa$stateroute
lat_scale_rich_taxa = lat_scale_rich_taxa[ -c(2:4)]
lat_scale_rich_taxa = lat_scale_rich_taxa[,c("datasetID","site", "unique", "taxa", "propTrans"  , "dId_site", "elev.point", "elev.mean" ,  "elev.var" ,"Lon","Lat", "optional","stateroute", "meanAbundance")]
lat_scale = filter(lat_scale, datasetID != 1) %>% filter(., taxa != "Fish") %>% filter(., taxa != "Plankton") %>% filter(., taxa != "Benthos")
lat_scale = lat_scale[ -13 ]

lat_scale_rich = rbind(lat_scale, lat_scale_rich_taxa)

# Model -  want 5 km radius here!!!!
# same model structure (but only terrestrial datasets, not necessarily hierarchically scaled datasets) as used in 
# core-transient-figure-4.R, but adding an elevational variance term
mod1 = lmer(propTrans ~ log10(meanAbundance) * taxa +  log10(elev.var) + (log10(meanAbundance)|datasetID) , data=lat_scale_rich) 

summary(mod1)
coefs <- data.frame(coef(summary(mod1)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))

ggplot(data=lat_scale_rich, aes(elev.var,propTrans)) +geom_point(aes(color = as.factor(lat_scale_rich$taxa)), size = 3) + xlab("Elevation Variance")+ ylab("% Transient")+ theme_classic()

# visualizing model results
mod1test = subset(lat_scale_rich, lat_scale_rich$datasetID == 1)
mod1test$scale =  strsplit(mod1test$site,"-")
mod1test$scaled = sapply(mod1test$scale, "[[", 2) # selects the second element in a list
ggplot(data=mod1test, aes(elev.var,propTrans)) + geom_point(aes(color = as.factor(as.numeric(mod1test$scaled))), size = 3)+ xlab("Elevation Variance")+ ylab("BBS Scaled % Transient")  + theme_classic() 
hist(mod1test$propTrans)

# simple linear model based on data in Fig 2b of % transient ~ taxonomic group, just to have a p-value associated with the statement "The proportion of an assemblage made up of transient species varied strongly across taxonomic group."
transmod = lm(pTrans~taxa, data = CT_long)
summary(transmod)

summ$scale = 1
summ$n = 1
summ_cut = summ[,c("datasetID", "site", "scale", "n")]
bbs_occ_scale$datasetID = 1
bbs_occ_scale$n = bbs_occ_scale$spRich
bbs_all = bbs_occ_scale[,c("datasetID", "site", "scale", "n")]
new_df = rbind(allrich, summ_cut, bbs_all)
new = data.frame(table(new_df$datasetID))



















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
