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

# pseudo r2 area
bbs_occ_area = merge(bbs_occ_pred, areamerge[,c("datasetID", "site", "area")], by = c("datasetID", "site"))

##### merge in 25%
occ_taxa25=read.csv("output/tabular_data/occ_taxa_25.csv",header=TRUE)
occ_taxa25$pctTrans25 = occ_taxa25$pctTrans
occ_taxa10=read.csv("output/tabular_data/occ_taxa_10.csv",header=TRUE)
occ_taxa10$pctTrans10 = occ_taxa10$pctTrans
occ_taxa = merge(occ_taxa25, occ_taxa10[,c("datasetID", "site", "pctTrans10")], by = c("datasetID", "site"))

summ25 = read.csv("output/tabular_data/core-transient_summary_25.csv", header = TRUE)
summ10 = read.csv("output/tabular_data/core-transient_summary_10.csv", header = TRUE)

bbs_focal_occs_pctTrans = read.csv("data/BBS/bbs_focal_occs_pctTrans_Site.csv", header = TRUE)
bbs_area2 = right_join(bbs_abun[, c("site", "area")], bbs_focal_occs_pctTrans, by = "site")
bbs_area2 = bbs_area2[!duplicated(bbs_area2), ]
bbs_area2$pctTrans25 = bbs_area2$propTrans25
bbs_area2$site = as.factor(bbs_area2$site)
bbs_area2  = bbs_area2[, c("datasetID", "site", "taxa", "pctTrans25", "area")]
bbs_area2$area = bbs_area2$area * 1000000

areamerge.5 = merge(occ_taxa25[,c("datasetID", "site", "pctTrans25")], area, by = c("datasetID", "site"))
areamerge  = areamerge.5[, c("datasetID", "site", "taxa", "pctTrans25", "area")]

areamerge = rbind(bbs_area2, areamerge)
areamerge = na.omit(areamerge)

bbs_spRich25 = merge(bbs_abun, bbs_focal_occs_pctTrans[,c("site", "propTrans25")], by = "site")  
bbs_spRich25$pctTrans25 = bbs_spRich25$propTrans25
bbs_spRich25 = bbs_spRich25[, c("datasetID", "site", "taxa",  "meanAbundance", "pctTrans25")]

occ_merge = occ_taxa25[,c("datasetID", "site","taxa", "meanAbundance")]
bbs_occ = rbind(occ_merge, bbs_spRich25)
bbs_occ = bbs_occ[!bbs_occ$site %in% c("53800-5-6", "53800-25-2"),]

bbs_occ_mod = bbs_occ[! bbs_occ$datasetID %in% c(207, 210, 217, 218, 222, 223, 225, 241,258, 282, 322, 280, 248, 254, 279, 291),]

areamerge_mod = areamerge[! areamerge$datasetID %in% c(207, 210, 217, 218, 222, 223, 225, 241,258, 282, 322, 280, 248, 254, 279, 291),]

occ25 = left_join(bbs_occ, areamerge[,c("datasetID", "site", "area")], by = c("datasetID","site"))




##### 10
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


occ10 = left_join(bbs_occ_pred, areamerge[,c("datasetID", "site", "area")], by = c("datasetID","site"))



##### merging
occ_supp = left_join(occ25, occ10, by = c("datasetID","site", "taxa"))
occ_all = left_join(bbs_occ_area, occ_supp, by = c("datasetID","site", "taxa"))

occ_all.5 = left_join(bbs_occ_area, occ25[,c("datasetID","site", "pctTrans25")], by = c("datasetID","site"))
occ_all1 = left_join(occ_all.5, occ10[,c("datasetID","site", "pctTrans10")], by = c("datasetID","site"))

# write.csv(occ_all, "occ_allmerged.csv", row.names = FALSE)


abunModel1 = lmer(pctTrans ~ log10(area) * taxa + (log10(meanAbundance) | datasetID), data = occ_all1)
r.squaredGLMM(abunModel1)

abunModel2 = lmer(pctTrans25 ~ log10(area) * taxa + (log10(meanAbundance) | datasetID), data = occ_all1)
r.squaredGLMM(abunModel2)

abunModel3 = lmer(pctTrans10 ~ log10(area) * taxa + (log10(meanAbundance) | datasetID), data = occ_all1)
r.squaredGLMM(abunModel3)



abunModel1 = lmer(pctTrans ~ log10(meanAbundance) * taxa + (log10(meanAbundance) | datasetID), data = occ_all1)
r.squaredGLMM(abunModel1)

abunModel2 = lmer(pctTrans25 ~ log10(meanAbundance) * taxa + (log10(meanAbundance) | datasetID), data = occ_all1)
r.squaredGLMM(abunModel2)

abunModel3 = lmer(pctTrans10 ~ log10(meanAbundance) * taxa + (log10(meanAbundance) | datasetID), data = occ_all1)
r.squaredGLMM(abunModel3)


