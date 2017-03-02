###############################################
# Code for running BBS 2000-2014 within core transient
# analysis. Preps BBS for merging to other datasets


setwd("C:/git/core-transient")

library(plyr) # for core-transient functions
library(tidyr)
library(dplyr)


source('scripts/R-scripts/core-transient_functions.R')

# Raw input files from Jenkins code
# read.csv("data/BBS/bbs_below.csv", header = TRUE)
# read.csv("data/BBS/bbs_below_pctTrans.csv", header = TRUE)
# read.csv("data/BBS/bbs_abun_occ.csv", header=TRUE)
# read.csv("data/latlongs/bbs_2000_2014_latlongs.csv", header = TRUE)

# Specify here the datasetIDs and then run the code below.
dataformattingtable = read.csv('data_formatting_table.csv', header = T) 

datasetIDs = dataformattingtable$dataset_ID[dataformattingtable$format_flag == 1]

#### BBS prep to replace dataset 1 #####
bbs_all_years = read.csv("data/BBS/bbs_all_raw.csv", header = TRUE)
bbs_all_years$datasetID = 1
bbs_all_years2 = bbs_all_years %>% filter(Year > 1999 & Year <2015) %>%  filter(Aou > 2880 & !(Aou >= 3650 & Aou <= 3810) & !(Aou >= 3900 & Aou <= 3910) & !(Aou >= 4160 & Aou <= 4210) & Aou != 7010) %>% dplyr::select(stateroute, Aou, SpeciesTotal, Year, datasetID)

bbs_all_years2$site = bbs_all_years2$stateroute
bbs_all_years2$species = bbs_all_years2$Aou
bbs_all_years2$count = bbs_all_years2$SpeciesTotal
bbs_all_years2$date = bbs_all_years2$Year

bbs_all_years2 = bbs_all_years2 %>% dplyr::select(site, species, count, date, datasetID)
write.csv(bbs_all_years2, "data/BBS/bbs_2000_2014.csv", row.names = FALSE)
#### BBS prep to merge with summ2 dataset #####
# need datasetID, site, system, taxa, propCore, propTrans, and meanAbundance
# read in below-scale bbs dataset 
bbs_below = read.csv("data/BBS/bbs_below.csv", header = TRUE)
bbs_below$site = paste(bbs_below$stateroute, bbs_below$scale, sep = "-")
bbs_below$datasetID = 1
bbs_below$system = "Terrestrial"
bbs_below$taxa = "Bird"
bbs_below$propCore = bbs_below$pctCore
bbs_below$propTrans = bbs_below$pctTran
bbs_below$meanAbundance = bbs_below$aveN
bbs_below = bbs_below[, c("datasetID","site","system","taxa","propCore","propTrans","meanAbundance")]
write.csv(bbs_below, "data/BBS/bbs_below_summ2.csv", row.names = FALSE)


#### BBS prep to merge with different percent transient thresholds at route scale #####
# read in BBS route level data for fig 2
bbs_focal_occs_pctTrans = read.csv("data/BBS/bbs_below_pctTrans.csv", header = TRUE)
bbs_focal_occs_pctTrans = subset(bbs_focal_occs_pctTrans, bbs_focal_occs_pctTrans$scale == '50-1')
bbs_focal_occs_pctTrans$site = bbs_focal_occs_pctTrans$stateroute
bbs_focal_occs_pctTrans$datasetID = 1
bbs_focal_occs_pctTrans$system = "Terrestrial"
bbs_focal_occs_pctTrans$taxa = "Bird"
bbs_focal_occs_pctTrans$propCore33 = bbs_focal_occs_pctTrans$pctCore
bbs_focal_occs_pctTrans$propTrans33 = bbs_focal_occs_pctTrans$spRichTrans33
bbs_focal_occs_pctTrans$propTrans25 = bbs_focal_occs_pctTrans$spRichTrans25
bbs_focal_occs_pctTrans$propTrans10 = bbs_focal_occs_pctTrans$spRichTrans10

# 2a
bbs_below_st = bbs_focal_occs_pctTrans
bbs_below_st$propCore = bbs_below_st$propCore33
bbs_below_st$propTrans = bbs_below_st$propTrans33
bbs_below_st = bbs_below_st [, c("datasetID","site","system","taxa","propCore","propTrans")]

write.csv(bbs_below_st, "data/BBS/bbs_below_st.csv", row.names = FALSE)

# 2b
bbs_focal_occs_pctTrans = bbs_focal_occs_pctTrans[, c("datasetID","site","system","taxa","propCore33", "propTrans33", "propTrans25", "propTrans10")]
write.csv(bbs_focal_occs_pctTrans, "data/BBS/bbs_focal_occs_pctTrans.csv", row.names = FALSE)


#### BBS prep to merge area with abundance data ####
bbs_abun = read.csv("data/BBS/bbs_abun_occ.csv", header=TRUE)
bbs_abun$site = bbs_abun$stateroute
bbs_area = merge(bbs_below_st, bbs_abun, by = "site")
bbs_area$pctTrans = bbs_area$propTrans
bbs_area = bbs_area[, c("datasetID", "site", "taxa", "pctTrans", "area")]
write.csv(bbs_area, "data/BBS/bbs_area.csv", row.names = FALSE)


#### BBS prep for figure 4 ####
# exclude AOU species codes <=2880 [waterbirds, shorebirds, etc], (>=3650 & <=3810) [owls],
# (>=3900 &  <=3910) [kingfishers], (>=4160 & <=4210) [nightjars], 7010 [dipper]
bbs_spRich = bbs_abun %>% 
  filter(AOU > 2880 & !(AOU >= 3650 & AOU <= 3810) & !(AOU >= 3900 & AOU <= 3910) & 
           !(AOU >= 4160 & AOU <= 4210) & AOU != 7010)   %>% 
  count(stateroute, scale, subrouteID)
bbs_spRich$subscale = substring(bbs_spRich$subrouteID, 5)
bbs_spRich$site = paste(bbs_spRich$stateroute, bbs_spRich$scale, bbs_spRich$subscale, sep = "-")

bbs_abun3.5 = merge(bbs_spRich, bbs_below, by = c("site"))

bbs_abun4 = bbs_abun3.5
bbs_abun4$datasetID = 1
bbs_abun4$taxa = "Bird"
bbs_abun4$pctTrans = bbs_abun4$propTrans
bbs_abun4$pctCore = bbs_abun4$propCore
bbs_abun4$pctNeither = 1-(bbs_abun4$pctTrans + bbs_abun4$propCore)
bbs_abun4$spRich = bbs_abun4$n
bbs_abun4$scale[bbs_abun4$scale == 50] = 1
bbs_abun4$scale[bbs_abun4$scale == 25] = 2
bbs_abun4$scale[bbs_abun4$scale == 10] = 3
bbs_abun4$scale[bbs_abun4$scale == 5] = 4
# bbs_abun4$meanOcc = bbs_abun4$meanAbundance

bbs_abun4 = bbs_abun4[,c("datasetID", "site","taxa","meanAbundance", "pctTrans","pctCore","pctNeither", "scale", "spRich")]
write.csv(bbs_abun4, "data/BBS/bbs_abun4_spRich.csv", row.names = FALSE)

#### BBS w propOcc ###
bbs_abun_propOcc = bbs_abun[,c("datasetID", "site", "AOU", "occupancy", "scale")]

#### BBS prep for model, combine with lat longs ####
bbs_latlong = read.csv("data/latlongs/bbs_2000_2014_latlongs.csv", header = TRUE)
bbs_below = read.csv("data/BBS/bbs_below.csv", header = TRUE)
bbs_be_lat = merge(bbs_below, bbs_latlong, by = "stateroute", all.x = TRUE)
bbs_be_lat$site = paste(bbs_below$stateroute, bbs_below$scale, sep = "-")
bbs_be_lat$datasetID = 1
bbs_be_lat$taxa = "Bird"
bbs_be_lat$Lat = bbs_be_lat$Lati
bbs_be_lat$Lon = bbs_be_lat$Longi
bbs_be_lat$propTrans = bbs_be_lat$pctTran
bbs_be_lat = bbs_be_lat[,c("datasetID", "Lat","Lon", "taxa","site", "propTrans")]
write.csv(bbs_be_lat, "data/BBS/bbs_be_lat.csv", row.names = FALSE)
