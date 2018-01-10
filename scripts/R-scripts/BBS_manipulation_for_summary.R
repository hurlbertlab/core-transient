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

##### r data retriever to download bbs data and derive occupancy values #####
# bbs_eco = rdataretriever::fetch("breed-bird-survey") 

Years = (bbs_eco$counts$Year)
bbs = bbs_eco$breed_bird_survey_counts
bbs$Year = as.numeric(bbs$year)
bbs$stateroute = bbs$statenum*1000 + bbs$route

#### BBS prep to replace dataset 1 #####
# Get subset of stateroutes that have been surveyed every year from 2001-2015
good_rtes = bbs %>% 
  filter(year > 1999, year < 2015) %>% 
  dplyr::select(year, stateroute) %>%
  unique() %>%    
  dplyr::count(stateroute) %>% 
  filter(n == 15) # have to stay at 15 to keep # of years consistent

# Calculate occupancy for all species at subset of stateroutes above
bbs_sub1 = bbs %>% 
  filter(Year > 1999, Year < 2015, stateroute %in% good_rtes$stateroute) %>% 
  dplyr::select(stateroute, Year, aou, speciestotal)# %>% filter(stateroute != 7008)

# bbs_sub1 = read.csv("data/BBS/bbs_2000_2014.csv", header = TRUE)
bbs_w_aou = bbs_sub1 %>% filter(aou > 2880) %>%
  filter(aou < 3650 | aou > 3810) %>%
  filter(aou < 3900 | aou > 3910) %>%
  filter(aou < 4160 | aou > 4210) %>%
  filter(aou != 7010)

write.csv(bbs_w_aou, "data/BBS/bbs_2000_2014.csv", row.names = FALSE)
#### creating new bbs_abun_occ from scratch #####
bbs_occ = bbs_w_aou %>% 
  dplyr::count(aou, stateroute) %>% 
  filter(n < 16) %>% 
  dplyr::mutate(occ = n/15) 
 
write.csv(bbs_occ, "data/BBS/bbs_occ_2000_2014.csv", row.names = FALSE)

#### getting spRich by route
bbs_sprich = bbs_w_aou %>% group_by(aou,Year) %>%
  dplyr::count(stateroute) 

#### BBS prep to merge with summ2 dataset #####
# need datasetID, site, system, taxa, propCore, propTrans, and meanAbundance
# read in below-scale bbs dataset 
bbs_below.5 = read.csv("scripts/R-scripts/scale_analysis/bbs_allscales.csv", header = TRUE) %>%
  filter(area <25.14, scale != '1')
bbs_below = merge(bbs_below.5, bbs_occ, by.y = "stateroute", by.x = "focalrte")
bbs_below$site = paste(bbs_below$focalrte, bbs_below$scale, sep = "-")
bbs_below$datasetID = 1
bbs_below$system = "Terrestrial"
bbs_below$taxa = "Bird"
bbs_below$propCore = bbs_below$pctCore
bbs_below$propTrans = bbs_below$pctTran
bbs_below$meanAbundance = bbs_below$aveN
bbs_below = bbs_below[, c("datasetID","site","system","taxa","propCore","propTrans","meanAbundance", "area", "aou","n", "occ")]
write.csv(bbs_below, "Z:/Snell/data/bbs_below.csv", row.names = FALSE)

# BBS scales area
numstops = c(5, 10, 25, 50)
bbsArea_m2 = data.frame(stops = numstops, area = numstops*pi*400^2)

bbs_below$stops = as.numeric(matrix(unlist(strsplit(bbs_below$site, "-")), byrow = T, ncol = 3)[,2])

bbs_allscales33 = left_join(bbs_below, bbsArea_m2)
write.csv(bbs_below, "data/BBS/bbs_allscales33.csv", row.names = FALSE)

#### BBS prep to merge with different percent transient thresholds at route scale #####
occ.summ = bbs_allscales33 %>% #occupancy
  count(site) %>%
  mutate(occ = n/15, scale = scale, subrouteID = countColumns[1]) %>%
  group_by(stateroute) %>%
  summarize(meanOcc = mean(occ), 
            pctCore = sum(occ > 2/3)/length(occ),
            pctTran = sum(occ <= 1/3)/length(occ)) %>%
  #spRichTrans33  
  # spRichTrans25 = sum(occ <= 1/4)/length(occ),
  # spRichTrans10 = sum(occ <= 0.1)/length(occ)) %>%
  mutate(scale = paste(scale, g, sep = "-")) %>%
  left_join(abun.summ, by = 'stateroute')
return(occ.summ)


#read in BBS route level data for fig 2
bbs_focal_occs_pctTrans = read.csv("data/BBS/bbs_below_pctTrans.csv", header = TRUE)
stateroutes = unlist(strsplit(as.character(bbs_focal_occs_pctTrans$site), "-"))[,1]
bbs_focal_occs_pctTrans$datasetID = 1
bbs_focal_occs_pctTrans$system = "Terrestrial"
bbs_focal_occs_pctTrans$taxa = "Bird"
bbs_focal_occs_pctTrans$propCore33 = bbs_focal_occs_pctTrans$pctCore
bbs_focal_occs_pctTrans$propTrans33 = bbs_focal_occs_pctTrans$spRichTrans33
bbs_focal_occs_pctTrans$propTrans25 = bbs_focal_occs_pctTrans$spRichTrans25
bbs_focal_occs_pctTrans$propTrans10 = bbs_focal_occs_pctTrans$spRichTrans10
#bbs_focal_occs_pctTrans$meanAbundance = bbs_focal_occs_pctTrans$aveN
bbs_focal_occs_pctTrans = bbs_focal_occs_pctTrans[, c("datasetID","site","system","taxa","propTrans33","propTrans25","propTrans10")]
# write.csv(bbs_focal_occs_pctTrans, "data/BBS/bbs_focal_occs_pctTrans_Site.csv", row.names = FALSE)

# 2a
bbs_below_st = bbs_focal_occs_pctTrans
bbs_below_st$propCore = bbs_below_st$propCore33
bbs_below_st$propTrans = bbs_below_st$propTrans33
# bbs_below_st = bbs_below_st [, c("datasetID","site","system","taxa","propTrans")]

write.csv(bbs_below_st, "data/BBS/bbs_below_st.csv", row.names = FALSE)

# 2b
bbs_focal_occs_pctTrans = bbs_focal_occs_pctTrans[, c("datasetID","site","system","taxa","propCore33", "propTrans33", "propTrans25", "propTrans10")]
write.csv(bbs_focal_occs_pctTrans, "data/BBS/bbs_focal_occs_pctTrans.csv", row.names = FALSE)

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
bbs_be_lat$site = paste(bbs_be_lat$stateroute, bbs_be_lat$scale, sep = "-")
bbs_be_lat$datasetID = 1
bbs_be_lat$taxa = "Bird"
bbs_be_lat$Lat = bbs_be_lat$Lati
bbs_be_lat$Lon = bbs_be_lat$Longi
bbs_be_lat$propTrans = bbs_be_lat$pctTran
bbs_be_lat = bbs_be_lat[,c("datasetID", "Lat","Lon", "taxa","site", "propTrans")]
write.csv(bbs_be_lat, "data/BBS/bbs_be_lat.csv", row.names = FALSE)
