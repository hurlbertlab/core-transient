# Biotic Interactions script
# In this script we compare the differences in occupancy and abundance between
# 63 focal species and their competitors using occupancy, abundance, and environmental data.
# Env data was formatted in Snell_code.R from BIOL 465 project. Occupancy data from BBS ecoretriever.

#setwd("C:/git/core-transient/scripts/R-scripts/Biotic Interactions Snell")
#### ---- Inital Formatting ---- ####
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ecoretriever)

# read in range occupancy dataset 
Hurlbert_o = read.csv('Master_RO_Correlates_20110610.csv', header = T)
# subset species whose range occupancies were between 0.3 and 0.7 over a 10 year period
subsetocc = Hurlbert_o[Hurlbert_o$X10yr.Prop > .3 & Hurlbert_o$X10yr.Prop < .7,]

# read in BBS temporal occupancy data
temp_occ = read.csv("bbs_sub1.csv", header=TRUE)

# read in BBS abundance data - from Hurlbert Lab
bbs = read.csv('dataset_1.csv', header = T)
# subset bbs abundance columns
bbs = bbs[, (names(bbs) %in% c("stateroute", "Aou", "Year","SpeciesTotal",  'routeID', 'Lati', 'Longi'))]
# read in expected presence data based on BBS 
expect_pres = read.csv('expected_presence_on_BBS_routes.csv', header = T)

############# ---- Set up pairwise comparison table ---- #############
# read in species trophic assignment table
ttable = read.csv("trophic_table.csv", header = TRUE)
# merge trophic table with range occupancy data
ttable2 = merge(ttable, Hurlbert_o, by = "AOU")

# read in table with pairwise comparison of each focal species to several potential competitors - created by hand
focal_competitor_table = read.csv("focal spp.csv", header = TRUE)
focal_competitor_table = data.frame(focal_competitor_table$AOU, focal_competitor_table$CommonName, focal_competitor_table$Competitor)
names(focal_competitor_table)= c("FocalAOU", "Focal", "Competitor")

# create data frame of unique focal species
focal_unique = data.frame(unique(focal_competitor_table$Focal), unique(focal_competitor_table$FocalAOU))
names(focal_unique) = c("Focal_Common","FocalAOU")
    
# read in all species table to get unique list of species commmon names
allspp = read.csv("all spp.csv", header = TRUE)
allspp = data.frame(unique(allspp$CommonName))
names(allspp)="CommonName"
  
# read in taxonomy data
AOU = read.csv("Bird_Taxonomy.csv", header = TRUE)
AOU=AOU[, (names(AOU) %in% c("SCI_NAME", "AOU_OUT", "PRIMARY_COM_NAME","FAMILY"))]
names(AOU) = c("SciName","CommonName",  "AOU", "Family")

# remove duplicates/subspecies from AOU data frame
AOUsub = AOU[-grep("sp.", AOU$CommonName),] 
AOUsub2 = AOUsub[-grep("\\)", AOUsub$CommonName),]
AOUsub3 = AOUsub2[-grep(" \\(", AOUsub2$CommonName),]
AOUsub4 = unique(AOUsub3)
AOUsub4 = na.omit(AOUsub4)

############# ----  nomenclature corrections for shapefiles,correct name is "match" column ---- ######
# merge w all sp list to get info for each sp
sp_list = merge(AOUsub4, allspp, by = "CommonName")
sp_list$match = as.character(sp_list$SciName)
# renaming to get latest scientific names for mismatch spp
sp_list$match[sp_list$match =="Oreothlypis peregrina"] = "Vermivora peregrina"

sp_list$match[sp_list$match =="Vermivora pinus"] = "Vermivora cyanoptera"

sp_list$match[sp_list$match =="Stellula calliope"] = "Selasphorus calliope"

sp_list$match = gsub('Setophaga ', 'Dendroica ', sp_list$match)

sp_list$match[sp_list$match =="Dendroica ruticilla"] = "Setophaga ruticilla"

sp_list$match[sp_list$match =="Picoides nuttallii"] = "Dryobates nuttallii"

sp_list$match[sp_list$match =="Cardellina canadensis"] = "Wilsonia canadensis"

sp_list$match[sp_list$match =="Geothlypis philadelphia"] = "Oporornis philadelphia"

sp_list$match[sp_list$match =="Oreothlypis ruficapilla"] = "Vermivora ruficapilla"

sp_list$match[sp_list$match =="Oreothlypis celata"] = "Vermivora celata"

sp_list$match[sp_list$match =="Cardellina pusilla"] = "Wilsonia pusilla"

sp_list$match[sp_list$match =="Oreothlypis virginiae"] = "Vermivora virginiae"

sp_list$match[sp_list$match =="Poecile hudsonica"] = "Parus hudsonicus"

sp_list$match[sp_list$match =="Pica hudsonia"] = "Pica pica"

sp_list$match = gsub('Poecile ', 'Parus ', sp_list$match)

sp_list$match[sp_list$match =="Dendroica citrina"] = "Wilsonia citrina"

sp_list$match[sp_list$match =="Geothlypis formosus"] = "Oporornis formosus"

sp_list$match[sp_list$match =="Oreothlypis luciae"] = "Vermivora luciae"

sp_list$match[sp_list$match =="Geothlypis tolmiei"] = "Oporornis tolmiei"

sp_list$match[sp_list$match =="Troglodytes hiemalis"] = "Troglodytes troglodytes"

# Winter Wren had AOU code change (7220 to 7222), changing in occ code to reflect that
temp_occ$Aou[temp_occ$Aou == 7220] <- 7222

###### ---- Create final focal-comp table ----######
#merge pairwise table with taxonomy info
comp_AOU = merge(focal_competitor_table, sp_list, by.x = "Competitor", by.y = "CommonName")
names(comp_AOU) = c("Competitor", "focalAOU", "Focal", "old", "CompAOU", "Family","CompSciName")
comp_AOU$old = NULL
comp_AOU <- na.omit(comp_AOU)

# merging in focal sci name to table
focal_AOU = merge(comp_AOU, sp_list[,c("CommonName", "match")], by.x = "Focal", by.y = "CommonName")
focal_AOU$AOU = NULL
focal_AOU$SciName = NULL
names(focal_AOU) = c("Focal", "Competitor", "focalAOU", "CompAOU", "Family","CompSciName", "FocalSciName")

# import body size data from Dunning 2008
bsize = read.csv("DunningBodySize_old_2008.11.12.csv", header = TRUE)
bsize$AOU = NULL
bsize = bsize[!duplicated(bsize),]

# merge in competitor and focal body size
spec_w_bsize = merge(focal_AOU, bsize[,c("CommonName", "Mass.g.")], by.x = "Focal", by.y = "CommonName")
spec_w_bsize2 = merge(spec_w_bsize, bsize[,c("CommonName", "Mass.g.")], by.x = "Competitor", by.y = "CommonName")

spec_w_weights = data.frame(spec_w_bsize2$Focal, spec_w_bsize2$FocalSciName, spec_w_bsize2$focalAOU,
                            spec_w_bsize2$Mass.g..x, spec_w_bsize2$Competitor,
                            spec_w_bsize2$CompSciName,spec_w_bsize2$CompAOU, spec_w_bsize2$Mass.g..y)
names(spec_w_weights) = c("Focal", "FocalSciName","FocalAOU", "FocalMass", "Competitor","CompSciName", "CompetitorAOU", "CompMass")

# want to compare body size - if competitor is double or more in size to focal, then delete
new_spec_weights = subset(spec_w_weights, spec_w_weights$FocalMass / spec_w_weights$CompMass >= 0.5 &
                            spec_w_weights$FocalMass / spec_w_weights$CompMass <= 2)
  
# adding in underscore for file name matching
new_spec_weights$focalcat = gsub(" ", "_", new_spec_weights$FocalSciName)
new_spec_weights$compcat = gsub(" ", "_", new_spec_weights$CompSciName)

#write this data frame for GIS script
write.csv(new_spec_weights, "new_spec_weights.csv", row.names=FALSE) 

# read in area shapefile if not running GIS code 
filesoutput = read.csv("shapefile_areas.csv", header = TRUE)

############# ---- Generate total species occupancies ---- #############
# pull out stateroutes that have been continuously sampled 1996-2010
routes = unique(temp_occ$stateroute)
# merge expected presence data with species name information
sub_ep = merge(expect_pres[,c('stateroute', 'AOU')], focal_AOU, by.x = 'AOU',by.y="focalAOU", all = TRUE) 
# merge expected presence with occupancy data
new_occ = merge(sub_ep, temp_occ, by.x = c('stateroute', 'AOU'), by.y = c('stateroute', 'Aou'), all = TRUE) 
new_occ$n[is.na(new_occ$n)] <- 0
new_occ$occ[is.na(new_occ$occ)] <- 0
# subset to routes in the well sampled list of 'routes'
new_occ2 = new_occ[new_occ$stateroute %in% routes, ]

# Pull out summary of different levels of occupancy by species (Allen wrote loop)
occ_dist_output = data.frame(AOU = NA, occ = NA, count = NA)
bins = seq(0.1, 1, by = .1)
for (s in unique(new_occ2$AOU)) {
  tmp = subset(new_occ2, AOU == s)
  occ_counts = sapply(bins, function(x) sum(tmp$occ <= x & tmp$occ > (x-0.1)))
  tmp_out = data.frame(AOU = s, occ = bins, count = occ_counts/nrow(tmp))
  occ_dist_output = rbind(occ_dist_output, tmp_out)
}
occ_dist_output = occ_dist_output[-1, ]

# calculate average distribution for all species
avg_occ_dist = aggregate(occ_dist_output$count, by = list(occ_dist_output$occ), mean)
names(avg_occ_dist) = c('occupancy', 'frequency')
avg_occ_dist$occupancy = as.numeric(as.character(avg_occ_dist$occupancy))

# Exploratory plot of total avg avian occupancy distribution
ggplot(data = avg_occ_dist, aes(x = occupancy, y = frequency)) + geom_line(data=avg_occ_dist, lwd = 2) +theme_classic() +xlab("Temporal occupancy")+ylab("Species frequency at given site")+ annotate("rect", xmin = 0.3, xmax = 0.7, ymin = 0.03, ymax = 0.06, alpha = .2, fill = "Red") + annotate("text", x = 0.5, y = 0.05, label = "Species of Interest")

#### ---- Gathering Occupancy and Abundance Data for Biotic Comparisons ---- ####
# filter BBS mean abundance by AOU/stateroute by year
bbs_pool = bbs %>% 
  group_by(stateroute, Aou) %>% 
  dplyr::summarize(abundance = mean(SpeciesTotal))
names(bbs_pool)[names(bbs_pool)=="Aou"] <- "AOU"
# need to change winter wren AOU to 7222 from 7220 in bbs_pool
bbs_pool$AOU[bbs_pool$AOU == 7220] <- 7222

focalspecies = unique(new_spec_weights$FocalAOU)

# filter to relevant species
bbs_abun = filter(bbs_pool, AOU %in% focalspecies) 

# merge in occupancies of focal
occ_abun = merge(bbs_abun, new_occ2, by = c("AOU", "stateroute"))

# Take range overlap area to assign "main competitor" for each focal species
# "area.df" with cols: FocalAOU, CompAOU, focalArea, compArea, intArea, intProp
shapefile_areas = read.csv("shapefile_areas.csv", header = TRUE) # from for loop above
shapefile_areas$X = NULL

# calculate proportion of overlap between focal range and overlap range
shapefile_areas$PropOverlap = shapefile_areas$area_overlap/shapefile_areas$FocalArea

# Which competitor has greatest area of overlap? -- main competitor
shapefile_areas$mainCompetitor = 0 # set up main competitor column, 0 = not the primary competitor
for (s in focalspecies) {
  maxOverlap = max(shapefile_areas$PropOverlap[shapefile_areas$focalAOU == s], na.rm = TRUE) #largest area of proportion overlap
  shapefile_areas$mainCompetitor[shapefile_areas$focalAOU == s & shapefile_areas$PropOverlap == maxOverlap] = 1 # 1 assigns main competitor
}

#------------------------------------------------

foo1 = left_join(occ_abun, bbs_pool, by = c('CompAOU' = 'AOU', 'stateroute' = 'stateroute')) %>%
  left_join(shapefile_areas[, c('focalAOU', 'compAOU', 'mainCompetitor')], 
                by = c('AOU' = 'focalAOU', 'CompAOU' = 'compAOU')) %>%
  group_by(AOU, stateroute, Focal, Family, occ) %>%
  summarize(allCompN = sum(abundance.y, na.rm = T))

foo2 = left_join(occ_abun, bbs_pool, by = c('CompAOU' = 'AOU', 'stateroute' = 'stateroute')) %>%
  left_join(shapefile_areas[, c('focalAOU', 'compAOU', 'mainCompetitor')], 
            by = c('AOU' = 'focalAOU', 'CompAOU' = 'compAOU')) %>%
  group_by(AOU, stateroute, Focal, Family, occ) %>%
  filter(mainCompetitor == 1) %>%
  summarize(mainCompN = abundance.y)

foo3 = left_join(foo2, foo1)
















# for loop to select sp and compare to competitor(s) 
### select strongest competitor, sum competitor abundance by stateroute
focalcompoutput = c()
for (sp in focalspecies) {
  print(sp)
  tmp = filter(occ_abun, AOU == sp)  # using indexed species in loop
  comp_spp = subset(shapefile_areas,focalAOU == sp) # subset to sp of interest in comp/focal table
  
  bbs_comp = bbs_pool %>% # pull out main competitor abundance&stateroute of interest
    filter(AOU %in% comp_spp$compAOU[comp_spp$mainCompetitor == 1] & stateroute %in% tmp$stateroute) 
  bbs_comp2 = merge(bbs_comp, comp_spp, by.x = 'AOU', by.y = 'compAOU', all.x=TRUE)  # merge comp/focal table with abundance data
  bbs_comp2$mainCompN = bbs_comp2$abundance # rename column
  
  compsum = bbs_pool %>%  # filter all comp abundance and state route of interest
    filter(AOU %in% comp_spp$compAOU & stateroute %in% tmp$stateroute) %>% 
    group_by(stateroute, AOU) %>% # group by stateroute/AOU combo
    dplyr::summarize(AllCompN = sum(abundance), MainCompN = sum(bbs_comp2$mainCompN)) # sum all competitors vs. main competitor (only 1 main sum/AOU), should there be diff sums for diff aous?
  
  focalout = merge(tmp, compsum, by = 'stateroute')  # merge new info with orig info by stateroute
  focalout[is.na(focalout)] = 0
  focalout$MainCompAOU = unique(comp_spp$compAOU[comp_spp$mainCompetitor == 1]) 
  
  names(focalout)[names(focalout)=="occupancy"] <- "FocalOcc"
  # main competitor occupancy
  MainCompAOU =  unique(focalout$MainCompAOU)
  
  # subset occupancy by state route, merge in main competitor
  match_occ_stroute = filter(new_occ2, new_occ2$stateroute %in% focalout$stateroute)

  focal_comp_occ = merge(focalout, match_occ_stroute[,c('AOU', 'stateroute', 'occ')], 
                         by.x = c('MainCompAOU', 'stateroute'), by.y = c('AOU', 'stateroute'))
  names(focal_comp_occ)[names(focal_comp_occ)=="occupancy"] <- "MainCompOcc" 
  focalcompoutput = rbind(focalcompoutput, focal_comp_occ)
}

focalcompoutput = data.frame(focalcompoutput)
colnames(focalcompoutput) = c("MainCompAOU", "stateroute","FocalAOU", "FocalAbundance", "FocalCommonName","Competitor", "CompAOU","Family","CompSciName","FocalSciName","nyears","FocalOcc","AllCompSum", "MainCompSum", "CompOcc")

# Filter number to spp present in at least 20 routes for better model results
# Subset to get the count of routes for each spp
numroutes = c()
for (sp in focalspecies) {
  print(sp)
  tmp = filter(focalcompoutput, FocalAOU == sp) 
  nroutes = tmp %>%
    group_by(FocalAOU) %>%
    summarise(n_distinct(stateroute))
  numroutes = rbind(numroutes, nroutes)
}
numroutes = data.frame(numroutes)
colnames(numroutes) = c("FocalAOU","nroutes")

# Filter count to greater than or equal to 20
numroutes20 = filter(numroutes, nroutes >= 20)
numroutes20$nroutes = as.numeric(numroutes20$nroutes)
# Merge with focalcompoutput data table, new # of focal spp is 63 with filters applied
focalcompsub = merge(focalcompoutput, numroutes20, by = "FocalAOU")
# Create scaled competitor column = main comp abundance/(focal abundance + main comp abundance)
focalcompsub$comp_scaled = focalcompsub$MainCompSum/(focalcompsub$FocalAbundance + focalcompsub$MainCompSum)

# Creating new focalspecies index
subfocalspecies = unique(focalcompsub$FocalAOU)

#### ---- Processing Environmental Data - Re-done from Snell_code.R ---- ####
# read in raw env data (from Coyle et al)
all_env = read.csv('All Env Data.csv', header = T)
# merge in ENV
all_expected_pres = merge(all_env[,c("stateroute", "Longi", "Lati",  'sum.EVI', 'elev.mean', 'mat', 'ap.mean')], 
     focalcompsub, by = "stateroute")

write.csv(all_expected_pres,"all_expected_pres.csv", row.names= FALSE)

#For loop to calculate mean & standard dev environmental variables for each unique species (from BIOL 465)
birdsoutputm = c()
for (sp in subfocalspecies) {
  print(sp)
  spec.routes <- all_expected_pres[(all_expected_pres$FocalAOU) == sp, "stateroute"] #subset routes for each species (i) in tidybirds
  env.sub <- all_expected_pres[all_expected_pres$stateroute %in% spec.routes,] #subset routes for each env in tidybirds
  envmeans = as.vector(apply(env.sub[, c("mat", "ap.mean","elev.mean","sum.EVI")], 2, mean))
  envsd = as.vector(apply(env.sub[, c("mat", "ap.mean","elev.mean","sum.EVI")], 2, sd))
  
  birdsoutputm = rbind(birdsoutputm, c(sp, envmeans, envsd))
  
}
birdsoutputm = data.frame(birdsoutputm)
names(birdsoutputm) = c("Species", "Mean.Temp", "Mean.Precip", "Mean.Elev", "Mean.EVI", "SD.Temp", "SD.Precip", "SD.Elev", "SD.EVI")

# merge in global mean env data with species-specific env data & occ data
occuenv = merge(birdsoutputm, all_expected_pres, by.x = "Species", by.y = "FocalAOU")

#Calculating z scores for each environmental variable (observed mean - predicted mean/predicted SD)
occuenv$zTemp = (occuenv$mat - occuenv$Mean.Temp) / occuenv$SD.Temp
occuenv$zPrecip = (occuenv$ap.mean - occuenv$Mean.Precip) / occuenv$SD.Precip
occuenv$zElev = (occuenv$elev.mean - occuenv$Mean.Elev) / occuenv$SD.Elev
occuenv$zEVI = (occuenv$sum.EVI - occuenv$Mean.EVI) / occuenv$SD.EVI

write.csv(occuenv, "occuenv.csv", row.names=FALSE)
####### END DATA CLEANING, see analysis script ##########