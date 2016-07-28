# Biotic Interactions script
# In this script we are comparing the differences in occupancy and abundance between
# green-tailed towhees and spotted towhees using occupancy, abundance, and environmental data.
# Env data was formatted in Snell_code.R from BIOL 465 project. Occupancy data from BBS, Coyle, and Hurlbert.

setwd("C:/git/core-transient/scripts/R-scripts/Biotic Interactions Snell")
#### ---- Inital Formatting ---- ####
library(plyr)
library(dplyr)
library(maps)
library(rgdal)
library(shapefiles)
library(maptools)
library(tidyr)
library(raster)
library(rgeos)
library(ggplot2)
library(lme4)
library(lmtest)
library(gtools)

# read in temporal occupancy dataset 
Hurlbert_o = read.csv('Master_RO_Correlates_20110610.csv', header = T)
# subset species whose occupancies were between 0.3 and 0.7 over a 10 year period
subsetocc = Hurlbert_o[Hurlbert_o$X10yr.Prop > .3 & Hurlbert_o$X10yr.Prop < .7,]
# write.csv(subsetocc, "focal.csv")

# read in BBS data
bbs = read.csv('dataset_1.csv', header = T)
# paring down BBS cols
bbs = bbs[, (names(bbs) %in% c("stateroute", "Aou", "Year","SpeciesTotal",  'routeID', 'Lati', 'Longi'))]
# read in Coyle occupancy data - organized by site 
coyle_o = read.csv('site_sp_occupancy_matrix_Coyle.csv', header = T)
# gather into long format
coyle_long = gather(coyle_o, Aou, occupancy, X2881:X22860)
# remove x
coyle_long$Aou = substring(coyle_long$Aou, 2)
# name 1st col stateroute
colnames(coyle_long)[1] = "stateroute"

# read in expected presence data based on BBS 
expect_pres = read.csv('expected_presence_on_BBS_routes.csv', header = T)

############# ---- Set up pairwise comparison table ---- #############
ttable = read.csv("trophic_table.csv", header = TRUE)
ttable2 = merge(ttable, Hurlbert_o, by = "AOU")
write.csv(ttable2, "warbler_all.csv")

# create a table with pairwise comparison of each focal species to several potential competitors
focal_competitor_table = read.csv("focal spp.csv", header = TRUE)
focal_competitor_table = data.frame(focal_competitor_table$AOU, focal_competitor_table$CommonName, focal_competitor_table$Competitor)
focal_competitor_table = plyr::rename(focal_competitor_table, c("focal_competitor_table.AOU" = "focalAOU", "focal_competitor_table.CommonName" = "Focal", "focal_competitor_table.Competitor" = "Competitor"))

# create data frame of unique focal species
focal_unique = data.frame(unique(focal_competitor_table$Focal), unique(focal_competitor_table$focalAOU))
focal_unique = plyr::rename(focal_unique, c("unique.focal_competitor_table.Focal." = "Focal_Common", 
                                            "unique.focal_competitor_table.focalAOU." = "FocalAOU"))
    
# read in all species table to get unique list of sp
allspp = read.csv("all spp.csv", header = TRUE)
all_unique = data.frame(unique(allspp$CommonName))
  
# read in taxonomy data
AOU = read.csv("Bird_Taxonomy.csv", header = TRUE)
AOU2 = data.frame(AOU$SCI_NAME, AOU$AOU_OUT, AOU$PRIMARY_COM_NAME, AOU$FAMILY)
AOU2 = plyr::rename(AOU2, c("AOU.SCI_NAME" = "SciName", "AOU.AOU_OUT" = "AOU", "AOU.PRIMARY_COM_NAME" = "CommonName", "AOU.FAMILY" = "Family"))

# remove duplicates/subspecies
AOUsub = AOU2[-grep("sp.", AOU2$CommonName),] 
AOUsub2 = AOUsub[-grep("\\)", AOUsub$CommonName),]
AOUsub3 = AOUsub2[-grep(" \\(", AOUsub2$CommonName),]
AOUsub4 = unique(AOUsub3)
AOUsub4 = na.omit(AOUsub4)

############# ----  nomenclature corrections for shapefiles,correct name is "match" column ---- ######
# merge w all sp list to get info for each sp
sp_list = merge(AOUsub4, all_unique, by.x = "CommonName", by.y = "unique.allspp.CommonName.")
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

###### ---- Continued cleaning to create final focal-comp table ----######
#merge pairwise table with taxonomy info
comp_AOU = merge(focal_competitor_table, sp_list, by.x = "Competitor", by.y = "CommonName")
comp_AOU <- plyr::rename(comp_AOU, c("Competitor" = "Competitor", "focalAOU" = "focalAOU", 
                                  "Focal" = "Focal", "SciName" = "old", "AOU" = "CompAOU", "match" = "CompSciName"))
comp_AOU$old = NULL
comp_AOU <- na.omit(comp_AOU)

# merging in focal sci name to table
focal_AOU = merge(comp_AOU, sp_list, by.x = "Focal", by.y = "CommonName")
focal_AOU$AOU = NULL
focal_AOU$SciName = NULL

focal_AOU <- plyr::rename(focal_AOU, c("Focal" = "Focal", "Competitor" = "Competitor", "focalAOU" = "focalAOU", 
                                      "CompAOU" = "CompAOU", "CompSciName" = "CompSciName", "match" = "FocalSciName"))

# import body size data
bsize = read.csv("DunningBodySize_old_2008.11.12.csv", header = TRUE)
bsize$AOU = NULL
bsize = bsize[!duplicated(bsize),]

# merge in competitor and focal body size
spec_w_bsize = merge(focal_AOU, bsize, by.x = "Focal", by.y = "CommonName")
spec_w_bsize2 = merge(spec_w_bsize, bsize, by.x = "Competitor", by.y = "CommonName")

spec_w_weights = data.frame(spec_w_bsize2$Focal, spec_w_bsize2$focalAOU, spec_w_bsize2$CompSciName, 
                            spec_w_bsize2$Mass.g..x, spec_w_bsize2$Competitor,
                            spec_w_bsize2$CompAOU, spec_w_bsize2$FocalSciName, spec_w_bsize2$Mass.g..y)

spec_w_weights = plyr::rename(spec_w_weights, c("spec_w_bsize2.Focal" = "Focal", "spec_w_bsize2.focalAOU" = "FocalAOU", 
                                                "spec_w_bsize2.CompSciName" = "CompSciName","spec_w_bsize2.Mass.g..x" = "FocalMass", 
                                                "spec_w_bsize2.Competitor" = "Competitor","spec_w_bsize2.CompAOU" = "CompetitorAOU", 
                                                "spec_w_bsize2.Mass.g..y" = "CompMass", "spec_w_bsize2.FocalSciName" = "FocalSciName"))

# want to compare body size - if competitor is double or more in size to focal, then delete
new_spec_weights = subset(spec_w_weights, spec_w_weights$FocalMass / spec_w_weights$CompMass >= 0.5 &
                            spec_w_weights$FocalMass / spec_w_weights$CompMass <= 2)
  
# adding in underscore for file name matching
new_spec_weights$focalcat = gsub(" ", "_", new_spec_weights$FocalSciName)
new_spec_weights$compcat = gsub(" ", "_", new_spec_weights$CompSciName)

# read in bird range shps
all_spp_list = list.files('Z:/GIS/birds/All/All')

# for loop to select a genus_spp from pairwise table, read in shp, subset to permanent habitat, plot focal distribution
filesoutput = c()
focal_spp = c(unique(new_spec_weights$focalcat))

intl_proj = CRS("+proj=longlat +datum=WGS84")
sp_proj = CRS("+proj=laea +lat_0=40 +lon_0=-100 +units=km")
  #("+proj=laea +lat_0=40 +lon_0=-100 +units=m") # lambert azimuthal equal area
# usa = readShapePoly('Z:/GIS/geography/na_base_Lambert_Azimuthal', proj4string = sp_proj)
# usa = spTransform(usa, sp_proj)
# proj4string(usa) <- sp_proj
# plot(usa)

####### for loop generating shapefiles and area table for all spp - DO NOT RUN! ######
if(FALSE) {  #Blocking out the for loop below. Need to change to TRUE if you want the loop to run.

for (sp in focal_spp) {
  #sp = 'Troglodytes_troglodytes'
  print(sp)
  t1 = all_spp_list[grep(sp, all_spp_list)]
  t2 = t1[grep('.shp', t1)]
  t3 = strsplit(t2, ".shp")

  test.poly <- readShapePoly(paste("z:/GIS/birds/All/All/", t3, sep = "")) # reads in species-specific shapefile
  proj4string(test.poly) <- intl_proj
  colors = c("blue", "yellow", "green", "red", "purple")
  # subset to just permanent or breeding residents
  sporigin = test.poly[test.poly@data$SEASONAL == 1|test.poly@data$SEASONAL == 2|test.poly@data$SEASONAL ==5,]
  sporigin = spTransform(sporigin, CRS("+proj=laea +lat_0=40 +lon_0=-100 +units=km"))
  plot(sporigin, col = colors, border = NA) 
  gArea(spTransform(sporigin, CRS("+proj=laea +lat_0=40 +lon_0=-100 +units=km")))

  # list this focal spp competitor
  tmp = filter(new_spec_weights, sp == new_spec_weights$focalcat)
  comp_spp = tmp$compcat
  
  # match competitor sp to focal spp, intersect its range with the focal range,
  # and calcualte the area of overlap between the two species.
  for(co in comp_spp) {          
      #co = 'Cistothorus_palustris' 
      #print(co)
      c1 = all_spp_list[grep(co, all_spp_list)]
      c2 = c1[grep('.shp', c1)]
      c3 = strsplit(c2, ".shp")
      comp.poly <- readShapePoly(paste("Z:/GIS/birds/All/All/", c3, sep = "")) # reads in species-specific shapefile
      proj4string(comp.poly) <- intl_proj
      corigin = comp.poly[comp.poly@data$SEASONAL == 1|comp.poly@data$SEASONAL == 2|comp.poly@data$SEASONAL ==5,]
      corigin = spTransform(corigin, sp_proj)
      plot(corigin, add = TRUE ,col = colors, border = NA) 
      # intersect from raster package
      sporigin = gBuffer(sporigin, byid=TRUE, width=0)
      corigin = gBuffer(corigin, byid=TRUE, width=0)
      
      pi = intersect(sporigin, corigin)
      #plot(pi)
      spArea = gArea(sporigin) # in m
      coArea = gArea(corigin)
      area_overlap = gArea(pi)
      focalAOU = unique(new_spec_weights[new_spec_weights$focalcat == sp, c('FocalAOU')])
      compAOU = unique(new_spec_weights[new_spec_weights$compcat == co, c('CompetitorAOU')])
      filesoutput = rbind(filesoutput, c(sp, focalAOU, co, compAOU, spArea, coArea, area_overlap))
  }
} 

filesoutput = data.frame(filesoutput)
colnames(filesoutput) = c("Focal", "focalAOU","Competitor", "compAOU","FocalArea", "CompArea", "area_overlap")
# string split to get sci name with spaces
filesoutput$Focal = gsub('_',' ',filesoutput$Focal)
write.csv(filesoutput, file = "shapefile_areas.csv")
}

# read in area shapefile if not running code 
filesoutput = read.csv("shapefile_areas.csv", header = TRUE)

############# ---- Generate total species occupancies ---- #############
# gathering occupancy data for all species
all_occ = gather(coyle_o, "AOU", "occupancy", 2:ncol(coyle_o))
all_occ$AOU = as.character(all_occ$AOU)
all_occ$AOU = as.numeric(substr(all_occ$AOU, 2, nchar(all_occ$AOU)))
all_occ = all_occ[!is.na(all_occ$occupancy), ]

# Winter Wren had AOU code change (7220 to 7222), changing in occ code to reflect that
all_occ$AOU[all_occ$AOU == 7220] <- 7222

# pull out stateroutes that have been continuously sampled 1996-2010
routes = unique(all_occ$X)

sub_ep = merge(expect_pres, sp_list, by = 'AOU', all = TRUE) 
# merge expected presence with occupancy data
new_occ = merge(sub_ep, all_occ, by.x = c('stateroute', 'AOU'), by.y = c('X', 'AOU'), all = TRUE)
new_occ$occupancy[is.na(new_occ$occupancy)] = 0

# subset to routes in the well sampled list of 'routes'
new_occ2 = new_occ[new_occ$stateroute %in% routes, ]

# Pull out summary of different levels of occupancy by species (Allen wrote loop)
occ_dist_output = data.frame(AOU = NA, occupancy = NA, count = NA)
bins = seq(0.1, 1, by = .1)
for (s in unique(new_occ2$AOU)) {
  tmp = subset(new_occ2, AOU == s)
  occ_counts = sapply(bins, function(x) sum(tmp$occupancy <= x & tmp$occupancy > (x-0.1)))
  tmp_out = data.frame(AOU = s, occupancy = bins, count = occ_counts/nrow(tmp))
  occ_dist_output = rbind(occ_dist_output, tmp_out)
}
occ_dist_output = occ_dist_output[-1, ]

# average distribution for all species
avg_occ_dist = aggregate(occ_dist_output$count, by = list(occ_dist_output$occupancy), mean)
names(avg_occ_dist) = c('occupancy', 'frequency')
avg_occ_dist$occupancy = as.numeric(as.character(avg_occ_dist$occupancy))

# plot total avg avian occupancy distribution
plot(avg_occ_dist$occupancy, avg_occ_dist$frequency, type = 'l', lwd = 3,
     xlab = "Average Occupancy Distribution", ylab = "Frequency of Occupancy")
# add plotting in center, subtract .05 in x axis
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
occ_abun = merge(bbs_abun, new_occ2[, c('AOU', 'stateroute' ,'occupancy', 'SciName')], 
                by = c("AOU", "stateroute"))

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

# for loop to select sp and compare to their competitor(s) 
### select strongest competitor, sum competitor abundance by stateroute
focalcompoutput = c()
for (sp in focalspecies) {
  print(sp)
  tmp = filter(occ_abun, AOU == sp)  #why is this occ_abun
  comp_spp = shapefile_areas[shapefile_areas$focalAOU == sp, c('compAOU', 'mainCompetitor')]
  
  mainComp = comp_spp$compAOU[comp_spp$mainCompetitor == 1]
  bbs_comp = bbs_pool %>% filter(AOU %in% comp_spp$compAOU & stateroute %in% tmp$stateroute) 
  bbs_comp2 = merge(bbs_comp, comp_spp, by.x = 'AOU', by.y = 'compAOU')
  bbs_comp2$mainCompN = bbs_comp2$abundance * bbs_comp2$mainCompetitor
  
  compsum = bbs_comp2 %>% group_by(stateroute) %>% 
      dplyr::summarize(AllCompN = sum(abundance), MainCompN = sum(mainCompN))
  
  focalout = merge(tmp, compsum, by = 'stateroute', all.x = TRUE)  
  focalout[is.na(focalout)] = 0
  focalout$MainCompAOU = unique(comp_spp$compAOU[comp_spp$mainCompetitor == 1]) 
  
  names(focalout)[names(focalout)=="occupancy"] <- "FocalOcc"
  # main competitor occupancy
  MainCompAOU =  unique(focalout$MainCompAOU)
  
  # subset occupancy by state route, merge in main competitor
  match_occ_stroute = filter(new_occ2, new_occ2$stateroute %in% focalout$stateroute)

  focal_comp_occ = merge(focalout, match_occ_stroute[,c('AOU', 'stateroute', 'occupancy')], 
                         by.x = c('MainCompAOU', 'stateroute'), by.y = c('AOU', 'stateroute'))
  names(focal_comp_occ)[names(focal_comp_occ)=="occupancy"] <- "MainCompOcc" # do we need competitor occ? makes DF huge
  focalcompoutput = rbind(focalcompoutput, focal_comp_occ)
}

focalcompoutput = data.frame(focalcompoutput)
colnames(focalcompoutput) = c( "MainCompAOU", "stateroute","FocalAOU", "FocalAbundance", "FocalOcc","FocalSciName",
                               "AllCompSum", "MainCompSum", "CompOcc")

# Filter number to spp present in at least 20 routes for better model results
# Subset to get the count of routes for each spp
numroutes = c()
for (sp in focalspecies) {
  print(sp)
  tmp = filter(focalcompoutput, FocalAOU == sp) 
  nroutes = tmp %>%
  group_by(FocalAOU) %>%
  summarise(n_distinct(stateroute))
  numroutes = rbind(numroutes, c(unique(tmp$FocalAOU), nroutes))
}
numroutes = data.frame(numroutes)
colnames(numroutes) = c("FocalAOU","AOU", "nroutes")
# Filter count to greater than or equal to 20
focalcompoutput1 = filter(numroutes, nroutes >= 20)
focalcompoutput1$nroutes = as.numeric(focalcompoutput1$nroutes)
# Merge with original data table, new # of focal spp is 63
focalcompsub = merge(focalcompoutput, focalcompoutput1, by = "FocalAOU")
# Creating new focalspecies index
subfocalspecies = unique(focalcompsub$FocalAOU)

# Create scaled competitor column
focalcompsub$comp_scaled = focalcompsub$MainCompSum/(focalcompsub$FocalAbundance + focalcompsub$MainCompSum)

######## PDF of each species BBS occurrences ########
# merge in lat/long
latlongs = read.csv('routes 1996-2010 consecutive.csv', header = T)
plotdata_all = merge(focalcompsub, latlongs, by = "stateroute") 

# Making pdf of ranges for each focal spp
pdf('Plots_RangeMaps.pdf', height = 8, width = 10)
par(mfrow = c(3, 4))

for(sp in subfocalspecies){ 
  print(sp)
  plotsub = plotdata_all[plotdata_all$FocalAOU == sp,]
  map("state") 
  points(plotsub$Longi, plotsub$Lati, col = 3,  pch = 20)
  title(main = (unique(plotdata_all$FocalSciName[plotdata_all$FocalAOU == sp])))
}

dev.off()   


#### ---- Processing Environmental Data - Re-done from Snell_code.R ---- ####
# read in raw env data (from Coyle et al)
all_env = read.csv('All Env Data.csv', header = T)
# merge in ENV
all_expected_pres = merge(all_env[,c("stateroute", "Longi", "Lati",  'sum.EVI', 'elev.mean', 'mat', 'ap.mean')], 
     focalcompsub, by = "stateroute")

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

# rescaling all occupancy values  - odds ratio
# need to get rid of ones in order to not have infinity values 
edge_adjust = .005 
occuenv$FocalOcc_scale = (occuenv$FocalOcc * (1 - 2*edge_adjust)) + edge_adjust
# create logit transformation function, did on rescaled vals
occuenv$occ_logit =  log(occuenv$FocalOcc_scale/(1-occuenv$FocalOcc_scale)) 

##### LIN REG #######
# create beta output data frame
beta_lm = matrix(NA, nrow = length(subfocalspecies), ncol = 10)
beta_abun = matrix(NA, nrow = length(subfocalspecies), ncol = 10)

# for loop subsetting env data to expected occurrence for focal species
envoutput = c()
envoutputa = c()
for (sp in 1:length(subfocalspecies)){
  temp = occuenv[occuenv$Species == subfocalspecies[sp],] 
  
  competition <- lm(temp$occ_logit ~  temp$comp_scaled) 
  # z scores separated out for env effects (as opposed to multivariate variable)
  env_z = lm(occ_logit ~ abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI), data = temp)
  # z scores separated out for env effects
  both_z = lm(temp$occ_logit ~  temp$comp_scaled + abs(temp$zTemp)+abs(temp$zElev)+abs(temp$zPrecip)+abs(temp$zEVI), data = temp)
  
  # abundance, not temp occ - same results?
  competition_abun <- lm(temp$FocalAbundance ~  temp$comp_scaled) 
  # z scores separated out for env effects - abundance
  env_abun = lm(temp$FocalAbundance ~ abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI), data = temp)
  # z scores separated out for env effects - abundance
  both_abun = lm(temp$FocalAbundance ~  comp_scaled + abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI), data = temp)
  
  beta_lm[sp,1] = subfocalspecies[sp]
  beta_lm[sp,2] = summary(competition)$coef[2,"Estimate"]
  beta_lm[sp,3] = summary(competition)$coef[2,"Pr(>|t|)"]
  beta_lm[sp,4] = summary(competition)$r.squared #using multiple rsquared
  beta_lm[sp,5] = summary(env_z)$coef[2,"Estimate"]
  beta_lm[sp,6] = summary(env_z)$coef[2,"Pr(>|t|)"]
  beta_lm[sp,7] = summary(env_z)$r.squared 
  beta_lm[sp,8] = summary(both_z)$coef[2,"Estimate"]
  beta_lm[sp,9] = summary(both_z)$coef[2,"Pr(>|t|)"]
  beta_lm[sp,10] = summary(both_z)$r.squared 
  
  beta_abun[sp,1] = subfocalspecies[sp]
  beta_abun[sp,2] = summary(competition_abun)$coef[2,"Estimate"]
  beta_abun[sp,3] = summary(competition_abun)$coef[2,"Pr(>|t|)"]
  beta_abun[sp,4] = summary(competition_abun)$r.squared #using multiple rsquared
  beta_abun[sp,5] = summary(env_abun)$coef[2,"Estimate"]
  beta_abun[sp,6] = summary(env_abun)$coef[2,"Pr(>|t|)"]
  beta_abun[sp,7] = summary(env_abun)$r.squared 
  beta_abun[sp,8] = summary(both_abun)$coef[2,"Estimate"]
  beta_abun[sp,9] = summary(both_abun)$coef[2,"Pr(>|t|)"]
  beta_abun[sp,10] = summary(both_abun)$r.squared
  
  #variance_partitioning 
    ENV = summary(both_z)$r.squared - summary(competition)$r.squared
    print(ENV) #env only
    COMP = summary(both_z)$r.squared - summary(env_z)$r.squared
    print(COMP) #competition only
    SHARED = summary(competition)$r.squared - COMP
    print(SHARED) #shared variance
    NONE = 1 - summary(both_z)$r.squared
    print(NONE) #neither variance
  sp1 = unique(temp$Species)
  envoutput = rbind(envoutput, c(sp1, ENV, COMP, SHARED, NONE))
  
  #variance_partitioning 
  ENVa = summary(both_abun)$r.squared - summary(competition_abun)$r.squared

  COMPa = summary(both_abun)$r.squared - summary(env_abun)$r.squared

  SHAREDa = summary(competition_abun)$r.squared - COMP

  NONEa = 1 - summary(both_abun)$r.squared

  sp1 = unique(temp$Species)
  envoutputa = rbind(envoutputa, c(sp1, ENVa, COMPa, SHAREDa, NONEa))
}         
dev.off()

envoutput = data.frame(envoutput)
envoutputa = data.frame(envoutputa)
names(envoutput) = c("FocalAOU", "ENV", "COMP", "SHARED", "NONE")
names(envoutputa) = c("FocalAOU", "ENV", "COMP", "SHARED", "NONE")
beta_lm = data.frame(beta_lm)
names(beta_lm) = c("FocalAOU", "Competition_Est", "Competition_P", "Competition_R2", "EnvZ_Est", "EnvZ_P", "EnvZ_R2", "BothZ_Est", "BothZ_P", "BothZ_R2")
beta_abun = data.frame(beta_abun)
names(beta_abun) = c("FocalAOU", "Competition_Est", "Competition_P", "Competition_R2", "EnvZ_Est", "EnvZ_P", "EnvZ_R2", "BothZ_Est", "BothZ_P", "BothZ_R2")

tax_code = read.csv("Tax_AOU_Alpha.csv", header = TRUE)
forplots = merge(Hurlbert_o[, c("AOU","Trophic.Group","Foraging","migclass")], AOUsub4[, c("AOU","Family", "CommonName")], by = "AOU")
# midpoint long of US is -98.5795, so 1 indicates east of that line, 0 = west

# Dark-eyed Junko and Winter Wren need AOUs changed
tax_code$AOU_OUT[tax_code$AOU_OUT == 5677] = 5660
tax_code$AOU_OUT[tax_code$AOU_OUT == 7220] = 7222
forplots$AOU[forplots$AOU == 7220] = 7222

envloc = merge(envoutput, plotdata_all, by = 'FocalAOU', all = TRUE)
envloc$EW <- 0
envloc$EW[envloc$Longi >= -98] <- 1
envloc$EW[is.na(envloc$EW)] = 0

envoutput = merge(envoutput, tax_code[,c('AOU_OUT', 'ALPHA.CODE')], by.x = 'FocalAOU', by.y = "AOU_OUT")
envoutput = merge(envoutput, forplots, by.x = "FocalAOU", by.y = "AOU")


write.csv(envoutput, "envouput.csv")

#### ---- Plotting LMs ---- ####
# Making pdf of ranges for each focal spp
pdf('Lin_Reg.pdf', height = 8, width = 10)
par(mfrow = c(3, 4))
# Plotting basic lms to understand relationships
for(sp in subfocalspecies){ 
  print(sp)
  psub = occuenv[occuenv$Species == sp,]
  #psub = filter(psub, occ_logit < 9) # eliminating 100% occ?
  title = unique(psub$FocalSciName)
  #ggplot(psub, aes(x = psub$occ_logit, y = psub$MainCompSum)) + geom_point(data=psub, pch = 16)+geom_smooth(method = "lm", col = "red")+ theme_classic()+ xlab("Focal Occupancy")+ylab("Competitor Abundance")+ggtitle(title)
  
  #+ ggtitle(title[1])
  competition <- lm(psub$occ_logit ~  psub$comp_scaled) 
  # z scores separated out for env effects (as opposed to multivariate variable)
  env_z = lm(occ_logit ~ abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI), data = psub)
  # z scores separated out for env effects
  both_z = lm(psub$occ_logit ~  psub$comp_scaled + abs(psub$zTemp)+abs(psub$zElev)+abs(psub$zPrecip)+abs(psub$zEVI), data = psub)
  
  plot(psub$comp_scaled, psub$occ_logit, pch = 20, xlab = "Main Competitor Abundance", ylab = "Focal Occupancy (logit link)", main = psub$FocalSciName[1], sub = "Competition", abline(competition, col = "red"))
  plot(psub$comp_scaled,psub$occ_logit,  pch = 20, xlab = "Main Competitor Abundance", ylab = "Focal Occupancy (logit link)", main = psub$FocalSciName[1], sub = "Environment", abline(env_z, col = "red"))
  plot(psub$comp_scaled, psub$occ_logit,  pch = 20, xlab = "Main Competitor Abundance", ylab = "Focal Occupancy (logit link)", main = psub$FocalSciName[1], sub = "Both", abline(both_z, col = "red"))
}
dev.off()

ggplotRegression <- function (fit) {
  
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() + ylim(0, 1)
  stat_smooth(method = "lm", col = "red") + theme_classic() + 
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}
# source = https://susanejohnston.wordpress.com/2012/08/09/a-quick-and-easy-function-to-plot-lm-results-in-r/

# lm for all
plot(occuenv$occ_logit, occuenv$comp_scaled, pch = 20, abline(competition, col = "red", lwd = 3))
par(new= TRUE)
plot(occuenv$occ_logit, occuenv$comp_scaled, pch = 16, abline(env_z, col = "green"), lwd = 3)
par(new= TRUE)
plot(occuenv$occ_logit, occuenv$comp_scaled, pch = 16, abline(both_z, col = "blue"), lwd = 3, xlab = "Focal Occupancy (logit link)", ylab = "Main Competitor Abundance")

# Plotting basic lm hists to understand relationships btwn occ and abun
hist(beta_lm$Competition_R2, 10, main = "R Squared Distribution for Competition", xlab = "Competition R Squared")
hist(beta_lm$EnvZ_R2, 10, main = "R Squared Distribution for Env", xlab = "Env R Squared")
hist(beta_lm$BothZ_R2, 10, main = "R Squared Distribution for Both", xlab = "Both R Squared")

hist(beta_lm$Competition_Est, 10, main = "Slope Distribution for Competition", xlab = "Competition Slope")
abline(v = mean(beta_lm$Competition_Est), col = "red", lwd = 3)
hist(beta_lm$EnvZ_Est, 10, main = "Slope Distribution for Environment", xlab = "Environment Slope")
abline(v = mean(beta_lm$EnvZ_Est), col = "red", lwd = 3)
hist(beta_lm$BothZ_Est, 10, main = "Slope Distribution for Both", xlab = "Both Slope")
abline(v = mean(beta_lm$BothZ_Est), col = "red", lwd = 3)

hist(beta_abun$Competition_R2, 10, main = "R Squared Distribution for Competition", xlab = "Competition R Squared")
hist(beta_abun$EnvZ_R2, 10, main = "R Squared Distribution for Env", xlab = "Env R Squared")
hist(beta_abun$BothZ_R2, 10, main = "R Squared Distribution for Both", xlab = "Both R Squared")

hist(beta_abun$Competition_Est, 10, main = "Slope Distribution for Competition", xlab = "Competition Slope")
abline(v = mean(beta_abun$Competition_Est), col = "red", lwd = 3)
hist(beta_abun$EnvZ_Est, 10, main = "Slope Distribution for Environment", xlab = "Environment Slope")
abline(v = mean(beta_abun$EnvZ_Est), col = "red", lwd = 3)
hist(beta_abun$BothZ_Est, 10, main = "Slope Distribution for Both", xlab = "Both Slope")
abline(v = mean(beta_abun$BothZ_Est), col = "red", lwd = 3)

beta_lm$sumR2 = beta_lm$BothZ_R2+beta_lm$Competition_R2+beta_lm$EnvZ_R2 
#### ---- GLM fitting  ---- ####
# add on success and failure columns by creating # of sites where birds were found
# and # of sites birds were not found from original bbs data
# create counter column to sum across years
occuenv$counter = 1
# aggregate to sum across years by site
binom = aggregate(occuenv$counter, by = list(occuenv$stateroute), FUN = sum) 
#rename columns to make more clear
colnames(binom) <- c("stateroute", "numyears")

# merge success/failure columns w environmnetal data, missing 0 occupancies
occumatrix = merge(occuenv, binom, by = "stateroute", all.x = TRUE)

# using equation species sum*Focal occ to get success and failure for binomial anlaysis
occumatrix$sp_success = as.factor(occumatrix$numyears * occumatrix$FocalOcc)
occumatrix$sp_fail = as.factor(occumatrix$numyears * (1 - occumatrix$FocalOcc))

# using equation species sum*Focal abun to get success and failure for binomial anlaysis
occumatrix$sp_success_abun = as.factor(occumatrix$numyears * occumatrix$FocalAbundance)
occumatrix$sp_fail_abun = as.factor(occumatrix$numyears * (1 - occumatrix$FocalAbundance))

cs <- function(x) scale(x,scale=TRUE,center=TRUE)
# source: http://permalink.gmane.org/gmane.comp.lang.r.lme4.devel/12080
# need to scale predictor variables
beta = matrix(NA, nrow = length(subfocalspecies), ncol = 31)
pdf('Occupancy_glms.pdf', height = 8, width = 10)
par(mfrow = c(3, 4))
# for loop to store model output as a DF
for(i in 1:length(subfocalspecies)){ 
  print(i)

  occsub = occumatrix[occumatrix$Species == subfocalspecies[i],]
  
  glm_abundance_binom = glm(cbind(sp_success, sp_fail) ~ comp_scaled + 
       abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI), family = binomial(link = logit), data = occsub)
  summary(glm_abundance_binom)

  glm_abundance_rand_site = glmer(cbind(sp_success_abun, sp_fail_abun) ~ cs(comp_scaled) + 
      abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI) + (1|stateroute:Species), family = binomial(link = logit), data = occsub)
  summary(glm_abundance_rand_site)

  glm_occ_rand_site = glmer(cbind(sp_success, sp_fail) ~ cs(comp_scaled) + 
     abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI) + (1|stateroute:Species), family = binomial(link = logit), data = occsub)
  summary(glm_occ_rand_site) 
 
  beta[i,1] = subfocalspecies[i]
  beta[i,2] = summary(glm_abundance_binom)$coef[2,"Estimate"] # comp_scaled
  beta[i,3] = summary(glm_abundance_binom)$coef[3,"Estimate"] # abs(zTemp)
  beta[i,4] = summary(glm_abundance_binom)$coef[4,"Estimate"] # abs(zElev)
  beta[i,5] = summary(glm_abundance_binom)$coef[5,"Estimate"] # abs(zPrecip)
  beta[i,6] = summary(glm_abundance_binom)$coef[6,"Estimate"] # abs(zEVI)
  beta[i,7] = summary(glm_abundance_binom)$coef[2,"Pr(>|z|)"] # comp_scaled
  beta[i,8] = summary(glm_abundance_binom)$coef[3,"Pr(>|z|)"] # abs(zTemp)
  beta[i,9] = summary(glm_abundance_binom)$coef[4,"Pr(>|z|)"] # abs(zElev)
  beta[i,10] = summary(glm_abundance_binom)$coef[5,"Pr(>|z|)"] # abs(zPrecip)
  beta[i,11] = summary(glm_abundance_binom)$coef[6,"Pr(>|z|)"] # abs(zEVI)
    
  beta[i,12] = summary(glm_abundance_rand_site)$coef[2,"Estimate"] # comp_scaled
  beta[i,13] = summary(glm_abundance_rand_site)$coef[3,"Estimate"] # abs(zTemp)
  beta[i,14] = summary(glm_abundance_rand_site)$coef[4,"Estimate"] # abs(zElev)
  beta[i,15] = summary(glm_abundance_rand_site)$coef[5,"Estimate"] # abs(zPrecip)
  beta[i,16] = summary(glm_abundance_rand_site)$coef[6,"Estimate"] # abs(zEVI)
  beta[i,17] = summary(glm_abundance_rand_site)$coef[2,"Pr(>|z|)"] # comp_scaled
  beta[i,18] = summary(glm_abundance_rand_site)$coef[3,"Pr(>|z|)"] # abs(zTemp)
  beta[i,19] = summary(glm_abundance_rand_site)$coef[4,"Pr(>|z|)"] # abs(zElev)
  beta[i,20] = summary(glm_abundance_rand_site)$coef[5,"Pr(>|z|)"] # abs(zPrecip)
  beta[i,21] = summary(glm_abundance_rand_site)$coef[6,"Pr(>|z|)"] # abs(zEVI)

  beta[i,22] = summary(glm_occ_rand_site)$coef[2,"Estimate"] # comp_scaled
  beta[i,23] = summary(glm_occ_rand_site)$coef[3,"Estimate"] # abs(zTemp)
  beta[i,24] = summary(glm_occ_rand_site)$coef[4,"Estimate"] # abs(zElev)
  beta[i,25] = summary(glm_occ_rand_site)$coef[5,"Estimate"] # abs(zPrecip)
  beta[i,26] = summary(glm_occ_rand_site)$coef[6,"Estimate"] # abs(zEVI)
  beta[i,27] = summary(glm_occ_rand_site)$coef[2,"Pr(>|z|)"] # comp_scaled
  beta[i,28] = summary(glm_occ_rand_site)$coef[3,"Pr(>|z|)"] # abs(zTemp)
  beta[i,29] = summary(glm_occ_rand_site)$coef[4,"Pr(>|z|)"] # abs(zElev)
  beta[i,30] = summary(glm_occ_rand_site)$coef[5,"Pr(>|z|)"] # abs(zPrecip)
  beta[i,31] = summary(glm_occ_rand_site)$coef[6,"Pr(>|z|)"] # abs(zEVI)

}
beta = data.frame(beta)
names(beta) = c("FocalAOU", "Binom_comp_scaled_Estimate", "Binom_zTemp_Estimate", "Binom_zElev_Estimate", "Binom_zPrecip_Estimate", "Binom_zEVI_Estimate", "Binom_comp_scaled_P", "Binom_zTemp_P", "Binom_zElev_P", "Binom_zPrecip_P", "Binom_zEVI_P", "Abundance_comp_scaled_Estimate","Abundance_zTemp_Estimate","Abundance_zElev_Estimate","Abundance_zPrecip_Estimate","Abundance_zEVI_Estimate","Abundance_comp_scaled_P", "Abundance_zTemp_P", "Abundance_zElev_P", "Abundance_zPrecip_P", "Abundance_zEVI_P", "Randsite_comp_scaled_Estimate", "Randsite_zTemp_Estimate", "Randsite_zElev_Estimate", "Randsite_zPrecip_Estimate", "Randsite_zEVI_Estimate", "Randsite_comp_scaled_P", "Randsite_zTemp_P", "Randsite_zElev_P", "Randsite_zPrecip_P", "Randsite_zEVI_P")
dev.off()
AIC(glm_abundance_binom, glm_abundance_rand_site,glm_occ_rand_site) ## abundace rand site is clear winner
#Plot winning glm
# GLM of all matrices not just subset
glm_occ_rand_site = glmer(cbind(sp_success, sp_fail) ~ cs(comp_scaled) + 
    abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI) + (1|stateroute:Species), family = binomial(link = logit), data = occumatrix)
summary(glm_occ_rand_site) 

glm_abun_rand_site = glmer(cbind(sp_success_abun, sp_fail_abun) ~ cs(comp_scaled) + 
   abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI) + (1|stateroute:Species), family = binomial(link = logit), data = occumatrix)
summary(glm_abundance_rand_site) 

#### PLOTTING MODELS ####
ggplot(data = occumatrix, aes(x = comp_scaled, y = FocalOcc)) +stat_smooth(data=glm_occ_rand_site, lwd = 1.5) +xlab("Scaled Competitor Abundance")+ylab("Focal Occupancy") +theme_bw() +theme(axis.title.x=element_text(size=24),axis.title.y=element_text(size=24, angle=90), axis.text=element_text(size=12)) + theme(plot.margin = unit(c(.5,6,.5,.5),"lines")) 
ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/glmoutput.png")

ggplot(data = occumatrix, aes(x = comp_scaled, y = FocalAbundance)) +stat_smooth(data=glm_abun_rand_site, lwd = 1.5) +theme_bw()


ggplot(data = occumatrix, aes(x = zTemp, y = FocalOcc)) +stat_smooth(data=glm_occ_rand_site, lwd = 1.5, se = FALSE) +xlab("Mean Temperature Deviation")+ylab("Focal Occupancy")+ geom_vline(xintercept = 0, colour="red", linetype = "longdash") +theme_bw() +theme_bw() +theme(axis.title.x=element_text(size=28),axis.title.y=element_text(size=28, angle=90), axis.text=element_text(size=12)) + theme(plot.margin = unit(c(.5,6,.5,.5),"lines"))#+ annotate("text", x = 3, y = 0.56, label = "Environmental centroid\n for focal species", size=7,vjust=0.5, color = "black")
ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/glmtemp.png")

ggplot(data = occumatrix, aes(x = zElev, y = FocalOcc)) +stat_smooth(data=glm_occ_rand_site, lwd = 1.5, se = FALSE) +xlab("Mean Elevation Deviation")+ylab("Focal Occupancy")+ geom_vline(xintercept = 0, colour="red", linetype = "longdash") +theme_bw() +theme_bw() +theme(axis.title.x=element_text(size=28),axis.title.y=element_text(size=28, angle=90), axis.text=element_text(size=12)) + theme(plot.margin = unit(c(.5,6,.5,.5),"lines"))
ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/glmelev.png")
ggplot(data = occumatrix, aes(x = zPrecip, y = FocalOcc)) +stat_smooth(data=glm_occ_rand_site, lwd = 1.5, se = FALSE) +xlab("Mean Precipitation Deviation")+ylab("Focal Occupancy")+ geom_vline(xintercept = 0, colour="red", linetype = "longdash") +theme_bw() +theme_bw() +theme(axis.title.x=element_text(size=28),axis.title.y=element_text(size=28, angle=90), axis.text=element_text(size=12)) + theme(plot.margin = unit(c(.5,6,.5,.5),"lines"))
ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/glmprecip.png")
ggplot(data = occumatrix, aes(x = zEVI, y = FocalOcc)) +stat_smooth(data=glm_occ_rand_site, lwd = 1.5, se = FALSE) +xlab("Mean Vegetation Deviation")+ylab("Focal Occupancy")+ geom_vline(xintercept = 0, colour="red", linetype = "longdash") +theme_bw() +theme_bw() +theme(axis.title.x=element_text(size=28),axis.title.y=element_text(size=28, angle=90), axis.text=element_text(size=12)) + theme(plot.margin = unit(c(.5,6,.5,.5),"lines"))
ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/glmevi.png")


#### ---- Plotting GLMs ---- ####
# Making pdf of ranges for each focal spp
pdf('precip_Reg.pdf', height = 8, width = 10)
par(mfrow = c(3, 4))
# Plotting basic lms to understand relationships
for(sp in subfocalspecies){ 
  print(sp)
  psub = occumatrix[occumatrix$Species == sp,]
  glm_occ_rand_site = glmer(cbind(sp_success, sp_fail) ~ cs(comp_scaled) + 
                              abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI) + (1|stateroute:Species), family = binomial(link = logit), data = psub)
  
  tes = ggplot(data = psub, aes(x = zPrecip, y = FocalOcc)) +stat_smooth(data=glm_occ_rand_site, lwd = 1.5,se = FALSE) +xlab(psub$Species)+theme_bw()
  plot(tes)
}
dev.off()
# Making pdf of ranges for each focal spp
pdf('Temp_Reg.pdf', height = 8, width = 10)
par(mfrow = c(3, 4))
# Plotting basic lms to understand relationships
for(sp in subfocalspecies){ 
  print(sp)
  psub = occumatrix[occumatrix$Species == sp,]
  glm_occ_rand_site = glmer(cbind(sp_success, sp_fail) ~ cs(comp_scaled) + 
                              abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI) + (1|stateroute:Species), family = binomial(link = logit), data = psub)
  
  tes = ggplot(data = psub, aes(x = zTemp, y = FocalOcc)) +stat_smooth(data=glm_occ_rand_site, lwd = 1.5,se = FALSE) +xlab(psub$Species) +theme_bw()
  plot(tes)
}
dev.off()
# Making pdf of ranges for each focal spp
pdf('GLM_Reg.pdf', height = 8, width = 10)
par(mfrow = c(3, 4))
# Plotting basic lms to understand relationships
for(sp in subfocalspecies){ 
  print(sp)
  psub = occumatrix[occumatrix$Species == sp,]
  glm_occ_rand_site = glmer(cbind(sp_success, sp_fail) ~ cs(comp_scaled) + 
                              abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI) + (1|stateroute:Species), family = binomial(link = logit), data = psub)
  
  tes = ggplot(data = psub, aes(x = comp_scaled, y = FocalOcc)) +stat_smooth(data=glm_occ_rand_site, lwd = 1.5,se = FALSE) +theme_bw()
  plot(tes)
}
dev.off()

###### plots for poster ######
plotsub = all_expected_pres[all_expected_pres$AOU == 7280,] # red-breasted nuthatch
proutes = plotsub$stateroute
comp1 = all_expected_pres[all_expected_pres$AOU == 7260|all_expected_pres$AOU == 7270,] # Brown Creeper
comp1plot = comp1[comp1$stateroute %in% proutes,]
#comp2 = plotdata[plotdata$AOU == 7270,] # White-breasted Nuthatch
#comp2plot = comp2[comp2$stateroute %in% proutes,]

map("state") 
Red_breasted_Nuthatch = points(plotsub$Longi, plotsub$Lati, col = "black",  pch = 16, cex = plotsub$FocalOcc*6)
Brown_Creeper = points(comp1plot$Longi, comp1plot$Lati, col = alpha("darkorchid1", 0.5),  pch = 16, cex = comp1$comp_scaled*6)
legend("bottomleft", legend = c("Red-breasted Nuthatch", "Competitors"), col = c("black","darkorchid1"), pch = 19, cex = 1)

# showing the number of species present at each route
#numspp_route = focalcompoutput %>%
 # group_by(stateroute) %>%
 # summarise(numspp = n_distinct(FocalAOU))
#numspp = merge(numspp_route, latlongs, by = "stateroute" )
#map("state") 
#points(numspp$Longi, numspp$Lati, col = "dark green",  pch = 20, cex = numspp$numspp/5)
envoutput = read.csv("envoutput.csv", header = TRUE)
#####PLOTTING variance partitioning
## Creating env data table to plot ranked data

nrank = envoutput %>% 
  mutate(rank = row_number(-ENV))# change here for comp
envflip = gather(nrank, "Type", "value", 2:5)
envflip$rank <- factor(envflip$rank, levels = envflip$rank[order(envflip$rank)])
envflip = plyr::arrange(envflip,(envflip$rank),envflip$FocalAOU)

envrank = envflip %>% 
  group_by(Type == 'ENV') %>% # change here for comp
  mutate(rank = row_number(-value)) # need to get just the envs to rank, then plot
envrank <- envrank[order(envrank$rank),]


# Stacked bar plot for each focal aou
ggplot(data=envflip, aes(x=factor(FocalAOU), y=value, fill=Type)) + geom_bar(stat = "identity") + xlab("Focal AOU") + ylab("Percent Variance Explained") + theme(axis.text.x=element_text(angle=90,size=10,vjust=0.5)) + theme_classic()

### CREATE LABEL DF FAMilY ########
lab1 = filter(envflip, Type == "ENV") # change here for comp
lab1$Fam_abbrev = lab1$Family
lab1$Fam_abbrev = gsub('Emberizidae','E', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Turdidae','Tu', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Fringillidae','F', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Parulidae','P', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Tyrannidae','Ty', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Mimidae','M', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Hirundinidae','H', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Regulidae','R', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Vireonidae','V', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Aegithalidae','A', lab1$Fam_abbrev)                        
lab1$Fam_abbrev = gsub('Corvidae','Co', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Troglodytidae','T', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Certhiidae','C', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Cuculidae','Cu', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Sittidae','S', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Icteridae','I', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Picidae','Pi', lab1$Fam_abbrev)

lab1$Fam_abbrevf = as.factor(as.character(lab1$Fam_abbrev))
lab1$Fam_abbrevf = as.factor(as.numeric(lab1$Fam_abbrevf))
lab1$Fam_abbrevf = gsub('1','hello', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('2','its', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('3','meee', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('4','was', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('5','wondering', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('6','iffffff', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('7','after', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('8','all', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('9','these', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('hello0','years', lab1$Fam_abbrevf)                        
lab1$Fam_abbrevf = gsub('hellohello','youd', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('helloits','like', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('hellomeee','tooooo', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('hellowas','meet', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('hellowondering','gooooo', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('helloiffffff','over', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('helloafter','everything', lab1$Fam_abbrevf)

lab1$Fam_abbrevf = gsub('hello','#000000', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('its','#000000', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('meee','#000000', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('was','#000000', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('wondering','#9ecae1', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('iffffff','#000000', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('after','#000000', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('all','#000000', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('these','#000000', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('years','#0080ff', lab1$Fam_abbrevf)                       
lab1$Fam_abbrevf = gsub('youd','#000000', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('like','#000000', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('tooooo','#000000', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('meet','#000000', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('gooooo','#7f7fff', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('over','#0000ff', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('everything','#048691', lab1$Fam_abbrevf)
famlabel = lab1$Fam_abbrev
####### OTHER LABEL ######
lab1$mig_abbrev = lab1$migclass
lab1$mig_abbrev = gsub("neotrop", 'L', lab1$mig_abbrev)
lab1$mig_abbrev = gsub("resid", 'R', lab1$mig_abbrev)
lab1$mig_abbrev = gsub("short", 'S', lab1$mig_abbrev)
lab1$mig_abbrevf = as.factor(as.character(lab1$mig_abbrev))
lab1$mig_abbrevf = as.factor(as.numeric(lab1$mig_abbrevf))
lab1$mig_abbrevf = gsub('1','hello', lab1$mig_abbrevf)
lab1$mig_abbrevf = gsub('2','its', lab1$mig_abbrevf)
lab1$mig_abbrevf = gsub('3','meee', lab1$mig_abbrevf)
lab1$mig_abbrevf = gsub('hello','#bae4b3', lab1$mig_abbrevf)
lab1$mig_abbrevf = gsub('its','#31a354', lab1$mig_abbrevf)
lab1$mig_abbrevf = gsub('meee','#006d2c', lab1$mig_abbrevf)
miglabel= lab1$mig_abbrev

lab1$trophlabel = lab1$Trophic.Group
lab1$trophlabel = gsub("frugivore", 'F', lab1$trophlabel)
lab1$trophlabel = gsub("granivore", 'G', lab1$trophlabel)
lab1$trophlabel = gsub("herbivore", 'H', lab1$trophlabel)
lab1$trophlabel = gsub("insct/om", 'X', lab1$trophlabel)
lab1$trophlabel = gsub("insectivore", 'I', lab1$trophlabel)
lab1$trophlabel = gsub("nectarivore", 'N', lab1$trophlabel)
lab1$trophlabel = gsub("omnivore", 'O', lab1$trophlabel)
lab1$trophlabelf = as.factor(as.character(lab1$trophlabel))
lab1$trophlabelf = as.factor(as.numeric(lab1$trophlabelf))
lab1$trophlabelf = gsub('1','hello', lab1$trophlabelf)
lab1$trophlabelf = gsub('2','its', lab1$trophlabelf)
lab1$trophlabelf = gsub('3','meee', lab1$trophlabelf)
lab1$trophlabelf = gsub('4','ive', lab1$trophlabelf)
lab1$trophlabelf = gsub('hello','#fbb4b9', lab1$trophlabelf)
lab1$trophlabelf = gsub('its','#f768a1', lab1$trophlabelf)
lab1$trophlabelf = gsub('meee','#c51b8a', lab1$trophlabelf)
lab1$trophlabelf = gsub('ive','#7a0177', lab1$trophlabelf)

###### PLOTTING #####
# Plot with ENV ranked in decreasing order
t = ggplot(data=envflip, aes(factor(rank), y=value, fill=factor(Type, levels = c("ENV","COMP","SHARED","NONE")))) + 
  geom_bar(stat = "identity")  + theme_classic() +
  theme(axis.text.x=element_text(angle=90,size=10,vjust=0.5)) + xlab("Focal Species") + ylab("Percent Variance Explained") +
  scale_fill_manual(values=c("#2ca25f","#dd1c77","#43a2ca","white"), labels=c("Environment", "Competition","Shared Variance", "")) +theme(axis.title.x=element_text(size=20),axis.title.y=element_text(size=20, angle=90),legend.title=element_text(size=12), legend.text=element_text(size=12)) + guides(fill=guide_legend(title=""))+ theme(plot.margin = unit(c(.5,6,.5,.5),"lines")) 

tt = t + annotate("text", x = 1:63, y = -.03, label = unique(envflip$ALPHA.CODE), angle=90,size=6,vjust=0.5, color = "black") + annotate("text", x = 1:63, y = -.06, label = lab1$Fam_abbrev, size=6,vjust=0.5, color = lab1$Fam_abbrevf, fontface =2) + annotate("text", x = 1:63, y = -.08, label = lab1$mig_abbrev, size=6,vjust=0.5, color = lab1$mig_abbrevf, fontface =2) + annotate("text", x = 1:63, y = -.1, label = lab1$trophlabel, size=6,vjust=0.5, color = lab1$trophlabelf, fontface =2) + theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.ticks=element_blank(), axis.text.y=element_text(size = 20)) 
plot(tt)

ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/barplot.pdf", height = 26, width = 34)

#Violin plots w location, trophic group, mig
ggplot(envflip, aes(x = Type, y = value, color = Type)) + geom_violin() 
ggplot(envdiet, aes(x = Trophic.Group, y = value, color = Type)) + geom_violin() 
ggplot(envdiet, aes(x = migclass, y = value, color = Type)) + geom_violin() 
ggplot(envdiet, aes(x = Foraging, y = value, color = Type)) + geom_violin() 
ggplot(envfam, aes(x = FAMILY, y = value, color = Type)) + geom_violin() 

ggplot(envfliploc, aes(x = factor(EW), y = value, color = Type)) + geom_violin() + scale_x_discrete(labels=c("West", "East")) 

# R2 plot - lm in ggplot
R2plot = merge(envoutput, envoutputa, by = "FocalAOU")

tomerge = c()
for (sp in subfocalspecies) {
  spsub = R2plot[R2plot$FocalAOU == sp,]
  total.x = sum(spsub$COMP.x + spsub$ENV.x + spsub$SHARED.x)
  total.y = sum(spsub$COMP.y + spsub$ENV.y + spsub$SHARED.y)
  tomerge = rbind(tomerge, c(sp, total.x, total.y))
}
tomerge = data.frame(tomerge)
names(tomerge) = c("FocalAOU","Total.x", "Total.y")

R2plot2 = merge(R2plot, tomerge, by = "FocalAOU")

ggplot(R2plot2, aes(x = COMP.x, y = COMP.y)) +theme_bw()+ theme(axis.title.x=element_text(size=35),axis.title.y=element_text(size=35, angle=90)) + xlab("Occupancy R2") + ylab("Abundance R2") + geom_point(col = "#9ecae1", cex =4) + geom_point(data = R2plot2, aes(x = ENV.x, y = ENV.y), shape = 24, col = "#3182bd", cex =4, stroke = 1) + geom_point(data = R2plot2, aes(Total.x,Total.y), shape = 3, col = "#253494", cex =5, stroke = 1) +geom_abline(intercept = 0, slope = 1, col = "red", lwd = 1.25)+ theme(axis.text.x=element_text(size = 20),axis.ticks=element_blank(), axis.text.y=element_text(size=20))

# R2 plot - glm
ggplot(R2plot2, aes(x = FocalAOU, y = Total.x)) + geom_violin(lwd = 2, fill = "grey", color = "grey") + xlab("Focal Species") + ylab("Total R2")+ theme_bw()+theme(axis.title.x=element_text(size=30),axis.title.y=element_text(size=30, angle=90)) + theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.ticks=element_blank(), axis.text.y=element_text(size=28, angle=90))

ggplot(R2plot2, aes(x = FocalAOU, y = COMP.x)) + geom_violin(lwd = 2, fill = "#dd1c77", color = "#dd1c77") + xlab("Focal Species") + ylab("Competition R2")+ theme_bw()+theme(axis.title.x=element_text(size=30),axis.title.y=element_text(size=30, angle=90)) + theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.ticks=element_blank(), axis.text.y=element_text(size=28, angle=90))

ggplot(R2plot2, aes(x = FocalAOU, y = ENV.x)) + geom_violin(lwd = 2, fill = "#2ca25f", color = "#2ca25f") + xlab("Focal Species") + ylab("Environment R2")+ theme_bw()+theme(axis.title.x=element_text(size=30),axis.title.y=element_text(size=30, angle=90),legend.title=element_text(size=12), legend.text=element_text(size=12)) + theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.ticks=element_blank(), axis.text.y=element_text(size=28, angle=90)) +scale_y_continuous(limits = c(0, 0.6))
