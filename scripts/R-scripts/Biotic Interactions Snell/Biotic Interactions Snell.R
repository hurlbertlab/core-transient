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
AOU2 = data.frame(AOU$SCI_NAME, AOU$AOU_OUT, AOU$PRIMARY_COM_NAME)
AOU2 = plyr::rename(AOU2, c("AOU.SCI_NAME" = "SciName", "AOU.AOU_OUT" = "AOU", "AOU.PRIMARY_COM_NAME" = "CommonName"))

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
  #sp = 'Dendroica_palmarum'
  print(sp)
  t1 = all_spp_list[grep(sp, all_spp_list)]
  t2 = t1[grep('.shp', t1)]
  t3 = strsplit(t2, ".shp")
 # GET LINK FIXED AND WRITE IN FOCAL AND COMP AOU
  test.poly <- readShapePoly(paste("z:/GIS/birds/All/All/", t3, sep = "")) # reads in species-specific shapefile
  proj4string(test.poly) <- intl_proj
  colors = c("red", "yellow", "green", "blue", "purple")
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
      #co = 'Parkesia_noveboracensis' 
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
plot(avg_occ_dist$occupancy, avg_occ_dist$frequency, type = 'l', 
     xlab = "Average Occupancy Distribution", ylab = "Frequency of Occupancy")
# add plotting in center, subtract .05 in x axis

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


######## Skipping plots with raw abundances for now, can revisit later ########
# merge in lat/long
latlongs = read.csv('routes 1996-2010 consecutive.csv', header = T)
plotdata_all = merge(focalcompoutput, latlongs, by = "stateroute") 

# plot of states
map("state") 
# adding ranges of spp
points(compabun$Longi.x, compabun$Lati.x, col = 2,  pch = 20, cex = compabun$SpeciesTotal/5) #spotted range = RED
points(focal_abun$Longi.x, focal_abun$Lati.x, col = 3, pch = 16, cex = focal_abun$SpeciesTotal/5) #GT range = GREEN
# points(plotdata_gaps$Longi.x, plotdata_gaps$Lati.x, col = 4, pch = 17) #where GT == 0 but predicted presence BLUE 

pdf('Plots_RangeMaps.pdf', height = 8, width = 10)
par(mfrow = c(3, 4))

for(sp in focalspecies){ 
  map("state") 
  plot(plotdata_all$Longi, plotdata_all$Lati, pch = 20, xlab = sp, main = plotdata_all$FocalSciName[plotdata_all$FocalAOU == sp][1])
}

dev.off()   
#### ---- Processing Environmental Data - Re-done from Snell_code.R ---- ####
# read in raw env data (from Coyle et al)
all_env = read.csv('All Env Data.csv', header = T)
# merge in ENV
all_expected_pres = merge(all_env[,c("stateroute", "Longi", "Lati",  'sum.EVI', 'elev.mean', 'mat', 'ap.mean')], 
     focalcompoutput, by = "stateroute")

#For loop to calculate mean & standard dev environmental variables for each unique species (from BIOL 465)
birdsoutputm = c()
for (sp in focalspecies) {
  spec.routes <- all_expected_pres[(all_expected_pres$FocalAOU) == sp, "stateroute"] #subset routes for each species (i) in tidybirds
  env.sub <- all_expected_pres[all_expected_pres$stateroute %in% routes,] #subset routes for each env in tidybirds
  envmeans = as.vector(apply(all_expected_pres[, c("mat", "ap.mean","elev.mean","sum.EVI")], 2, mean))
  envsd = as.vector(apply(all_expected_pres[, c("mat", "ap.mean","elev.mean","sum.EVI")], 2, sd))
  
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

# for loop subsetting env data to expected occurrence for focal species
envoutput = c()
for (sp in focalspecies){
  temp = occuenv[occuenv$Species == sp,] 

  competition <- lm(temp$FocalOcc ~  temp$MainCompSum) #LOGIT LINK HERE
  # z scores separated out for env effects (as opposed to multivariate variable)
  env_z = lm(FocalOcc ~ abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI), data = temp)
  # z scores separated out for env effects
  both_z = lm(temp$FocalOcc ~  temp$MainCompSum + abs(temp$zTemp)+abs(temp$zElev)+abs(temp$zPrecip)+abs(temp$zEVI), data = temp)
  
  #variance_partitioning = function(x, y) { # change to x and y
    ENV = summary(both_z)$r.squared - summary(competition)$r.squared
    print(ENV) #env only
    COMP = summary(both_z)$r.squared - summary(env_z)$r.squared
    print(COMP) #competition only
    SHARED = summary(competition)$r.squared - COMP
    print(SHARED) #shared variance
    NONE = 1 - summary(both_z)$r.squared
    print(NONE) #neither variance
 # }
  envoutput = rbind(envoutput, c(sp, ENV, COMP, SHARED, NONE))
}         
envoutput = data.frame(envoutput)
names(envoutput) = c("FocalAOU", "ENV", "COMP", "SHARED", "NONE")


# Which competitor has greatest area of overlap? -- main competitor
envoutput$VarPar = 0 # set up main competitor column, 0 = not the primary competitor
for (s in focalspecies) {
  maxVarPar = pmax(envoutput$ENV|envoutput$COMP|envoutput$ENV|envoutput$NONE[envoutput$FocalAOU == s], na.rm = TRUE) 
  envoutput$VarPar[envoutput$focalAOU == s & shapefile_areas$PropOverlap == maxOverlap] = 1 # 1 assigns main competitor
}

#### ---- Variance partitioning ---- ####
# create logit transformation function
occ_logit =  log(env_occu_matrix$GT_occ / (1 - env_occu_matrix$GT_occ)) #### NEED LOGIT

#### ---- Plotting LMs ---- ####
library(ggplot2)

# Plotting basic lms to understand relationships
ggplot(env_occu_matrix, aes(x = GT_occ, y = SpottedTotal)) + 
  geom_point(pch = 16) +
  stat_smooth(method = "lm", col = "red") + theme_classic()

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() + ylim(0, 1)
  stat_smooth(method = "lm", col = "red") + theme_classic() + 
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

ggplotRegression(lm((GT_occ) ~ (SpottedTotal), data = env_occu_matrix))
# source = https://susanejohnston.wordpress.com/2012/08/09/a-quick-and-easy-function-to-plot-lm-results-in-r/

#### ---- GLM fitting  ---- ####
# add on success and failure columns by creating # of sites where birds were found
# and # of sites birds were not found from original bbs data
library(lme4)

# subset to get just GT towhees in raw bbs data
gt_bbs_subset = subset(bbs, Aou == 5900, 
                       select = c("stateroute", "Aou", "SpeciesTotal", "Year"))
# add column of ones to sum up # of sites for each row
gt_bbs_subset$counter = 1

# need to have env info for competitor species
gt_comp_subset = subset(bbs, Aou == 5880,
                        select = c("stateroute", "Aou", "SpeciesTotal", "Year"))

# aggregate to sum across years by site
gt_binom = aggregate(gt_bbs_subset$counter, by = list(gt_bbs_subset$stateroute), FUN = sum) 
#rename columns to make more clear
colnames(gt_binom) <- c("stateroute", "numyears")

# merge success/failure columns w environmnetal data, missing 0 occupancies
env_occu_matrix_1 = merge(env_occu_matrix, gt_binom, by = "stateroute", all.x = TRUE)

# using equation species sum*GT occ to get success and failure for binomial anlaysis
env_occu_matrix_1$sp_success = as.factor(env_occu_matrix_1$numyears * env_occu_matrix_1$GT_occ)
env_occu_matrix_1$sp_fail = as.factor(env_occu_matrix_1$numyears * (1 - env_occu_matrix_1$GT_occ))

# merge Hurlbert_o w env to get diet guilds
# dietguild = merge(occ_dist_output, Hurlbert_o, by = "AOU")
library(lmtest)
library(lme4)
# GLM trials

cs <- function(x) scale(x,scale=TRUE,center=TRUE)
# source: http://permalink.gmane.org/gmane.comp.lang.r.lme4.devel/12080
# need to scale predictor variables

glm_abundance_binom = glm(cbind(sp_success, sp_fail) ~ SpottedTotal + 
                            abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI), family = binomial(link = logit), data = env_occu_matrix_1)
summary(glm_abundance_binom)

glm_abundance_quasibinom = glm(cbind(sp_success, sp_fail) ~ SpottedTotal + 
                                 abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI), family = quasibinomial, data = env_occu_matrix_1)
summary(glm_abundance_quasibinom)

glm_abundance_rand_site = glmer(cbind(sp_success, sp_fail) ~ cs(SpottedTotal) + 
                                  abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI) + (1|stateroute), family = binomial(link = logit), data = env_occu_matrix_1)
summary(glm_abundance_rand_site) 


# what is the cs?!

# want to do a likelihood ratio test on them
anova(glm_abundance_rand_site, test = "Chisq")
anova(glm_abundance_quasibinom, test = "Chisq")
anova(glm_abundance_binom, test = "Chisq")

lrtest(glm_abundance_binom)
lrtest(glm_abundance_quasibinom)
lrtest(glm_abundance_rand_site)


#lr test
logLik(glm_abundance_binom)
logLik(glm_abundance_rand_site)

d0 = deviance(glm_abundance_binom)
d1 = deviance(glm_abundance_rand_site)

LR = d0- d1
pchisq(LR, 1, lower = FALSE)








