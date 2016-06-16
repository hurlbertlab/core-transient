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

########NEED  to expand to take any species here
# compare green-tailed towhee to spotted towhee occupancies
# towhees = subsetocc[subsetocc$CommonName == "Spotted Towhee"| subsetocc$CommonName == "Green-tailed Towhee",]
# subset spotted towhees based on AOU code
# spotted = bbs[bbs$Aou == 5880,] 
# aggregate based on year to get just spotted towhee abundance
# spot_agg = aggregate(spotted, by = list(spotted$stateroute), FUN = mean) 

# subset GT towhee within coyle occupancy data
# gt_occ = data.frame(coyle_o$stateroute, coyle_o$X5900)
# subset spotted towhee within coyle occupancy data
# spot_occ = data.frame(coyle_o$stateroute, coyle_o$X5880)
# merge occupancy with bbs for spotted towhee to get raw abundances
# t1 = merge(spot_agg, gt_occ, by.x = "stateroute", by.y = "coyle_o.stateroute")
# insert GT occupancy = 0 instead of NA
# t1$coyle_o.X5900[is.na(t1$coyle_o.X5900)] <- 0
#remove duplicate columns
# drops <- c("Group.1", "Year", "Aou")
# t1 = t1[, !(names(t1) %in% drops)]
# merge occupancy with bbs for spotted towhee
# t2 = merge(spot_occ, gt_occ, by="coyle_o.stateroute")

# read in expected presence data based on BBS 
# clarify expected
expect_pres = read.csv('expected_presence_on_BBS_routes.csv', header = T)
# subset GT towhee within occupancy data

# merge expected occupancy w real occupancy SPOT TOTAL 
# obs_exp_total = merge(gt_ep, t1, by = "stateroute")
# drop extra columns
# drops <- c("SSTATENUMB","SROUTE", "AOU") # -drops
# obs_exp_total = obs_exp_total[, !(names(obs_exp_total) %in% drops)]

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
for (sp in focal_spp) {
  #sp = 'Dendroica_palmarum'
  print(sp)
  t1 = all_spp_list[grep(sp, all_spp_list)]
  t2 = t1[grep('.shp', t1)]
  t3 = strsplit(t2, ".shp")
 # filesoutput = rbind(filesoutput)
 # tp = readOGR("Z:/GIS/birds/All/All", paste(t3, sep = ""), proj4string = sp_proj)
  test.poly <- readShapePoly(paste("Z:/GIS/birds/All/All/", t3, sep = "")) # reads in species-specific shapefile
  proj4string(test.poly) <- intl_proj
  colors = c("red", "yellow", "green", "blue", "purple")
  # subset to just permanent or breeding residents
  sporigin = test.poly[test.poly@data$SEASONAL == 1|test.poly@data$SEASONAL == 2|test.poly@data$SEASONAL ==5,]
  sporigin = spTransform(sporigin, CRS("+proj=laea +lat_0=40 +lon_0=-100 +units=km"))
  plot(sporigin, col = colors, border = NA) 
  gArea(spTransform(sporigin, CRS("+proj=laea +lat_0=40 +lon_0=-100 +units=km")))
  # projection(sporigin), is.projected(sporigin)
  #areas<- unlist(lapply(sporigin@polygons, function(x) a<- x@area)) 
  # list this focal spp competitor
  tmp = filter(new_spec_weights, sp == new_spec_weights$focalcat)
  comp_spp = tmp$compcat
  

  for(co in comp_spp) {         # for loop to match competitor sp to focal spp, intersect its range with the focal range, 
      #co = 'Seiurus_aurocapilla' # and calcualte the area of overlap between the two species.
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

      #gIsValid(corigin,reason=TRUE,byid=TRUE)
      
      pi = intersect(sporigin, corigin)
      plot(pi)
      spArea = gArea(sporigin) # in m
      coArea = gArea(corigin)
      area_overlap = gArea(pi)
      filesoutput = rbind(filesoutput, c(sp, co, spArea, coArea, area_overlap))
  }
} 
write.csv(filesoutput, file = "shapefile_areas.csv")
filesoutput = data.frame(filesoutput)
colnames(filesoutput) = c("sp", "co", "spArea", "coArea", "area_overlap")

# read in area shapefile if not running code 
filesoutput = read.csv("shapefile_areas.csv", header = TRUE)
############# ---- Generate total species occupancies ---- #############
# gathering occupancy data for all species
all_occ = gather(coyle_o, "AOU", "occupancy", 2:ncol(coyle_o))
all_occ$AOU = as.character(all_occ$AOU)
all_occ$AOU = as.numeric(substr(all_occ$AOU, 2, nchar(all_occ$AOU)))
all_occ = all_occ[!is.na(all_occ$occupancy), ]

# pull out stateroutes that have been continuously sampled 1996-2010
routes = all_occ$X

sub_ep = merge(expect_pres, sp_list, by = 'AOU')
# merge expected presence with occupancy data
new_occ = merge(sub_ep, all_occ, by.x = c('stateroute', 'AOU'), by.y = c('X', 'AOU'), all.x = T)
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

#### ---- Retry analysis with final list ---- ####
# filter BBS mean abundance by AOU/stateroute by year
bbs_pool = bbs %>% 
  group_by(stateroute, Aou) %>% 
  summarize(mean(SpeciesTotal))

# filter to relevant species
bbs_abun = filter(bbs_pool, Aou %in% new_occ2$AOU) 

# merge in occupancies of focal and competitor
occ_abun = merge(bbs_abun, new_occ2[, c('AOU', 'occupancy')], by.x = "Aou", by.y = "AOU")
# merge in focal/competitor table to split out focal and competitor
foc_comp_occ_abun = merge(new_spec_weights, occ_abun,  by.x = "FocalAOU", by.y = "Aou")
# for loop to select sp and compare to their competitor ----- focal by stateroute
focal_spp = c(new_spec_weights$focalcat)
names(foc_comp_occ_abun)[names(foc_comp_occ_abun)=="stateroute"] <- "FocalStRoute"
names(foc_comp_occ_abun)[names(foc_comp_occ_abun)=="mean(SpeciesTotal)"] <- "FocalAbun"
names(foc_comp_occ_abun)[names(foc_comp_occ_abun)=="occupancy"] <- "FocalOcc"

final_occ_abun = merge(new_spec_weights, occ_abun,  by.x = "CompetitorAOU", by.y = "Aou")
names(final_occ_abun)[names(final_occ_abun)=="stateroute"] <- "CompStRoute"
names(final_occ_abun)[names(final_occ_abun)=="mean(SpeciesTotal)"] <- "CompAbun"
names(final_occ_abun)[names(final_occ_abun)=="occupancy"] <- "CompOcc"

# for loop to select sp and compare to their competitor(s) 
### select strongest competitor, sum competitor abundance by stateroute
focal_spp_2 = c(unique(final_occ_abun$focalcat))
focalcompoutput = c()
for (sp in focal_spp_2) {
  #sp = "Mniotilta_varia"
  print(sp)
  tmp = filter(final_occ_abun, focalcat == sp)
  comp_spp = unique(tmp$CompetitorAOU)
  
  for(co in comp_spp) {
    #co = 6370
    compsum = final_occ_abun %>% 
      group_by(CompStRoute, FocalSciName, CompetitorAOU) %>% 
      summarize(sum(CompAbun))
    
    focalcompoutput = rbind(focalcompoutput,compsum)
  }}
focalcompoutput = data.frame(focalcompoutput)
colnames(focalcompoutput) = c( "CompStateRoute", "FocalSciName", "CompetitorAOU","SumCompAbun")
#focalcompoutput = write.csv(focalcompoutput, "summed_comp_abun.csv")

######## Skipping plots with raw abundances for now, can revisit later ########
# merge in lat/long
latlongs = read.csv('routes 1996-2010 consecutive.csv', header = T)
plotdata_all = merge(focalcompoutput, latlongs, by.x = "CompStateRoute",by.y = "stateroute") 
# plotdata_gaps = merge(GT_gaps, latlongs, by = "stateroute") #where expected didnt equal observed GT only in coyle
# spotted range in point format
bbs_loc = merge(bbs, latlongs, by = "stateroute")
focal_spp_2 = c(unique(final_occ_abun$FocalAOU))
sp = 7540
focal_abun = bbs_loc[bbs_loc$Aou == sp,] 
compabun = bbs_loc[bbs_loc$Aou == co,]
# view where coyle_occupancy = 0 but predicted presence
# GT_gaps = obs_exp_total[obs_exp_total$coyle_o.X5900 == 0,] 

# plot of states
map("state") 
# adding ranges of spp
points(compabun$Longi.x, compabun$Lati.x, col = 2,  pch = 20, cex = compabun$SpeciesTotal/5) #spotted range = RED
points(focal_abun$Longi.x, focal_abun$Lati.x, col = 3, pch = 16, cex = focal_abun$SpeciesTotal/5) #GT range = GREEN
# points(plotdata_gaps$Longi.x, plotdata_gaps$Lati.x, col = 4, pch = 17) #where GT == 0 but predicted presence BLUE 


#### ---- Processing Environmental Data ---- ####
# read in raw env data (from Coyle et al)
all_env = read.csv('All Env Data.csv', header = T)
# merge in ENV
all_expected_pres = merge(all_env, expect_pres, by = "stateroute")
col_keep_1 <- c("stateroute", "Longi", "Lati",  'sum.EVI', 'elev.mean', 'mat', 'ap.mean', "AOU", 'CommonName', 'match', 'occupancy')
all_expected_pres = all_expected_pres[, (names(all_expected_pres) %in% col_keep_1)]

latlongs = read.csv('routes 1996-2010 consecutive.csv', header = T)
bbs_loc = merge(bbs, latlongs, by = "stateroute")
# for loop subsetting env data to expected occurrence for focal species
focal_aou = c(new_spec_weights$FocalAOU)
output = c()
for (sp in focal_aou){
  temp = all_expected_pres[all_expected_pres$AOU == sp,] # need to automate these
  focal_abun = bbs_loc[bbs_loc$Aou == sp,] 
  env_output = merge(temp, focal_abun, by.x = "AOU", by.y = "Aou", all = FALSE)
  output = rbind(output,c(sp, focal_abun$stateroute, env_output))
}         

# merge focal abundance w expected pres/env variables #NEEDS TO BE IN A LOOP
env_abun = merge(all_expected_pres, focal_abun, by = "stateroute")
env_abun_subset = env_abun[, (names(env_abun) %in% c("stateroute", "elev.mean", "sum.EVI", 
                                                     "mat", "ap.mean","SpeciesTotal","AOU"))]

# NEED: for loop for species-spec weighted avg of env variables
env_abun_subset$meantemp = (env_abun_subset$mat * env_abun_subset$SpeciesTotal)/sum(env_abun_subset$SpeciesTotal)
env_abun_subset$meanelev = (env_abun_subset$elev.mean * env_abun_subset$SpeciesTotal)/sum(env_abun_subset$SpeciesTotal)
env_abun_subset$meanprecip = (env_abun_subset$ap.mean * env_abun_subset$SpeciesTotal)/sum(env_abun_subset$SpeciesTotal)
env_abun_subset$meanevi = (env_abun_subset$sum.EVI * env_abun_subset$SpeciesTotal)/sum(env_abun_subset$SpeciesTotal)


env_focal = env_abun_subset[env_abun_subset$AOU == 5900,]

# weighted SD
output = c()
for (r in env_abun_subset$stateroute){
  output = c(output, rep(env_abun_subset$meantemp, env_abun_subset$SpeciesTotal))
  
}

# read in env data from biol 465 final project, Snell Project Final.R script
env = read.csv('occuenv.csv', header = T)
# subset to GT species  
env_gt = env[env$Species == 5900 |env$Species == 5880,] 
# pulling out environmental z-scores by state route 
col_keeps <- c("stateroute", "Species", "Lati", "Longi", "zTemp","zPrecip", "zElev", "zEVI")
env_zscore = env_gt[, (names(env_gt) %in% col_keeps)]


library(dplyr)


# subset to stateroute, recalculate z-scores from scratch
unique(env_zscore$stateroute)

competitor_focal_env = merge(env_zscore, test, by = "stateroute")

# pulling out environmental z-scores by state route 
col_keep_2 <- c("stateroute", "SpeciesTotal", "coyle_o.X5900")
obs_exp_edit = obs_exp_total[, (names(obs_exp_total) %in% col_keep_2)]

# merge env data w obs_exp_total
env_occu_matrix = merge(env_zscore, obs_exp_edit, by = "stateroute") 
#calculate euclidean distance with z scores
env_occu_matrix$eucdist = sqrt((env_occu_matrix$zTemp)^2 + (env_occu_matrix$zPrecip)^2 + (env_occu_matrix$zElev)^2 + (env_occu_matrix$zEVI)^2)
#renaming columns
colnames(env_occu_matrix)[colnames(env_occu_matrix)=="SpeciesTotal"] <- "SpottedTotal"
colnames(env_occu_matrix)[colnames(env_occu_matrix)=="coyle_o.X5900"] <- "GT_occ"

#### ---- Variance partitioning ---- ####
# create logit transformation function
occ_logit =  log(env_occu_matrix$GT_occ / (1 - env_occu_matrix$GT_occ))

# Interaction between GT occupancy and ST abundance where GT exists
competition <- lm(trans.arcsine(GT_occ) ~  SpottedTotal, data = env_occu_matrix) #LOGIT LINK HERE
# z scores separated out for env effects (as opposed to multivariate variable)
env_z = lm(GT_occ ~ abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI), data = env_occu_matrix)
# z scores separated out for env effects
both_z = lm(GT_occ ~  Spotted_abun + abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI), data = env_occu_matrix)

# Variance partitioning analysis
variance_partitioning = function(x, y) { # change to x and y
  ENV = summary(both_z)$r.squared - summary(competition)$r.squared
  print(ENV) #env only
  COMP = summary(both_z)$r.squared - summary(env_z)$r.squared
  print(COMP) #competition only
  SHARED = summary(competition)$r.squared - COMP
  print(SHARED) #shared variance
  NONE = 1 - summary(both_z)$r.squared
  print(NONE) #neither variance
}
# abiotic variables explain twice as much variation as biotic

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








