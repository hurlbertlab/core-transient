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

# read in temporal occupancy dataset 
Hurlbert_o = read.csv('Master_RO_Correlates_20110610.csv', header = T)
# subset species whose occupancies were between 0.3 and 0.7 over a 10 year period
subsetocc = Hurlbert_o[Hurlbert_o$X10yr.Prop > .3 & Hurlbert_o$X10yr.Prop < .7,]
# write.csv(subsetocc, "focal.csv")
# compare green-tailed towhee to spotted towhee occupancies
towhees = subsetocc[subsetocc$CommonName == "Spotted Towhee"| subsetocc$CommonName == "Green-tailed Towhee",]

# read in BBS data
bbs = read.csv('dataset_1.csv', header = T)
# paring down BBS cols
bbs = bbs[, (names(bbs) %in% c("stateroute", "Aou", "SpeciesTotal",  'routeID', 'Lati', 'Longi'))]
# read in Coyle occupancy data - organized by site 
coyle_o = read.csv('site_sp_occupancy_matrix_Coyle.csv', header = T)
# gather into long format
# coyle_long = gather(coyle_o, Aou, occupancy, X2881:X22860)
# remove x
# coyle_long$Aou = substring(coyle_long$Aou, 2)
# name 1st col stateroute
colnames(coyle_o)[1] = "stateroute"

########NEED  to expand to take any species here
# subset spotted towhees based on AOU code
spotted = bbs[bbs$Aou == 5880,] 
# aggregate based on year to get just spotted towhee abundance
spot_agg = aggregate(spotted, by = list(spotted$stateroute), FUN = mean) 

# subset GT towhee within coyle occupancy data
gt_occ = data.frame(coyle_o$stateroute, coyle_o$X5900)
# subset spotted towhee within coyle occupancy data
spot_occ = data.frame(coyle_o$stateroute, coyle_o$X5880)
# merge occupancy with bbs for spotted towhee to get raw abundances
t1 = merge(spot_agg, gt_occ, by.x = "stateroute", by.y = "coyle_o.stateroute")
# insert GT occupancy = 0 instead of NA
t1$coyle_o.X5900[is.na(t1$coyle_o.X5900)] <- 0
#remove duplicate columns
drops <- c("Group.1", "Year", "Aou")
t1 = t1[, !(names(t1) %in% drops)]
# merge occupancy with bbs for spotted towhee
t2 = merge(spot_occ, gt_occ, by="coyle_o.stateroute")

# read in expected presence data based on BBS 
# clarify expected
expect_pres = read.csv('expected_presence_on_BBS_routes.csv', header = T)
# subset GT towhee within occupancy data
gt_ep = expect_pres[expect_pres$AOU == 5900,] 
# merge expected occupancy w real occupancy SPOT TOTAL 
obs_exp_total = merge(gt_ep, t1, by = "stateroute")
# drop extra columns
drops <- c("SSTATENUMB","SROUTE", "AOU") # -drops
obs_exp_total = obs_exp_total[, !(names(obs_exp_total) %in% drops)]

############# ---- Set up pairwise comparison table ---- #############

ttable = read.csv("trophic_table.csv", header = TRUE)
ttable2 = merge(ttable, Hurlbert_o, by = "AOU")
write.csv(ttable2, "warbler_all.csv")

# create a table with pairwise comparison of each focal species to several potential competitors
focal_competitor_table = read.csv("focal spp.csv", header = TRUE)
focal_competitor_table = data.frame(focal_competitor_table$AOU, focal_competitor_table$CommonName, focal_competitor_table$Competitor)
focal_competitor_table = plyr::rename(focal_competitor_table, c("focal_competitor_table.AOU" = "focalAOU", "focal_competitor_table.CommonName" = "Focal", "focal_competitor_table.Competitor" = "Competitor"))

# read in taxonomy data
AOU = read.csv("Bird_Taxonomy.csv", header = TRUE)
AOU2 = data.frame(AOU$SCI_NAME, AOU$AOU_OUT, AOU$PRIMARY_COM_NAME, AOU$FAMILY)
AOU2 = plyr::rename(AOU2, c("AOU.SCI_NAME" = "SciName", "AOU.AOU_OUT" = "CompetitorAOU", "AOU.PRIMARY_COM_NAME" = "Competitor", "AOU.FAMILY" = "FAMILY"))

# remove duplicates/subspecies
AOUsub = AOU2[-grep("sp.", AOU2$Competitor),]
AOUsub2 = AOUsub[-grep("\\)", AOUsub$Competitor),]
AOUsub3 = AOUsub2[-grep(" \\(", AOUsub2$Competitor),]
AOUsub4 = unique(AOUsub3)

#merge pairwise table with taxonomy info
comp_AOU = merge(focal_competitor_table, AOUsub4, by = "Competitor")
comp_AOU <- comp_AOU[c("Focal", "focalAOU", "Competitor", "CompetitorAOU", "SciName")]


# renaming to get latest scientific names for mismatch spp
tempnames = filter(comp_AOU, SciName == 'Oreothlypis peregrina')
tempnames$SciName = 'Vermivora peregrina'
comp_AOU2 = rbind(comp_AOU, tempnames)
comp_AOU2 = comp_AOU2[!(comp_AOU2$SciName =='Oreothlypis peregrina'), ]

tempnames = filter(comp_AOU, SciName == 'Vermivora pinus')
tempnames$SciName = 'Vermivora cyanoptera'
comp_AOU3 = rbind(comp_AOU2, tempnames)
comp_AOU3 = comp_AOU3[!(comp_AOU3$SciName =='Vermivora pinus'), ]

tempnames = filter(comp_AOU, SciName == 'Stellula calliope')
tempnames$SciName = 'Selasphorus calliope'
comp_AOU4 = rbind(comp_AOU3, tempnames)
comp_AOU4 = comp_AOU4[!(comp_AOU4$SciName =='Stellula calliope'), ]

# import body size data
bsize = read.csv("DunningBodySize_old_2008.11.12.csv", header = TRUE)
bsize = unite(bsize, SciName, Genus, Species, sep = " ")

# renaming to get latest scientific names for mismatch spp
tempnames = filter(bsize, SciName == 'Helmitheros vermivorus')
tempnames$SciName = 'Helmitheros vermivorum'
bsize2 = rbind(bsize, tempnames)
bsize2 = bsize2[!(bsize2$SciName =='Helmitheros vermivorus'), ]

tempnames = filter(bsize, SciName == 'Seiurus noveboracensis')
tempnames$SciName = 'Parkesia noveboracensis'
bsize3 = rbind(bsize2, tempnames)
bsize3 = bsize3[!(bsize3$SciName =='Seiurus noveboracensis'), ]

tempnames = filter(bsize, SciName == 'Vermivora pinus')
tempnames$SciName = 'Vermivora cyanoptera'
bsize4 = rbind(bsize3, tempnames)
bsize4 = bsize4[!(bsize4$SciName =='Vermivora pinus'), ]

tempnames = filter(bsize, SciName == 'Seiurus motacilla')
tempnames$SciName = 'Parkesia motacilla'
bsize5 = rbind(bsize4, tempnames)
bsize5 = bsize5[!(bsize5$SciName =='Seiurus motacilla'), ]

tempnames = filter(bsize, SciName == 'Baeolophus griseus')
tempnames$SciName = 'Baeolophus ridgwayi'
bsize6 = rbind(bsize5, tempnames)
bsize6 = bsize6[!(bsize6$SciName =='Baeolophus griseus'), ]

tempnames = filter(bsize, SciName == 'Stellula calliope')
tempnames$SciName = 'Selasphorus calliope'
bsize7 = rbind(bsize6, tempnames)
bsize7 = bsize7[!(bsize7$SciName =='Stellula calliope'), ]

tempnames = filter(bsize, SciName == 'Pipilo crissalis')
tempnames$SciName = 'Melozone crissalis'
bsize8 = rbind(bsize7, tempnames)
bsize8 = bsize8[!(bsize8$SciName =='Pipilo crissalis'), ]

tempnames = filter(bsize, SciName == 'Contopus borealis')
tempnames$SciName = 'Melozone crissalis'
bsize9 = rbind(bsize8, tempnames)
bsize9 = bsize9[!(bsize9$SciName =='Contopus borealis'), ]

# merge in competitor and focal body size
spec_w_bsize = merge(comp_AOU4, bsize9, by.x = "Focal", by.y = "CommonName")
spec_w_bsize2 = merge(spec_w_bsize, bsize9, by.x = "Competitor", by.y = "CommonName")

spec_w_weights = data.frame(spec_w_bsize2$Focal, spec_w_bsize2$focalAOU, spec_w_bsize2$SciName.y, spec_w_bsize2$Mass.g..x, spec_w_bsize2$Competitor,
                            spec_w_bsize2$CompetitorAOU, spec_w_bsize2$SciName.x, spec_w_bsize2$Mass.g..y)
spec_w_weights = plyr::rename(spec_w_weights, c("spec_w_bsize2.Focal" = "Focal", "spec_w_bsize2.focalAOU" = "FocalAOU", 
                                                "spec_w_bsize2.SciName.y" = "FocalSciName", "spec_w_bsize2.Mass.g..x" = "FocalMass", "spec_w_bsize2.Competitor" = "Competitor",
                                                "spec_w_bsize2.CompetitorAOU" = "CompAOU", "spec_w_bsize2.SciName.x" = "CompSciName", "spec_w_bsize2.Mass.g..y" = "CompMass"))

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
focal_spp = c(new_spec_weights$focalcat)
comp_spp = c(new_spec_weights$compcat) # this may not work...

CRS("+proj=laea +lat_0=40 +lon_0=-100") # lambert azimuthal equal area
usa1 = map(database='state', fill=T, plot=F)
IDs = usa1$names
usa_sp = map2SpatialPolygons(usa1, IDs, CRS("+proj=longlat"))


for (sp in focal_spp){
        sp = 'Setophaga_ruticilla'
  print(sp)
  t1 = all_spp_list[grep(sp, all_spp_list)]
  t2 = t1[grep('.shp', t1)]
  t3 = strsplit(t2, ".shp")
  filesoutput = rbind(filesoutput, t1)
  test.poly <- readShapePoly(paste("Z:/GIS/birds/All/All/", t3, sep = "")) # reads in species-specific shapefile
  
  plot(usa_sp)
  colors = c("red", "yellow", "green", "blue", "purple")
  plot(test.poly[test.poly@data$ORIGIN == '1',], add = TRUE, col = colors, border = NA) 
  
  sporigin = test.poly[test.poly@data$ORIGIN == '1'|test.poly@data$ORIGIN == '2'|test.poly@data$ORIGIN =='5',]
  
  # focal polygon intersection prep: http://gis.stackexchange.com/questions/140504/extracting-intersection-areas-in-r
  n1 = as(sporigin, 'SpatialPolygons')
  focalpoly = SpatialPolygonsDataFrame(n1, data.frame(focalpoly = focalpoly@data$SISID[1:5]), match.ID = FALSE)
  
    for co in comp_spp{         # for loop to match competitor sp to focal spp, intersect its range with the focal range, 
      co = 'Geothlypis_trichas' # and calcualte the area of overlap between the two species.
      print(co)
      c1 = all_spp_list[grep(co, all_spp_list)]
      c2 = c1[grep('.shp', c1)]
      c3 = strsplit(c2, ".shp")
      comp.poly <- readShapePoly(paste("Z:/GIS/birds/All/All/", c3, sep = "")) # reads in species-specific shapefile
      
      # competitor polygon intersection prep
      p1 = union(as(extent(focalpoly), 'SpatialPolygons'), as(extent(comp.poly), 'SpatialPolygons'))
    
      compoly = SpatialPolygonsDataFrame(p1, data.frame(compoly=c('x','y')), match.ID=FALSE)
      projection(compoly) <- projection(focalpoly) # setting projections equal
    
      # intersect from raster package
      pi <- intersect(focalpoly, compoly)
      plot(focalpoly, axes=T); plot(compoly, add=T); plot(pi, add=T, col='red')

      # Extract areas from polygon objects then attach as attribute
      areas <- data.frame(area=sapply(pi@polygons, FUN=function(x) {slot(x, 'area')}))
      row.names(areas) <- sapply(pi@polygons, FUN=function(x) {slot(x, 'ID')})
      # Combine attributes info and areas 
      attArea <- spCbind(pi, areas)
      
      # For each field, get area
      aggregate(focalpoly~compoly, data=attArea, FUN=sum)
  }
} 


roadColors <- c("blue","green","grey","purple", "gold")[test.poly@data$ORIGIN]
plot(test.poly@data$ORIGIN, col=roadColors,
     main="NEON Harvard Forest Field Site\n Roads & Trails")

############# ---- Generate total species occupancies ---- #############
# gathering occupancy data for all species
all_occ = gather(coyle_o, "AOU", "occupancy", 2:ncol(coyle_o))
all_occ$AOU = as.character(all_occ$AOU)
all_occ$AOU = as.numeric(substr(all_occ$AOU, 2, nchar(all_occ$AOU)))
all_occ = all_occ[!is.na(all_occ$occupancy), ]

# pull out stateroutes that have been continuously sampled 1996-2010
routes = all_occ$stateroute

# merge expected presence with occupancy data
new_occ = merge(expect_pres[, c('stateroute', 'AOU')], all_occ, by = c('stateroute', 'AOU'), all.x = T)
new_occ$occupancy[is.na(new_occ$occupancy)] = 0
# subset to routes in the well sampled list of 'routes'
new_occ2 = new_occ[new_occ$stateroute %in% routes, ]

# Pull out summary of different levels of occupancy by species (Allen wrote for loop)
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

#### ---- Plotting ---- ####


# plot total avg avian occupancy distribution
plot(avg_occ_dist$occupancy, avg_occ_dist$frequency, type = 'l', 
     xlab = "Average Occupancy Distribution", ylab = "Frequency of Occupancy")
# add plotting in center, subtract .05 in x axis

# merge in lat/long
latlongs = read.csv('routes 1996-2010 consecutive.csv', header = T)
plotdata_all = merge(obs_exp_total, latlongs, by = "stateroute") 
plotdata_gaps = merge(GT_gaps, latlongs, by = "stateroute") #where expected didnt equal observed GT only in coyle
# spotted range in point format
bbs_loc = merge(bbs, latlongs, by = "stateroute")
GT_abun = bbs_loc[bbs_loc$Aou == 5900,] #EXPAND TO OTHER SPP HERE
spotty = bbs_loc[bbs_loc$Aou == 5880,]
# view where coyle_occupancy = 0 but predicted presence
GT_gaps = obs_exp_total[obs_exp_total$coyle_o.X5900 == 0,] 

# plot of states
map("state") 
# adding ranges of spp
# ggplot(spotted, aes(Lati, Longi)) + geom_point(size = spotted$SpeciesTotal)
points(spotted$Longi, spotted$Lati, col = 2,  pch = 20, cex = spotted$SpeciesTotal/25) #spotted range = RED
points(GT_abun$Longi.x, GT_abun$Lati.x, col = 3, pch = 16, cex = GT_abun$SpeciesTotal/25) #GT range = GREEN
# points(plotdata_gaps$Longi.x, plotdata_gaps$Lati.x, col = 4, pch = 17) #where GT == 0 but predicted presence BLUE 


#### ---- Processing Environmental Data ---- ####
# read in raw env data (from Coyle et al)
all_env = read.csv('All Env Data.csv', header = T)
# merge in ENV
all_expected_pres = merge(all_env, expect_pres, by = "stateroute")
col_keep_1 <- c("stateroute", "Longi", "Lati",  'sum.EVI', 'elev.mean', 'mat', 'ap.mean', "AOU", 'SSTATENUMB', 'SROUTE')
all_expected_pres = all_expected_pres[, (names(all_expected_pres) %in% col_keep_1)]

# for loop subsetting env data to expected occurrence for focal species
#allspecies = unique(all_expected_pres$AOU)
allspecies = all_expected_pres$AOU 
s = 5900
for (s in allspecies){
  temp = all_expected_pres[all_expected_pres$AOU == 5900,] # need to automate these
  focal_abun = bbs_loc[bbs_loc$Aou == 5900,] 
  env_output = merge(temp, focal_abun, by.x = "AOU", by.y = "Aou", all = FALSE)
}         ###### how to store separately?

focal_abun = bbs_loc[bbs_loc$Aou == 5900,] 
competitor = bbs_loc[bbs_loc$Aou == 5880,]

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








