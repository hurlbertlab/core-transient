# Biotic Interactions script
# In this script we are comparing the differences in occupancy and abundance between
# green-tailed towhees and spotted towhees using occupancy, abundance, and environmental data.
# Env data from Snell_code.R from BIOL 465 project. Occupancy data from BBS, Coyle, and Hurlbert.

#### ---- Inital Formatting ---- ####
library(plyr)
# reading in dataset 
Hurlbert_o = read.csv('scripts/R-scripts/data_cleaning_scripts/Biotic Interactions/Master_RO_Correlates_20110610.csv', header = T)
# subsetting species whose occupancies were between 0.3 and 0.6 over a 10 eyar period
subsetocc = Hurlbert_o[Hurlbert_o$X10yr.Prop > .3 & Hurlbert_o$X10yr.Prop < .7,]
# compare green-tailed towhee to spotted towhee
towhees = subsetocc[subsetocc$CommonName == "Spotted Towhee"| subsetocc$CommonName == "Green-tailed Towhee",]

# read in BBS data
bbs = read.csv('data/raw_datasets/dataset_1.csv', header = T)
# subsetting spotted towhees
spotted = bbs[bbs$Aou == 5880,] 
# aggregate based on year to get just spotted towhee abundance
spot_agg = aggregate(spotted, by = list(spotted$stateroute), FUN = mean) 
# read in Coyle occupancy data
coyle_o = read.csv('scripts/R-scripts/data_cleaning_scripts/Biotic Interactions/site_sp_occupancy_matrix_Coyle.csv', header = T)
# rename column one to stateroute
colnames(coyle_o)[1] = c("stateroute")
# subset GT towhee within occupancy data
gt_occ = data.frame(coyle_o$stateroute, coyle_o$X5900)
# subset spotted towhee within occupancy data
spot_occ = data.frame(coyle_o$stateroute, coyle_o$X5880)
# merge occupancy with bbs for spotted towhee
t1 = merge(spot_agg, gt_occ, by.x = "stateroute", by.y = "coyle_o.stateroute")
# insert GT occupancy = 0 instead of NA
t1$coyle_o.X5900[is.na(t1$coyle_o.X5900)] <- 0
#remove duplicate column
drops <- c("Group.1", "Year", "Aou")
t1 = t1[, !(names(t1) %in% drops)]
# merge occupancy with bbs for spotted towhee
t2 = merge(spot_occ, gt_occ, by="coyle_o.stateroute")
# read in expected presence data
ep = read.csv('scripts/R-scripts/data_cleaning_scripts/Biotic Interactions/expected_presence_on_BBS_routes.csv', header = T)
# subset GT towhee within occupancy data
gt_ep = ep[ep$AOU == 5900,] 
# merge expected occupancy w real occupancy SPOT TOTAL 
# going to run analyses with this only for now
obs_exp_total = merge(gt_ep, t1, by = "stateroute")
# drop extra columns
drops <- c("SSTATENUMB","SROUTE", "AOU")
obs_exp_total = obs_exp_total[, !(names(obs_exp_total) %in% drops)]
# merge expected occupancy w real occupancy SPOT OCC
obs_exp_occ = merge(gt_ep, t2, by.x = "stateroute", by.y = "coyle_o.stateroute")
# view where coyle_occupancy = 0 but predicted presence
GT_gaps = obs_exp_total[obs_exp_total$coyle_o.X5900 == 0,] 

#### ---- Plotting ---- ####
# merge in lat/long
latlongs = read.csv('scripts/R-scripts/data_cleaning_scripts/Biotic Interactions/routes 1996-2010 consecutive.csv', header = T)
plotdata_all = merge(obs_exp_total, latlongs, by = "stateroute") #where expected didnt equal observed
plotdata_gaps = merge(GT_gaps, latlongs, by = "stateroute") #where expected didnt equal observed GT only
# spotted range in point format
spotty = merge(bbs, latlongs, by = "stateroute")
spotty = spotty[spotty$Aou == 5880,]

library(maps)
# plot of states
map("state") 
# adding ranges of spp
points(spotty$Longi, spotty$Lati, col = 2, pch = 18) #spotted range = RED
points(plotdata_all$Longi, plotdata_all$Lati, col = 3, pch = 16) #GT range = GREEN
points(plotdata_gaps$Longi, plotdata_gaps$Lati, col = 4, pch = 17) #where GT == 0 BLUE

# NOTES
#BBS abundances of just spotted towhee
#merge occupancy data of species by stateroute
#occupancy on GT, abundance on spotteed
#LT averge for GT towhee for 10 year

#### ---- Processing Environmental Data ---- ####
# read in env data from biol 465 final project, Snell Project Final.R script
env = read.csv('scripts/R-scripts/data_cleaning_scripts/Biotic Interactions/occuenv.csv', header = T)
# subset to GT species  
env_gt = env[env$Species == 5900,] 
# pulling out environmental z-scores by state route 
col_keeps <- c("stateroute", "Lati", "Longi", "zTemp","zPrecip", "zElev", "zEVI", "euc.dist.spp")
env_zscore = env_gt[, (names(env_gt) %in% col_keeps)]
# merge env data w obs_exp_total
env_occu_matrix = merge(env_zscore, obs_exp_total, by = "stateroute")

#### ---- Modelling ---- ####
# Interaction between GT occupancy and ST abundance where GT exists
f1 = lm(coyle_o.X5900 ~  SpeciesTotal, data = env_occu_matrix)
summary(f1)

# summaries using zscore of env variables
f2 = lm(coyle_o.X5900 ~  SpeciesTotal+abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI)+abs(euc.dist.spp),
        data = env_occu_matrix)
summary(f2)  

# Polynomial Fit for spp
q2 <- lm(coyle_o.X5900 ~ SpeciesTotal + I(SpeciesTotal^2), data = env_occu_matrix)
summary(q2) 
q3 <- lm(coyle_o.X5900 ~ SpeciesTotal + I(SpeciesTotal^2) + I(SpeciesTotal^3), data = env_occu_matrix)
summary(q2) 

# Polynomial Fit for Precip
q2_env <- lm(coyle_o.X5900 ~ SpeciesTotal * zPrecip + I(zPrecip^2), data = env_occu_matrix)
summary(q2_env)

q3_env <- lm(coyle_o.X5900 ~ SpeciesTotal * zPrecip + I(zPrecip^2) * zTemp + I(zTemp^2) *
               zElev + I(zElev^2) * zEVI + I(zEVI^2) * euc.dist.spp + I(euc.dist.spp^2), data = env_occu_matrix)
summary(q3_env)
  
q4_env <- lm(coyle_o.X5900 ~ SpeciesTotal * zPrecip + I(zPrecip^2) * zTemp + I(zTemp^2) *
              zEVI + I(zEVI^2), data = env_occu_matrix) #paring down model decreases significance
summary(q4_env)

# GLM fit with logit link 
library(MASS)
glm1 = glm(coyle_o.X5900 ~  SpeciesTotal+abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI)+abs(euc.dist.spp),
    family = quasibinomial, data = env_occu_matrix)

#NOTES
#flycathcers, green tailed towhee
#most likely asymmetric interactions - one sp will be dominant
#clue could be occupancy values themselves - green tailed tohee (spotted tohee)
#look at sparrows too - same sized birds
#MAcArthurs warblers paper- fine-scale partitioning
#get a bird phylogeny -  family specific
#functional diversity - Jes lichen paper 
#add continuous variables later
#chose one closely related species
# there are x sp with an occupancy in this range, for each sp examined a funcitonally
# and phylogenetically closest species and results suggest this is excting
#have one key value
#pick one species or sum across species - competitor is sum of all other of same species
#agreggate by bbs route
#start with one-by-one towhee
#every row is a site, index is abundance of potential competitors
