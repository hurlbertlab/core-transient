# Biotic Interactions script
# In this script we are comparing the differences in occupancy and abundance between
# green-tailed towhees and spotted towhees using occupancy, abundance, and environmental data.
# Env data formatted in Snell_code.R from BIOL 465 project. Occupancy data from BBS, Coyle, and Hurlbert.

#### ---- Inital Formatting ---- ####
library(plyr)
# reading in dataset 
Hurlbert_o = read.csv('scripts/R-scripts/data_cleaning_scripts/Biotic Interactions/Master_RO_Correlates_20110610.csv', header = T)
# subsetting species whose occupancies were between 0.3 and 0.6 over a 10 year period
subsetocc = Hurlbert_o[Hurlbert_o$X10yr.Prop > .3 & Hurlbert_o$X10yr.Prop < .7,]
# compare green-tailed towhee to spotted towhee
towhees = subsetocc[subsetocc$CommonName == "Spotted Towhee"| subsetocc$CommonName == "Green-tailed Towhee",]

# read in BBS data
bbs = read.csv('data/raw_datasets/dataset_1.csv', header = T)
# subsetting spotted towhees based on AOU code
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
#remove duplicate columns
drops <- c("Group.1", "Year", "Aou")
t1 = t1[, !(names(t1) %in% drops)]
# merge occupancy with bbs for spotted towhee
t2 = merge(spot_occ, gt_occ, by="coyle_o.stateroute")
# read in expected presence data based on BBS
ep = read.csv('scripts/R-scripts/data_cleaning_scripts/Biotic Interactions/expected_presence_on_BBS_routes.csv', header = T)
# subset GT towhee within occupancy data
gt_ep = ep[ep$AOU == 5900,] 
# merge expected occupancy w real occupancy SPOT TOTAL 
obs_exp_total = merge(gt_ep, t1, by = "stateroute")
# drop extra columns
drops <- c("SSTATENUMB","SROUTE", "AOU")
obs_exp_total = obs_exp_total[, !(names(obs_exp_total) %in% drops)]
# merge expected occupancy w real occupancy SPOT OCC
# obs_exp_occ = merge(gt_ep, t2, by.x = "stateroute", by.y = "coyle_o.stateroute")
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
points(plotdata_gaps$Longi, plotdata_gaps$Lati, col = 4, pch = 17) #where GT == 0 but predicted presence BLUE 


#### ---- Processing Environmental Data ---- ####
# read in env data from biol 465 final project, Snell Project Final.R script
env = read.csv('scripts/R-scripts/data_cleaning_scripts/Biotic Interactions/occuenv.csv', header = T)
# subset to GT species  
env_gt = env[env$Species == 5900|env$Species == 5880,] 
# pulling out environmental z-scores by state route 
col_keeps <- c("stateroute", "Species", "Lati", "Longi", "zTemp","zPrecip", "zElev", "zEVI")
env_zscore = env_gt[, (names(env_gt) %in% col_keeps)]
# merge env data w obs_exp_total
env_occu_matrix = merge(env_zscore, obs_exp_total, by = "stateroute")
#calculate euclidean distance with z scores
env_occu_matrix$eucdist = sqrt((env_occu_matrix$zTemp)^2 + (env_occu_matrix$zPrecip)^2 + (env_occu_matrix$zElev)^2 + (env_occu_matrix$zEVI)^2)
#renaming columns
names(env_occu_matrix) = c("stateroute", "AOU", "Longi", "Lati", "zTemp", "zPrecip", "zElev", "zEVI", "Spotted_abun", "GT_occ", "eucdist")

write.csv(env_occu_matrix, "scripts/R-scripts/data_cleaning_scripts/Biotic Interactions/env_occu.csv")
#### ---- Variance partitioning ---- ####
# Interaction between GT occupancy and ST abundance where GT exists
competition <- lm(GT_occ ~  Spotted_abun, data = env_occu_matrix)
# Env effects summed
# env_eff = lm(GT_occ ~ eucdist, data = env_occu_matrix)
# Env effects summed and interaction
# both = lm(GT_occ ~  Spotted_abun + eucdist, data = env_occu_matrix)
 
# z scores separated out for env effects
env_z = lm(GT_occ ~ zTemp + I(zTemp^2) + zPrecip + I(zPrecip^2), zElev + 
             (zElev^2) + zEVI + I(zEVI^2), data = env_occu_matrix)

env_z = lm(GT_occ ~ abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI), data = env_occu_matrix)
# z scores separated out for env effects
both_z = lm(GT_occ ~  Spotted_abun + abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI), data = env_occu_matrix)

#####################abiotic variables explain twice as much variation as biotic
variance_partitioning = function(x) {
ENV = summary(both_z)$r.squared - summary(competition)$r.squared
return(ENV)
COMP = summary(both_z)$r.squared - summary(env_z)$r.squared
return(COMP)
SHARED = summary(competition)$r.squared - C
return(SHARED)
NONE = 1 - summary(both_z)$r.squared
return(NONE)
}

#### ---- Plotting LMs ---- ####
library(ggplot2)
ggplot(env_occu_matrix, aes(x = GT_occ, y = Spotted_abun)) + 
  geom_point(pch = 16) +
  stat_smooth(method = "lm", col = "red") + theme_classic()

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") + theme_classic() + 
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

ggplotRegression(lm(GT_occ ~ Spotted_abun, data = env_occu_matrix))
# source = https://susanejohnston.wordpress.com/2012/08/09/a-quick-and-easy-function-to-plot-lm-results-in-r/

#### ---- GLM fit with logit link ---- ####
# merge Hurlbert_o w env to get diet guilds
dietguild = merge(env_occu_matrix, Hurlbert_o, by = "AOU")
# add on success and failure columns by creating # of sites where birds were found
# and # of sites birds were not found from original bbs data
# subset to get just GT towhees in raw bbs data
gt = bbs[bbs$Aou == 5900,] 
# add column of ones to sum up # of sites for each row
gt1 = cbind(gt, 1)
#renaming columns to make more clear
colnames(gt1) <- c("stateroute", "year","Aou","speciestotal", "numsites")
# aggregate to sum across years by site
gt_binom = aggregate(gt1$numsites, by = list(gt$stateroute), FUN = sum) 
#renaming columns to make more clear
colnames(gt_binom) <- c("stateroute", "numsites")


# merge success/failure columns w environmnetal data
env_occu_matrix_1 = merge(env_occu_matrix, gt_binom, by = "stateroute", )
# using equation species sum*GT occ to get success and failure for binomial anlaysis
env_occu_matrix_1$sp_success = env_occu_matrix_1$numsites * env_occu_matrix_1$GT_occ
env_occu_matrix_1$sp_fail = env_occu_matrix_1$numsites * (1 - env_occu_matrix_1$GT_occ)


library(lme4)
glm1 = glm(sp_success ~ sp_fail + Spotted_abun + eucdist, family = binomial, data = env_occu_matrix_1)
summary(glm1)

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
#BBS abundances of just spotted towhee
#merge occupancy data of species by stateroute
#occupancy on GT, abundance on spotteed
#LT averge for GT towhee for 10 year



##### modelling working out #####
# summaries using zscore of env variables
#f2 = lm(coyle_o.X5900 ~  SpeciesTotal+abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI)+abs(euc.dist.spp),
#        data = env_occu_matrix)
#summary(f2)  

# Polynomial Fit for spp
#q2 <- lm(coyle_o.X5900 ~ SpeciesTotal + I(SpeciesTotal^2), data = env_occu_matrix)
#summary(q2) 
#q3 <- lm(coyle_o.X5900 ~ SpeciesTotal + I(SpeciesTotal^2) + I(SpeciesTotal^3), data = env_occu_matrix)
#summary(q2) 

#q2_env <- lm(coyle_o.X5900 ~ SpeciesTotal * zPrecip + I(zPrecip^2), data = env_occu_matrix)
#summary(q2_env)

#q3_env <- lm(coyle_o.X5900 ~ SpeciesTotal * zPrecip + I(zPrecip^2) * zTemp + I(zTemp^2) *
#               zElev + I(zElev^2) * zEVI + I(zEVI^2), data = env_occu_matrix)
#summary(q3_env)

# q4_env <- lm(coyle_o.X5900 ~ SpeciesTotal * zPrecip + I(zPrecip^2) * zTemp + I(zTemp^2) *
#              zEVI + I(zEVI^2), data = env_occu_matrix) #paring down model decreases significance
# summary(q4_env)

