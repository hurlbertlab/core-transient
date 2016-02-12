#Biotic Interactions script
library(plyr)
#reading in dataset 
Hurlbert_o = read.csv('scripts/R-scripts/data_cleaning_scripts/Biotic Interactions/Master_RO_Correlates_20110610.csv', header = T)
#subsetting species whose occupancies were between 0.3 and 0.6 over a 10 eyar period
subsetocc = occupancy[occupancy$X10yr.Prop > .3 & occupancy$X10yr.Prop < .7,]
#compare green-tailed towhee to spotted towhee
towhees = subsetocc[subsetocc$CommonName == "Spotted Towhee"| subsetocc$CommonName == "Green-tailed Towhee",]

#read in BBS data
bbs = read.csv('data/raw_datasets/dataset_1.csv', header = T)
# subsetting spotted towhees
spotted = bbs[bbs$Aou == 5880,] 

#read in Coyle occupancy data
coyle_o = read.csv('scripts/R-scripts/data_cleaning_scripts/Biotic Interactions/site_sp_occupancy_matrix_Coyle.csv', header = T)
#rename column one to stateroute
colnames(coyle_o)[1] = c("stateroute")
#subset GT towhee within occupancy data
gt = data.frame(coyle_o$stateroute, coyle_o$X5900)
#merge occupancy with bbs for spotted towhee
t1 = merge(spotted, gt, by.x = "stateroute", by.y = "coyle_o.stateroute")
#insert GT occupancy = 0 instead of NA
t1$coyle_o.X5900[is.na(t1$coyle_o.X5900)] <- 0

#read in expected presence data
ep = read.csv('scripts/R-scripts/data_cleaning_scripts/Biotic Interactions/expected_presence_on_BBS_routes.csv', header = T)
#subset GT towhee within occupancy data
gt_ep = ep[ep$AOU == 5900,] 
#merge expected occupancy w real occupancy
obs_exp = merge(gt_ep, t1, by = "stateroute")
#dropping duplicate columns
drops <- c("SSTATENUMB","SROUTE", "AOU", "Year", "Aou")
obs_exp = obs_exp[, !(names(obs_exp) %in% drops)]
#renaming columns for clarity
names(obs_exp) = c("stateroute", "spottedtotal", "GToccupancy")
#aggregate based on year to get an average for each route
obs_exp_2 = aggregate(obs_exp, by = list(obs_exp$stateroute), FUN = mean)
#double checked with subset: mean(obs_exp$coyle_o.X5900[obs_exp$stateroute == 14168])
#remove duplicate column
drops <- c("Group.1")
obs_exp_2 = obs_exp_2[, !(names(obs_exp_2) %in% drops)]
#view where coyle_occupancy = 0 but predicted presence
GT_gaps = obs_exp_2[obs_exp_2$GToccupancy == 0,] 

#merge in lat/long
latlongs = read.csv('scripts/R-scripts/data_cleaning_scripts/Biotic Interactions/routes 1996-2010 consecutive.csv', header = T)
plotdata_all = merge(obs_exp_2, latlongs, by = "stateroute") #where expected didnt equal observed
plotdata = merge(GT_gaps, latlongs, by = "stateroute") #where expected didnt equal observed GT only
#spotted range in point format
spotty = merge(bbs, latlongs, by = "stateroute")
spotty = spotty[spotty$Aou == 5880,]

#attempting to plot
library(maps)
map("state")
points(spotty$Longi, spotty$Lati, col = 2, pch = 18)
points(plotdata_all$Longi, plotdata_all$Lati, col = 3, pch = 16)
points(plotdata$Longi, plotdata$Lati, col = 4, pch = 17)
#BBS abundances of just spotted towhee
#merge occupancy data of species by stateroute
#occupancy on GT, abundance on spotteed
#LT averge for GT towhee for 10 year

#read in env data 
#from biol 465 final project, Snell Project Final.R script
env = read.csv('scripts/R-scripts/data_cleaning_scripts/Biotic Interactions/BIOL465_final.csv', header = T)
#merge env data w obs_exp
env_occu_matrix = merge(env, bbs, by.x = "Species", by.y = "Aou")
#aggregate based on year to get an average for each route
#t = aggregate(env_occu_matrix, by = list(env_occu_matrix$stateroute), FUN = mean)
#merge env matrix w occupancy for towhee
finalmodelmatrix = merge(obs_exp_2, env_occu_matrix, by = "stateroute", all = FALSE)
#starting lm analysis
f1 = lm(GToccupancy ~  stateroute, data = finalmodelmatrix)
summary(f1)

f2 = lm(GToccupancy ~  stateroute*spottedtotal, data = finalmodelmatrix)
summary(f2)

f3 = lm(GToccupancy ~  stateroute*spottedtotal, data = finalmodelmatrix)
  
  
  
#NOTES
#flycathcers, green tailed tohee
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
