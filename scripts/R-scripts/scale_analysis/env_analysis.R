#Occupancy-scale analysis
# author: Molly F. Jenkins
# date: 06/27/2017
# setwd("C:/git/core-transient")
#'#' Please download and install the following packages:
library(raster)
library(maps)
library(sp)
library(rgdal)
library(maptools)
library(rgeos)
library(dplyr)
library(fields)
library(tidyr)
library(ggplot2)
library(nlme)
library(gridExtra)
library(wesanderson)
library(stats)
library(geometry)
# To run this script, you need temperature, precip, etc data, 
# which are currently stored in the following directories off of github: 

# Data directories
BBS = '//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/'

#Have env data means for all routes 
#Now need to calc var across clustrs - and in ways 

####Pare down routes to exclude routes that are missing above OR below scale####
bbs_envs = read.csv("scripts/R-scripts/scale_analysis/bbs_envs.csv", header = TRUE)
bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
bbs_envs = filter(bbs_envs, stateroute %in% bbs_allscales$focalrte)
write.csv(bbs_envs, "scripts/R-scripts/scale_analysis/bbs_envs.csv", row.names = FALSE) #updated 09/20 to reflect fixed above scale

####Calc z-scores, quantiles pre-variance loop####
bbs_envs = read.csv("scripts/R-scripts/scale_analysis/bbs_envs.csv", header = TRUE)

#alt simplistic standardization using z scores
bbs_envs$ztemp = (bbs_envs$temp.mean - mean(bbs_envs$temp.mean)) / sd(bbs_envs$temp.mean)
bbs_envs$zprec = (bbs_envs$prec.mean - mean(bbs_envs$prec.mean)) / sd(bbs_envs$prec.mean)
bbs_envs$zelev = (bbs_envs$elev.mean - mean(bbs_envs$elev.mean)) / sd(bbs_envs$elev.mean)
bbs_envs$zndvi = ((bbs_envs$ndvi.mean) - mean(na.exclude(bbs_envs$ndvi.mean))) / sd(na.exclude(bbs_envs$ndvi.mean)) 
#NA's for ndvi vals around statertes in the 3000's
#with z scores

# Calc quantiles
bbs_envs$ndvi_q= rank(bbs_envs$ndvi.mean)/nrow(bbs_envs) #needs to be corrected since NA values in ndvi col
bbs_envs$elev_q= rank(bbs_envs$elev.mean)/nrow(bbs_envs) 
bbs_envs$temp_q= rank(bbs_envs$temp.mean)/nrow(bbs_envs) 
bbs_envs$prec_q= rank(bbs_envs$prec.mean)/nrow(bbs_envs)
write.csv(bbs_envs, "scripts/R-scripts/scale_analysis/bbs_envs.csv", row.names = FALSE) #updated 09/21
#with z scores and quantiles both


####Convex polygon comparison of variables####
#not variances yet 
#notes on geometry package and min convex polygon:
#convhulln from geometry package, optimized by qhull -> convex hull 
#http://www.qhull.org/html/qconvex.htm#synopsis
bbs_envs = read.csv("scripts/R-scripts/scale_analysis/bbs_envs.csv", header = TRUE)
#subset to just appropriate dims for convhulln 
sub_envs = bbs_envs %>% select(temp_q, prec_q, elev_q, ndvi_q) %>% filter(ndvi_q != 'NA') #cuts nothing, all there 

# 
# hull = convhulln(sub_envs, "FA")
# hull$area #189.74 #4.502 
# hull$vol #66.22 #0.54 second time around....
# rtes = unique(bbs_envs$stateroute)
# output = c()
# for(s in rtes){
#   sub_envs = bbs_envs %>% filter(stateroute == s)
#   hull = convhulln(sub_envs, "FA")
#   temp = summarize(stateroute = s, 
#                     zhull = hull$vol)  
#   output = rbind(output, temp) 
# } 
#   
 #not possible ! need 20 pts! 
  
#bbs_envs has env data @ scale of single route 
#following code compiles vars of env data @scale of 66 rtes

#ALTERNATIVE: add together z scores as in early code using euclidean distance bet/points or ranking in euclidean space
#- so between scale at 1 and scale at 66 


####Pair env data to secondary rtes associated with each focal rte; calc variance across top scale for each focal rte####
bbs_envs = read.csv("scripts/R-scripts/scale_analysis/bbs_envs.csv", header = TRUE)
dist.df = read.csv("scripts/R-scripts/scale_analysis/dist_df.csv", header = TRUE)

#need to pair down by routes existing in bbs_envs (which have been sorted appropriately) and then calculated the top n 66 based on distance
dist.df2 = filter(dist.df, rte1 %in% bbs_envs$stateroute & rte2 %in% bbs_envs$stateroute) 

#num of rows matches num of rows in dts.df, good 
#now calc var for each focal rte (rte1)
env_hetero = data.frame(stateroute = NULL,
                        top_ndvi_zv = NULL,
                        top_elev_zv = NULL,
                        top_prec_zv = NULL,
                        top_temp_zv = NULL,
                        top_zhull = NULL)

focal_rtes = unique(bbs_envs$stateroute)
#need to structure using focal_clustr template 

#need to add scale component and not just do for top scale (66) alone....???? 
for(r in focal_rtes){
  # for(nu in scales) { #just running for top scale for now 
  #to characterize total var that can be encompassed on a rte
 
    #takes dist.df and generates a new list that changes based on which route in uniqrtes is being focused on 
    #and the length of the list varies with the scale or nu 
    rte_group = dist.df2 %>% 
    filter(rte1 == r) %>% 
    top_n(66, desc(dist)) %>%
    select(rte2) %>% as.vector()
  
  tempenv = bbs_envs %>%
    filter(stateroute %in% rte_group$rte2)
  
  tempenv_z = tempenv %>% #subset of tempenv for convhull calcs 
    select(ztemp, zprec, zelev, zndvi) %>% 
    filter(zndvi != 'NA')
  
  zhull_df = convhulln(tempenv_z, "FA")
  
  #get variance of rte means, calc across entire rte_group according to scale   
  temp = data.frame(stateroute = r,
                    top_ndvi_zv = var(tempenv$zndvi, na.rm = TRUE), #fix missing values!!!!
                    top_elev_zv = var(tempenv$zelev), #bc each of these values is calculated across the 2ndary rtes for each focal rte
                    top_prec_zv = var(tempenv$zprec), #such that all 66 2ndary rtes will be summed into one variance value for each focal rte
                    top_temp_zv = var(tempenv$ztemp), #a vector is going in and a single val is coming out
                    top_zhull = zhull_df$vol)
  
  env_hetero = rbind(env_hetero, temp)
  }
  #may need a second level of output rbinding here
#}
top_envhetero = env_hetero
write.csv(top_envhetero, "scripts/R-scripts/scale_analysis/top_envhetero.csv", row.names = FALSE)
#updated 09/21

####Coef vs env hetero models####
top_envhetero = read.csv("scripts/R-scripts/scale_analysis/top_envhetero.csv", header = TRUE) #landscape habitat vars 
bbs_envs = read.csv("scripts/R-scripts/scale_analysis/bbs_envs.csv", header = TRUE) #single rte habitat vars 
coefs = read.csv("scripts/R-scripts/scale_analysis/coefs.csv", header = TRUE) #AUC etc. 

#Merge top_envhetero to coefs for comparing env variation for a site to its associated AUC 
#at the scale of a landscape and scale of a rte
env_coefs = coefs %>% 
  inner_join(top_envhetero, by = "stateroute") %>% #coefs to top scale env characterizing data
  inner_join(bbs_envs, by = "stateroute") %>% #and also join single rte
  select(-contains("temp"), -contains("prec")) #and also rm precip and temp vars since now wrapped up in convhull 





#mod env coef names to reflect that they have to do with max scale #1003 rows, 54 cols 

#join original coef vars at scale of single focal rte and also make sure reflected in names 
auc_mod1 = lm(OA.AUC ~ elev_v, data = env_auc)
summary(auc_mod1)
#test example model

write.csv(env_coefs, "scripts/R-scripts/scale_analysis/env_coefs.csv", row.names = FALSE)
#updated 09/21


####Coef & habitat heterogeneity models####
env_coefs = read.csv("scripts/R-scripts/scale_analysis/env_coefs.csv", header = TRUE)
#ADAPT BELOW CODE to reflect NEW COEFFICIENTS INCLUDING AUC and RE-RUN MODELS

#check out cov matrix to inform model generation and predictions:
covmatrix = round(cor(env_coefs[, 2:ncol(env_coefs)]), 2)
covmatrix = as.data.frame(covmatrix)

write.csv(covmatrix, "scripts/R-scripts/scale_analysis/covmatrix.csv", row.names = FALSE)


# nested loop for examining variation in coefs/fitted curves explained by env heterogeneity 
#so: response = coefficients = dependent; predictor = environmental heterogeneity = independent
rsqrd_hetero = data.frame(dep = character(), ind = character(), r2 = numeric())

for (d in 2:25) { #adjust columns appropriately -> make sure correct order of ind and dep vars!
  for (i in 26:ncol(env_coefs)) {
    tempmod = lm(env_coefs[,d] ~ env_coefs[,i])
    tempdf = data.frame(dep = names(env_coefs)[d], 
                        ind = names(env_coefs)[i], 
                        r2 = summary(tempmod)$r.squared)
    rsqrd_hetero = rbind(rsqrd_hetero, tempdf)
  }
}
write.csv(rsqrd_hetero, "scripts/R-scripts/scale_analysis/rsqrd_hetero.csv", row.names = FALSE) #updated 06/20 using best stateroutes


####Visually Characterizing measures of habitat heterogeneity####
rsqrd_hetero = read.csv("scripts/R-scripts/scale_analysis/rsqrd_hetero.csv", header = TRUE)
env_coefs = read.csv("scripts/R-scripts/scale_analysis/env_coefs.csv", header = TRUE)

ggplot(data = rsqrd_hetero, aes(x = ind, y = r2, fill = ind))+geom_boxplot()+theme_classic()+
  theme(legend.position="none")+
  labs(x = "Environmental variables", y = "Variation Explained (R^2)")

#excluding transient data for incompleteness, selecting only relevant measures of heterogeneity 
#INFLEXION POINTS: 
rsub_i = rsqrd_hetero %>%
  filter((dep == "OA.i" | dep == "ON.i" | dep == "CA.i" | dep == "CN.i") & 
           (ind == "elev_qv" | ind == "ndvi_qv" | ind == "qhull_vol" | ind == "zhull_vol"))
rsub_i = droplevels(rsub_i) #removing ghost levels to ensure correct plotting/analyses

ggplot(data = rsub_i, aes(x = ind, y = r2)) + geom_boxplot()+theme_classic() 
#variance in elevation (quantiles) and convex hull polygon volume (all 4 env vars, z scores) 
#both explain more variance in the INFLEXION POINTS (i) of the occ-scale relationship than ndvi or qhull volume 

#ASYMPTOTES (A)
rsub_A = rsqrd_hetero %>%
  filter((dep == "OA.A" | dep == "ON.A" | dep == "CA.A" | dep == "CN.A") & 
           (ind == "elev_qv" | ind == "ndvi_qv" | ind == "qhull_vol" | ind == "zhull_vol"))
rsub_A = droplevels(rsub_A) #removing ghost levels to ensure correct plotting/analyses

ggplot(data = rsub_A, aes(x = ind, y = r2)) + geom_boxplot()+theme_classic() 
#variance in elevation (quantiles) and convex hull polygon volume (all 4 env vars, z scores) 
#once again, elevation performs well, altho in this case ndvi explains more variation more consistently than convex hull volume. 


#separate analysis for just transients since relationship not immediately apparent
rsub_t = rsqrd_hetero %>%
  filter((dep == "TAexp" | dep == "TApow" | dep == "TNexp" | dep == "TNpow") & 
  (ind == "elev_qv" | ind == "ndvi_qv" | ind == "qhull_vol" | ind == "zhull_vol"))
rsub_t = droplevels(rsub_t) #removing ghost levels to ensure correct plotting/analyses

ggplot(data = rsub_t, aes(x = ind, y = r2)) + geom_boxplot()+theme_classic() #elev explains more variation in the transients




# the above are all based on the models themselves...what about the numbers? 
p1A = ggplot(data = env_coefs, aes(elev_qv, OA.A))+geom_point()
p2A = ggplot(data = env_coefs, aes(ndvi_qv, OA.A))+geom_point()
p3A = ggplot(data = env_coefs, aes(qhull_vol, OA.A))+geom_point()
p4A = ggplot(data = env_coefs, aes(zhull_vol, OA.A))+geom_point()

p5_1 = gridExtra::grid.arrange(p1A, p2A, p3A, p4A)

max(env_coefs$OA.A, na.rm = T) #why.....is there a 7 in my OA.A values....? still a 7

p1B = ggplot(data = env_coefs, aes(elev_qv, OA.i))+geom_point()
p2B = ggplot(data = env_coefs, aes(ndvi_qv, OA.i))+geom_point()
p3B = ggplot(data = env_coefs, aes(qhull_vol, OA.i))+geom_point()
p4B = ggplot(data = env_coefs, aes(zhull_vol, OA.i))+geom_point()

p5_2 = gridExtra::grid.arrange(p1B, p2B, p3B, p4B)
max(env_coefs$OA.i, na.rm = T) #16??? really shouldn't be possible. is this bc dealing with log area? -> after data fixes, now 8 

p_final = grid.arrange(p5_1, p5_2)

#what we expected: 
#Homogenous communities (i.e. low ndvi, low elev values)
  #i as a coefficient should be lower, accumulation of core species more linear early on in scale relationship 
  #k is difficult to predict but is the slope AT the inflexion point i 
  #A should be higher, relationship should asymptote out earlier and a greater stretch should be in an asymptotic form 

#Heterogenous communities (i.e. high ndvi, high elev values)
  #i as a coef should be higher (relative to what?) due to a longer period of Transient dominance in the lower scales pulling down the average, 
    #meaning i is further along in the x axis relative to the y 
  #k is again, difficult to predict 
  #A should be lower and shorter, as asymptotic form barely reached 

#what does this comparison look like? How do I split sites up via a threshold for hetero vs homogenous 
#to compare their avg coefs? 

#a distribution of hull_vol (panel 1), elev (panel 2), and ndvi (panel 3)? 

ggplot(data = env_coefs, aes(x = qhull_vol))+geom_histogram(binwidth = 0.005)







####Variance Partitioning of Env Predictors####
#would I be basing my total remaining unexplained variation off of the meanOcc~logA relationship? (OA.i?)
#so the 12% remaining
#focusing just on OA.i and main env vars
#how do variance partitioning with more than 4 parts? 

globalmod<-lm(OA.i~elev+meanP+temp+ndvi, data=env_coefs)
mod1<-lm(OA.i~elev, data=env_coefs)
mod2<-lm(OA.i~meanP, data=env_coefs)
mod3<-lm(OA.i~ndvi, data=env_coefs)
mod4<-lm(OA.i~temp, data=env_coefs)
#and then Euclid_mod2
summary(globalmod)$r.squared
summary(mod1)$r.squared
summary(mod2)$r.squared
summary(mod3)$r.squared
summary(mod4)$r.squared 


#running with mods 2+3 bc best ranked and most interesting 
a= summary(globalmod)$r.squared - summary(mod2)$r.squared
a
c= summary(globalmod)$r.squared - summary(mod3)$r.squared
c
b= summary(mod2)$r.squared - c
b
d= 1- summary(globalmod)$r.squared
d
#isn't it ok that d = ~87.5% tho, given that the r^2 for occ~logA was 88%? 

########


devtools::load_all(path = 'C:/git/core-transient')

ndvi_data_raw <- get_bbs_gimms_ndvi()

ndvi_data_summer <- ndvi_data_raw %>%
  filter(!is.na(ndvi), month %in% c('may', 'jun', 'jul'), year > 1981) %>%
  group_by(site_id, year) %>%
  summarise(ndvi_sum = mean(ndvi)) %>%
  ungroup()
