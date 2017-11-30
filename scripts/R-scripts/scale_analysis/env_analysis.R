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

####Calc z-scores, quantiles pre-variance loop####
bbs_envs = read.csv("scripts/R-scripts/scale_analysis/bbs_envs.csv", header = TRUE)

#alt simplistic standardization using z scores 
#means (need for calc top scale variance)
bbs_envs$temp_zm = (bbs_envs$temp.mean - mean(bbs_envs$temp.mean)) / sd(bbs_envs$temp.mean)
bbs_envs$prec_zm = (bbs_envs$prec.mean - mean(bbs_envs$prec.mean)) / sd(bbs_envs$prec.mean)
bbs_envs$elev_zm = (bbs_envs$elev.mean - mean(bbs_envs$elev.mean)) / sd(bbs_envs$elev.mean)
bbs_envs$ndvi_zm = ((bbs_envs$ndvi.mean) - mean(na.exclude(bbs_envs$ndvi.mean))) / sd(na.exclude(bbs_envs$ndvi.mean)) 
#NA's for ndvi vals around statertes in the 3000's
#with z scores for convhull only, don't z scores of variances 

write.csv(bbs_envs, "scripts/R-scripts/scale_analysis/bbs_envs.csv", row.names = FALSE) #updated 09/21 to reflect fixed above scale
#conversion done for single-rte scale envs 
#use pre-standardized means to calc above-rte variance and THEN convert to z scores for comparison to lower scales 

####Convex polygon comparison of variables####
#not variances yet 
#notes on geometry package and min convex polygon:
#convhulln from geometry package, optimized by qhull -> convex hull 
#http://www.qhull.org/html/qconvex.htm#synopsis
bbs_envs = read.csv("scripts/R-scripts/scale_analysis/bbs_envs.csv", header = TRUE)
#subset to just appropriate dims for convhulln 
#sub_envs = bbs_envs %>% select(temp_zm, prec_zm, elev_zm, ndvi_zm) %>% filter(ndvi_zm != 'NA') #cuts nothing, all there 

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
bbs_envs = read.csv("scripts/R-scripts/scale_analysis/bbs_envs.csv", header = TRUE) #10/06
dist.df = read.csv("scripts/R-scripts/scale_analysis/dist_df.csv", header = TRUE)

#need to pair down by routes existing in bbs_envs (which have been sorted appropriately) and then calculated the top n 66 based on distance
dist.df2 = filter(dist.df, rte1 %in% bbs_envs$stateroute & rte2 %in% bbs_envs$stateroute) 

#num of rows matches num of rows in dts.df, good 
#now calc var for each focal rte (rte1)
top_envhetero = data.frame(stateroute = NULL,
                           scale = NULL, 
                           reg_ndvi_v = NULL,
                           reg_elev_v = NULL,
                           reg_ndvi_m = NULL,
                           reg_elev_m = NULL)

focal_rtes = unique(bbs_envs$stateroute)
#need to structure using focal_clustr template 
scales = c(2:66)
output = data.frame(stateroute = NULL,
                    scale = NULL, 
                    reg_ndvi_v = NULL,
                    reg_elev_v = NULL,
                    reg_ndvi_m = NULL,
                    reg_elev_m = NULL)

#add nu scale component for calculating means at each scale interval 
for(r in focal_rtes){
  for(nu in scales) { #just running for top scale for now 
  #to characterize total var that can be encompassed on a rte
 
    #takes dist.df and generates a new list that changes based on which route in uniqrtes is being focused on 
    #and the length of the list varies with the scale or nu 
    rte_group = dist.df2 %>% 
    filter(rte1 == r) %>% 
    top_n(nu, desc(dist)) %>%
    dplyr::select(rte2) %>% as.vector()
  
  tempenv = bbs_envs %>%
    filter(stateroute %in% rte_group$rte2)
  
  #can I incorporate an if else conditional here -> 
  #but would only have entry for one row, no, best do it outside
  
  #only want to do this at top scale of 66 rtes, maybe just do sep 
  # tempenv_z = tempenv %>% #subset of tempenv for convhull calcs 
  #   select(temp_zm, prec_zm, elev_zm, ndvi_zm) %>% #using means for convhull calc
  #   filter(ndvi_zm != 'NA') #this is ok since for convhull 
  #   zhull_df = convhulln(tempenv_z, "FA")
  
  #once hull calcs have been done, don't need to keep temp or precip in final df
  
  #get variance of rte means, calc across entire rte_group according to scale   
  temp = data.frame(stateroute = r,
                    scale = nu,
                    reg_ndvi_v = var(tempenv$ndvi.mean, na.rm = TRUE), #fix missing values!!!!
                    reg_elev_v = var(tempenv$elev.mean), #bc each of these values is calculated across the 2ndary rtes for each focal rte
                    reg_ndvi_m = mean(tempenv$ndvi.mean), 
                    reg_elev_m = mean(tempenv$elev.mean)) #a vector is going in and a single val is coming out
                    #top_zhull = zhull_df$vol)
  
  output = rbind(output, temp)
  }
  reg_envhetero = rbind(top_envhetero, output) #may need a second level of output rbinding here
}
#removed top scale z score calcs bc not necessary outside of polygon hull calcs 

write.csv(reg_envhetero, "scripts/R-scripts/scale_analysis/reg_envhetero.csv", row.names = FALSE)
#updated 10/06

#naming convention for comparisons: #env.var_zv -> z score origin, variance/habheterogeneity at rt, from 40km buffer circle raster clip.  
                 #topenv.var_zv -> z score origin, variance/habhet at landscape (var across rt buffermeans in top clustr (zm vector))
                #zm from 40 km buffer circle raster clip too

####Coef vs env hetero models####
reg_envhetero = read.csv("scripts/R-scripts/scale_analysis/reg_envhetero.csv", header = TRUE) #landscape habitat vars 2:66 scale
bbs_envs = read.csv("scripts/R-scripts/scale_analysis/bbs_envs.csv", header = TRUE) #single rte habitat vars 1 scale

#Merge top_envhetero to coefs for comparing env variation for a site to its associated AUC 
#at the scale of a landscape and scale of a rte

#also prep datasets for merge 
bbs_envs = bbs_envs[, 1:5] %>% 
  mutate(scale = 1)

reg_envhetero = reg_envhetero %>% 
  rename(elev.mean = reg_elev_m, 
         elev.var = reg_elev_v, 
         ndvi.mean = reg_ndvi_m, 
         ndvi.var = reg_ndvi_v) 

reg_envhetero$scale = as.numeric(reg_envhetero$scale)

env_all = reg_envhetero %>% 
  full_join(bbs_envs) %>% #fixed, but scales out of order with row position, but everything still where it should be 
  arrange(stateroute, scale)
  
write.csv(env_all, "scripts/R-scripts/scale_analysis/env_all.csv", row.names = FALSE)  
  
####abitat hetero measures and scale independent of coefs (for predictions)####   
coefs = read.csv("scripts/R-scripts/scale_analysis/coefs.csv", header = TRUE) #AUC etc. 
env_all = read.csv("scripts/R-scripts/scale_analysis/env_all.csv", header = TRUE) #AUC etc. 
#calc areas of scales and run against logA consistently ? 

env_all$area = env_all$scale #*40 etc etc etc



#first visualize how habitat hetero changes with scale and run model on this to corroborate occ-scale patterns 
p1 = ggplot(env_all, aes(x = scale, y = ndvi.var))+geom_line(aes(group = stateroute), color = "grey")
p2 = ggplot(env_all, aes(x = scale, y= elev.var))+geom_line(aes(group = stateroute), color = "grey")
p3 = ggplot(env_all, aes(x = scale, y = ndvi.mean))+geom_line(aes(group = stateroute), color = "grey")
p4 = ggplot(env_all, aes(x = scale, y = elev.mean))+geom_line(aes(group = stateroute), color = "grey")

mod1 = lm(ndvi.var ~ scale, data = env_all)
mod2 = lm(elev.var ~ scale, data = env_all)
mod3 = lm(ndvi.mean ~ scale, data = env_all)
mod4 = lm(elev.mean ~ scale, data = env_all)

summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)

env_all$nv_preds = predict(mod1)
env_all$nm_preds = predict(mod3)
env_all$ev_preds = predict(mod2)
env_all$em_preds = predict(mod4)

p5 = p1+geom_line(data = env_all, aes(y = nv_preds), color = "red")+ 
  theme_classic()+
  annotate("text", x = 40, y = 0.10, colour = "red", label = "italic(R) ^ 2 == 0.06378", parse = TRUE)
p6 = p2+geom_line(data = env_all, aes(y = ev_preds), color = "red")+
  theme_classic()+
  annotate("text", x = 35, y = 756000, colour = "red", label = "italic(R) ^ 2 == 0.04223", parse = TRUE)
p7 = p3+geom_line(data = env_all, aes(y= nm_preds), color = "red")+
  theme_classic()+
  annotate("text", x = 40, y = 0.2, colour = "red", label = "italic(R) ^ 2 == 0.0001299", parse = TRUE)
p8 = p4+ geom_line(data = env_all, aes(y = em_preds), color = "red")+
  theme_classic()+
  annotate("text", x = 40, y = 3000, colour = "red", label = "italic(R) ^ 2 == 0.0004142", parse = TRUE)  

p9 = grid.arrange(p5, p6, p7, p8) 
#visually = more tightening up of patterns at larger scales, EXCEPT elevational variance
#recall that env vars calculated iteratively over consecutively larger and larger radius out from each focal route, 
#and so across progressively larger and larger vector of values from each subsumed secondary route therein 
#so: mean(vector of means of secondary routes, vector length based on nu/scale) AND 
#variance(vector of means of secondary routes, vector length based on nu/scale)
#only var and means encompassed by each stateroute, NOT these values across all focal routes

#all sig, but only explain like 6% and 4% of var across scale 
#directionally, consider interpretations and how predictions may follow 
#elevational variances are tighter at low scales, and much higher at high scales 
#variances in ndvi are similarly higher at high scales
#exceptions to this in both make relationship weaker 
#what are those exceptions, and why are they like that? 

####Coefs to habitat het####

#coefs to top scale env characterizing data
env_coefs = env_all %>%
  inner_join(coefs, by = "stateroute") #and also join single rte
  
#since now reflective of all scales, 62370 rows, 36 cols 
write.csv(env_coefs, "scripts/R-scripts/scale_analysis/env_coefs.csv", row.names = FALSE)
#updated 11/29


####Coef & habitat heterogeneity models####
env_coefs = read.csv("scripts/R-scripts/scale_analysis/env_coefs.csv", header = TRUE)

#check out cov matrix to inform model generation and predictions:
covmatrix = round(cor(env_coefs[, 3:ncol(env_coefs)]), 2) #since clipped stateroute don't need to clip again - should I clip scale?
covmatrix = as.data.frame(covmatrix)
write.csv(covmatrix, "scripts/R-scripts/scale_analysis/covmatrix.csv", row.names = FALSE)
#mean and var - interpret how covary with coefs and direction - i.e. ndvi mean covaries positively with pmin, 
#but elev mean covaries negatively with pmin 
#and both variances covary negatively with pmin 
#ndvi mean covaries negatively with pthresh, elev mean positively
#no other variables have this divergent relationship in directionality of covariance, 
#ndvi var and elev var always in unison when strong
#hab het variance measures: pslope and pthresh positive, pmin and pmax both negative covariance


#join original coef vars at scale of single focal rte and also make sure reflected in names 
#e.g. "Does the min and the predicted min vary with environmental heterogeneity 
#at the scale of a single route? at the scale of a landscape?
min_mod1 = lm(OA.min ~ ndvi.var, data = env_coefs)
min_mod2 = lm(ON.min ~ ndvi.var, data = env_coefs)

min_mod3 = lm(OA.min ~ elev.var, data = env_coefs)
min_mod4 = lm(ON.min ~ elev.var, data = env_coefs)

summary(min_mod1)
summary(min_mod2)
summary(min_mod3)
summary(min_mod4)

#explains a good deal of variation in our predicted vals and their deviance from the actual 
#test example models -> elev and ndvi explain more variation at the landscape scale than local scale. 



####UP NEXT: run series of models and reassess/renew plots from spring####

# nested loop for examining variation in coefs/fitted curves explained by env heterogeneity 
#so: response = coefficients = dependent; predictor = environmental heterogeneity = independent

#first need to make sure JUST looking at variance characterizing site, not means -> filter out 

rsqrd_hetero = data.frame(dep = character(), ind = character(), 
                          r2 = numeric(), adjr = numeric(), corr_r = numeric())
#modify to include plotting of obs values for each stateroute vs pred line 
#and plot these with r squared vals as annotations to plots too 
setwd("C:/git/core-transient/output/plots/'Molly Plots'/habhet/")


for (d in 3:6) { #adjust columns appropriately -> make sure correct order of ind and dep vars!
  for (i in 8:17) {
    tempmod = lm(env_coefs[,d] ~ env_coefs[,i])
    tempcor = cor.test(env_coefs[,d], env_coefs[,i], method = "pearson")
    
    
    tempdf = data.frame(dep = names(env_coefs)[d], 
                        ind = names(env_coefs)[i], 
                        r2 = summary(tempmod)$r.squared, 
                        adjr = summary(tempmod)$adj.r.squared, 
                        corr_r = as.numeric(tempcor$estimate))
    
    # templot = ggplot(data = env_coefs, aes(x = env_coefs[,i], y = env_coefs[,d]))+geom_point()+
    #   geom_line(aes(y = predict(tempmod), color = 'Model'))+
    #   labs(x = names(env_coefs)[i], y = names(env_coefs)[d])+guides(color = "none")+
    #   annotate("text", x = 0.5*max(env_coefs[,i]), y = 0.5*max(env_coefs[,d]), 
    #            label = paste("italic(R) ^ 2 ==", tempdf$r2, sep = ""), parse = TRUE, 
    #            color = "red", size = 5.5) 
    # ggsave(templot, filename=paste("env_coefs", names(env_coefs)[d], 
    #                                names(env_coefs)[i],".png",sep=""))
    
    rsqrd_hetero = rbind(rsqrd_hetero, tempdf)
    }
}

dev.off()
write.csv(rsqrd_hetero, "scripts/R-scripts/scale_analysis/rsqrd_hetero.csv", row.names = FALSE) 
#updated 10/30 using corrected hab_het vals, only variances characterizing sites


####Visually Characterizing measures of habitat heterogeneity####
rsqrd_hetero = read.csv("scripts/R-scripts/scale_analysis/rsqrd_hetero.csv", header = TRUE)
# hab_het = read.csv("scripts/R-scripts/scale_analysis/hab_het.csv", header = TRUE)

r_plot = ggplot(data = rsqrd_hetero, aes(y = corr_r))+geom_col(aes(x=dep))+facet_wrap(~ind)+
  theme_bw()
r_plot 


env_coefs = read.csv("scripts/R-scripts/scale_analysis/env_coefs.csv", header = TRUE)

#scale on x and r on y, panel by coef of interest, line color by var measure

# goal plot -> ggplot(envcoefs, aes(x = scale, y = corr_r))+geom_line(aes(color = dep))+facet_wrap(~ind)
#I want a corr_r value for every dep and ind variable at every scale, for every focal
#for every scale, for every focal route - will have a LOT - maybe just do a subset for meeting 

#the correlation coefficients themselves won't change, bc representative of the overall 
#occ-scale relationship, that's fine - the hab_het vals will change though bc measures 
#at each scale 
#starting at scale of 1 since that's lowest res we have for habhet across scales, 
#rerun previous dep/ind loop with new mods


scales_hetero = data.frame(dep = character(), ind = character(), 
                          r2 = numeric(), adjr = numeric(), corr_r = numeric(), scale = numeric())
#modify to include plotting of obs values for each stateroute vs pred line 
#and plot these with r squared vals as annotations to plots too 
#setwd("C:/git/core-transient/output/plots/Molly_Plots/habhet/")
scales = unique(env_coefs$scale)


for (s in scales) {
  env_coefs2 = env_coefs %>% 
    filter(scale == s)
    for (d in 3:6) { #adjust columns appropriately -> make sure correct order of ind and dep vars!
      for (i in 7:16) {
       tempmod = lm(env_coefs2[,d] ~ env_coefs2[,i])
       tempcor = cor.test(env_coefs2[,d], env_coefs2[,i], method = "pearson")
    
    
       tempdf = data.frame(dep = names(env_coefs2)[d], 
                        ind = names(env_coefs2)[i], 
                        r2 = summary(tempmod)$r.squared, 
                        adjr = summary(tempmod)$adj.r.squared, 
                        corr_r = as.numeric(tempcor$estimate), 
                        scale = s)
    
    # templot = ggplot(data = env_coefs, aes(x = env_coefs[,i], y = env_coefs[,d]))+geom_point()+
    #   geom_line(aes(y = predict(tempmod), color = 'Model'))+
    #   labs(x = names(env_coefs)[i], y = names(env_coefs)[d])+guides(color = "none")+
    #   annotate("text", x = 0.5*max(env_coefs[,i]), y = 0.5*max(env_coefs[,d]), 
    #            label = paste("italic(R) ^ 2 ==", tempdf$r2, sep = ""), parse = TRUE, 
    #            color = "red", size = 5.5) 
    # ggsave(templot, filename=paste("env_coefs", names(env_coefs)[d], 
    #                                names(env_coefs)[i],".png",sep=""))
    
        scales_hetero = rbind(scales_hetero, tempdf)
    }
  }
}

write.csv(scales_hetero, "scripts/R-scripts/scale_analysis/scales_hetero.csv", row.names = FALSE) 
#updated 11/09 using corrected hab_het vals, only variances characterizing sites







#INTERPRETATION: 
#elevation and ndvi at the highest scales explain more variation in the pmin and pthresh values 
#compared to any other measures of habitat heterogeneity 
#However, elevation explains more variation than ndvi at the top scales - AND 
#the gulf between ndvi and elevation in terms of explanatory power widens at the larger scales 
# - the two measures are closer together in importance at the lowest scales. 
#NDVI variance at local scale actually has an 'outlier' point further above even local elev variation 
#depending on which coefficient metric that corresponds to, it may be that ndvi explains more 
#local variation in heterogeneity and variation at the lower scales of the occ-scale relationship 
#while elevational heterogeneity explains variation at the higher scales of the occ-scale relationship?  

