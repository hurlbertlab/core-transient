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

write.csv(reg_envhetero, "scripts/R-scripts/scale_analysis/top_envhetero.csv", row.names = FALSE)
#updated 10/06

#naming convention for comparisons: #env.var_zv -> z score origin, variance/habheterogeneity at rt, from 40km buffer circle raster clip.  
                 #topenv.var_zv -> z score origin, variance/habhet at landscape (var across rt buffermeans in top clustr (zm vector))
                #zm from 40 km buffer circle raster clip too

####Coef vs env hetero models####
top_envhetero = read.csv("scripts/R-scripts/scale_analysis/top_envhetero.csv", header = TRUE) #landscape habitat vars 
bbs_envs = read.csv("scripts/R-scripts/scale_analysis/bbs_envs.csv", header = TRUE) #single rte habitat vars 
coefs = read.csv("scripts/R-scripts/scale_analysis/coefs.csv", header = TRUE) #AUC etc. 

#Merge top_envhetero to coefs for comparing env variation for a site to its associated AUC 
#at the scale of a landscape and scale of a rte
env_coefs = coefs %>% 
  inner_join(top_envhetero, by = "stateroute") %>% #coefs to top scale env characterizing data
  inner_join(bbs_envs, by = "stateroute") #and also join single rte
  
#mod env coef names to reflect that they have to do with max scale #1003 rows, 54 cols 
write.csv(env_coefs, "scripts/R-scripts/scale_analysis/env_coefs.csv", row.names = FALSE)
#updated 09/21


####Coef & habitat heterogeneity models####
env_coefs = read.csv("scripts/R-scripts/scale_analysis/env_coefs.csv", header = TRUE)

subenv_coefs = env_coefs %>%
  select(-matches("CA"), -matches("TA")) #for now ignoring %Core and %Tran and focusing on mean Occupancy 

#check out cov matrix to inform model generation and predictions:
covmatrix = round(cor(subenv_coefs[, 1:ncol(subenv_coefs)]), 2) #since clipped stateroute don't need to clip again
covmatrix = as.data.frame(covmatrix)
write.csv(covmatrix, "scripts/R-scripts/scale_analysis/covmatrix.csv", row.names = FALSE)


#join original coef vars at scale of single focal rte and also make sure reflected in names 
#e.g. "Does the min and the predicted min vary with environmental heterogeneity 
#at the scale of a single route? at the scale of a landscape?
min_mod1 = lm(OA.min ~ ndvi_zv, data = env_coefs)
min_mod2 = lm(OA.pmin ~ top_ndvi_zv, data = env_coefs)

pmin_mod1 = lm(OA.pmin ~ ndvi_zv, data = env_coefs)
pmin_mod2 = lm(OA.pmin ~ top_ndvi_zv, data = env_coefs)


summary(pmin_mod2)
#test example model -> elev and ndvi explain more variation at the landscape scale than local scale. 



####UP NEXT: run series of models and reassess/renew plots from spring####

# nested loop for examining variation in coefs/fitted curves explained by env heterogeneity 
#so: response = coefficients = dependent; predictor = environmental heterogeneity = independent

#first need to make sure JUST looking at variance characterizing site, not means -> filter out 

hab_het = env_coefs %>% 
  select(-elev.mean, -ndvi.mean)

rsqrd_hetero = data.frame(dep = character(), ind = character(), r2 = numeric())
#modify to include plotting of obs values for each stateroute vs pred line 
#and plot these with r squared vals as annotations to plots too 
setwd("C:/git/core-transient/output/plots/Molly Plots/habhet/")


for (d in 2:10) { #adjust columns appropriately -> make sure correct order of ind and dep vars!
  for (i in 32:ncol(hab_het)) {
    tempmod = lm(hab_het[,d] ~ hab_het[,i])
    
    tempdf = data.frame(dep = names(hab_het)[d], 
                        ind = names(hab_het)[i], 
                        r2 = summary(tempmod)$r.squared)
    
    templot = ggplot(data = hab_het, aes(x = hab_het[,i], y = hab_het[,d]))+geom_point()+
      geom_line(aes(y = predict(tempmod), color = 'Model'))+
      labs(x = names(hab_het)[i], y = names(hab_het)[d])+guides(color = "none")+
      annotate("text", x = 0.5*max(hab_het[,i]), y = 0.5*max(hab_het[,d]), 
               label = paste("italic(R) ^ 2 ==", tempdf$r2, sep = ""), parse = TRUE, 
               color = "red", size = 5.5) 
    ggsave(templot, filename=paste("hab_het", names(hab_het)[d], 
                                   names(hab_het)[i],".png",sep=""))
    
    rsqrd_hetero = rbind(rsqrd_hetero, tempdf)
    }
}

dev.off()
write.csv(rsqrd_hetero, "scripts/R-scripts/scale_analysis/rsqrd_hetero.csv", row.names = FALSE) 
#updated 10/03 using corrected hab_het vals, only variances characterizing sites


####Visually Characterizing measures of habitat heterogeneity####
rsqrd_hetero = read.csv("scripts/R-scripts/scale_analysis/rsqrd_hetero.csv", header = TRUE)
hab_het = read.csv("scripts/R-scripts/scale_analysis/hab_het.csv", header = TRUE)

ggplot(data = rsqrd_hetero, aes(x = ind, y = r2, fill = ind))+geom_boxplot()+theme_classic()+
  theme(legend.position="none")+
  labs(x = "Environmental variables", y = "Variation Explained (R^2)")

ggplot(data = rsqrd_hetero, aes(x = ind, y = r2, color = dep))+geom_point()+theme_classic()+
  labs(x = "Environmental variables", y = "Variation Explained (R^2)")

ggplot(data = hab_het, aes(x = elev.var, y = OA.pmin))+geom_point()+geom_smooth()
ggplot(data = hab_het, aes(x = ndvi.var, y = OA.pmin))+geom_point()+geom_smooth()
ggplot(data = hab_het, aes(x = top_elev_v, y = OA.pmin))+geom_point()+geom_smooth()
ggplot(data = hab_het, aes(x = top_ndvi_v, y = OA.pmin))+geom_point()+geom_smooth()


ggplot(data = hab_het, aes(x = elev.var, y = OA.pthresh))+geom_point()+geom_smooth()
ggplot(data = hab_het, aes(x = ndvi.var, y = OA.pthresh))+geom_point()+geom_smooth()
ggplot(data = hab_het, aes(x = top_elev_v, y = OA.pthresh))+geom_point()+geom_smooth()
ggplot(data = hab_het, aes(x = top_ndvi_v, y = OA.pthresh))+geom_point()+geom_smooth()



ggplot(data = hab_het, aes(x = elev.var, y = OA.pslope))+geom_point()+geom_smooth()
ggplot(data = hab_het, aes(x = ndvi.var, y = OA.pslope))+geom_point()+geom_smooth()
ggplot(data = hab_het, aes(x = top_elev_v, y = OA.pslope))+geom_point()+geom_smooth()
ggplot(data = hab_het, aes(x = top_ndvi_v, y = OA.pslope))+geom_point()+geom_smooth()


#why are pmin and pthresh consistently highest vals for explaining variation? how do these change 
#with different env variables? Draw graphically in notes to help conceptualize. 



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

#Let's check it out: 





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
