#Figures and tables 
#wd1: setwd("C:/git/core-transient")
#wd2: setwd("\\bioark.ad.unc.edu\HurlbertLab\Jenkins\Final folder") 
library(viridis)
library(tidyverse)
library(raster)
library(maps)
library(sp)
library(rgdal)
library(maptools)
library(rgeos)
library(fields)
library(gridExtra)
library(wesanderson)
library(stats)
library(gimms)
library(devtools)
library(geometry)
library(zoo)

#Figure 1: Bimodal dist images; number of spp on y vs # years present  
#A: original bimodal dist 
#B: distribution at smallest scales 
#C: distribution at max scale 

#refer to coylefig1a.R script in core-transient scripts folder for guidance 
#need to recreate spp_matrix with current data 
#AOU codes columns following first stateroute column 
#individual occ values for spp at each stateroute across the 15 year window 


BBS = '//bioark.ad.unc.edu/HurlbertLab/Jenkins/Intermediate scripts/BBS scaled/'

fifty_allyears = read.csv(paste(BBS, "fifty_allyears.csv", sep = ""), header = TRUE) #using updated version, 50 stop data, 07/12
bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)

fifty_bestAous = fifty_allyears %>% 
  filter(AOU > 2880 & !(AOU >= 3650 & AOU <= 3810) & !(AOU >= 3900 & AOU <= 3910) & 
           !(AOU >= 4160 & AOU <= 4210) & AOU != 7010) #leaving out owls, waterbirds as less reliable data

#occ_counts function for calculating occupancy at any scale
#countcolumns can refer to the stops in a stateroute OR 
#it can refer to the associated secondary routes to aggregate across 
#occ_counts function for calculating occupancy at any scale
#countcolumns can refer to the stops in a stateroute OR 
#it can refer to the associated secondary routes to aggregate across 
occ_counts = function(countData, countColumns, scale) {
  bbssub = countData[, c("stateroute", "year", "AOU", countColumns)] #these are our grouping vars
  bbssub$groupCount = rowSums(bbssub[, countColumns]) 
  bbsu = unique(bbssub[bbssub[, "groupCount"]!= 0, c("stateroute", "year", "AOU")]) 
  
  abun.summ = bbssub %>% #abundance
    group_by(stateroute, year) %>%  
    summarize(totalN = sum(groupCount))  #we want to go further and summarize across focal + secondary rtes tho
  
  occ.summ = bbsu %>% #occupancy
    count(stateroute, AOU) %>%
    mutate(occ = n/15, scale = scale) %>% #, #may want to get rid of, this is at the column-counting scale
    #scale = scale) %>%
    left_join(abun.summ, by = 'stateroute')
  return(occ.summ)
}


# Generic calculation of occupancy for a specified scale
#fix to run all at once, so no sep run for above-scale, USE occ-counts for both 
b_scales = c(5, 10, 25, 50)
output = c()
for (s in b_scales) {
  numGroups = floor(50/s)
  for (g in 1:numGroups) {
    groupedCols = paste("Stop", ((g-1)*s + 1):(g*s), sep = "")
    temp = occ_counts(fifty_bestAous, groupedCols, s) 
    output = rbind(output, temp) 
  } 
}

min_dist = output
#transformation into matrix unnecessary with ggplot version 
#write.csv(min_dist, "//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/min_dist.csv", row.names = FALSE)
min_dist = read.csv("//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/min_dist.csv", header = TRUE)


#filter to scale == 50, check
min_dist2 = min_dist %>% 
  filter(scale == "50")

fig1a = ggplot(min_dist2, aes(occ))+
  geom_density(bw = "bcv", kernel = "gaussian", n = 2000, na.rm = TRUE)+
  labs(x = "Proportion of time present at site", y = "Probability Density", title = "Single Route Scale")+ 
  theme_classic() #coord_cartesian(xlim = c(0, 1), ylim = c(0, 2.5))+
fig1a

#repeat for scale of 5 stop segment and scale of 66 routes 

####Figure 1b: at scale of 5 stop segments####
# Generic calculation of occupancy for a specified scale
#fix to run all at once, so no sep run for above-scale, USE occ-counts for both 

#scale of 5 segments (min) 
min_dist = min_dist[, -3]
#need to avg occs between unique stateroute-AOU pairs since 5 for every 1 
min_dist3 = min_dist %>% 
  group_by(AOU, stateroute, scale) %>% 
  summarise(occ = mean(occ)) %>% dplyr::select(everything()) 

min_out = as.data.frame(min_dist3)
#write.csv(min_out, "//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/min_out.csv", row.names = FALSE)

fig1b = ggplot(min_dist3, aes(occ, group = scale, color = scale))+
  geom_density(kernel = "gaussian", n = 2000, na.rm = TRUE)+
  labs(x = "Proportion of time present at site", y = "Probability Density", title = "Local Scales")+ 
  theme_classic() #coord_cartesian(xlim = c(0, 1), ylim = c(0, 2.5))+
fig1b 

####Fig 1c: distribution at the maximum scale####
dist.df = read.csv("scripts/R-scripts/scale_analysis/dist_df.csv", header = TRUE)
bbs_above_guide = read.csv("scripts/R-scripts/scale_analysis/bbs_above_guide.csv", header = TRUE)
#groupcounts for each AOU for each year at scale of ONE stateroute 

#occ_counts function for calculating occupancy at any scale
#countcolumns can refer to the stops in a stateroute OR 
#it can refer to the associated secondary routes to aggregate across 

uniqrtes = unique(bbs_above_guide$stateroute) #all routes present are unique, still 953 which is great
scales = c(2, 4, 8, 16, 32, 66) # based on min common number in top 6 grid cells, see grid_sampling_justification script 
max_out = c()

for (nu in scales){
  #test example route 2010 and nu at 57 routes -> large scale, should have high occ 
  for (r in uniqrtes) { #for each focal route
    tmp_rte_group = dist.df %>% #changes with size of nu but caps at 66
      filter(rte1 == r) %>% 
      top_n(66, desc(dist)) %>% #fixed ordering by including arrange parm, 
      #remove/skip top row 
      arrange(dist) %>%
      slice(1:nu) %>% 
      dplyr::select(everything()) %>% data.frame()
    
    
    focal_clustr = bbs_above_guide %>% 
      filter(stateroute %in% tmp_rte_group$rte2) 
    
    occ.summ = focal_clustr %>% 
      dplyr::select(year, AOU) %>% #duplicates remnant of distinct secondary routes - finally ID'd bug
      distinct() %>% #removing duplicates 09/20
      count(AOU) %>% #how many times does that AOU show up in that clustr that year 
      dplyr::mutate(occ = n/15, stateroute = r, scale = nu) 
    
    max_out = rbind(max_out, occ.summ)
    
  }
}

max_out = max_out[, -2]
max_out = as.data.frame(max_out)

fig1c = ggplot(max_out, aes(occ))+
  geom_density(bw = "bcv", kernel = "gaussian", n = 2000, na.rm = TRUE)+
  labs(x = "Proportion of time present at site", y = "Probability Density", title = "Maximum Scale")+theme_classic()
#so it was the limits giving me crap in the original 
fig1c

write.csv(max_out, "//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/max_out.csv", row.names = FALSE)

####Figure 4 all graphs overlay####
## merge output, min, and max into single df while adding new column delineating which 
## category: single, min, or max the data corresponds to so multiple lines can be 
## overlaid on single density plot 

# <<<<<<< HEAD
# output$scale = 1
# min_out2$scale = .10
# # max_out$scale = c("Largest Scale")
# =======
# output$scale = c("Single Route Scale")
# min_out2$scale = c("Local Scale")
# max_out$scale = c("Regional Scale")
# >>>>>>> f1c590e6043bf38c744f891e4c763df712f21b21


#read in min and single route scale occ density data 
min_out = read.csv("//bioark.ad.unc.edu/HurlbertLab/Jenkins/Intermediate scripts/BBS scaled/min_out.csv", header = TRUE)
#scales 2:66 agg routes 
max_out = read.csv("//bioark.ad.unc.edu/HurlbertLab/Jenkins/Intermediate scripts/BBS scaled/max_out.csv", header = TRUE)
#updated 12/12 


#organize by scales; label and differentiate scales so that below-rtes are appropriately smaller
#do area calcs and color by area? 

min_out = min_out %>% 
  dplyr::select(stateroute, AOU, occ, scale) %>% 
  dplyr::mutate(area = scale*(pi*(0.4^2))) %>% #scale corresponds to the number of stops
  dplyr::select(stateroute, AOU, occ, area)


max_out = max_out %>% 
  dplyr::select(stateroute, AOU, occ, scale) %>% 
  dplyr::mutate(area = scale*50*(pi*(0.4^2))) %>% #scale corresponds to the number of agg routes; 50 stops per rte
  dplyr::select(stateroute, AOU, occ, area)


dist.df = read.csv("scripts/R-scripts/scale_analysis/intermed/dist_df.csv", header = TRUE)
#groupcounts for each AOU for each year at scale of ONE stateroute 
#filter out to only routes that are up to 1000km radius away from each other before analyses 
far = dist.df %>% arrange(rte1, dist) %>% group_by(rte1) %>% slice(66)
hist(far$dist)
far2 = far %>% filter(dist < 1000)

min_out2 = min_out %>% filter(stateroute %in% far2$rte1)
max_out2 = max_out %>% filter(stateroute %in% far2$rte1)

all_fig = rbind(max_out2, min_out2)
length(unique(all_fig$stateroute)) #968, as it should be 
write.csv(all_fig, paste(BBS, "all_figoutput.csv", sep = ""), row.names = FALSE)
#stored in bioark folder 


####Plotting how distributions change across scale, using area####
all_fig = read.csv("//bioark.ad.unc.edu/HurlbertLab/Jenkins/Intermediate scripts/BBS scaled/all_figoutput.csv", header = TRUE)
#all_fig$area = as.factor(all_fig$area)
all_fig$area_f = factor(signif(all_fig$area, digits = 2),
                         levels = c(2.5, 5, 13, 25, 50, 100, 200, 400, 800, 1700),
                         labels = c("2.5, 5 point count stops", "5", "13", "25, 1 BBS route", "50", "100", "200", "400", "800", "1700, 66 aggregate BBS routes")) 

  
  
  
all_figplot = ggplot(all_fig, aes(occ, group = area_f, color = area_f))+
  stat_density(geom = "path", position = "identity", bw = "bcv", kernel = "gaussian", n = 4000, na.rm = TRUE, size = 1.3)+
  labs(x = "Proportion of time present at site", y = "Probability Density")+theme_classic()+
  scale_color_viridis(discrete = TRUE, name = expression("Spatial Scale in km"^{2}))+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 16))+
  theme(legend.text = element_text(size = 16), legend.title = element_text(size = 16))+
  theme(legend.position = c(0.50, 0.50))
all_figplot


minplot = ggplot(min_out, aes(occ, group = scale, color = scale))+
  stat_density(geom = "path", position = "identity", bw = "bcv", kernel = "gaussian", n = 4000, na.rm = TRUE)+
  labs(x = "Proportion of time present at site", y = "Probability Density", title = "Local scales")+theme_classic()
minplot

#interesting problem - geom line does not permit lines to crisscross, but geom_density does 
# - although then we have polygon problem again. How to fix? 


####Results Figure occ-scale: area and abundance####
#Using same data from distribution plots, visualize occ and scale 
#troubleshoot discrepancies in bbs_allscales occ calcs from occ_processing script

BBS = '//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/'
fifty_allyears = read.csv(paste(BBS, "fifty_allyears.csv", sep = ""), header = TRUE) #using updated version, 50 stop data, 07/12
bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
fifty_bestAous = fifty_allyears %>% 
  filter(AOU > 2880 & !(AOU >= 3650 & AOU <= 3810) & !(AOU >= 3900 & AOU <= 3910) & 
           !(AOU >= 4160 & AOU <= 4210) & AOU != 7010) #leaving out owls, waterbirds as less reliable data

plot(bbs_allscales$meanOcc~bbs_allscales$logA)
plot(bbs_allscales$meanOcc~bbs_allscales$logN)
#jump between 1 and 2 scales -> diagnose 





####Fig 3####
##Make background grey, illustrate 66 points region in black, with red star centerpt 
NorthAm = readOGR(dsn = "//bioark.ad.unc.edu/HurlbertLab/GIS/geography", layer = "continent")
NorthAm2 = spTransform(NorthAm, CRS("+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km"))
bbs_latlon = read.csv(paste(BBS, "good_rtes2.csv", sep = ""), header = TRUE)
dist.df = read.csv("scripts/R-scripts/scale_analysis/intermed/dist_df.csv", header = TRUE)
dist.df_sub = dist.df %>% 
  filter(rte1 == "2001")%>% 
  top_n(66, desc(dist)) %>% #fixed ordering by including arrange parm, 
  arrange(dist) 

dist.df_sub2 = dist.df %>% 
  filter(rte1 == "89152") %>%
  top_n(66, desc(dist)) %>% #fixed ordering by including arrange parm, 
  arrange(dist) 



#exclude routes that have missing above OR below scale data, such that sites are only calculated for routes that cover all 83 scales
bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
bbs_latlon = filter(bbs_latlon, stateroute %in% bbs_allscales$focalrte)
bbs_latlon$stateroute = as.character(bbs_latlon$stateroute)
bbs_secnd = filter(bbs_latlon, stateroute %in% dist.df_sub$rte2) #things get out of order! 
bbs_thrd = filter(bbs_latlon, stateroute %in% dist.df_sub2$rte2)

sites1 = data.frame(longitude = bbs_secnd$Longi, latitude = bbs_secnd$Lati) 
sites2 = data.frame(longitude = bbs_thrd$Longi, latitude = bbs_thrd$Lati) 
star1 = bbs_secnd %>% filter(stateroute == "2001")
star2 = bbs_thrd %>% filter(stateroute == "89152")

plot(NorthAm, xlim = c(-160, -60), ylim = c(25, 65))
points(bbs_latlon$Longi, bbs_latlon$Lati, col= "grey", pch=16)
points(sites1$longitude, sites1$latitude, col = "#FDE725FF", pch = 16)
points(sites2$longitude, sites2$latitude, col = viridis(1, begin = 0.5, end = 1, option = "D"), pch = 16)
points(star1$Longi, star1$Lati, col = "black", pch = 17, cex = 2)
points(star2$Longi, star2$Lati, col = "black", pch = 17, cex = 2)

####Results section figs####
#scales hetero derived at end of env_analysis script
scales_hetero = read.csv("scripts/R-scripts/scale_analysis/core_scales_hetero.csv", header = TRUE) #pulling from core output

scales_hetero_v = scales_hetero %>% 
  filter(dep == "elev.var" | dep == "ndvi.var") %>%
  filter(ind == "PCA.curvature" | ind == "PCA.max" | ind == "PCA.mid"| ind == "PCA.min" | ind == "PCA.slope")

scales_hetero_v$ind = factor(scales_hetero_v$ind, 
                             levels = c("PCA.min","PCA.mid", "PCA.slope","PCA.curvature", "PCA.max"),
                             labels = c("Min", as.character(expression("Scale"[50])), "Slope", "Curvature", "Max"))

scales_hetero_v$dep = factor(scales_hetero_v$dep, 
                                levels=c("elev.var", "ndvi.var"),
                                labels=c("Elevation", "NDVI"))

#scale on x and r on y, panel by coef of interest, line color by var measure
ggplot(scales_hetero_v, aes(x = scale, y = corr_r))+
  geom_line(aes(color = dep), size = 1.4)+facet_wrap(~ind, labeller = label_parsed)+
  theme_classic()+
  geom_abline(intercept = 0, slope = 0)+
  theme_classic()+theme(text = element_text(size = 18))+
  labs(color = "Environmental Heterogeneity", x = "Number of aggregated BBS Routes", y = "Pearson's correlation estimate")+theme(legend.position = c(0.84, 0.20))+
  scale_color_viridis(begin = 0, end = 0.7, discrete = TRUE, option = "D") 



# scale_shape_discrete(name="Habitat Heterogeneity",
#                      breaks=c("elev.var", "ndvi.var"),
#                      labels=c("Variance in Elevation", "Variance in NDVI"))+

#I want a corr_r value for every dep and ind variable at every scale, for every focal
#for every scale, for every focal route - will have a LOT - maybe just do a subset for meeting 

#the correlation coefficients themselves won't change, bc representative of the overall 
#occ-scale relationship, that's fine - the hab_het vals will change though bc measures 
#at each scale 
#starting at scale of 1 since that's lowest res we have for habhet across scales, 
#rerun previous dep/ind loop with new mods


#at top scales, with just variances - diamond shape figure that parallels prediction table (alt to outcome table)
scales_hetero2 = scales_hetero %>% 
  filter(scale == 66) %>% 
  filter(dep == "elev.var" | dep == "ndvi.var") %>% 
  filter(ind == "PCA.curvature" | ind == "PCA.max" | ind == "PCA.mid"| ind == "PCA.min" | ind == "PCA.slope")

ggplot(scales_hetero2, aes(x = ind, y = corr_r))+
  geom_pointrange(aes(color = dep, ymin = lowr, ymax = uppr), size = 1.2, position = position_dodge(width = 0.35))+geom_abline(intercept = 0, slope = 0)+
  theme_classic()+theme(axis.title = element_text(size = 18), axis.text = element_text(size = 16), legend.position = c(0.55, 0.25), legend.text = element_text(size = 16), legend.title = element_text(size = 16))+
  labs(x = "Occupancy-scale parameters", y = "Pearson's correlation estimate")+
  scale_x_discrete(limit = c("PCA.min", "PCA.mid","PCA.slope","PCA.curvature","PCA.max"),
                   labels = c("Min", expression("Scale"[50]),"Slope","Curvature","Max"))+
  scale_y_continuous(breaks = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4))+
  scale_color_manual(name = "Environmental Heterogeneity",
                     values=c("#440154FF", "#55C667FF"),
                     labels = c("Elevation", "NDVI"))
#likely #440154FF purple and #55C667FF



####Plot stateroutes 1 by 1, pick out some emblematic "types"####
bbs_allscales = na.omit(read.csv("C:/git/core-transient/data/BBS/bbs_allscales.csv", header = TRUE))
focalrtes = unique(bbs_allscales$focalrte)
setwd("C:/rte_imgs")

for (r in focalrtes) {
  bbs_allsub = bbs_allscales %>% filter(focalrte == r)
  ggplot(bbs_allsub, aes(x = logA, y = meanOcc))+
    geom_line()+ #coord_cartesian(xlim = c(0, 3.5), ylim = c(0, 1))+ tweak with 
  theme_classic()+ labs(x = "Log Area", y = "Mean Community Occupancy", 
                        title = r)
  ggsave(paste("plot", r, ".tiff", sep = ""))
}

dev.off()



####Plotting NULL all routes with 3 highlighted "types####
bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
coefs = read.csv("scripts/R-scripts/scale_analysis/intermed/coefs.csv", header = TRUE) #AUC etc.

coefs_ranked = coefs %>% 
  arrange(OA.curvature) #middle teal line should be least curvy 


bbs_allsub = bbs_allscales %>% filter(focalrte == 33901 | focalrte == 72035 | focalrte == 44032)
bbs_allsub$focalrte = as.factor(bbs_allsub$focalrte)
#use this to assign diff colors for each factor level per what color scheme is ideal?


pred_plot = ggplot(bbs_allscales, aes(x = logA, y = meanOcc))+geom_line(aes(group = focalrte), color = "grey")+
  theme_classic()+geom_line(data = bbs_allsub, aes(x = logA, y = meanOcc, group = as.factor(focalrte), color = as.factor(focalrte)), size = 2)+ #geom_smooth(model = lm, color = 'red')+
  labs(x = "Log Area", y = "Mean Community Occupancy")+scale_color_viridis(discrete = TRUE)+
  theme(axis.title = element_text(size = 18))+theme(legend.position = c(0.80, 0.25)) 
pred_plot 

#pctcore version 
####Plotting NULL all routes with 3 highlighted "types####
bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
bbs_allscales = bbs_allscales %>% 
  dplyr::filter(logN != "NA")

core_coefs = read.csv("scripts/R-scripts/scale_analysis/intermed/core_coefs.csv", header = TRUE) #AUC etc.

coefs_ranked = core_coefs %>% 
  arrange(PCA.curvature) #middle teal line should be least curvy


#compare rtes in homogeneous vs heterogeneous regions, illustrate in color to prove point 
env_all = read.csv("scripts/R-scripts/scale_analysis/intermed/env_all.csv", header = TRUE) #AUC etc.
ndvi_ranked = env_all %>% 
  group_by(stateroute) %>% 
  summarize(ndvi_m = mean(ndvi.var)) %>%
  arrange(desc(ndvi_m)) 
#lowest var in NDVI: rtes 72151, 72049, 72052, #mostly 72's and 2,000's 
#highest var in NDVI: rtes 14059, 14140, 69253, 69021, 85011

elev_ranked = env_all %>% 
  group_by(stateroute) %>% 
  summarize(elev_m = mean(elev.var)) %>%
  arrange(desc(elev_m))
#lowest var in elev: rtes 34027 (best, closest to normal avgs), mostly 34's, 35010, 
#highest var in elev: rtes 17221, 6012, 17044, 6071, 85169, 14059 mostly 14's, 17's, and 6,000's

central = bbs_allscales %>%
  group_by(logA) %>%
  summarize(pctC_avg = mean(pctCore)) %>%
  mutate(logA = round(logA, digits = 2)) %>%
  group_by(logA) %>%
  summarize(pctCore = mean(pctC_avg, na.rm = TRUE)) %>% 
  mutate(focalrte = "99999")

#I need to make a focal rte dummy variable that says focalrte == "99999" with the titular level and a label of "mean" 
# bbs_allscales$cen = rollmean(bbs_allscales$pctCore, k = 5, fill = "extend")

bbs_allsub = bbs_allscales %>% 
  filter(focalrte == 34054 | focalrte == 85169) %>%
  dplyr::select(focalrte, logA, pctCore)

bbs_allsub2 = rbind(bbs_allsub, central)
  
bbs_allsub2$focalrte = factor(bbs_allsub2$focalrte,
                             levels=c( "99999","34054", "85169"),
                             labels=c("Mean",
                                      "Low Heterogeneity",
                                      "High Heterogeneity"))
#use this to assign diff colors for each factor level per what color scheme is ideal?
#72 is PA, 14 is Cali, 34 is Illinois, 17 is Colorado 

pred_plot = ggplot(bbs_allscales, aes(x = logA, y = pctCore))+geom_line(aes(group = focalrte), color = "grey")+
  theme_classic()+geom_abline(aes(intercept = 0.5, slope = 0), linetype = "dashed")+
  geom_line(data = bbs_allsub2, aes(x = logA, y = pctCore, group = as.factor(focalrte), color = as.factor(focalrte)), size = 2)+ #geom_smooth(model = lm, color = 'red')+
  labs(x = "Log Area", y = "", title = "A")+
  scale_color_viridis(discrete = TRUE, name = "", option = "B", begin = 0.05, end = .75)+
  theme(plot.title = element_text(size = 20), axis.title = element_text(size = 18), axis.text = element_text(size = 16), legend.text = element_text(size = 14), legend.title = element_text(size = 14))+
  theme(legend.position = c(0.74, 0.18)) 
pred_plot #yellow = high variation in habhet, purple = low variation, low habhet 



central2 = bbs_allscales %>%
  group_by(logN) %>%
  summarize(pctC_avg = mean(pctCore)) %>%
  mutate(logN = round(logN, digits = 1)) %>%
  group_by(logN) %>%
  summarize(pctCore = mean(pctC_avg, na.rm = TRUE)) %>% 
  mutate(focalrte = "99999")

#I need to make a focal rte dummy variable that says focalrte == "99999" with the titular level and a label of "mean" 
# bbs_allscales$cen = rollmean(bbs_allscales$pctCore, k = 5, fill = "extend")

bbs_allsub = bbs_allscales %>% 
  filter(focalrte == 34054 | focalrte == 85169) %>%
  dplyr::select(focalrte, logN, pctCore)

bbs_allsub3 = rbind(bbs_allsub, central2)

bbs_allsub3$focalrte = factor(bbs_allsub3$focalrte,
                              levels=c( "99999","34054", "85169"),
                              labels=c("Mean",
                                       "Low Heterogeneity",
                                       "High Heterogeneity"))
#use this to assign diff colors for each factor level per what color scheme is ideal?
#72 is PA, 14 is Cali, 34 is Illinois, 17 is Colorado 

pred_abuns = ggplot(bbs_allscales, aes(x = logN, y = pctCore))+geom_line(aes(group = focalrte), color = "grey")+
  theme_classic()+geom_abline(aes(intercept = 0.5, slope = 0), linetype = "dashed")+
  geom_line(data = bbs_allsub3, aes(x = logN, y = pctCore, group = as.factor(focalrte), color = as.factor(focalrte)), size = 2)+ #geom_smooth(model = lm, color = 'red')+
  labs(x = "Log Community size", y = "", title = "B")+
  scale_color_viridis(discrete = TRUE, name = "", option = "B", begin = 0.05, end = .75)+
  theme(plot.title = element_text(size = 20), axis.title = element_text(size = 18), axis.text = element_text(size = 16), legend.text = element_text(size = 14), legend.title = element_text(size = 14))+
  theme(legend.position = "none") 
pred_abuns #yellow = high variation in habhet, purple = low variation, low habhet 


p1 = grid.arrange(pred_plot, pred_abuns, ncol = 2, 
                  left = textGrob("Proportion Core Species in Community", 
                                  rot = 90, vjust = 1, gp = gpar(cex = 1.5)))

# gp = gpar(fontface = "bold", cex = 1.5)),
# left = textGrob("Global Y-axis Label", rot = 90, vjust = 1)

####Dummy data and predicted vals for adapted Coyle et al. distribution figure, Figure 1####
#base fig1a 
min_out = read.csv("//bioark.ad.unc.edu/HurlbertLab/Jenkins/Intermediate scripts/BBS scaled/min_out.csv", header = TRUE)

#filter to scale == 50, check
single_rte = min_out %>% 
  filter(scale == "50") %>% 
  mutate(area = scale*(pi*(0.4^2)))

minplot = ggplot(single_rte, aes(occ))+
  stat_density(geom = "path", position = "identity", bw = "bcv", kernel = "gaussian", n = 4000, na.rm = TRUE, size = 1.3)+
  labs(x = "Proportion of time present at site", y = "Probability Density")+theme_classic()+
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 2.5))+
  theme(axis.title = element_text(size = 18))+theme(legend.position = c(0.50, 0.50))
minplot

# ##dummy data 
# local_dummy = single_rte %>% 
#   mutate(occ2 = 1-occ,
#          scale = 5) %>% 
#   dplyr::select(-occ) %>% 
#   rename(occ = occ2) %>% 
#   mutate(area = scale*(pi*(0.4^2)),
#          occ = ifelse(occ >= 0.5, occ-0.5, occ)) #make it so occ vals greater than 0.5 are converted to 0.1
#   
# big_dummy = single_rte %>% 
#   mutate(scale = 66, 
#          occ2 = ifelse(occ < 0.2, 0.5+occ, occ),
#          area = scale*50*(pi*(0.4^2))) %>% 
#   dplyr::select(-occ) %>% 
#   rename(occ = occ2) 
# 
# pred_dist_df = rbind(single_rte, local_dummy)
# pred_dist = rbind(pred_dist_df, big_dummy)


#######
# single_half_big = single_rte %>% 
#   filter(occ > 0.6) %>%
#   mutate(scale = 66, 
#          area = scale*50*(pi*(0.4^2)))
# 
# single_half_small = single_rte %>% 
#   filter(occ < 0.4) %>% 
#   mutate(scale = 5, 
#          area = scale*pi*(0.4^2))
# 
# doubl_dist = as.data.frame(rbind(single_half_small, single_half_small))
# double_dist2 = as.data.frame(rbind(single_half_big, single_half_big))   
# pred_dist = rbind(doubl_dist, double_dist2) 

local = rollmean(rexp(n = 76889, rate = 12), k = 5, fill = "extend")
big = rollmean(rexp(n = 76889, rate= 139/144), k = 3000, fill = "extend")

big2 = local+0.92
# big2 = floor(big2)


preds = cbind(single_rte, local)
pred_dist = cbind(preds, big2)
# pred_central = pred_dist %>%
#   group_by(occ) %>%
#   summarize(local2 = mean(local)) %>%
#   mutate(occ = round(occ, digits = 2)) %>%
#   group_by(occ) %>%
#   summarize(local3 = mean(local2, na.rm = TRUE))

  
all_predplot = ggplot(pred_dist, aes(occ))+
  stat_density(geom = "path", position = "identity", bw = "bcv", kernel = "gaussian", n = 4000, na.rm = TRUE, size = 1.5, color = "black")+
  stat_density(aes(local), geom = "path", position = "identity", bw = "bcv", kernel = "gaussian", n = 4000, na.rm = TRUE, size = 1.5, color = "#287D8EFF", linetype = "dashed")+
  stat_density(aes(big2), geom = "path", position = "identity", bw = "bcv", kernel = "gaussian", n = 4000, na.rm = TRUE, size = 1.5, color = "#FDE725FF", linetype = "dashed")+
  labs(x = "Proportion of time present at site", y = "Probability Density")+theme_classic()+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 16))+ 
  coord_cartesian(xlim = c(0.11, .95), ylim = c(0, 5.5))
all_predplot

