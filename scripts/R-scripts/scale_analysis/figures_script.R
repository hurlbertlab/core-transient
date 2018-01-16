#Figures and tables 
#wd1: setwd("C:/git/core-transient")
#wd2: setwd("\\bioark.ad.unc.edu\HurlbertLab\Jenkins\Final folder") 

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

#Figure 1: Bimodal dist images; number of spp on y vs # years present  
#A: original bimodal dist 
#B: distribution at smallest scales 
#C: distribution at max scale 

#refer to coylefig1a.R script in core-transient scripts folder for guidance 
#need to recreate spp_matrix with current data 
#AOU codes columns following first stateroute column 
#individual occ values for spp at each stateroute across the 15 year window 


BBS = '//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/'

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
min_out = read.csv("//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/min_out.csv", header = TRUE)
#scales 2:66 agg routes 
max_out = read.csv("//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/max_out.csv", header = TRUE)
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



all_fig = rbind(max_out, min_out)
write.csv(all_fig, "//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/all_figoutput.csv", row.names = FALSE)
#stored in bioark folder 


####Plotting how distributions change across scale, using area####
all_fig = read.csv("//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/all_figoutput.csv", header = TRUE)
#all_fig$area = as.factor(all_fig$area)

all_figplot = ggplot(all_fig, aes(occ, group = factor(area), color = factor(area)))+
  stat_density(geom = "path", position = "identity", bw = "bcv", kernel = "gaussian", n = 4000, na.rm = TRUE, size = 1.3)+
  labs(x = "Proportion of time present at site", y = "Probability Density")+theme_classic()+
  scale_color_viridis(discrete = TRUE)+theme(axis.title = element_text(size = 18))
all_figplot
#edit fig for manuscript -? 2-3 colors color ramp thru viridis 
#chop 1-2 scales if needed 
#thicken lines 
#increase text size 5x or so 



minplot = ggplot(min_out, aes(occ, group = area, color = area))+
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
dist.df = read.csv("scripts/R-scripts/scale_analysis/dist_df.csv", header = TRUE)
dist.df_sub = dist.df %>% 
  filter(rte1 == "2001")%>% 
  top_n(66, desc(dist)) %>% #fixed ordering by including arrange parm, 
  arrange(dist) 

dist.df_sub2 = dist.df %>% 
  filter(rte1 == "11244") %>%
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
star2 = bbs_thrd %>% filter(stateroute == "11244")

plot(NorthAm, xlim = c(-160, -60), ylim = c(25, 70))
points(bbs_latlon$Longi, bbs_latlon$Lati, col= "grey", pch=16)
points(sites1$longitude, sites1$latitude, col = "lightseagreen", pch = 16)
points(sites2$longitude, sites2$latitude, col = "goldenrod", pch = 16)
points(star1$Longi, star1$Lati, col = "black", pch = 17, cex = 2)
points(star2$Longi, star2$Lati, col = "black", pch = 17, cex = 2)

####Results section figs####
#scales hetero derived at end of env_analysis script
scales_hetero = read.csv("scripts/R-scripts/scale_analysis/scales_hetero.csv", header = TRUE)


#scale on x and r on y, panel by coef of interest, line color by var measure
ggplot(scales_hetero, aes(x = scale, y = corr_r))+
  geom_line(aes(color = dep))+facet_wrap(~ind)+theme_classic()
#I want a corr_r value for every dep and ind variable at every scale, for every focal
#for every scale, for every focal route - will have a LOT - maybe just do a subset for meeting 

#the correlation coefficients themselves won't change, bc representative of the overall 
#occ-scale relationship, that's fine - the hab_het vals will change though bc measures 
#at each scale 
#starting at scale of 1 since that's lowest res we have for habhet across scales, 
#rerun previous dep/ind loop with new mods

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




