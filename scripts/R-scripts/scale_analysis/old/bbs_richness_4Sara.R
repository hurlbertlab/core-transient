#Species richness at below-route scales (FOR SARA) 
#Molly Jenkins 
#11/10/2017 

#  setwd("C:/core-transient/")
BBS = '//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/' #data connection for 50stop (too big to host on git)

#libraries 
library(tidyverse)


#variation on occ_counts function that stops at arriving at groupcount per stateroute year spp combo 
#for a given route and scale

occ_counts2 = function(countData, countColumns, scale) {
  bbssub = countData[, c("stateroute", "year", "AOU", countColumns)] #these are our grouping vars
  bbssub$groupCount = rowSums(bbssub[, countColumns]) 
  bbsu = unique(bbssub[bbssub[, "groupCount"]!= 0, c("stateroute", "year", "AOU", "groupCount")])
  return(bbsu)
}

#read in 50 stop data and subset AOU's to non-waterbird, non-owl etc. 
fifty_allyears = read.csv(paste(BBS, "fifty_allyears.csv", sep = ""), header = TRUE) #using updated version, 50 stop data, 07/12
fifty_bestAous = fifty_allyears %>% 
  filter(AOU > 2880 & !(AOU >= 3650 & AOU <= 3810) & !(AOU >= 3900 & AOU <= 3910) & 
           !(AOU >= 4160 & AOU <= 4210) & AOU != 7010) #leaving out owls, waterbirds as less reliable data




#spp richness 
c_scales = c(5, 10, 25, 50) #50 stops = scale of 1 single route 
output = c()
for (scale in c_scales) {
  numGroups = floor(50/scale)
  for (g in 1:numGroups) {
    groupedCols = paste("Stop", ((g-1)*scale + 1):(g*scale), sep = "")
    temp = occ_counts2(fifty_bestAous, groupedCols, scale)
    
    temp = mutate(temp, scale = scale, stops = g)
    output = rbind(output, temp) 
  }
}

bbs_richness = data.frame(output)

#write to file in data folder or on personal BioArk folder; too big to transfer efficiently
write.csv(bbs_richness, "bbs_richness.csv", row.names = FALSE)
