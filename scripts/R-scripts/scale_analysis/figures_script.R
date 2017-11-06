#Figures and tables 
#wd1: 
#wd2: setwd("\\bioark.ad.unc.edu\HurlbertLab\Jenkins\Final folder") 

#Figure 1: Bimodal dist images; number of spp on y vs # years present  
#A: original bimodal dist 
#B: distribution at smallest scales 
#C: distribution at max scale 

#refer to coylefig1a.R script in core-transient scripts folder for guidance 
#need to recreate spp_matrix with current data 
#AOU codes columns following first stateroute column 
#individual occ values for spp at each stateroute across the 15 year window 







fifty_allyears = read.csv(paste(BBS, "fifty_allyears.csv", sep = ""), header = TRUE) #using updated version, 50 stop data, 07/12
bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)

fifty_bestAous = fifty_allyears %>% 
  filter(AOU > 2880 & !(AOU >= 3650 & AOU <= 3810) & !(AOU >= 3900 & AOU <= 3910) & 
           !(AOU >= 4160 & AOU <= 4210) & AOU != 7010) #leaving out owls, waterbirds as less reliable data

#occ_counts function for calculating occupancy at any scale
#countcolumns can refer to the stops in a stateroute OR 
#it can refer to the associated secondary routes to aggregate across 
occ_counts = function(countData, countColumns, scale) {
  bbssub = countData[, c("stateroute", "year", "AOU", countColumns)] #these are our grouping vars
  bbssub$groupCount = rowSums(bbssub[, countColumns]) 
  bbsu = unique(bbssub[bbssub[, "groupCount"]!= 0, c("stateroute", "year", "AOU")]) 
  
  
  occ.summ = bbsu %>% #occupancy
    count(stateroute, AOU) %>%
    mutate(occ = n/15, AOU = AOU, stateroute = stateroute)
  return(occ.summ)
  
  }
  

# Generic calculation of occupancy for a specified scale
#fix to run all at once, so no sep run for above-scale, USE occ-counts for both 
b_scales = c(50)
output = c()
for (s in b_scales) {
  numGroups = floor(50/s)
  for (g in 1:numGroups) {
    groupedCols = paste("Stop", ((g-1)*s + 1):(g*s), sep = "")
    temp = occ_counts(fifty_bestAous, groupedCols, s) 
    output = rbind(output, temp) 
  } 
}

#transform output into matrix for use with coylefig script 
output = output[, -3]
#remove na's 
output = na.omit(output)


# output_o = output %>% not necessary 
#   spread(AOU, occ)
# output_o = as.matrix(output_o)

#repeat for scale of 5 stop segment and scale of 66 routes 

#scale of 5 segments (min) 
min_scales = c(5)
output2 = c()
for (s in min_scales) {
  numGroups = floor(50/s)
  for (g in 1:numGroups) {
    groupedCols = paste("Stop", ((g-1)*s + 1):(g*s), sep = "")
    temp = occ_counts(fifty_bestAous, groupedCols, s) 
    output2 = rbind(output2, temp) 
  } 
}



# plot(bbs_allscales$aveN~bbs_allscales$)


