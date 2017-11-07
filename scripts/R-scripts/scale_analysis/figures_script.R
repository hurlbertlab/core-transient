#Figures and tables 
#wd1: setwd("C:/git/core-transient")
#wd2: setwd("\\bioark.ad.unc.edu\HurlbertLab\Jenkins\Final folder") 

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

output_o = output %>% 
  spread(AOU, occ)
output_o = as.data.frame(output_o)

#remove na's 
output = na.omit(output)


density(output$occ)
par(mar=c(4,4,1,1)+0.5)
par(lend=2)
num.years = 15

pdf('C:/git/core-transient/output/plots/Molly_Plots/coyle_1.pdf', height = 8, width = 10)
# Add kernel density
partdensity = density(output_o[output_o>0],from=1/min(num.years),
                      to=(min(num.years)-1)/min(num.years),kernel='gaussian', na.rm=T, n=2000)
plot(partdensity$y~partdensity$x,
     main='',
     xlab='',
     ylab='',
     xlim=c(0,1),ylim=c(0,2.5),
     lwd=5,axes=F,type='l',lend=2
)

# Add breakpoints
segments(0.33,0,0.33,partdensity$y[which(round(partdensity$x,2)==0.33)[1]], lty=3,lwd=2) #v2
segments(0.66,0,0.66,partdensity$y[which(round(partdensity$x,2)==0.66)[1]], lty=3,lwd=2) #v2

# Add axes
axis(1,at=seq(0,1,0.2),pos=0,cex.axis=2)
axis(2, c(0,2),labels=c("",""))


# Add titles
title(main='',xlab='Proportion of time present at site',ylab='Probability Density',
      line=2,cex.lab=2.5)

# Add proportions
allsp = !is.na(occupancy.matrix)
coresp = output_o>=0.6667
occasp = output_o<0.3334

# text(0.66+(0.33/2),0.10,paste('(',round(sum(coresp,na.rm=T)/sum(allsp,na.rm=T)*100,1),' %)',sep=''),
# cex=3,font=1)
# text(0.33/2,0.10,paste('(',round(sum(occasp,na.rm=T)/sum(allsp,na.rm=T)*100,1),' %)',sep=''),
# cex=3,font=1)

#text(0.66+(0.33/2),0.35,'Core',cex=3,font=2) #v3
#text(0.33/2,0.35,'Transient',cex=3,font=2) #v3

dev.off()
#looks perfect 


#repeat for scale of 5 stop segment and scale of 66 routes 

####Figure 1b: at scale of 5 stop segments####
# Generic calculation of occupancy for a specified scale
#fix to run all at once, so no sep run for above-scale, USE occ-counts for both 

#scale of 5 segments (min) 
min_scales = c(5)
min_out = c()
for (s in min_scales) {
  numGroups = floor(50/s)
  for (g in 1:numGroups) {
    groupedCols = paste("Stop", ((g-1)*s + 1):(g*s), sep = "")
    temp = occ_counts(fifty_bestAous, groupedCols, s) 
    min_out = rbind(min_out, temp) 
  } 
}

#transform output into matrix for use with coylefig script 
min_out = min_out[, -3]
#need to avg occs between unique stateroute-AOU pairs since 5 for every 1 
min_out2 = min_out %>% 
  group_by(AOU, stateroute) %>% 
  summarise(occ = mean(occ)) %>% select(everything()) 
            


min_out3 = spread(data = min_out2, AOU, occ)

min_out2 = as.data.frame(min_out2)
min_out3 = as.data.frame(min_out3)
#remove na's 
min_out2 = na.omit(min_out2)


density(min_out2$occ)
par(mar=c(4,4,1,1)+0.5)
par(lend=2)
num.years = 15

pdf('C:/git/core-transient/output/plots/Molly_Plots/fig1b.pdf', height = 8, width = 10)
# Add kernel density
partdensity = density(min_out3[min_out3>0],from=1/min(num.years),
                      to=(min(num.years)-1)/min(num.years),kernel='gaussian', na.rm=T, n=2000)
plot(partdensity$y~partdensity$x,
     main='',
     xlab='',
     ylab='',
     xlim=c(0,1),ylim=c(0,2.5),
     lwd=5,axes=F,type='l',lend=2
)

# Add breakpoints
segments(0.33,0,0.33,partdensity$y[which(round(partdensity$x,2)==0.33)[1]], lty=3,lwd=2) #v2
segments(0.66,0,0.66,partdensity$y[which(round(partdensity$x,2)==0.66)[1]], lty=3,lwd=2) #v2

# Add axes
axis(1,at=seq(0,1,0.2),pos=0,cex.axis=2)
axis(2, c(0,2),labels=c("",""))


# Add titles
title(main='Scale of 1/10 of single route',xlab='Proportion of time present at site',ylab='Probability Density',
      line=2,cex.lab=2.5)

# Add proportions
allsp = !is.na(min_out3)
coresp = min_out3>=0.6667
occasp = min_out3<0.3334

# text(0.66+(0.33/2),0.10,paste('(',round(sum(coresp,na.rm=T)/sum(allsp,na.rm=T)*100,1),' %)',sep=''),
# cex=3,font=1)
# text(0.33/2,0.10,paste('(',round(sum(occasp,na.rm=T)/sum(allsp,na.rm=T)*100,1),' %)',sep=''),
# cex=3,font=1)

#text(0.66+(0.33/2),0.35,'Core',cex=3,font=2) #v3
#text(0.33/2,0.35,'Transient',cex=3,font=2) #v3

dev.off()

#again, perfect 

####Fig 1c: distribution at the maximum scale####
dist.df = read.csv("scripts/R-scripts/scale_analysis/dist_df.csv", header = TRUE)
bbs_above_guide = read.csv("scripts/R-scripts/scale_analysis/bbs_above_guide.csv", header = TRUE)
#groupcounts for each AOU for each year at scale of ONE stateroute 

#occ_counts function for calculating occupancy at any scale
#countcolumns can refer to the stops in a stateroute OR 
#it can refer to the associated secondary routes to aggregate across 

uniqrtes = unique(bbs_above_guide$stateroute) #all routes present are unique, still 953 which is great
nu = 66 # based on min common number in top 6 grid cells, see grid_sampling_justification script 
max_out = c()

#test example route 2010 and nu at 57 routes -> large scale, should have high occ 
for (r in uniqrtes) { #for each focal route
  #for each level of scale aggregated to each focal route
    
    #takes dist.df and generates a new list that changes based on which route in uniqrtes is being focused on 
    #and the length of the list varies with the scale or nu 
    
    tmp_rte_group = dist.df %>% #changes with size of nu but caps at 66
      filter(rte1 == r) %>% 
      top_n(66, desc(dist)) %>% #fixed ordering by including arrange parm, 
      #remove/skip top row 
      arrange(dist) %>%
      slice(1:nu) %>% 
      select(everything()) %>% data.frame()
    
    #takes varying list from above and uses it to subset the bbs data so that occ can be calculated for the cluster 
    
    focal_clustr = bbs_above_guide %>% 
      filter(stateroute %in% tmp_rte_group$rte2) #tmp_rte_group already ordered by distance so don't need 2x
    #(for a given focal rte, narrow input data to those nu secondary routes in focal cluster)
    #across 57 routes
    
    occ.summ = focal_clustr %>% #occupancy -> focal clustr should GROW with scale, larger avg pool -> 
      #increased likelihood that AOU will be present -> OH! I don't want stateroute in here! it doesn't matter! 
      #it just matters that it shows up in the cluster at all, not just the stateroutes that go in
      #how many years does each AOU show up in the cluster 
      select(year, AOU) %>% #duplicates remnant of distinct secondary routes - finally ID'd bug
      distinct() %>% #removing duplicates 09/20
      count(AOU) %>% #how many times does that AOU show up in that clustr that year 
      mutate(occ = n/15, stateroute = r) 
    
    max_out = rbind(max_out, occ.summ)
    
  }

    

#transform output into matrix for use with coylefig script 
max_out = max_out[, -2]

max_out_m = max_out %>% 
  spread(AOU, occ)
max_out_m = as.data.frame(max_out_m)
max_out = as.data.frame(max_out)
#remove na's from long form 
max_out = na.omit(max_out)


density(max_out$occ)
par(mar=c(4,4,1,1)+0.5)
par(lend=2)
num.years = 15

pdf('C:/git/core-transient/output/plots/Molly_Plots/fig1c.pdf', height = 8, width = 10)
# Add kernel density
test = ggplot(max_out, aes(occ))
test+geom_density(kernel = "gaussian")
#a little weird but SO much better than below coding 
#reconfig 1a and 1b to be ggplots then, futz with 

# 
# partdensity = density(max_out_m[max_out_m>0],from=1/min(num.years), #my smoothing bandwidth for this is perhaps inappropriate?
#                       to=(min(num.years)-1)/min(num.years),kernel='gaussian', na.rm=T, n=2000)
# plot(partdensity$y~partdensity$x,
#      main='',
#      xlab='',
#      ylab='',
#      #xlim=c(0,1),ylim=c(0,2.5),
#      lwd=5,axes=F,type='l',lend=2
# )

# Add breakpoints
segments(0.33,0,0.33,partdensity$y[which(round(partdensity$x,2)==0.33)[1]], lty=3,lwd=2) #v2
segments(0.66,0,0.66,partdensity$y[which(round(partdensity$x,2)==0.66)[1]], lty=3,lwd=2) #v2

# Add axes
axis(1,at=seq(0,1,0.2),pos=0,cex.axis=2)
axis(2, c(0,2),labels=c("",""))


# Add titles
title(main='At maximum scale',xlab='Proportion of time present at site',ylab='Probability Density',
      line=2,cex.lab=2.5)

# Add proportions
allsp = !is.na(max_out_m)
coresp = max_out_m>=0.6667
occasp = max_out_m<0.3334

# text(0.66+(0.33/2),0.10,paste('(',round(sum(coresp,na.rm=T)/sum(allsp,na.rm=T)*100,1),' %)',sep=''),
# cex=3,font=1)
# text(0.33/2,0.10,paste('(',round(sum(occasp,na.rm=T)/sum(allsp,na.rm=T)*100,1),' %)',sep=''),
# cex=3,font=1)

#text(0.66+(0.33/2),0.35,'Core',cex=3,font=2) #v3
#text(0.33/2,0.35,'Transient',cex=3,font=2) #v3

dev.off()
#looks SUPER weird and need to troubleshoot density dist 

