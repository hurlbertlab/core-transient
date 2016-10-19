library(lme4)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plyr)
#### ---- GLM fitting  ---- ####
occumatrix = read.csv('occumatrix.csv', header = TRUE)
# add on success and failure columns by creating # of sites where birds were found
# and # of sites birds were not found from original bbs data
# create counter column to sum across years
subfocalspecies = read.csv(" subfocalspecies.csv", header = TRUE)

# using equation species sum*Focal occ to get success and failure for binomial anlaysis
occumatrix$sp_success = as.factor(occumatrix$numyears * occumatrix$FocalOcc)
occumatrix$sp_fail = as.factor(occumatrix$numyears * (1 - occumatrix$FocalOcc))

# using equation species sum*Focal abun to get success and failure for binomial anlaysis
occumatrix$sp_success_abun = as.factor(occumatrix$numyears * occumatrix$FocalAbundance)
occumatrix$sp_fail_abun = as.factor(occumatrix$numyears * (1 - occumatrix$FocalAbundance))

cs <- function(x) scale(x,scale=TRUE,center=TRUE)
# source: http://permalink.gmane.org/gmane.comp.lang.r.lme4.devel/12080
########################################################################### NLCD
nlcd = read.csv('Z:/GIS/birds/NLCD_buffers/BBS_NLCD_400_m_buffer.csv', header = TRUE)
# summing dediduous forest (41), evergreen forest (42), mixed forest (43), all have >20% total vegetation cover
nlcd$forest = (nlcd$NLCD.41 + nlcd$NLCD.42 + nlcd$NLCD.43)/nlcd$SUM

occumatrix1 = merge(occumatrix, nlcd[, c('RT..NO.', 'forest')], by.x = "stateroute", by.y = "RT..NO.")

#### GLM of all matrices not just subset #### INCLUDES LC
glm_occ_rand_site = glmer(cbind(sp_success, sp_fail) ~ cs(comp_scaled) + 
                            abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI) + forest + (1|stateroute:Species), family = binomial(link = logit), data = occumatrix1)
summary(glm_occ_rand_site) 

glm_abun_rand_site = glmer(cbind(sp_success_abun, sp_fail_abun) ~ cs(comp_scaled) + 
                             abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI) + (1|stateroute:Species), family = binomial(link = logit), data = occumatrix)
summary(glm_abundance_rand_site) 

#### PLOTTING MODELS ####
ggplot(data = occumatrix1, aes(x = comp_scaled, y = FocalOcc)) +stat_smooth(data=glm_occ_rand_site, lwd = 1.5) +xlab("Scaled Competitor Abundance")+ylab("Focal Occupancy") +theme_bw() +theme(axis.title.x=element_text(size=24),axis.title.y=element_text(size=24, angle=90), axis.text=element_text(size=12)) + theme(plot.margin = unit(c(.5,6,.5,.5),"lines")) 
ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/glmoutput.png")

ggplot(data = occumatrix, aes(x = comp_scaled, y = FocalAbundance)) +stat_smooth(data=glm_abun_rand_site, lwd = 1.5) +theme_bw()

####### WORKING ##########################################################################################################
pTemp = predict(glm_occ_rand_site, newdata=with(occumatrix,data.frame(zTemp=0,comp_scaled,zPrecip,zElev,zEVI,stateroute,Species, FocalOcc)), allow.new.levels = TRUE) #predict values assuming zTemp=0

inverselogit <- function(p) {exp(p)/(1+exp(p))} 
newintercept <- function(p) {mean(exp(p)/(1+exp(p)))} 

# this relationship should be negative
ggplot(data = occumatrix, aes(x = abs(zTemp), y = FocalOcc)) + 
  stat_function(fun=inverselogit, color = "blue", lwd=2) + 
  geom_point(colour="black", shape=19, alpha = 0.2) + theme_classic()
ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/logittemp.png")

ggplot(data = occumatrix, aes(x = abs(zEVI), y = FocalOcc)) + 
  stat_function(fun=inverselogit, color = "blue") + 
  geom_point(colour="black", shape=19, alpha = 0.2)+ theme_classic()
ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/logitevi.png")

ggplot(data = occumatrix, aes(x = abs(zElev), y = FocalOcc)) + 
  stat_function(fun=inverselogit, color = "blue") + 
  geom_point(colour="black", shape=19, alpha = 0.2)+ theme_classic()
ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/logitelev.png")

ggplot(data = occumatrix, aes(x = abs(zPrecip), y = FocalOcc)) + 
  stat_function(fun=inverselogit, color = "blue") + 
  geom_point(colour="black", shape=19, alpha = 0.2)+ theme_classic()
ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/logitprecip.png")

ggplot(data = occumatrix, aes(x = comp_scaled, y = FocalOcc)) + 
  stat_function(fun=inverselogit, color = "blue") + 
  geom_point(colour="black", shape=19, alpha = 0.2)+ theme_classic()


hist(occumatrix$zTemp)
hist(p) # d/n look right
  
ggplot(data = occumatrix, aes(x = zTemp, y = FocalOcc)) + abline(glm_occ_rand_site)+geom_point(colour="black", shape=19, alpha = 0.2)

temperature = ggplot(data = occumatrix, aes(x = zTemp, y = FocalOcc))+geom_point(colour="black", shape=19, alpha = 0.2)  + stat_smooth(data=glm_occ_rand_site, lwd = 1.5, se = FALSE)+xlab("Mean Temperature Deviation")+ylab("Focal Occupancy")+ geom_vline(xintercept = newintercept, colour="red", linetype = "longdash") +theme_bw() +theme_bw() +theme(axis.title.x=element_text(size=28),axis.title.y=element_text(size=28, angle=90), axis.text=element_text(size=12)) + theme(plot.margin = unit(c(.5,6,.5,.5),"lines"))#+ annotate("text", x = 3, y = 0.56, label = "Environmental centroid\n for focal species", size=7,vjust=0.5, color = "black")

ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/glmtemp.png")

pElev = predict(glm_occ_rand_site, newdata=with(occumatrix1,data.frame(zTemp,comp_scaled,zPrecip,zElev=0,zEVI,stateroute,Species, FocalOcc,forest)), allow.new.levels = TRUE) #predict values assuming zElev=0

newintercept1 = mean(exp(pElev)/(1+exp(pElev))) #mean of the inverse logit of those values.

ggplot(data = occumatrix, aes(x = zElev, y = FocalOcc)) +stat_smooth(data=glm_occ_rand_site, lwd = 1.5, se = FALSE)+geom_point(colour="black", shape=19, alpha = 0.2) +xlab("Mean Elevation Deviation")+ylab("Focal Occupancy")+ geom_vline(xintercept = newintercept1, colour="red", linetype = "longdash") +theme_bw() +theme_bw() +theme(axis.title.x=element_text(size=28),axis.title.y=element_text(size=28, angle=90), axis.text=element_text(size=12)) + theme(plot.margin = unit(c(.5,6,.5,.5),"lines"))
ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/glmelev.png")

pPrecip = predict(glm_occ_rand_site, newdata=with(occumatrix1,data.frame(zTemp,comp_scaled,zPrecip=0,zElev,zEVI,stateroute,Species, FocalOcc,forest)), allow.new.levels = TRUE) #predict values assuming zPrecip=0

newintercept2 = mean(exp(pPrecip)/(1+exp(pPrecip))) #mean of the inverse logit of those values.

ggplot(data = occumatrix, aes(x = zPrecip, y = FocalOcc)) +stat_smooth(data=glm_occ_rand_site, lwd = 1.5, se = FALSE)+geom_point(colour="black", shape=19, alpha = 0.2) +xlab("Mean Precipitation Deviation")+ylab("Focal Occupancy")+ geom_vline(xintercept = newintercept2, colour="red", linetype = "longdash") +theme_bw() +theme_bw() +theme(axis.title.x=element_text(size=28),axis.title.y=element_text(size=28, angle=90), axis.text=element_text(size=12)) + theme(plot.margin = unit(c(.5,6,.5,.5),"lines"))
ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/glmprecip.png")

pEVI = predict(glm_occ_rand_site, newdata=with(occumatrix1,data.frame(zTemp,comp_scaled,zPrecip,zElev,zEVI=0,stateroute,Species, FocalOcc,forest)), allow.new.levels = TRUE) #predict values assuming zPrecip=0

newintercept3 = mean(exp(pEVI)/(1+exp(pEVI))) #mean of the inverse logit of those values.

ggplot(data = occumatrix, aes(x = zEVI, y = FocalOcc)) +stat_smooth(data=glm_occ_rand_site, lwd = 1.5, se = FALSE)+geom_point(colour="black", shape=19, alpha = 0.2) +xlab("Mean Vegetation Deviation")+ylab("Focal Occupancy")+ geom_vline(xintercept = newintercept3, colour="red", linetype = "longdash") +theme_bw() +theme_bw() +theme(axis.title.x=element_text(size=28),axis.title.y=element_text(size=28, angle=90), axis.text=element_text(size=12)) + theme(plot.margin = unit(c(.5,6,.5,.5),"lines"))
ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/glmevi.png")

ggplot(data = occumatrix, aes(x = comp_scaled, y = FocalOcc)) +stat_smooth(data=glm_occ_rand_site, lwd = 1.5, se = FALSE)+geom_point(colour="black", shape=19, alpha = 0.2) +xlab("Mean Vegetation Deviation")+ylab("Focal Occupancy")+ geom_vline(xintercept = newintercept3, colour="red", linetype = "longdash") +theme_bw() +theme_bw() +theme(axis.title.x=element_text(size=28),axis.title.y=element_text(size=28, angle=90), axis.text=element_text(size=12)) + theme(plot.margin = unit(c(.5,6,.5,.5),"lines"))

#### ---- Plotting GLMs ---- ####
# Making pdf of ranges for each focal spp
pdf('precip_Reg.pdf', height = 8, width = 10)
par(mfrow = c(3, 4))
# Plotting basic lms to understand relationships
for(sp in subfocalspecies){ 
  print(sp)
  psub = occumatrix[occumatrix$Species == sp,]
  glm_occ_rand_site = glmer(cbind(sp_success, sp_fail) ~ cs(comp_scaled) + 
                              abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI) + (1|stateroute:Species), family = binomial(link = logit), data = psub)
  
  tes = ggplot(data = psub, aes(x = zPrecip, y = FocalOcc)) +stat_smooth(data=glm_occ_rand_site, lwd = 1.5,se = FALSE) +xlab(psub$Species)+theme_bw()
  plot(tes)
}
dev.off()
# Making pdf of ranges for each focal spp
pdf('Temp_Reg.pdf', height = 8, width = 10)
par(mfrow = c(3, 4))
# Plotting basic lms to understand relationships
for(sp in subfocalspecies){ 
  print(sp)
  psub = occumatrix[occumatrix$Species == sp,]
  glm_occ_rand_site = glmer(cbind(sp_success, sp_fail) ~ cs(comp_scaled) + 
                              abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI) + (1|stateroute:Species), family = binomial(link = logit), data = psub)
  
  tes = ggplot(data = psub, aes(x = zTemp, y = FocalOcc)) +stat_smooth(data=glm_occ_rand_site, lwd = 1.5,se = FALSE) +xlab(psub$Species) +theme_bw()
  plot(tes)
}
dev.off()
# Making pdf of ranges for each focal spp
pdf('GLM_Reg.pdf', height = 8, width = 10)
par(mfrow = c(3, 4))
# Plotting basic lms to understand relationships
for(sp in subfocalspecies){ 
  print(sp)
  psub = occumatrix[occumatrix$Species == sp,]
  glm_occ_rand_site = glmer(cbind(sp_success, sp_fail) ~ cs(comp_scaled) + 
                              abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI) + (1|stateroute:Species), family = binomial(link = logit), data = psub)
  
  tes = ggplot(data = psub, aes(x = comp_scaled, y = FocalOcc)) +stat_smooth(data=glm_occ_rand_site, lwd = 1.5,se = FALSE) +theme_bw()
  plot(tes)
}
dev.off()

###### plots for poster ######
all_expected_pres = read.csv("all_expected_pres.csv", header = TRUE)
plotsub = all_expected_pres[all_expected_pres$AOU == 7280,] # red-breasted nuthatch
proutes = plotsub$stateroute
comp1 = all_expected_pres[all_expected_pres$AOU == 7260|all_expected_pres$AOU == 7270,] # Brown Creeper
comp1plot = comp1[comp1$stateroute %in% proutes,]
#comp2 = plotdata[plotdata$AOU == 7270,] # White-breasted Nuthatch
#comp2plot = comp2[comp2$stateroute %in% proutes,]

map("state") 
Red_breasted_Nuthatch = points(plotsub$Longi, plotsub$Lati, col = "black",  pch = 16, cex = plotsub$FocalOcc*6)
Brown_Creeper = points(comp1plot$Longi, comp1plot$Lati, col = alpha("darkorchid1", 0.5),  pch = 16, cex = comp1$comp_scaled*6)
legend("bottomleft", legend = c("Red-breasted Nuthatch", "Competitors"), col = c("black","darkorchid1"), pch = 19, cex = 1)

# showing the number of species present at each route
#numspp_route = focalcompoutput %>%
# group_by(stateroute) %>%
# summarise(numspp = n_distinct(FocalAOU))
#numspp = merge(numspp_route, latlongs, by = "stateroute" )
#map("state") 
#points(numspp$Longi, numspp$Lati, col = "dark green",  pch = 20, cex = numspp$numspp/5)
envoutput = read.csv("envoutput.csv", header = TRUE)
envoutput$X <- NULL
envloc = read.csv("envloc.csv", header = TRUE)
#####PLOTTING variance partitioning
## Creating env data table to plot ranked data

envloc$EW <- 0
envloc$EW[envloc$Long > -98.583333] <- 1 ## from https://tools.wmflabs.org/geohack/geohack.php?pagename=Geographic_center_of_the_contiguous_United_States&params=39_50_N_98_35_W_region:US-KS_type:landmark&title=Geographic+Center+of+the+Contiguous+United+States
# 1 = East

nrank = envoutput %>% 
  mutate(rank = row_number(-ENV))# change here for comp
envflip = gather(nrank, "Type", "value", 2:5)

envflip$rank <- factor(envflip$rank, levels = envflip$rank[order(envflip$rank)])
envflip = plyr::arrange(envflip,(envflip$rank),envflip$FocalAOU)
envflip = merge(envflip, envloc[,c("FocalAOU", "EW")], by = "FocalAOU")

envrank = envflip %>% 
  group_by(Type == 'ENV') %>% # change here for comp
  mutate(rank = row_number(-value)) # need to get just the envs to rank, then plot
envrank <- envrank[order(envrank$rank),]


# Stacked bar plot for each focal aou
ggplot(data=envflip, aes(x=factor(FocalAOU), y=value, fill=Type)) + geom_bar(stat = "identity") + xlab("Focal AOU") + ylab("Percent Variance Explained") + theme(axis.text.x=element_text(angle=90,size=10,vjust=0.5)) + theme_classic()

### CREATE LABEL DF FAMilY ########
lab1 = filter(envflip, Type == "ENV") # change here for comp
lab1$Fam_abbrev = lab1$Family
lab1$Fam_abbrev = gsub('Emberizidae','E', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Turdidae','Tu', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Fringillidae','F', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Parulidae','P', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Tyrannidae','Ty', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Mimidae','M', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Hirundinidae','H', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Regulidae','R', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Vireonidae','V', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Aegithalidae','A', lab1$Fam_abbrev)                        
lab1$Fam_abbrev = gsub('Corvidae','Co', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Troglodytidae','T', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Certhiidae','C', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Cuculidae','Cu', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Sittidae','S', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Icteridae','I', lab1$Fam_abbrev)
lab1$Fam_abbrev = gsub('Picidae','Pi', lab1$Fam_abbrev)

lab1$Fam_abbrevf = as.factor(as.character(lab1$Fam_abbrev))
lab1$Fam_abbrevf = as.factor(as.numeric(lab1$Fam_abbrevf))
lab1$Fam_abbrevf = gsub('1','hello', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('2','its', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('3','meee', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('4','was', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('5','wondering', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('6','iffffff', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('7','after', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('8','all', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('9','these', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('hello0','years', lab1$Fam_abbrevf)                        
lab1$Fam_abbrevf = gsub('hellohello','youd', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('helloits','like', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('hellomeee','tooooo', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('hellowas','meet', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('hellowondering','gooooo', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('helloiffffff','over', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('helloafter','everything', lab1$Fam_abbrevf)

lab1$Fam_abbrevf = gsub('hello','#000000', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('its','#000000', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('meee','#000000', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('was','#000000', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('wondering','#9ecae1', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('iffffff','#000000', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('after','#000000', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('all','#000000', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('these','#000000', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('years','#0080ff', lab1$Fam_abbrevf)                       
lab1$Fam_abbrevf = gsub('youd','#000000', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('like','#000000', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('tooooo','#000000', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('meet','#000000', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('gooooo','#7f7fff', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('over','#0000ff', lab1$Fam_abbrevf)
lab1$Fam_abbrevf = gsub('everything','#048691', lab1$Fam_abbrevf)
famlabel = lab1$Fam_abbrev
####### OTHER LABEL ######
lab1$mig_abbrev = lab1$migclass
lab1$mig_abbrev = gsub("neotrop", 'L', lab1$mig_abbrev)
lab1$mig_abbrev = gsub("resid", 'R', lab1$mig_abbrev)
lab1$mig_abbrev = gsub("short", 'S', lab1$mig_abbrev)
lab1$mig_abbrevf = as.factor(as.character(lab1$mig_abbrev))
lab1$mig_abbrevf = as.factor(as.numeric(lab1$mig_abbrevf))
lab1$mig_abbrevf = gsub('1','hello', lab1$mig_abbrevf)
lab1$mig_abbrevf = gsub('2','its', lab1$mig_abbrevf)
lab1$mig_abbrevf = gsub('3','meee', lab1$mig_abbrevf)
lab1$mig_abbrevf = gsub('hello','#bae4b3', lab1$mig_abbrevf)
lab1$mig_abbrevf = gsub('its','#31a354', lab1$mig_abbrevf)
lab1$mig_abbrevf = gsub('meee','#006d2c', lab1$mig_abbrevf)
miglabel= lab1$mig_abbrev

lab1$trophlabel = lab1$Trophic.Group
lab1$trophlabel = gsub("frugivore", 'F', lab1$trophlabel)
lab1$trophlabel = gsub("granivore", 'G', lab1$trophlabel)
lab1$trophlabel = gsub("herbivore", 'H', lab1$trophlabel)
lab1$trophlabel = gsub("insct/om", 'X', lab1$trophlabel)
lab1$trophlabel = gsub("insectivore", 'I', lab1$trophlabel)
lab1$trophlabel = gsub("nectarivore", 'N', lab1$trophlabel)
lab1$trophlabel = gsub("omnivore", 'O', lab1$trophlabel)
lab1$trophlabelf = as.factor(as.character(lab1$trophlabel))
lab1$trophlabelf = as.factor(as.numeric(lab1$trophlabelf))
lab1$trophlabelf = gsub('1','hello', lab1$trophlabelf)
lab1$trophlabelf = gsub('2','its', lab1$trophlabelf)
lab1$trophlabelf = gsub('3','meee', lab1$trophlabelf)
lab1$trophlabelf = gsub('4','ive', lab1$trophlabelf)
lab1$trophlabelf = gsub('hello','#fbb4b9', lab1$trophlabelf)
lab1$trophlabelf = gsub('its','#f768a1', lab1$trophlabelf)
lab1$trophlabelf = gsub('meee','#c51b8a', lab1$trophlabelf)
lab1$trophlabelf = gsub('ive','#7a0177', lab1$trophlabelf)

lab1$EW[lab1$EW == 1] <- "E"
lab1$EW[lab1$EW == 0] <- "W" 
###### PLOTTING #####
# Plot with ENV ranked in decreasing order
t = ggplot(data=envflip, aes(factor(rank), y=value, fill=factor(Type, levels = c("ENV","COMP","SHARED","NONE")))) + 
  geom_bar(stat = "identity")  + theme_classic() +
  theme(axis.text.x=element_text(angle=90,size=10,vjust=0.5)) + xlab("Focal Species") + ylab("Percent Variance Explained") +
  scale_fill_manual(values=c("#2ca25f","#dd1c77","#43a2ca","white"), labels=c("Environment", "Competition","Shared Variance", "")) +theme(axis.title.x=element_text(size=20),axis.title.y=element_text(size=20, angle=90),legend.title=element_text(size=12), legend.text=element_text(size=12)) + guides(fill=guide_legend(title=""))+ theme(plot.margin = unit(c(.5,6,.5,.5),"lines")) 

tt = t + annotate("text", x = 1:63, y = -.03, label = unique(envflip$ALPHA.CODE), angle=90,size=6,vjust=0.5, color = "black") + annotate("text", x = 1:63, y = -.06, label = lab1$Fam_abbrev, size=6,vjust=0.5, color = lab1$Fam_abbrevf, fontface =2) + annotate("text", x = 1:63, y = -.08, label = lab1$mig_abbrev, size=6,vjust=0.5, color = lab1$mig_abbrevf, fontface =2) + annotate("text", x = 1:63, y = -.1, label = lab1$trophlabel, size=6,vjust=0.5, color = lab1$trophlabelf, fontface =2) + annotate("text", x = 1:63, y = -.12, label = lab1$EW, angle=90,size=6,vjust=0.5, color = "black", fontface =2)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.ticks=element_blank(), axis.text.y=element_text(size = 20)) 
plot(tt)

ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/barplot.pdf", height = 26, width = 34)

#Violin plots w location, trophic group, mig
ggplot(envflip, aes(x = Type, y = value, color = Type)) + geom_violin() 

ggplot(envloc, aes(x = FocalAOU, y = factor(EW))) + geom_violin() + scale_x_discrete(labels=c("West", "East")) 

##################### TRAITS Model ####################################
env_lm = subset(envflip, Type == 'ENV')

env_traits = lm(value ~ Trophic.Group + migclass + EW, data = env_lm)
summary(env_traits) 

### attempt to plot observed vs expected
plot(env_lm$value, env_traits$value, xlab="predicted", ylab = "observed")
abline(env_traits)

comp_lm = subset(envflip, Type == 'COMP')

comp_traits = lm(value ~ Trophic.Group + migclass + EW, data = comp_lm)
summary(comp_traits) 

env_sum = subset(envflip, Type != 'NONE')
total = env_sum %>% 
  group_by(FocalAOU) %>%
 summarise(sum(value))

total_traits = lm(value ~ Trophic.Group + migclass + EW, data = env_sum)
summary(total_traits)

# R2 plot - lm in ggplot
envoutputa = read.csv("envoutputa.csv", header = TRUE)
names(envoutputa) = c("FocalAOU", "ENV", "COMP", "SHARED", "NONE")
R2plot = merge(envoutput, envoutputa, by = "FocalAOU")

tomerge = c()
for (s in subfocalspecies$x) {
  spsub = subset(R2plot,FocalAOU == s)
  print(s)
  total.x = sum(spsub$COMP.x + spsub$ENV.x + spsub$SHARED.x)
  total.y = sum(spsub$COMP.y + spsub$ENV.y + spsub$SHARED.y)
  tomerge = rbind(tomerge, c(s, total.x, total.y))
}
tomerge = data.frame(tomerge)
names(tomerge) = c("FocalAOU","Total.x", "Total.y")

# R2 plot
R2plot2 = merge(R2plot, tomerge, by = "FocalAOU")
#lm(formula = COMP.y ~ COMP.x, data = R2plot2)
#lm(formula = ENV.y ~ ENV.x, data = R2plot2)
#lm(formula = Total.y ~ Total.x, data = R2plot2)

ggplot(R2plot2, aes(x = COMP.x, y = COMP.y)) +theme_bw()+ theme(axis.title.x=element_text(size=10),axis.title.y=element_text(size=10, angle=90)) + xlab("Occupancy R2") + ylab("Abundance R2") + geom_point(col = "#dd1c77", cex =4, shape=24) + geom_point(data = R2plot2, aes(x = ENV.x, y = ENV.y), shape = 16, col = "#2ca25f", cex =4, stroke = 1) + geom_point(data = R2plot2, aes(Total.x,Total.y), shape = 3, col = "#43a2ca", cex =5, stroke = 1) +geom_abline(intercept = 0, slope = 1, col = "black", lwd = 1.25)+ theme(axis.text.x=element_text(size = 20),axis.ticks=element_blank(), axis.text.y=element_text(size=20))+geom_smooth(method='lm', se=FALSE, col="#dd1c77",linetype="dotdash") + geom_abline(intercept= 0.03536,slope=0.43437, col = "#2ca25f", lwd = 1,linetype="dotdash")+ geom_smooth(method="lm", se= F, size = 1, aes(linetype = "dotdash", group = ENV.y))+geom_abline(intercept=0.1424,slope=0.4193, col = "#43a2ca", lwd = 1,linetype="dotdash")
ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/occvabun.png")




# R2 plot - glm violin plots
ggplot(R2plot2, aes(x = FocalAOU, y = Total.x)) + geom_violin(lwd = 2, fill = "grey", color = "grey") + xlab("Focal Species") + ylab("Total R2")+ theme_bw()+theme(axis.title.x=element_text(size=30),axis.title.y=element_text(size=30, angle=90)) + theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.ticks=element_blank(), axis.text.y=element_text(size=28, angle=90))

ggplot(R2plot2, aes(x = FocalAOU, y = COMP.x)) + geom_violin(lwd = 2, fill = "#dd1c77", color = "#dd1c77") + xlab("Focal Species") + ylab("Competition R2")+ theme_bw()+theme(axis.title.x=element_text(size=30),axis.title.y=element_text(size=30, angle=90)) + theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.ticks=element_blank(), axis.text.y=element_text(size=28, angle=90))

ggplot(R2plot2, aes(x = FocalAOU, y = ENV.x)) + geom_violin(lwd = 2, fill = "#2ca25f", color = "#2ca25f") + xlab("Focal Species") + ylab("Environment R2")+ theme_bw()+theme(axis.title.x=element_text(size=30),axis.title.y=element_text(size=30, angle=90),legend.title=element_text(size=12), legend.text=element_text(size=12)) + theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.ticks=element_blank(), axis.text.y=element_text(size=28, angle=90)) +scale_y_continuous(limits = c(0, 0.6))


#Coyle fig 1: Z:\Coyle\Projects\BBS Core\Final Analysis