library(lme4)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plyr)
library(lmtest)

# read in files created in data cleaning script
tax_code = read.csv("Tax_AOU_Alpha.csv", header = TRUE)
temp_occ = read.csv("bbs_sub1.csv", header=TRUE)
centroid=read.csv("centroid.csv", header=TRUE)
occuenv=read.csv("occuenv.csv", header=TRUE)
subfocalspecies = unique(occuenv$Species)
# rescaling all occupancy values  - odds ratio
# need to get rid of ones in order to not have infinity values 
edge_adjust = .005 
occuenv$FocalOcc_scale = (occuenv$FocalOcc * (1 - 2*edge_adjust)) + edge_adjust
# create logit transformation function, did on rescaled vals
occuenv$occ_logit =  log(occuenv$FocalOcc_scale/(1-occuenv$FocalOcc_scale)) 

##### LIN REG #######
# create beta output data frame
beta_lm = matrix(NA, nrow = 67, ncol = 10)
beta_abun = matrix(NA, nrow = 67, ncol = 10)

# for loop subsetting env data to expected occurrence for focal species
envoutput = c()
envoutputa = c()
for (sp in 1:length(subfocalspecies)){
  
  temp = subset(occuenv,occuenv$Species == subfocalspecies[sp])
  
  competition <- lm(temp$occ_logit ~  temp$comp_scaled) 
  # z scores separated out for env effects (as opposed to multivariate variable)
  env_z = lm(occ_logit ~ abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI), data = temp)
  # z scores separated out for env effects
  both_z = lm(temp$occ_logit ~  temp$comp_scaled + abs(temp$zTemp)+abs(temp$zElev)+abs(temp$zPrecip)+abs(temp$zEVI), data = temp)
  
  # abundance, not temp occ - same results?
  competition_abun <- lm(temp$FocalAbundance ~  temp$comp_scaled) 
  # z scores separated out for env effects - abundance
  env_abun = lm(temp$FocalAbundance ~ abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI), data = temp)
  # z scores separated out for env effects - abundance
  both_abun = lm(temp$FocalAbundance ~  comp_scaled + abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI), data = temp)
  
  beta_lm[sp,1] = sp
  beta_lm[sp,2] = summary(competition)$coef[2,"Estimate"]
  beta_lm[sp,3] = summary(competition)$coef[2,"Pr(>|t|)"]
  beta_lm[sp,4] = summary(competition)$r.squared #using multiple rsquared
  beta_lm[sp,5] = summary(env_z)$coef[2,"Estimate"]
  beta_lm[sp,6] = summary(env_z)$coef[2,"Pr(>|t|)"]
  beta_lm[sp,7] = summary(env_z)$r.squared 
  beta_lm[sp,8] = summary(both_z)$coef[2,"Estimate"]
  beta_lm[sp,9] = summary(both_z)$coef[2,"Pr(>|t|)"]
  beta_lm[sp,10] = summary(both_z)$r.squared 
  
  beta_abun[sp,1] = subfocalspecies[sp]
  beta_abun[sp,2] = summary(competition_abun)$coef[2,"Estimate"]
  beta_abun[sp,3] = summary(competition_abun)$coef[2,"Pr(>|t|)"]
  beta_abun[sp,4] = summary(competition_abun)$r.squared #using multiple rsquared
  beta_abun[sp,5] = summary(env_abun)$coef[2,"Estimate"]
  beta_abun[sp,6] = summary(env_abun)$coef[2,"Pr(>|t|)"]
  beta_abun[sp,7] = summary(env_abun)$r.squared 
  beta_abun[sp,8] = summary(both_abun)$coef[2,"Estimate"]
  beta_abun[sp,9] = summary(both_abun)$coef[2,"Pr(>|t|)"]
  beta_abun[sp,10] = summary(both_abun)$r.squared
  
  #variance_partitioning 
  ENV = summary(both_z)$r.squared - summary(competition)$r.squared
  print(ENV) #env only
  COMP = summary(both_z)$r.squared - summary(env_z)$r.squared
  print(COMP) #competition only
  SHARED = summary(competition)$r.squared - COMP
  print(SHARED) #shared variance
  NONE = 1 - summary(both_z)$r.squared
  print(NONE) #neither variance
  sp1 = unique(temp$Species)
  envoutput = rbind(envoutput, c(sp1, ENV, COMP, SHARED, NONE))
  
  #variance_partitioning 
  ENVa = summary(both_abun)$r.squared - summary(competition_abun)$r.squared
  
  COMPa = summary(both_abun)$r.squared - summary(env_abun)$r.squared
  
  SHAREDa = summary(competition_abun)$r.squared - COMP
  
  NONEa = 1 - summary(both_abun)$r.squared
  
  sp1 = unique(temp$Species)
  envoutputa = rbind(envoutputa, c(sp1, ENVa, COMPa, SHAREDa, NONEa))
}         
dev.off()

envoutput = data.frame(envoutput)
envoutputa = data.frame(envoutputa)
names(envoutput) = c("FocalAOU", "ENV", "COMP", "SHARED", "NONE")
names(envoutputa) = c("FocalAOU", "ENV", "COMP", "SHARED", "NONE")

envoutput = merge(envoutput, tax_code[,c('AOU_OUT', 'ALPHA.CODE')], by.x = 'FocalAOU', by.y = "AOU_OUT")
write.csv(envoutput, "envoutputa.csv", row.names = FALSE)
write.csv(envoutputa, "envoutputa.csv", row.names = FALSE)

envloc = merge(envoutput, centroid[, c("FocalAOU", "Long", "Lat")], by = 'FocalAOU', all = TRUE)

beta_lm = data.frame(beta_lm)
names(beta_lm) = c("FocalAOU", "Competition_Est", "Competition_P", "Competition_R2", "EnvZ_Est", "EnvZ_P", "EnvZ_R2", "BothZ_Est", "BothZ_P", "BothZ_R2")
beta_abun = data.frame(beta_abun)
names(beta_abun) = c("FocalAOU", "Competition_Est", "Competition_P", "Competition_R2", "EnvZ_Est", "EnvZ_P", "EnvZ_R2", "BothZ_Est", "BothZ_P", "BothZ_R2")

#### ---- GLM fitting  ---- ####
# add on success and failure columns by creating # of sites where birds were found
# and # of sites birds were not found from original bbs data
# occumatrix = merge(temp_occ, occuenv, by.x=c("Aou", "stateroute"),by.y=c("Species", "stateroute"))
occumatrix=occuenv
occumatrix$c_s = scale(occumatrix$comp_scaled, scale = T, center = T)
occumatrix$abTemp=abs(occumatrix$zTemp)
occumatrix$abElev=abs(occumatrix$zElev)
occumatrix$abPrecip=abs(occumatrix$zPrecip)
occumatrix$abEVI=abs(occumatrix$zEVI)
names(occumatrix)[1] = 'Species'

# using equation species sum*Focal occ to get success and failure for binomial anlaysis
occumatrix$sp_success = as.factor(occumatrix$nyears * occumatrix$FocalOcc)
occumatrix$sp_fail = as.factor(occumatrix$nyears * (1 - occumatrix$FocalOcc))

#### GLM of all matrices not just subset ####
glm_occ_rand_site = glmer(cbind(sp_success, sp_fail) ~ c_s + 
         abTemp + abElev + abPrecip + abEVI + (1|stateroute:Species), family = binomial(link = logit), data = occumatrix)
summary(glm_occ_rand_site) 

### FIX, Poisson
glm_abun_rand_site = glmer.nb(FocalAbundance ~ c_s + 
        abTemp + abElev + abPrecip + abEVI + (1|stateroute:Species), link=log, data = occumatrix)
summary(glm_abundance_rand_site) 

#### PLOTTING MODELS ####
ggplot(data = occumatrix, aes(x = comp_scaled, y = FocalOcc)) +stat_smooth(data=glm_occ_rand_site, lwd = 1.5) +xlab("Scaled Competitor Abundance")+ylab("Focal Occupancy") +theme_bw() +theme(axis.title.x=element_text(size=24),axis.title.y=element_text(size=24, angle=90), axis.text=element_text(size=12)) + theme(plot.margin = unit(c(.5,6,.5,.5),"lines")) 
ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/glmoutput.png")

ggplot(data = occumatrix, aes(x = comp_scaled, y = FocalAbundance)) +stat_smooth(data=glm_abun_rand_site, lwd = 1.5) +theme_bw()

####### GLM FIT PLOTS #################################################################################################
pTemp = predict(glm_occ_rand_site, newdata=with(occumatrix,data.frame(zTemp=0,comp_scaled,zPrecip,zElev,zEVI,stateroute,Species, FocalOcc)), allow.new.levels = TRUE) #predict values assuming zTemp=0

inverselogit <- function(p) {exp(p)/(1+exp(p))} 
newintercept <- function(p) {mean(exp(p)/(1+exp(p)))} 

# this relationship should be negative
ggplot(data = occumatrix, aes(x = abs(zTemp), y = FocalOcc)) + 
  geom_segment(aes(x = 0, y = 0.97569, xend = abs(max(occumatrix$zTemp)), yend = 0.97569 +(-0.05176*max(abs(occumatrix$zTemp)))), col = "dark green", lwd=2) +
  geom_point(colour="black", shape=18, alpha = 0.02,position=position_jitter(width=0,height=.02)) + theme_classic()
  #geom_abline(intercept=0.97569, slope= -0.05176, lwd=1, col="blue")
ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/logittemp.png")

ggplot(data = occumatrix, aes(x = abs(zEVI), y = FocalOcc)) + 
  geom_segment(aes(x = 0, y = 0.97569, xend = abs(max(occumatrix$zEVI)), yend = 0.97569 +(-0.05117*max(abs(occumatrix$zEVI)))), col = "dark green", lwd=2) +
  geom_point(colour="black", shape=18, alpha = 0.02,position=position_jitter(width=0,height=.02))+ theme_classic()
 # geom_abline(intercept=0.97569, slope= -0.05117, lwd=1, col="blue")
ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/logitevi.png")

ggplot(data = occumatrix, aes(x = abs(zElev), y = FocalOcc)) + 
  geom_segment(aes(x = 0, y = 0.97569, xend = abs(max(occumatrix$zElev)), yend = 0.97569 +(-0.02104*max(abs(occumatrix$zElev)))), col = "dark green", lwd=2)  + 
  geom_point(colour="black", shape=18, alpha = 0.02,position=position_jitter(width=0,height=.02))+ theme_classic()
ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/logitelev.png")

ggplot(data = occumatrix, aes(x = abs(zPrecip), y = FocalOcc)) + 
  geom_segment(aes(x = 0, y = 0.97569, xend = abs(max(occumatrix$zPrecip)), yend = 0.97569 +(-0.0187*max(abs(occumatrix$zPrecip)))), col = "dark green", lwd=2) +
  geom_point(colour="black", shape=18, alpha = 0.02,position=position_jitter(width=0,height=.02))+ theme_classic()
ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/logitprecip.png")

ggplot(data = occumatrix, aes(x = comp_scaled, y = FocalOcc)) + 
  stat_function(fun=inverselogit, color = "blue") + 
  geom_point(colour="black", shape=18, alpha = 0.02,position=position_jitter(width=0,height=.02))+ theme_classic()

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
inverselogit <- function(p) {exp(p)/(1+exp(p))} 

env_lm = subset(envflip, Type == 'ENV')

env_traits = lm(inverselogit(value) ~ Trophic.Group + migclass + EW, data = env_lm)
summary(env_traits) 

comp_lm = subset(envflip, Type == 'COMP')

comp_traits = lm(inverselogit(value) ~ Trophic.Group + migclass + EW, data = comp_lm)
summary(comp_traits) 

env_sum = subset(envflip, Type != 'NONE')
total = env_sum %>% 
  group_by(FocalAOU) %>%
 summarise(sum(value))

total_traits = lm(inverselogit(value) ~ Trophic.Group + migclass + EW, data = env_sum)
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

ggplot(R2plot2, aes(x = COMP.x, y = COMP.y)) +theme_bw()+ theme(axis.title.x=element_text(size=35),axis.title.y=element_text(size=35, angle=90)) + xlab("Occupancy R2") + ylab("Abundance R2") + geom_point(col = "#dd1c77", cex =4, shape=24) + geom_point(data = R2plot2, aes(x = ENV.x, y = ENV.y), shape = 16, col = "#2ca25f", cex =4, stroke = 1) + geom_point(data = R2plot2, aes(Total.x,Total.y), shape = 3, col = "#43a2ca", cex =5, stroke = 1) +geom_abline(intercept = 0, slope = 1, col = "red", lwd = 1.25)+ theme(axis.text.x=element_text(size = 20),axis.ticks=element_blank(), axis.text.y=element_text(size=2))
ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/occvabun.png")

# R2 plot - glm violin plots
ggplot(R2plot2, aes(x = FocalAOU, y = Total.x)) + geom_violin(lwd = 2, fill = "grey", color = "grey") + xlab("Focal Species") + ylab("Total R2")+ theme_bw()+theme(axis.title.x=element_text(size=30),axis.title.y=element_text(size=30, angle=90)) + theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.ticks=element_blank(), axis.text.y=element_text(size=28, angle=90))

ggplot(R2plot2, aes(x = FocalAOU, y = COMP.x)) + geom_violin(lwd = 2, fill = "#dd1c77", color = "#dd1c77") + xlab("Focal Species") + ylab("Competition R2")+ theme_bw()+theme(axis.title.x=element_text(size=30),axis.title.y=element_text(size=30, angle=90)) + theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.ticks=element_blank(), axis.text.y=element_text(size=28, angle=90))

ggplot(R2plot2, aes(x = FocalAOU, y = ENV.x)) + geom_violin(lwd = 2, fill = "#2ca25f", color = "#2ca25f") + xlab("Focal Species") + ylab("Environment R2")+ theme_bw()+theme(axis.title.x=element_text(size=30),axis.title.y=element_text(size=30, angle=90),legend.title=element_text(size=12), legend.text=element_text(size=12)) + theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.ticks=element_blank(), axis.text.y=element_text(size=28, angle=90)) +scale_y_continuous(limits = c(0, 0.6))


#Coyle fig 1: Z:\Coyle\Projects\BBS Core\Final Analysis

#### basement ### poster
forplots = merge(Hurlbert_o[, c("AOU","Trophic.Group","Foraging","migclass")], AOUsub4[, c("AOU","Family", "CommonName")], by = "AOU")
# midpoint long of US is -98.5795, so 1 indicates east of that line, 0 = west

# Dark-eyed Junko and Winter Wren need AOUs changed
tax_code$AOU_OUT[tax_code$AOU_OUT == 5677] = 5660
tax_code$AOU_OUT[tax_code$AOU_OUT == 7220] = 7222
forplots$AOU[forplots$AOU == 7220] = 7222

envloc = merge(envoutput, centroid[, c("FocalAOU", "Long", "Lat")], by = 'FocalAOU', all = TRUE)
envloc$EW <- 0
envloc$EW[envloc$Long > -98.583333] <- 1 ## from https://tools.wmflabs.org/geohack/geohack.php?pagename=Geographic_center_of_the_contiguous_United_States&params=39_50_N_98_35_W_region:US-KS_type:landmark&title=Geographic+Center+of+the+Contiguous+United+States
# 1 = East
#### ---- Plotting LMs ---- ####
# Making pdf of ranges for each focal spp
pdf('Lin_Reg.pdf', height = 8, width = 10)
par(mfrow = c(3, 4))
# Plotting basic lms to understand relationships
for(sp in subfocalspecies){ 
  print(sp)
  psub = occuenv[occuenv$Species == sp,]
  #psub = filter(psub, occ_logit < 9) # eliminating 100% occ?
  title = unique(psub$FocalSciName)
  #ggplot(psub, aes(x = psub$occ_logit, y = psub$MainCompSum)) + geom_point(data=psub, pch = 16)+geom_smooth(method = "lm", col = "red")+ theme_classic()+ xlab("Focal Occupancy")+ylab("Competitor Abundance")+ggtitle(title)
  
  #+ ggtitle(title[1])
  competition <- lm(psub$occ_logit ~  psub$comp_scaled) 
  # z scores separated out for env effects (as opposed to multivariate variable)
  env_z = lm(occ_logit ~ abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI), data = psub)
  # z scores separated out for env effects
  both_z = lm(psub$occ_logit ~  psub$comp_scaled + abs(psub$zTemp)+abs(psub$zElev)+abs(psub$zPrecip)+abs(psub$zEVI), data = psub)
  
  plot(psub$comp_scaled, psub$occ_logit, pch = 20, xlab = "Main Competitor Abundance", ylab = "Focal Occupancy (logit link)", main = psub$FocalSciName[1], sub = "Competition", abline(competition, col = "red"))
  plot(psub$comp_scaled,psub$occ_logit,  pch = 20, xlab = "Main Competitor Abundance", ylab = "Focal Occupancy (logit link)", main = psub$FocalSciName[1], sub = "Environment", abline(env_z, col = "red"))
  plot(psub$comp_scaled, psub$occ_logit,  pch = 20, xlab = "Main Competitor Abundance", ylab = "Focal Occupancy (logit link)", main = psub$FocalSciName[1], sub = "Both", abline(both_z, col = "red"))
}
dev.off()

# Plotting basic lm hists to understand relationships btwn occ and abun
hist(beta_lm$Competition_R2, 10, main = "R Squared Distribution for Competition", xlab = "Competition R Squared")
hist(beta_lm$EnvZ_R2, 10, main = "R Squared Distribution for Env", xlab = "Env R Squared")
hist(beta_lm$BothZ_R2, 10, main = "R Squared Distribution for Both", xlab = "Both R Squared")

hist(beta_lm$Competition_Est, 10, main = "Slope Distribution for Competition", xlab = "Competition Slope")
abline(v = mean(beta_lm$Competition_Est), col = "red", lwd = 3)
hist(beta_lm$EnvZ_Est, 10, main = "Slope Distribution for Environment", xlab = "Environment Slope")
abline(v = mean(beta_lm$EnvZ_Est), col = "red", lwd = 3)
hist(beta_lm$BothZ_Est, 10, main = "Slope Distribution for Both", xlab = "Both Slope")
abline(v = mean(beta_lm$BothZ_Est), col = "red", lwd = 3)

hist(beta_abun$Competition_R2, 10, main = "R Squared Distribution for Competition", xlab = "Competition R Squared")
hist(beta_abun$EnvZ_R2, 10, main = "R Squared Distribution for Env", xlab = "Env R Squared")
hist(beta_abun$BothZ_R2, 10, main = "R Squared Distribution for Both", xlab = "Both R Squared")

hist(beta_abun$Competition_Est, 10, main = "Slope Distribution for Competition", xlab = "Competition Slope")
abline(v = mean(beta_abun$Competition_Est), col = "red", lwd = 3)
hist(beta_abun$EnvZ_Est, 10, main = "Slope Distribution for Environment", xlab = "Environment Slope")
abline(v = mean(beta_abun$EnvZ_Est), col = "red", lwd = 3)
hist(beta_abun$BothZ_Est, 10, main = "Slope Distribution for Both", xlab = "Both Slope")
abline(v = mean(beta_abun$BothZ_Est), col = "red", lwd = 3)

beta_lm$sumR2 = beta_lm$BothZ_R2+beta_lm$Competition_R2+beta_lm$EnvZ_R2  # tbd if we'll keep this
