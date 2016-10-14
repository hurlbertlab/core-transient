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
#### GLM of all matrices not just subset 
glm_occ_rand_site = glmer(cbind(sp_success, sp_fail) ~ cs(comp_scaled) + 
                            abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI) + (1|stateroute:Species), family = binomial(link = logit), data = occumatrix)
summary(glm_occ_rand_site) 

glm_abun_rand_site = glmer(cbind(sp_success_abun, sp_fail_abun) ~ cs(comp_scaled) + 
                             abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI) + (1|stateroute:Species), family = binomial(link = logit), data = occumatrix)
summary(glm_abun_rand_site) 

#### PLOTTING MODELS ####
ggplot(data = occumatrix, aes(x = comp_scaled, y = FocalOcc)) +stat_smooth(data=glm_occ_rand_site, lwd = 1.5) +xlab("Scaled Competitor Abundance")+ylab("Focal Occupancy") +theme_bw() +theme(axis.title.x=element_text(size=24),axis.title.y=element_text(size=24, angle=90), axis.text=element_text(size=12)) + theme(plot.margin = unit(c(.5,6,.5,.5),"lines")) 
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