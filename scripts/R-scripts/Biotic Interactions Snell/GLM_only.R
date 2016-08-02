occumatrix = read.csv("occumatrix.csv", header = TRUE)
#### ---- GLM fitting  ---- ####

cs <- function(x) scale(x,scale=TRUE,center=TRUE)
# source: http://permalink.gmane.org/gmane.comp.lang.r.lme4.devel/12080
# need to scale predictor variables
beta = matrix(NA, nrow = length(subfocalspecies), ncol = 31)
pdf('Occupancy_glms.pdf', height = 8, width = 10)
par(mfrow = c(3, 4))
# for loop to store model output as a DF
for(i in 1:length(subfocalspecies)){ 
  print(i)
  
  occsub = occumatrix[occumatrix$Species == subfocalspecies[i],]
  
  glm_abundance_binom = glm(cbind(sp_success, sp_fail) ~ comp_scaled + 
                              abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI), family = binomial(link = logit), data = occsub)
  summary(glm_abundance_binom)
  
  glm_abundance_rand_site = glmer(cbind(sp_success_abun, sp_fail_abun) ~ cs(comp_scaled) + 
                                    abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI) + (1|stateroute:Species), family = binomial(link = logit), data = occsub)
  summary(glm_abundance_rand_site)
  
  glm_occ_rand_site = glmer(cbind(sp_success, sp_fail) ~ cs(comp_scaled) + 
                              abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI) + (1|stateroute:Species), family = binomial(link = logit), data = occsub)
  summary(glm_occ_rand_site) 
  
  beta[i,1] = subfocalspecies[i]
  beta[i,2] = summary(glm_abundance_binom)$coef[2,"Estimate"] # comp_scaled
  beta[i,3] = summary(glm_abundance_binom)$coef[3,"Estimate"] # abs(zTemp)
  beta[i,4] = summary(glm_abundance_binom)$coef[4,"Estimate"] # abs(zElev)
  beta[i,5] = summary(glm_abundance_binom)$coef[5,"Estimate"] # abs(zPrecip)
  beta[i,6] = summary(glm_abundance_binom)$coef[6,"Estimate"] # abs(zEVI)
  beta[i,7] = summary(glm_abundance_binom)$coef[2,"Pr(>|z|)"] # comp_scaled
  beta[i,8] = summary(glm_abundance_binom)$coef[3,"Pr(>|z|)"] # abs(zTemp)
  beta[i,9] = summary(glm_abundance_binom)$coef[4,"Pr(>|z|)"] # abs(zElev)
  beta[i,10] = summary(glm_abundance_binom)$coef[5,"Pr(>|z|)"] # abs(zPrecip)
  beta[i,11] = summary(glm_abundance_binom)$coef[6,"Pr(>|z|)"] # abs(zEVI)
  
  beta[i,12] = summary(glm_abundance_rand_site)$coef[2,"Estimate"] # comp_scaled
  beta[i,13] = summary(glm_abundance_rand_site)$coef[3,"Estimate"] # abs(zTemp)
  beta[i,14] = summary(glm_abundance_rand_site)$coef[4,"Estimate"] # abs(zElev)
  beta[i,15] = summary(glm_abundance_rand_site)$coef[5,"Estimate"] # abs(zPrecip)
  beta[i,16] = summary(glm_abundance_rand_site)$coef[6,"Estimate"] # abs(zEVI)
  beta[i,17] = summary(glm_abundance_rand_site)$coef[2,"Pr(>|z|)"] # comp_scaled
  beta[i,18] = summary(glm_abundance_rand_site)$coef[3,"Pr(>|z|)"] # abs(zTemp)
  beta[i,19] = summary(glm_abundance_rand_site)$coef[4,"Pr(>|z|)"] # abs(zElev)
  beta[i,20] = summary(glm_abundance_rand_site)$coef[5,"Pr(>|z|)"] # abs(zPrecip)
  beta[i,21] = summary(glm_abundance_rand_site)$coef[6,"Pr(>|z|)"] # abs(zEVI)
  
  beta[i,22] = summary(glm_occ_rand_site)$coef[2,"Estimate"] # comp_scaled
  beta[i,23] = summary(glm_occ_rand_site)$coef[3,"Estimate"] # abs(zTemp)
  beta[i,24] = summary(glm_occ_rand_site)$coef[4,"Estimate"] # abs(zElev)
  beta[i,25] = summary(glm_occ_rand_site)$coef[5,"Estimate"] # abs(zPrecip)
  beta[i,26] = summary(glm_occ_rand_site)$coef[6,"Estimate"] # abs(zEVI)
  beta[i,27] = summary(glm_occ_rand_site)$coef[2,"Pr(>|z|)"] # comp_scaled
  beta[i,28] = summary(glm_occ_rand_site)$coef[3,"Pr(>|z|)"] # abs(zTemp)
  beta[i,29] = summary(glm_occ_rand_site)$coef[4,"Pr(>|z|)"] # abs(zElev)
  beta[i,30] = summary(glm_occ_rand_site)$coef[5,"Pr(>|z|)"] # abs(zPrecip)
  beta[i,31] = summary(glm_occ_rand_site)$coef[6,"Pr(>|z|)"] # abs(zEVI)
  
}
beta = data.frame(beta)
names(beta) = c("FocalAOU", "Binom_comp_scaled_Estimate", "Binom_zTemp_Estimate", "Binom_zElev_Estimate", "Binom_zPrecip_Estimate", "Binom_zEVI_Estimate", "Binom_comp_scaled_P", "Binom_zTemp_P", "Binom_zElev_P", "Binom_zPrecip_P", "Binom_zEVI_P", "Abundance_comp_scaled_Estimate","Abundance_zTemp_Estimate","Abundance_zElev_Estimate","Abundance_zPrecip_Estimate","Abundance_zEVI_Estimate","Abundance_comp_scaled_P", "Abundance_zTemp_P", "Abundance_zElev_P", "Abundance_zPrecip_P", "Abundance_zEVI_P", "Randsite_comp_scaled_Estimate", "Randsite_zTemp_Estimate", "Randsite_zElev_Estimate", "Randsite_zPrecip_Estimate", "Randsite_zEVI_Estimate", "Randsite_comp_scaled_P", "Randsite_zTemp_P", "Randsite_zElev_P", "Randsite_zPrecip_P", "Randsite_zEVI_P")
dev.off()

#Plot winning glm
# GLM of all matrices not just subset
glm_occ_rand_site = glmer(cbind(sp_success, sp_fail) ~ cs(comp_scaled) + 
                            abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI) + (1|stateroute:Species), family = binomial(link = logit), data = occumatrix)
summary(glm_occ_rand_site) 


#### PLOTTING MODELS ####
ggplot(data = occumatrix, aes(x = comp_scaled, y = FocalOcc)) +stat_smooth(data=glm_occ_rand_site, lwd = 1.5) +xlab("Scaled Competitor Abundance")+ylab("Focal Occupancy") +theme_bw() +theme(axis.title.x=element_text(size=24),axis.title.y=element_text(size=24, angle=90), axis.text=element_text(size=12)) + theme(plot.margin = unit(c(.5,6,.5,.5),"lines")) 
ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/glmoutput.png")


ggplot(data = occumatrix, aes(x = zTemp, y = FocalOcc)) +stat_smooth(data=glm_occ_rand_site, lwd = 1.5, se = FALSE) +xlab("Mean Temperature Deviation")+ylab("Focal Occupancy")+ geom_vline(xintercept = 0, colour="red", linetype = "longdash") +theme_bw() +theme_bw() +theme(axis.title.x=element_text(size=28),axis.title.y=element_text(size=28, angle=90), axis.text=element_text(size=12)) + theme(plot.margin = unit(c(.5,6,.5,.5),"lines"))#+ annotate("text", x = 3, y = 0.56, label = "Environmental centroid\n for focal species", size=7,vjust=0.5, color = "black")
ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/glmtemp.png")

ggplot(data = occumatrix, aes(x = zElev, y = FocalOcc)) +stat_smooth(data=glm_occ_rand_site, lwd = 1.5, se = FALSE) +xlab("Mean Elevation Deviation")+ylab("Focal Occupancy")+ geom_vline(xintercept = 0, colour="red", linetype = "longdash") +theme_bw() +theme_bw() +theme(axis.title.x=element_text(size=28),axis.title.y=element_text(size=28, angle=90), axis.text=element_text(size=12)) + theme(plot.margin = unit(c(.5,6,.5,.5),"lines"))
ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/glmelev.png")
ggplot(data = occumatrix, aes(x = zPrecip, y = FocalOcc)) +stat_smooth(data=glm_occ_rand_site, lwd = 1.5, se = FALSE) +xlab("Mean Precipitation Deviation")+ylab("Focal Occupancy")+ geom_vline(xintercept = 0, colour="red", linetype = "longdash") +theme_bw() +theme_bw() +theme(axis.title.x=element_text(size=28),axis.title.y=element_text(size=28, angle=90), axis.text=element_text(size=12)) + theme(plot.margin = unit(c(.5,6,.5,.5),"lines"))
ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/glmprecip.png")
ggplot(data = occumatrix, aes(x = zEVI, y = FocalOcc)) +stat_smooth(data=glm_occ_rand_site, lwd = 1.5, se = FALSE) +xlab("Mean Vegetation Deviation")+ylab("Focal Occupancy")+ geom_vline(xintercept = 0, colour="red", linetype = "longdash") +theme_bw() +theme_bw() +theme(axis.title.x=element_text(size=28),axis.title.y=element_text(size=28, angle=90), axis.text=element_text(size=12)) + theme(plot.margin = unit(c(.5,6,.5,.5),"lines"))
ggsave("C:/Git/core-transient/scripts/R-scripts/Biotic Interactions Snell/glmevi.png")

