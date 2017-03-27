#Poster figures 


#read in data
bbs_allscales = read.csv("data/bbs_allscales.csv", header = TRUE)
bbs_allscales$logA = log10(bbs_allscales$area)
bbs_allscales$logN = log10(bbs_allscales$aveN)
bbs_allscales$lnA = log(bbs_allscales$area) #log is the natural log 
bbs_allscales$lnN = log(bbs_allscales$aveN) #rerun plots with this?
coefs = read.csv("scripts/R-scripts/scale_analysis/coefs.csv", header = TRUE)
preds.df = read.csv("C:/git/core-transient/scripts/R-scripts/scale_analysis/preds.csv", header = TRUE)

#function for plotting relationship
logistic_fcn = function(x, Asym, xmid, scal) {
  out = Asym/(1 + exp((xmid - x)/scal))
  return(out)
}


#gen random subset of routes for poster
#all bbs routes plotting for poster 
coef_psub = coefs %>% 
  dplyr::sample_n(100, replace = TRUE) %>%
  inner_join(bbs_allscales, by = c("stateroute"="focalrte")) 

mpreds = preds.df %>%
  group_by(logA) %>%
  summarise(meanOcc = mean(OApreds))

groupcolors = c(mpreds = "firebrick", stateroute = "grey70")




ggplot(coef_psub, aes(x = logA, y = meanOcc))+ 
  geom_line(aes(group = stateroute, colour = "observed"))+
  geom_line(data = mpreds, size = 1.3, aes(colour = "mean predicted"))+ #using a sep subset dataset
  labs(x = "Log area", y = "Mean % Occupancy")+theme_classic()+
  scale_color_manual(values = c("observed" = "grey70", "mean predicted" = "firebrick"))+
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.2))

ggsave("C:/git/core-transient/output/plots/Molly Plots/predplot.tiff")  


scale_linetype_manual(values = c(rep("solid", 10), rep("dashed", 6))) +
  scale_color_manual(values = c(brewer.pal(10, "Set3"), brewer.pal(6, "Set3")))




