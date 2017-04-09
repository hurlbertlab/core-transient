#Poster figures 
library(ggplot2)
library(wesanderson)
library(viridis)

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

####Area vs abundance ####
modA = lm(meanOcc ~ logA, data = bbs_allscales) #r2 of 88% var explained
modN = lm(meanOcc ~ logN, data = bbs_allscales) #r2 of 90% var explained

#plotting -> need to make area and abun categorical vars 
subplot = bbs_allscales %>% 
  select(focalrte, meanOcc, logA, logN) %>% 
  gather(key = metric_type, 
         value = scale, 
         3:4)
subplot$metric_type = factor(subplot$metric_type, labels = c("Log Area", "Log Abundance"))

moda = lm(meanOcc ~ metric_type, data = subplot)

ggplot(subplot, aes(x = metric_type, y = meanOcc, fill = metric_type))+
  geom_boxplot()+
  scale_fill_manual(values = c("#00A08A", "#F98400"), guide = FALSE)+
  labs(x = "", y = "Mean % Occupancy")+theme_classic()+
  annotate("text", x = 1:2, y = 0.78, label = c("R^{2}==0.88", "R^{2}==0.90"), parse = TRUE)
ggsave("C:/git/core-transient/output/plots/Molly Plots/abunvarea.tiff")  

####R2 vals for env vars and coefs####
####Visually Characterizing r2 vals####
rsqrd_df = read.csv("scripts/R-scripts/scale_analysis/mod_rsqrds.csv", header = TRUE)
ggplot(data = rsqrd_df, aes(x = ind, y = r2))+geom_boxplot()
#subset to examine just r2 vals vs env var for the inflexion points (i) alone 

rsub = rsqrd_df %>%
  filter(dep == "OA.i" | dep == "ON.i" | dep == "CA.i" | dep == "CN.i") %>%
  filter(ind == "elev" | ind == "meanP" | ind == "ndvi" | ind == "temp")

rsub = droplevels(rsub) #removing ghost levels to ensure correct plotting/analyses
rsub$ind = factor(rsub$ind, labels = c("Elevation", "Mean Precipitation", "NDVI", "Mean Temperature"))
#checked to make sure labels appropriate order; in future ensure by reordering manually? 

ggplot(data = rsub, aes(x = ind, y = r2, fill = ind))+geom_boxplot()+
  scale_fill_manual(values = c("#00A08A", "#F2AD00", "#FF0000", "#F98400"), guide = FALSE)+
  labs(x = "Environmental Predictors", 
       y = expression(paste("Variation in Scale-Occupancy Relationship ", "(", R^{2}, ")")))+theme_classic()
ggsave("C:/git/core-transient/output/plots/Molly Plots/envr_inflxn.tiff")  


# sr2 = rsub %>% 
#   filter(dep == "OA.i")
# 
# 
# sr2 = droplevels(sr2)
# rsum = sum(sr2$r2)

# "#3B9AB2" "#78B7C5" "#EBCC2A" "#E1AF00" "#F21A00" Zissou
# 
# "#FF0000" "#00A08A" "#F2AD00" "#F98400" "#5BBCD6" Darjeelings 1 & 2
# "#ECCBAE" "#046C9A" "#D69C4E" "#ABDDDE" "#000000"
# 

