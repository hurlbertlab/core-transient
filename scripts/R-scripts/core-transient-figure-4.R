###############################################
# Code for running core-transient analysis
# and data summaries over all formatted datasets.
#
# Input files are named propOcc_XXX.csv where
# XXX is the dataset ID.

setwd("C:/git/core-transient")

library(lme4)
library(plyr) # for core-transient functions
library(ggplot2)
library(tidyr)
library(maps)
library(gridExtra)
library(RColorBrewer)
library(sp)
library(rgdal)
library(raster)
library(dplyr)
library(merTools)
library(digest)


source('scripts/R-scripts/core-transient_functions.R')

# Specify here the datasetIDs and then run the code below.
dataformattingtable = read.csv('data_formatting_table.csv', header = T) 

datasetIDs = dataformattingtable$dataset_ID[dataformattingtable$format_flag == 1]

# BBS (dataset 1) will be analyzed separately for now.
datasetIDs = datasetIDs[!datasetIDs %in% c(1)]

##### Boxplots showing distribution of core and transient species by taxon #####
# Read in datasets
taxcolors = read.csv("output/tabular_data/taxcolors.csv", header = TRUE)
occ_taxa = read.csv("output/tabular_data/occ_taxa.csv", header = TRUE)
areamerge = read.csv("output/tabular_data/areamerge.csv", header = TRUE)
transrich = read.csv("output/tabular_data/transrich.csv", header = TRUE)
minustransrich = read.csv("output/tabular_data/minustransrich.csv", header = TRUE)
bbs_abun_occ = read.csv("data/BBS/bbs_abun_occ.csv", header = TRUE)
bbs_occ = read.csv("data/BBS/bbs_abun4_spRich.csv", header = TRUE)

minustransbbs = bbs_abun_occ %>% filter(occupancy > 1/3) %>% dplyr::count(stateroute, scale) %>% filter(scale == 50)
names(minustransbbs) = c("stateroute", "scale", "spRichnotrans")

transbbs = bbs_abun_occ %>% dplyr::count(stateroute, scale) %>% filter(scale == 50)
names(transbbs) = c("stateroute", "scale", "spRich")

# occ_merge = occ_taxa[,c("datasetID", "site","taxa", "meanAbundance", "pctTrans","pctCore","pctNeither","scale", "spRich")]
# all_occ = rbind(bbs_spRich,occ_merge)

#### Figure 4b ####
# read in route level ndvi and elevation data (radius = 5 km?!)
# we want to agg by month here
gimms_ndvi = read.csv("output/tabular_data/gimms_ndvi_bbs_data.csv", header = TRUE)
gimms_agg = gimms_ndvi %>% filter(month == c("may", "jun", "jul")) %>% 
  group_by(site_id, year, month)  %>%  summarise(mean=mean(ndvi))

lat_scale_rich = read.csv("output/tabular_data/lat_scale_rich.csv", header = TRUE)
lat_scale_bbs = filter(lat_scale_rich, datasetID == 1)
lat_scale_bbs$site_id = sapply(strsplit(as.character(lat_scale_bbs$site), split='-', fixed=TRUE), function(x) (x[1]))
lat_scale_bbs$site_id = as.integer(lat_scale_bbs$site_id)

bbs_spRich = merge(transbbs, minustransbbs[c("stateroute", "spRichnotrans")], by = "stateroute")
bbs_spRich$site_id <- bbs_spRich$stateroute
# merging ndvi and elevation to bbs data
bbs_env = join(bbs_spRich, gimms_ndvi, type = "left")
bbs_env = merge(bbs_env, lat_scale_bbs[,c("site_id", "elev.point", "elev.mean", "elev.var")], by = "site_id")

# cor test not really working - need for loop?
cor.test(bbs_env$spRich, bbs_env$ndvi)
bar1 = cor.test(bbs_env$spRich, bbs_env$ndvi)$estimate
CI1lower = 0.01965912
CI1upper = 0.03821317
bar3 = cor.test(bbs_env$spRich, bbs_env$elev.mean)$estimate
CI3lower = -0.004563033
CI3upper =  -0.001176073

bar2 = cor.test(bbs_env$spRichnotrans, bbs_env$ndvi)$estimate
CI2lower = -0.03501665
CI2upper =  -0.01645934
bar4 = cor.test(bbs_env$spRichnotrans, bbs_env$elev.mean)$estimate
CI4lower = -0.005683127
CI4upper =  -0.002296194

corr_res <- data.frame(Trans = c(bar1, bar3), Ntrans = c(bar2, bar4)) 
corr_res$env = c("NDVI", "Elevation")
corr_res_long = gather(corr_res, "class","value", c(Trans:Ntrans))
corr_res_long$CIlower = c(CI1lower,CI3lower,CI2lower,CI4lower)
corr_res_long$CIupper = c(CI1upper,CI3upper,CI2upper,CI4upper)

colscale = c("light blue","#225ea8")
limits = aes(ymax = corr_res_long$CIupper, ymin=corr_res_long$CIlower)
# no variation - add in CIS?
ggplot(data=corr_res_long, aes(factor(env), value))+ geom_bar(aes(fill = class), position = "dodge", stat="identity")+ geom_errorbar(limits, position="dodge", width=0.25) + scale_fill_manual(values = c("Trans" = "#225ea8","Ntrans" = "light blue"), labels = c("No Transients", "Transients"))+ theme_classic() + theme(axis.text.x=element_text(size=24),axis.text.y=element_text(size=24),axis.title.x=element_text(size=24),axis.title.y=element_text(size=24,angle=90,vjust = 2))+ xlab(NULL) + ylab("Correlation Coefficient")  + ylim(-0.04,0.04) + guides(fill=guide_legend(title=NULL)) + theme(legend.text = element_text(size = 16))
ggsave(file="C:/Git/core-transient/output/plots/corrcoeff_4b.pdf", height = 10, width = 15)

#### Figure 4c ####
turnover = read.csv("output/tabular_data/temporal_turnover.csv", header = TRUE)
turnover_taxa = merge(turnover,dataformattingtable[,c("dataset_ID", "taxa")], by.x = "datasetID", by.y = "dataset_ID")
turnover_col = merge(turnover_taxa, taxcolors, by = "taxa")

turnover_col$taxa = factor(turnover_col$taxa,
                                levels = c('Invertebrate','Fish','Plankton','Mammal','Plant','Bird','Benthos'),ordered = TRUE)
colscale = c("gold2","turquoise2", "red", "purple4","forestgreen","#1D6A9B", "azure4")


m <- ggplot(turnover_col, aes(x = TJ, y = TJnotrans))
m + geom_abline(intercept = 0,slope = 1, lwd =1.5,linetype="dashed")+geom_point(aes(colour = taxa), size = 6) + xlab("Transients Slope") + ylab("Without Transients Slope") + scale_colour_manual(breaks = turnover_col$taxa,values = colscale) + theme(axis.text.x=element_text(size=24),axis.text.y=element_text(size=24),axis.title.x=element_text(size=32),axis.title.y=element_text(size=32,angle=90,vjust = 2))+ theme_classic()
ggsave(file="C:/Git/core-transient/output/plots/spturnover_4c.pdf", height = 10, width = 15)


##### Figure 4d ##### only scaled vars
minustransrich$minustrans = minustransrich$n

minustransbbsscale = bbs_abun_occ %>% filter(occupancy > 1/3) %>% dplyr::count(stateroute, scale)
names(minustransbbsscale) = c("stateroute", "scale", "spRichnotrans")

transbbsscale = bbs_abun_occ %>% dplyr::count(stateroute, scale) 
names(transbbsscale) = c("stateroute", "scale", "spRich")

bbs_occ_scale = merge(transbbsscale, minustransbbsscale[c("stateroute", "spRichnotrans")], by = "stateroute")

bbs_occ_trans = merge(bbs_occ, transrich, by = c("datasetID", "site", "scale"), all.x = TRUE)
bbs_occ_trans = merge(bbs_occ_trans, minustransrich[, c("datasetID", "site", "scale", "minustrans")], by = c("datasetID", "site", "scale"), all.x = TRUE)

bbs_occ_trans_area = merge(areamerge[,c("datasetID", "site", "area")], bbs_occ_trans, by = c("datasetID", "site"))

scaleIDs = unique(bbs_occ_trans_area$datasetID)

scaleIDs = scaleIDs[! scaleIDs %in% c(225,248,254, 282,291)] # 248 tbd

slopes = c()
for(id in scaleIDs){
  print(id)
  plotsub = subset(bbs_occ_trans_area,datasetID == id) 
  taxa = as.character(unique(plotsub$taxa))
  mod.t = lm(log10(plotsub$spRich) ~ log10(plotsub$area))
  mod.t.slope = summary(mod.t)$coef[2,"Estimate"]
  mod.n= lm(log10(plotsub$minustrans) ~ log10(plotsub$area))
  mod.n.slope = summary(mod.n)$coef[2,"Estimate"]
  print(mod.n.slope)
  taxcolor = subset(taxcolors, taxa == as.character(plotsub$taxa)[1])
  slopes = rbind(slopes, c(mod.t.slope, mod.n.slope, taxa))
}
colnames(slopes) = c("spRich_slope","minustrans_slope", "taxa")
plot_relationship = merge(slopes, taxcolors, by = "taxa")
plot_relationship$spRich_slope = as.numeric(as.character(plot_relationship$spRich_slope))
plot_relationship$minustrans_slope = as.numeric(as.character(plot_relationship$minustrans_slope))


plot_relationship$taxa = factor(plot_relationship$taxa,
                                levels = c('Invertebrate','Fish','Plankton','Mammal','Plant','Bird','Benthos'),ordered = TRUE)
colscale = c("gold2","turquoise2", "red", "purple4","forestgreen","#1D6A9B", "azure4")

p <- ggplot(plot_relationship, aes(x = spRich_slope, y = minustrans_slope))
p + geom_abline(intercept = 0,slope = 1, lwd =1.5,linetype="dashed")+geom_point(aes(colour = taxa), size = 6) + xlab("Transients Slope") + ylab("Without Transients Slope") + scale_colour_manual(breaks = plot_relationship$taxa,values = colscale) + theme(axis.text.x=element_text(size=24),axis.text.y=element_text(size=24),axis.title.x=element_text(size=32),axis.title.y=element_text(size=32,angle=90,vjust = 2))+ theme_classic()
ggsave(file="C:/Git/core-transient/output/plots/sparea_4d.pdf", height = 10, width = 15)



