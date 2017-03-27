###############################################
# Code for running core-transient analysis
# and data summaries over all formatted datasets.
#
# Input files are named propOcc_XXX.csv where
# XXX is the dataset ID.


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

##### prep for all figures #####
# Read in datasets
taxcolors = read.csv("output/tabular_data/taxcolors.csv", header = TRUE)
occ_taxa = read.csv("output/tabular_data/occ_taxa.csv", header = TRUE)
areamerge = read.csv("output/tabular_data/areamerge.csv", header = TRUE)
allrich = read.csv("output/tabular_data/allrich.csv", header = TRUE)
notransrich = read.csv("output/tabular_data/notransrich.csv", header = TRUE)
bbs_abun_occ = read.csv("data/BBS/bbs_abun_occ.csv", header = TRUE)
bbs_occ = read.csv("data/BBS/bbs_abun4_spRich.csv", header = TRUE)

# addings symbols to taxcolors
symbols = c(15, 16, 15, 17, 16, 15, 16) 
type = c("invert", "vert", "invert", "plant", "vert", "invert", "vert") 
taxcolors = cbind(taxcolors, type,symbols)

# calc bbs with and without trans
notransbbs = bbs_abun_occ %>% filter(occupancy > 1/3) %>% dplyr::count(stateroute, scale) %>% filter(scale == 50)
names(notransbbs) = c("stateroute", "scale", "spRichnotrans")

allbbs = bbs_abun_occ %>% dplyr::count(stateroute, scale) %>% filter(scale == 50)
names(allbbs) = c("stateroute", "scale", "spRich")

# Figure 4a in sep script

#### Figure 4b ####
# read in route level ndvi and elevation data (radius = 5 km?!)
# we want to agg by month here
gimms_ndvi = read.csv("output/tabular_data/gimms_ndvi_bbs_data.csv", header = TRUE)
gimms_agg = gimms_ndvi %>% filter(month == c("may", "jun", "jul")) %>% 
  group_by(site_id)  %>%  summarise(ndvi=mean(ndvi))

lat_scale_rich = read.csv("output/tabular_data/lat_scale_rich.csv", header = TRUE)
lat_scale_bbs = filter(lat_scale_rich, datasetID == 1)
lat_scale_bbs$site_id = sapply(strsplit(as.character(lat_scale_bbs$site), split='-', fixed=TRUE), function(x) (x[1]))
lat_scale_bbs$site_id = as.integer(lat_scale_bbs$site_id)

bbs_spRich = merge(allbbs, notransbbs[c("stateroute", "spRichnotrans")], by = "stateroute")
bbs_spRich$site_id <- bbs_spRich$stateroute
# merging ndvi and elevation to bbs data
bbs_env = join(bbs_spRich, gimms_agg, type = "left")
bbs_env = merge(bbs_env, lat_scale_bbs[,c("site_id", "elev.point", "elev.mean", "elev.var")], by = "site_id")

# cor test not really working - need for loop?
cor.test(bbs_env$spRich, bbs_env$ndvi)
bar1 = cor.test(bbs_env$spRich, bbs_env$ndvi)$estimate
CI1lower =  cor.test(bbs_env$spRich, bbs_env$ndvi)$conf.int[1]
CI1upper = cor.test(bbs_env$spRich, bbs_env$ndvi)$conf.int[2]
bar3 = cor.test(bbs_env$spRich, bbs_env$elev.mean)$estimate
CI3lower = cor.test(bbs_env$spRich, bbs_env$elev.mean)$conf.int[1]
CI3upper =  cor.test(bbs_env$spRich, bbs_env$elev.mean)$conf.int[2]

bar2 = cor.test(bbs_env$spRichnotrans, bbs_env$ndvi)$estimate
CI2lower = cor.test(bbs_env$spRichnotrans, bbs_env$ndvi)$conf.int[1]
CI2upper =   cor.test(bbs_env$spRichnotrans, bbs_env$ndvi)$conf.int[2]
bar4 = cor.test(bbs_env$spRichnotrans, bbs_env$elev.mean)$estimate
CI4lower =  cor.test(bbs_env$spRichnotrans, bbs_env$elev.mean)$conf.int[1]
CI4upper =  cor.test(bbs_env$spRichnotrans, bbs_env$elev.mean)$conf.int[2]

bar5 = cor.test(bbs_env$spRich-bbs_env$spRichnotrans, bbs_env$ndvi)$estimate
CI5lower = cor.test(bbs_env$spRich-bbs_env$spRichnotrans, bbs_env$ndvi)$conf.int[1]
CI5upper =  cor.test(bbs_env$spRich-bbs_env$spRichnotrans, bbs_env$ndvi)$conf.int[2]
bar6 = cor.test(bbs_env$spRich-bbs_env$spRichnotrans, bbs_env$elev.mean)$estimate
CI6lower = cor.test(bbs_env$spRich-bbs_env$spRichnotrans, bbs_env$elev.mean)$conf.int[1]
CI6upper =  cor.test(bbs_env$spRich-bbs_env$spRichnotrans, bbs_env$elev.mean)$conf.int[2]

corr_res <- data.frame(All = c(bar1, bar3), Ntrans = c(bar2, bar4), Trans = c(bar5, bar6)) 
corr_res$env = c("NDVI", "Elevation")
corr_res_long = gather(corr_res, "class","value", c(All:Trans))
corr_res_long$CIlower = c(CI1lower,CI3lower,CI2lower,CI4lower, CI5lower, CI6lower)
corr_res_long$CIupper = c(CI1upper,CI3upper,CI2upper,CI4upper, CI5upper, CI6upper)
corr_res_long$env = factor(corr_res_long$env, levels = c("NDVI", "Elevation"), ordered = TRUE)

colscale = c("dark green","light blue","#225ea8")
limits = aes(ymax = corr_res_long$CIupper, ymin=corr_res_long$CIlower)
# no variation - add in CIS?
ggplot(data=corr_res_long, aes(factor(env), value, fill = class))+ geom_bar(width = 0.8, position = position_dodge(width = 0.9), stat="identity")+ geom_errorbar(aes(ymin = CIlower, ymax = CIupper), width =.1, position = position_dodge(.9))+ scale_fill_manual(values = c("All" = "dark green","Trans" = "#225ea8","Ntrans" = "light blue"), labels = c("All species","All species excluding transients", "Transients only"))+ theme_classic() + theme(axis.text.x=element_text(size=24),axis.text.y=element_text(size=24),axis.title.x=element_text(size=24),axis.title.y=element_text(size=24,angle=90,vjust = 2))+ xlab(NULL) + ylab("Correlation Coefficient") + scale_y_continuous(breaks = pretty(corr_res_long$value, n = 8))+ guides(fill=guide_legend(title=NULL)) + theme(legend.text = element_text(size = 16))
ggsave(file="C:/Git/core-transient/output/plots/4b_corrcoeff.pdf", height = 10, width = 15)

#### Figure 4c ####
turnover = read.csv("output/tabular_data/temporal_turnover.csv", header = TRUE)
turnover_taxa = merge(turnover,dataformattingtable[,c("dataset_ID", "taxa")], by.x = "datasetID", by.y = "dataset_ID")
turnover_col = merge(turnover_taxa, taxcolors, by = "taxa")

turnover_col$taxa = factor(turnover_col$taxa,
                                levels = c('Invertebrate','Fish','Plankton','Mammal','Plant','Bird','Benthos'),ordered = TRUE)
# turnover_col$type = factor(turnover_col$type,
                           # levels = c("invert", "vert", "invert","vert", "plant","vert","invert"),ordered = TRUE)
colscale = c("gold2","turquoise2", "red", "purple4","forestgreen","#1D6A9B", "azure4")

# stuck here why?
m <- ggplot(turnover_col, aes(x = TJ, y = TJnotrans))
m + geom_abline(intercept = 0,slope = 1, lwd =1.5,linetype="dashed")+ geom_point(data=slopes_bbs, alpha = 1/100, size = 2)+geom_point(aes(colour = taxa, shape = type), size = 5.5) + xlab("z (all species)") + ylab("z (excluding transients)") + scale_colour_manual(breaks = turnover_col$taxa,values = colscale) + theme_classic() + theme(axis.text.x=element_text(size=24),axis.text.y=element_text(size=24),axis.title.x=element_text(size=32),axis.title.y=element_text(size=32,angle=90,vjust = 2))+ guides(colour = guide_legend(title = "Taxa"))
ggsave(file="C:/Git/core-transient/output/plots/4c_spturnover.pdf", height = 10, width = 15)

##### Figure 4d ##### only scaled vars
bbs_uniq_area = bbs_abun_occ %>% dplyr::select(stateroute,scale,subrouteID,area) %>% unique()

notransbbsscale = bbs_abun_occ %>% filter(occupancy > 1/3) %>% dplyr::count(stateroute, scale, subrouteID)
names(notransbbsscale) = c("stateroute", "scale", "subrouteID","notrans")
noarea = left_join(notransbbsscale, bbs_uniq_area)

allbbsscale = bbs_abun_occ %>% dplyr::count(stateroute, scale, subrouteID) 
names(allbbsscale) = c("stateroute", "scale","subrouteID", "spRich")
allarea = left_join(allbbsscale, bbs_uniq_area)

bbs_occ_scale = merge(allarea, noarea, by = c("stateroute", "scale", "subrouteID", "area"))
bbs_occ_scale$subrouteID = gsub("Stop", "", bbs_occ_scale$subrouteID)
bbs_occ_scale$site = paste(bbs_occ_scale$stateroute, bbs_occ_scale$scale, bbs_occ_scale$subrouteID, sep = "-")



scaleIDs = unique(bbs_occ_scale$stateroute)
slopes_bbs = data.frame(stateroute = NULL,
                    site = NULL,
                    taxa = NULL,
                    areaSlope = NULL,
                    areaSlope_noTrans = NULL)
for(id in scaleIDs){
  print(id)
  plotsub = subset(bbs_occ_scale,stateroute == id) 
  site = plotsub$site
  taxa = "Bird"
  mod.t = lm(log10(plotsub$spRich) ~ log10(plotsub$area))
  mod.t.slope = summary(mod.t)$coef[2,"Estimate"]
  mod.n= lm(log10(plotsub$notrans) ~ log10(plotsub$area))
  mod.n.slope = summary(mod.n)$coef[2,"Estimate"]
  print(mod.n.slope)
  taxcolor = subset(taxcolors, taxa == as.character(plotsub$taxa)[1])
  slopes_bbs = rbind(slopes_bbs, data.frame(stateroute = id,
                                    site = site,
                                    taxa = taxa,
                                    areaSlope = mod.t.slope, 
                                    areaSlope_noTrans = mod.n.slope))
}
slopes_bbs$bbs = 'yes'

slopes_bbs$datasetID = 1
slopes_bbs = slopes_bbs[,c("datasetID","taxa","areaSlope", "areaSlope_noTrans", "bbs")]

# merge sp rich and minus trans sprich other datasets
notransrich$notrans = notransrich$n

datasetrich = merge(allrich, notransrich[,c("datasetID", "site", "scale","notrans")], by = c("datasetID", "site", "scale"), all.x = TRUE)
colnames(datasetrich)[4] <- "spRich" # rename a single column - make sure index is right

occ_trans_area = merge(areamerge[,c("datasetID", "site", "area")], datasetrich, by = c("datasetID", "site"))
occ_trans_area = merge(occ_trans_area, dataformattingtable[,c("dataset_ID", "taxa")], by.x = "datasetID", by.y = "dataset_ID")
scaleIDs = unique(occ_trans_area$datasetID)

scaleIDs = scaleIDs[! scaleIDs %in% c(279,225,248,254, 282,291)] # 248 tbd

slopes = data.frame(datasetID = NULL,
                    taxa = NULL,
                    areaSlope = NULL,
                    areaSlope_noTrans = NULL)
for(id in scaleIDs){
  print(id)
  plotsub = subset(occ_trans_area,datasetID == id) 
  taxa = as.character(unique(plotsub$taxa))
  mod.t = lm(log10(plotsub$spRich) ~ log10(plotsub$area))
  mod.t.slope = summary(mod.t)$coef[2,"Estimate"]
  mod.n= lm(log10(plotsub$notrans) ~ log10(plotsub$area))
  mod.n.slope = summary(mod.n)$coef[2,"Estimate"]
  print(mod.n.slope)
  taxcolor = subset(taxcolors, taxa == as.character(plotsub$taxa)[1])
  slopes = rbind(slopes, data.frame(datasetID = id,
                                    taxa = taxa,
                                    areaSlope = mod.t.slope, 
                                    areaSlope_noTrans = mod.n.slope))
}
slopes$bbs = 'no'

all_slopes =  rbind(slopes, slopes_bbs)

plot_relationship = merge(slopes, taxcolors, by = "taxa")
slopes_bbs = merge(slopes_bbs, taxcolors, by = "taxa")

plot_relationship$taxa = factor(plot_relationship$taxa,
                                levels = c('Invertebrate','Fish','Plankton','Mammal','Plant','Bird','Benthos'),ordered = TRUE)
colscale = c("gray","#1D6A9B","turquoise2","gold2","purple4", "red", "forestgreen") 

p <- ggplot(plot_relationship, aes(x = areaSlope, y = areaSlope_noTrans))
p + geom_abline(intercept = 0,slope = 1, lwd =1.5,linetype="dashed") +geom_point(data=slopes_bbs, aes(colour = taxa, shape = type),alpha = 5/100, size = 2)+  geom_point(aes(colour = taxa, shape = type), size = 5.5)+ theme_classic() + scale_color_manual("Taxa", breaks = plot_relationship$taxa,values = colscale)+ xlab(expression(paste(italic("z "), "(all species)"))) + ylab(expression(paste(italic("z "), "(excluding transients)")))  + theme(axis.text.x=element_text(size=24),axis.text.y=element_text(size=24),axis.title.x=element_text(size=32),axis.title.y=element_text(size=32,angle=90,vjust = 2))

ggsave(file="C:/Git/core-transient/output/plots/4d_sparea.pdf", height = 10, width = 15)

