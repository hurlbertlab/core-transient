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
library(digest)
library(sads)
library(purrr)
library(ggplot2)
library(cowplot)
library(grid)

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
bbs_count = read.csv("data/BBS/bbs_2000_2014.csv", header = TRUE)
bbs_occ_aou = read.csv("data/BBS/bbs_occ_2000_2014.csv", header = TRUE)
bray_output = read.csv("output/tabular_data/temporal_turnover_bray.csv", header = TRUE)

# addings symbols to taxcolors
symbols = c(15, 16, 15, 17, 16, 15, 16) 
Type = c("Invertebrate", "Vertebrate", "Invertebrate", "Plant", "Vertebrate", "Invertebrate", "Vertebrate") 
taxcolors = cbind(taxcolors, Type,symbols)

# create bbs files
bbs_count5a = dplyr::rename(bbs_count, year = Year, site = stateroute, species = aou, count = speciestotal)
bbs_count5a$datasetID = 1

bbs_occ_aou = dplyr::rename(bbs_occ_aou, site = stateroute, species = aou, propOcc = occ)
bbs_occ_aou$datasetID  = 1
bbs_occ5a = bbs_occ_aou[, c("datasetID", "site", "species", "propOcc")]

sad_examp = c(109, 14, 4, 4, 680, 195, 13, 3, 123, 116, 1, 5, 105, 26, 14, 2, 9, 29, 15, 133, 5, 41, 45, 33, 
              17, 27, 37, 11, 169, 1, 27, 7, 19, 23, 100, 4, 8, 5, 19, 1, 21, 12, 6, 1, 10, 2, 1, 94, 2, 4, 28, 1, 3, 
              34, 3, 20, 72, 21, 1, 84, 10, 528, 18, 1, 1, 10, 10, 48, 7)

datasetIDs = get_valid_datasetIDs()
abund_data = get_abund_data(datasetIDs)
propocc_data = get_propocc_data(datasetIDs)
summed_abunds = sum_abunds(abund_data)
sad_data = left_join(summed_abunds, propocc_data, by = c('datasetID', 'site', 'species'))

colscale = c("azure4","#1D6A9B","turquoise2","gold2","purple4","red", "forestgreen")  

#### null model ####
null_output = c()
init.time = Sys.Date()
for(id in datasetIDs[,1]){
  subdata = subset(sad_data, datasetID == id)
  sites = unique(subdata$site)
  for(site in sites){
    sitedata = subdata[subdata$site == site,]
    notrans = na.omit(sitedata[sitedata$propOcc > 1/3,])
    trans = na.omit(sitedata[sitedata$propOcc <= 1/3,])
    num_notrans = length(notrans$propOcc)
    num_trans = length(trans$propOcc)
    
    for(r in 1:10){
      print(paste(id, site, r, Sys.Date()))
      
      if(num_notrans >= num_trans) {
        null_sample = sample_n(notrans, num_notrans - num_trans, replace = FALSE) %>%
          rbind(trans)
      } else {
        null_sample = sample_n(trans, num_notrans, replace = FALSE)  
      }

      logseries_weights_excl = null_sample %>%
        group_by(datasetID, site) %>% 
        dplyr::summarize(weights = get_logseries_weight(abunds), treatment = 'Null')
      
      null_output = rbind(null_output, c(r, id, site, logseries_weights_excl[,3], num_notrans))
      nwd = nrow(sad_data)
      
    } # end r loop
  } # end site loop
} # end dataset loop


null_output = data.frame(null_output)
colnames(null_output) = c("number", "datasetID", "site", "SAD_excl","Non_trans")
null_output$number = as.numeric(null_output$number)
null_output$datasetID = as.numeric(null_output$datasetID)
null_output$site = as.character(null_output$site)
null_output$SAD_excl = as.numeric(null_output$SAD_excl)
null_output$Non_trans = as.numeric(null_output$Non_trans)
# write.csv(null_output, "output/tabular_data/null_output_SAD_100.csv", row.names = FALSE)
null_output = read.csv("output/tabular_data/null_output_SAD_100.csv", header = TRUE)
null_output$combo = paste(null_output$datasetID, null_output$site, sep = "_")

null_5a_sum = null_output %>%
  dplyr::group_by(datasetID, site) %>% 
  dplyr::summarize(SAD_excl = mean(SAD_excl), var_excl = var(SAD_excl))

# read in output from figure 5 script
logseries_weights = read.csv("output/tabular_data/logseries_weights.csv", header = TRUE)
logseries_all = subset(logseries_weights, treatment == "All")
logseries_all = logseries_all[,c("datasetID", "site", "weights", "treatment")]


null_output$weights = null_output$SAD_excl
null_output$treatment = "Null"
null_output = filter(null_output, number == 10)
null_output_excl = null_output[,c("datasetID", "site", "weights", "treatment")]

SAD_plot = rbind(logseries_all, null_output_excl)

#### ggplot fig1a #####
excl = ggplot(SAD_plot,aes(x=weights,fill=treatment))+geom_histogram(bins = 20, position = "identity", alpha = 0.7)+ xlab("Transient Status") + ylab("Proportion of Species") + scale_y_continuous(breaks=c(0,200,350)) + scale_fill_manual(labels = c("All species", "Excluding non-transients"),values = c("dark orange2","yellow"))+ theme_classic() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.text.y=element_text(size=25, color = "black"),axis.title.y=element_text(size=30,angle=90,vjust = 8),axis.title.x=element_text(size=30, vjust = -7))  + ylab("Frequency") + xlab("Akaike Weight") +theme(plot.margin=unit(c(0.35,1,2,1.7),"cm")) + theme(legend.text = element_text(size = 20), legend.title = element_blank(), legend.position = "none")

ggsave(file="C:/Git/core-transient/output/plots/1a_null.pdf", height = 8, width = 10)

logseries_weights = read.csv("output/tabular_data/logseries_weights.csv", header = TRUE)
logseries_excl = subset(logseries_weights, treatment == "Excluding") # excl transients
SAD_hist = merge(logseries_excl, null_output[,c("datasetID", "site", "SAD_excl")], by = c("datasetID", "site"))
hist(SAD_hist$SAD_excl - SAD_hist$weights, xlab = "Akaike weight difference \n Excluding null transients - Excluding transients", cex.lab=1, cex.axis=2.5)

ggplot(data=SAD_hist, aes(SAD_hist$SAD_excl - SAD_hist$weights)) + 
  geom_histogram(bins = 16, col="black",  fill="white") + theme_classic() + theme(axis.text.x=element_text(size=30,angle=90, color = "black"), axis.ticks.x=element_blank(),axis.text.y=element_text(size=30, color = "black"),axis.title.y=element_text(size=30,angle=90, vjust = 4),axis.title.x=element_text(size=30, vjust = -4))  + ylab("Frequency") + xlab("Akaike weight difference \n (excluding non-transients - excluding transients)") +theme(plot.margin=unit(c(0.35,1,2,1.7),"cm")) 
ggsave(file="C:/Git/core-transient/output/plots/1a_hist.pdf", height = 8, width = 10.6)

ks.test(logseries_all$weights, null_output_excl$weights)
# D = 0.73906, p-value < 2.2e-16 diff between true and null
# D = 0.18843, p-value < 2.2e-16 diff between all and null

wilcox.test(SAD_hist$SAD_excl - SAD_hist$weights)

#### Figure 5c ####
source('scripts/R-scripts/temporal_turnover.R')
turnover = function(splist1, splist2) {
  tot_uniq_sp = length(unique(c(splist1, splist2)))
  shared_sp = length(splist1) + length(splist2) - tot_uniq_sp
  Jturnover = 1 - shared_sp/tot_uniq_sp
  return(Jturnover)
}

datasetIDs = dataformattingtable %>%
  filter(format_flag == 1, 
         countFormat %in% c('count', 'cover', 'density', 'abundance', 'presence', 'biomass')) %>% 
  dplyr::select(dataset_ID)

abund_data = get_abund_data(datasetIDs)
propocc_data = get_propocc_data(datasetIDs)
all_data.5 = left_join(abund_data, propocc_data, by = c('datasetID', 'site', 'species'))
# removing duplicate bbs routes
dup_rtes= read.csv("data/BBS/rtes_duplicate_records.csv", header = TRUE)
all_data = filter(all_data.5, !site %in% dup_rtes$stateroute)


null_5c = c()
init.time = Sys.time()
for (dataset in datasetIDs[,1]) {
  subdata = subset(all_data, datasetID == dataset)
  sites = unique(subdata$site)
  print(paste("Calculating turnover: dataset", dataset))
  for (site in sites) {
    sitedata = subdata[subdata$site == site,]
    notrans = sitedata[sitedata$propOcc > 1/3,]
    trans = sitedata[sitedata$propOcc <= 1/3,]
    years = as.numeric(unique(sitedata$year))
    num_notrans = length(notrans$propOcc)
    num_trans = length(trans$propOcc)
    TJ_notrans = c()
    
    for(i in 1:100){
      print(c(i, dataset, site))
      if(num_notrans >= num_trans & length(years) > 0) {
        null_sample = sample_n(notrans, num_notrans - num_trans, replace = FALSE) %>%
          rbind(trans)
      } else {
        null_sample = sample_n(trans, num_notrans, replace = FALSE)  
      }
      
      for (year in years[1:(length(years)-1)]) {
        
        notrans2 = null_sample[null_sample$propOcc > 1/3,]
        comm1_noT = unique(notrans2$species[notrans2$year == year])
        comm2_noT = unique(notrans2$species[notrans2$year == year + 1])
        T_J_notran = turnover(comm1_noT, comm2_noT)
        TJ_notrans = c(TJ_notrans, T_J_notran)
      }
      
      null_5c = rbind(null_5c, c(i, dataset, site, T_J_notran, num_notrans))
      nwd = nrow(all_data)
      curr.time = Sys.time()
      elapsed = curr.time - init.time
      percelltime = elapsed/i
      estimated.end = (nwd - i)*percelltime + curr.time
      print(paste(i, "out of",nwd, "; current time:", curr.time,
                  "; estimated end time:", estimated.end))
    } # end r loop
  } # end site loop
} # end dataset loop



null_5c = data.frame(null_5c)
colnames(null_5c) = c("r", "datasetID", "site", "notransturn", "numnon")
null_5c$r = as.numeric(null_5c$r)
null_5c$datasetID = as.numeric(as.character(null_5c$datasetID))
null_5c$site = as.character(null_5c$site)
null_5c$notransturn = as.numeric(as.character(null_5c$notransturn))
null_5c$numnon = as.numeric(as.character(null_5c$numnon))

# write.csv(null_5c, "output/tabular_data/null_5c100.csv", row.names = FALSE)
# null_5c = read.csv("output/tabular_data/null_5c100.csv", header = TRUE)

# read in output from figure 5 script
turnover_output = read.csv("output/tabular_data/temporal_turnover.csv", header = TRUE)
null_5cplot = merge(turnover_output, null_5c, by = c("datasetID", "site"))

##### plot 5c ####
taxcolors = read.csv("output/tabular_data/taxcolors.csv", header = TRUE)
null_5cplot = subset(null_5cplot, r == 1)
turnover_taxa = merge(null_5cplot,dataformattingtable[,c("dataset_ID", "taxa")], by.x = "datasetID", by.y = "dataset_ID")
turnover_col = merge(turnover_taxa, taxcolors, by = "taxa")
# bbs column for diff point symbols
turnover_col$bbs =ifelse(turnover_col$datasetID == 1, "yes", "no")
turnover_bbs = filter(turnover_col, bbs == "yes")
turnover_else = filter(turnover_col, bbs == "no")

turnover_else$taxa = factor(as.character(turnover_else$taxa),
                            levels = c('Bird','Fish','Invertebrate','Mammal','Plankton','Plant'),ordered = TRUE)

colscale = c("#1D6A9B","turquoise2","gold2", "purple4","red", "forestgreen") 

m <- ggplot(turnover_bbs, aes(x = TJ, y = notransturn))
four_c <- m + geom_abline(intercept = 0,slope = 1, lwd =1.5,linetype="dashed")+geom_point(data = turnover_bbs, aes(colour = taxa),size = 2)+geom_point(data = turnover_else, aes(colour = taxa), size = 5) + xlab("Turnover (all species)") + ylab("Turnover \n (excluding non-transients)")  + scale_colour_manual(breaks = turnover_col$taxa,values = colscale) + theme_classic() + theme(axis.text.x=element_text(size=30, color = "black"),axis.text.y=element_text(size=30, color = "black"),axis.ticks.x=element_blank(),axis.title.x=element_text(size=42, color = "black"),axis.title.y=element_text(size=42,angle=90,vjust = 3))+ guides(colour = guide_legend(title = "Taxa"))
ggsave(file="C:/Git/core-transient/output/plots/null_turnover_2.pdf", height = 10, width = 15)

turnover_taxa$diff = turnover_taxa$notransturn-turnover_taxa$TJnotrans
hist(turnover_taxa$diff, xlab = "Excluding null transients - Excluding transients", cex.lab=1.5, cex.axis=1.5)


ggplot(data=turnover_taxa, aes(turnover_taxa$notransturn-turnover_taxa$TJnotrans)) + 
  geom_histogram(bins = 16, col="black",  fill="white") + theme_classic() + theme(axis.text.x=element_text(size=30,angle=90, color = "black"), axis.ticks.x=element_blank(),axis.text.y=element_text(size=30, color = "black"),axis.title.y=element_text(size=30,angle=90, vjust = 4),axis.title.x=element_text(size=30, vjust = -4))  + ylab("Frequency") + xlab("Akaike weight difference \n (excluding non-transients - excluding transients)") +theme(plot.margin=unit(c(0.35,1,2,1.7),"cm")) 
ggsave(file="C:/Git/core-transient/output/plots/1c_hist.pdf", height = 10, width = 14)


##### paired t-test #####
wilcox.test(turnover_taxa$notransturn-turnover_taxa$TJnotrans)

df_ttest_5c = merge(null_5c, turnover_output, by = c("datasetID", "site"))

t.test(df_ttest_5c$notransturn, 
       df_ttest_5c$TJnotrans, 
       paired=TRUE, 
       conf.level=0.95)



ggplot(data=turnover_taxa, aes(turnover_taxa$diff)) + 
  geom_histogram(bins = 12, col="black",  fill="white") + scale_y_continuous(breaks=c(0,400,800))+ theme_classic() + theme(axis.text.x=element_text(size=30,angle=90, color = "black"), axis.ticks.x=element_blank(),axis.text.y=element_text(size=25, color = "black"),axis.title.y=element_text(size=30,angle=90, vjust = 4),axis.title.x=element_text(size=30, vjust = -4))  + ylab("Frequency") + xlab("Excluding non-transients - All species") +theme(plot.margin=unit(c(0.35,1,2,1.7),"cm")) 

