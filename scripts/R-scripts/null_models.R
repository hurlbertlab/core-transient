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
# write.csv(bbs_count5a, "data/standardized_datasets/dataset_1.csv", row.names = FALSE)

# bbs_abun_occ1 = subset(bbs_abun_occ, scale ==  50)
bbs_occ_aou = dplyr::rename(bbs_occ_aou, site = stateroute, species = aou, propOcc = occ)
bbs_occ_aou$datasetID  = 1
bbs_occ5a = bbs_occ_aou[, c("datasetID", "site", "species", "propOcc")]
# write.csv(bbs_occ5a, "data/propOcc_datasets/propOcc_1.csv", row.names = FALSE)

#' Get list of dataset IDS for datasets that meet criteria for analysis including:
#' * Study wide criteria
#' * Has raw abundance data (not cover or density)
get_valid_datasetIDs = function(){
  dataformattingtable = read.csv('data_formatting_table.csv')
  datasetIDs = dataformattingtable %>%
    filter(format_flag == 1, countFormat %in% c('count', 'abundance')) %>% 
    # Remove count datasets with decimal values
    filter(!dataset_ID %in% c(226, 228, 247, 264, 298, 299, 300, 301)) %>%
    # exclude BBS for now and analyze it separately
    # filter(dataset_ID !=1) %>% 
    dplyr::select(dataset_ID)
}


#' Get table of species abundances
#' 
get_abund_data  = function(datasetIDs){
  datasetIDs = datasetIDs$dataset_ID
  dataset_path = 'data/standardized_datasets/'
  abund_data = data.frame()
  for (dataset in datasetIDs){
    filename = paste('dataset_', dataset, '.csv', sep = '')
    print(paste("Loading:", filename))
    site_data = read.csv(file.path(dataset_path, filename), stringsAsFactors = FALSE, fileEncoding = 'latin1')
    abund_data = rbind(abund_data, site_data)
  }
  # Strip zeros which are included to document a sampling event occurred
  abund_data = abund_data[abund_data$count != 0,]
  return(abund_data)
}

#' Get table of species proportional occupancies
#' 
get_propocc_data  = function(datasetIDs){
  datasetIDs = datasetIDs$dataset_ID
  dataset_path = 'data/propOcc_datasets/'
  propocc_data = data.frame()
  for (dataset in datasetIDs){
    filename = paste('propOcc_', dataset, '.csv', sep = '')
    print(paste("Loading:", filename))
    site_data = read.csv(file.path(dataset_path, filename), stringsAsFactors = FALSE, fileEncoding = 'latin1')
    propocc_data = rbind(propocc_data, site_data)
  }
  return(propocc_data)
}


#' Sum the abundances for each species-site combination across years
sum_abunds = function(abund_data){
  summed_abunds = abund_data %>%
    group_by(datasetID, site, species) %>%
    dplyr::summarize(abunds = sum(count)) %>%
    filter(abunds != 0)
  return(summed_abunds)
}

sad_examp = c(109, 14, 4, 4, 680, 195, 13, 3, 123, 116, 1, 5, 105, 26, 14, 2, 9, 29, 15, 133, 5, 41, 45, 33, 
              17, 27, 37, 11, 169, 1, 27, 7, 19, 23, 100, 4, 8, 5, 19, 1, 21, 12, 6, 1, 10, 2, 1, 94, 2, 4, 28, 1, 3, 
              34, 3, 20, 72, 21, 1, 84, 10, 528, 18, 1, 1, 10, 10, 48, 7)

#' Get the AICc weight for the log-series compared to the Poisson log-normal
#' abundance distribution
#' 
#' @param abunds vector of abundances
#' 
#' @return vector of weights for the log-series
#' 
#' @examples 
#' get_sad_weights(c(10, 20, 5, 1, 1, 2, 3, 7))
get_logseries_weight = function(abunds){
  stopifnot(all(abunds == floor(abunds))) # Check that all values are counts
  abunds = abunds[abunds != 0] # SADs are fit only on species that are present
  tryCatch({
    fits = c(fitsad(abunds, 'ls'), fitsad(abunds, 'poilog'))
    aics = sapply(fits, AICc)
    min_aic = min(aics)
    deltas = aics - min_aic
    rellike = exp(-0.5 * deltas)
    weights = rellike / sum(rellike)
    logseries_weight = weights[1]
    return(logseries_weight)
  }, error = function(e) {
    logseries_weight = NA
    return(logseries_weight)
  }
  )
}

datasetIDs = get_valid_datasetIDs()
abund_data = get_abund_data(datasetIDs)
propocc_data = get_propocc_data(datasetIDs)
summed_abunds = sum_abunds(abund_data)
sad_data = left_join(summed_abunds, propocc_data, by = c('datasetID', 'site', 'species'))

colscale = c("azure4","#1D6A9B","turquoise2","gold2","purple4","red", "forestgreen")  

#### null model ####
null_output = c()
init.time = Sys.time()
for(id in datasetIDs[,1]){
  subdata = subset(sad_data, datasetID == id)
  sites = unique(subdata$site)
  for(site in sites){
    sitedata = subdata[subdata$site == site,]
    notrans = na.omit(sitedata[sitedata$propOcc > 1/3,])
    trans = na.omit(sitedata[sitedata$propOcc <= 1/3,])
    num_notrans = length(notrans$propOcc)
    num_trans = length(trans$propOcc)
    
    for(r in 1:100){
      print(c(r, id, Sys.time()))
      
      if(num_notrans >= num_trans) {
        null_sample = sample_n(notrans, num_notrans - num_trans, replace = FALSE) %>%
          rbind(trans)
      } else {
        null_sample = sample_n(trans, num_notrans, replace = FALSE)  
      }

      logseries_weights_excl = null_sample %>%
        filter(propOcc > 1/3) %>%
        group_by(datasetID, site) %>% 
        dplyr::summarize(weights = get_logseries_weight(abunds), treatment = 'Excluding')
      
      null_output = rbind(null_output, c(r, id, site, logseries_weights_excl[,3], num_notrans))
      nwd = nrow(sad_data)
      #curr.time = Sys.time()
     # elapsed = curr.time - init.time
      #percelltime = elapsed/i
     # estimated.end = (nwd - i)*percelltime + curr.time
     # print(paste(i, "out of",nwd, "; current time:", curr.time,
                 # "; estimated end time:", estimated.end))
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


write.csv(null_output, "output/tabular_data/null_output_SAD_1000.csv", row.names = FALSE)
null_output = read.csv("output/tabular_data/null_output_SAD_1000.csv", header = TRUE)
null_output$combo = paste(null_output$datasetID, null_output$site, sep = "_")

null_output1 = subset(null_output, number == 1)

null_5a_sum = null_output %>%
  dplyr::group_by(datasetID, site) %>% 
  dplyr::summarize(SAD_incl = mean(SAD_incl), var_incl = var(SAD_incl), 
                   SAD_excl = mean(SAD_excl), var_excl = var(SAD_excl))

# read in output from figure 5 script
logseries_weights = read.csv("output/tabular_data/logseries_weights.csv", header = TRUE)
logseries_excl = subset(logseries_weights, treatment == "Excluding")


logseries_excl = logseries_excl[,c("datasetID", "site", "weights", "treatment")]
null_output$weights = null_output$SAD_excl
null_output$treatment = "Null"
null_output = filter(null_output, number == 1)
null_output_excl = null_output[,c("datasetID", "site", "weights", "treatment")]
# removing BBS as a test
null_output_excl = filter(null_output_excl, datasetID != 1)

SAD_excl = rbind(logseries_excl, null_output_excl)


sad_excl = merge(logseries_excl, null_output, by = c("datasetID", "site"))
sad_excl_p = sad_excl %>% group_by(datasetID, site) %>%
  tally(SAD_excl >= weights)
num_excl = subset(sad_excl_p, n > 0)

logseries_incl = subset(logseries_weights, treatment == "All")
sad_incl = merge(logseries_incl, null_5a_sum, by = c("datasetID", "site"))

sad_incl_p = sad_incl %>% group_by(datasetID, site) %>%
  tally(SAD_incl >= weights)
num_incl = subset(sad_incl_p, n > 0)

logseries_null = gather(null_output, "treatment","weights", c(SAD_excl,SAD_incl))
logseries_null$treatment = factor(logseries_null$treatment, levels = c('SAD_incl','SAD_excl'),ordered = TRUE)
logseries_null = subset(logseries_null, number == 1)

##### need to merge logseries weights to null and plot 1 excl over null
logseries_join = merge(logseries_null, logseries_weights, by = c("datasetID", "site"))

#### ggplot fig1a #####

colscale = c("dark orange2","yellow")
k = ggplot(logseries_null,aes(x=weights,fill=treatment))+geom_histogram(bins = 20, position = "identity", alpha = 0.7)+ xlab("Transient Status") + ylab("Proportion of Species") + scale_y_continuous(breaks=c(0,500,1000)) + scale_fill_manual(labels = c("All species","Excluding transients"),values = colscale)+ theme_classic() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.text.y=element_text(size=30, color = "black"),axis.title.y=element_text(size=46,angle=90,vjust = 5),axis.title.x=element_text(size=46, vjust = -7))  + ylab("Frequency") + xlab("Akaike Weight") + theme(legend.position = "none") +theme(plot.margin=unit(c(0.35,1,2,1.7),"cm")) 

excl = ggplot(SAD_excl,aes(x=weights,fill=treatment))+geom_histogram(bins = 20, position = "identity", alpha = 0.7)+ xlab("Transient Status") + ylab("Proportion of Species") + scale_y_continuous(breaks=c(0,500,1000, 1300)) + scale_fill_manual(labels = c("Excluding transients", "Excluding non-transients"),values = c("black", "gray"))+ theme_classic() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.text.y=element_text(size=30, color = "black"),axis.title.y=element_text(size=46,angle=90,vjust = 8),axis.title.x=element_text(size=46, vjust = -7))  + ylab("Frequency") + xlab("Akaike Weight") +theme(plot.margin=unit(c(0.35,1,2,1.7),"cm")) + theme(legend.text = element_text(size = 15), legend.title = element_blank(), legend.position = "none")

ggsave(file="C:/Git/core-transient/output/plots/1a_null.pdf", height = 10, width = 16)


ks.test(logseries_excl$weights, null_output_excl$weights)
# D = 0.24116, p-value < 2.2e-16


#### NULL 5B #####
# read in route level ndvi and elevation data (radius = 40 km)
# we want to agg by month here
bbs_occ_2014 = read.csv("data/BBS/bbs_occ_2000_2014.csv", head = TRUE)
gimms_ndvi = read.csv("output/tabular_data/gimms_ndvi_bbs_data.csv", header = TRUE)
gimms_agg = gimms_ndvi %>% filter(month == c("may", "jun", "jul")) %>% 
  group_by(site_id)  %>%  summarise(ndvi=mean(ndvi))

lat_scale_rich = read.csv("output/tabular_data/lat_scale_rich.csv", header = TRUE)
lat_scale_bbs = filter(lat_scale_rich, datasetID == 1)
lat_scale_bbs$site_id = sapply(strsplit(as.character(lat_scale_bbs$site), split='-', fixed=TRUE), function(x) (x[1]))
lat_scale_bbs$site_id = as.integer(lat_scale_bbs$site_id)



null_5b = c()
null_sample = c()
sites = unique(bbs_occ_2014$stateroute)[1:20]
for(r in 1:10){
  print(r)
  for(site in sites){
    print(site)
  id = 1
  sitedata = bbs_occ_2014[bbs_occ_2014$stateroute == site,]
  notrans = sitedata[sitedata$occ > 1/3,]
  trans = sitedata[sitedata$occ <= 1/3,]
  size = abs(length(notrans$occ) - length(trans$occ))

  if(size < length(notrans$occ)){
    subcor = sample_n(notrans, size, replace = FALSE)  
  }
  null_sample.5 = rbind(trans, subcor)
  null_sample = rbind(null_sample, null_sample.5)

    # calc bbs with and without trans
    notransbbs = null_sample %>% filter(occ > 1/3) %>% dplyr::count(stateroute) 
    names(notransbbs) = c("site_id", "spRich")
    allbbs = null_sample %>% dplyr::count(stateroute) 
    names(allbbs) = c("site_id", "spRich")

    bbs_spRich = merge(allbbs, notransbbs, by = "site_id")
    colnames(bbs_spRich) = c("site_id", "spRich", "spRichnotrans")
    
    # merging ndvi and elevation to bbs data
    bbs_env = join(bbs_spRich, gimms_agg, type = "left")
    bbs_env = join(bbs_env, unique(lat_scale_bbs[,c("site_id", "elev.point", "elev.mean", "elev.var")]), type = "left")
  }
    null_sample = c()
    bar1 = cor.test(bbs_env$spRich, bbs_env$ndvi)$estimate
    print(bar1)
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


    null_5b = rbind(null_5b, c(r, id, bar1, CI1lower,CI1upper, bar3, CI3lower, CI3upper, bar2, CI2lower,CI2upper, bar4, CI4lower,CI4upper, bar5, CI5lower,CI5upper, bar6, CI6lower,CI6upper, numnon = as.numeric(length(notrans$occ))))
    
  }

null_5b = data.frame(null_5b)
colnames(null_5b) = c("r", "datasetid", "all_est_ndvi1", "CI1lower","CI1upper", "all_est_elev3", "CI3lower", "CI3upper", "excl_est_ndvi2", "CI2lower","CI2upper", "excl_est_elev4", "CI4lower","CI4upper", "trans_est_ndvi5", "CI5lower","CI5upper", "trans_est_elev6", "CI6lower","CI6upper", "numnon")


corr_res <- data.frame(All = c(bar1, bar3), Ntrans = c(bar2, bar4), Trans = c(bar5, bar6)) 
corr_res$env = c("NDVI", "Elevation")
corr_res_long = gather(corr_res, "class","value", c(All:Trans))
corr_res_long$CIlower = c(CI1lower,CI3lower,CI2lower,CI4lower, CI5lower, CI6lower)
corr_res_long$CIupper = c(CI1upper,CI3upper,CI2upper,CI4upper, CI5upper, CI6upper)
corr_res_long$env = factor(corr_res_long$env, levels = c("NDVI", "Elevation"), ordered = TRUE)

corr_NDVI = filter(corr_res_long, env == "NDVI")
corr_elev = filter(corr_res_long, env == "Elevation")
colscale = c("dark orange2","yellow","#c51b8a")
limits = aes(ymax = corr_res_long$CIupper, ymin=corr_res_long$CIlower)
# no variation - add in CIS?
l = ggplot(data=corr_res_long, aes(factor(env), value, fill = class, alpha = 0.7))+ geom_bar(width = 0.8, position = position_dodge(width = 0.9), stat="identity")+ scale_fill_manual(values = c("All" = "dark orange2","Trans" = "#c51b8a","Ntrans" = "yellow"), labels = c("All species","Excluding transients", "Transients only"))+ geom_bar(data=corr_res_long, aes(factor(env), value, fill = class), width = 0.8, position = position_dodge(width = 0.9), stat="identity")+ geom_errorbar(aes(ymin = corr_res_long$CIlower, ymax = corr_res_long$CIupper), width =.1, position = position_dodge(.9))+ theme_classic() + theme(axis.text.x=element_text(size=46, color = "black", vjust = 5), axis.ticks.x=element_blank(),axis.text.y=element_text(size=30, color = "black"),axis.title.x=element_text(size=46, color = "black"),axis.title.y=element_text(size=46,angle=90,vjust = 2))+ xlab(NULL) + ylab(expression(paste(italic("r")))) + scale_y_continuous(breaks=c(-0.5,-0.3,-0.1,.1,.3,.5))+ guides(fill=guide_legend(title=NULL)) + theme(legend.text = element_text(size = 38), legend.title = element_blank(), legend.key.height=unit(2,"line")) + geom_hline(yintercept=0, lty = "dashed", lwd = 1.25) + theme(plot.margin=unit(c(1,1,2,1),"cm"))
four_b <- l
ggsave(file="C:/Git/core-transient/output/plots/5b_corrcoeff_NDVI.pdf", height = 5, width = 15)






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

# datasetIDs = subset(datasetIDs, datasetIDs != 1)

abund_data = get_abund_data(datasetIDs)
propocc_data = get_propocc_data(datasetIDs)
all_data = left_join(abund_data, propocc_data, by = c('datasetID', 'site', 'species'))

# add in bbs data (id, site, year, sp, count, propOcc)
bbs = read.csv("data/BBS/bbs_2000_2014.csv", header = TRUE)
bbs$year = bbs$Year
bbs_abun_occ = read.csv("data/BBS/bbs_abun_occ.csv", header = TRUE)
bbs_abun_occ$species = bbs_abun_occ$AOU
bbs_abun_occ$site = bbs_abun_occ$stateroute
bbs_occ= merge(bbs_abun_occ[,c("species","site", "occupancy")],bbs, by.x = c("species","site"), by.y = c("aou","stateroute"))
bbs_occ$propOcc = bbs_occ$occupancy
bbs_occ$count = bbs_occ$speciestotal
bbs_occ$datasetID = 1
bbs_occ = bbs_occ[,c("datasetID", "site", "year", "species", "count", "propOcc")]

# rbind to get single data frame
all_data = rbind(all_data, bbs_occ)


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
    TJs = c()
    TJ_notrans = c()
    
    for(i in 1:100){
      print(c(i, dataset, site))
      if(num_notrans >= num_trans) {
        null_sample = sample_n(notrans, num_notrans, replace = FALSE) %>%
          rbind(trans)
      } else {
        null_sample = sample_n(trans, num_notrans, replace = FALSE)  
      }
      if(length(years) > 0){
        
        for (year in years[1:(length(years)-1)]) {
          comm1 = unique(null_sample$species[null_sample$year == year])
          comm2 = unique(null_sample$species[null_sample$year == year + 1])
          T_J = turnover(comm1, comm2)
          TJs = c(TJs, T_J)
          
          notrans2 = null_sample[null_sample$propOcc > 1/3,]
          comm1_noT = unique(notrans2$species[notrans2$year == year])
          comm2_noT = unique(notrans2$species[notrans2$year == year + 1])
          T_J_notran = turnover(comm1_noT, comm2_noT)
          TJ_notrans = c(TJ_notrans, T_J_notran)
        }
      }
      nwd = nrow(all_data)
      null_5c = rbind(null_5c, c(i, dataset, site, T_J, T_J_notran, num_notrans))
      curr.time = Sys.time()
      elapsed = curr.time - init.time
      percelltime = elapsed/i
      estimated.end = (nwd - i)*percelltime + curr.time
      print(paste(i, "out of",nwd, "; current time:", curr.time,
           "; estimated end time:", estimated.end))

    }
  }
}


null_5c = data.frame(null_5c)
colnames(null_5c) = c("r", "datasetID", "site", "turnover","notransturn", "numnon")
write.csv(null_5c, "output/tabular_data/null_5c.csv", row.names = FALSE)
null_5c = read.csv("output/tabular_data/null_5c.csv", header = TRUE)

null_5c_output = null_5c %>% group_by(datasetID, site) %>%
  summarize(mean = mean(turnover), var = var(turnover))

# read in output from figure 5 script
turnover_output = read.csv("output/tabular_data/temporal_turnover.csv", header = TRUE)
turnover_merge = merge(turnover_output, null_5c, by = c("datasetID", "site"))

turnover_excl = turnover_merge %>% group_by(datasetID, site) %>%
  tally(TJnotrans >= notransturn)
num_excl_5c = subset(turnover_excl, n > 0)

turnover_incl = turnover_merge %>% group_by(datasetID, site) %>%
  tally(TJ >= turnover)
num_incl_5c = subset(turnover_incl, n > 0)

hist(turnover_excl$n, xlab = "", main = "Distribution of turnover excluding transients")
abline(v=mean(na.omit(logseries_excl$weights)), col = "blue", lwd = 2)

hist(sad_incl_p$n, xlab = "", main = "Distribution of the number of null sites greater \n than logseries weights including transients")
abline(v=mean(na.omit(logseries_incl$weights)), col = "blue", lwd = 2)

##### plot 5c ####
null_5cplot = subset(null_5c, r == 1)
turnover_taxa = merge(null_5cplot,dataformattingtable[,c("dataset_ID", "taxa")], by.x = "datasetID", by.y = "dataset_ID")
turnover_col = merge(turnover_taxa, taxcolors, by = "taxa")

# bbs column for diff point symbols
turnover_col$bbs =ifelse(turnover_col$datasetID == 1, "yes", "no")
turnover_bbs = filter(turnover_col, bbs == "yes")
turnover_else = filter(turnover_col, bbs == "no")

turnover_else$taxa = factor(as.character(turnover_else$taxa),
                            levels = c('Bird','Fish','Invertebrate','Mammal','Plankton','Plant'),ordered = TRUE)

colscale = c("#1D6A9B","turquoise2","gold2", "purple4","red", "forestgreen") 

m <- ggplot(turnover_bbs, aes(x = turnover, y = notransturn))
four_c <-m + geom_abline(intercept = 0,slope = 1, lwd =1.5,linetype="dashed")+geom_point(data = turnover_bbs, aes(colour = taxa),size = 2)+geom_point(data = turnover_else, aes(colour = taxa), size = 5) + xlab("Turnover (all species)") + ylab("Turnover \n (excluding non-transients)")  + scale_colour_manual(breaks = turnover_col$taxa,values = colscale) + theme_classic() + theme(axis.text.x=element_text(size=28, color = "black"),axis.text.y=element_text(size=28, color = "black"),axis.ticks.x=element_blank(),axis.title.x=element_text(size=42, color = "black"),axis.title.y=element_text(size=42,angle=90,vjust = 3))+ guides(colour = guide_legend(title = "Taxa"))
ggsave(file="C:/Git/core-transient/output/plots/null_turnover.pdf", height = 10, width = 15)




##### paired t-test #####
df_ttest_5c = merge(null_5c, turnover_output, by = c("datasetID", "site"))

t.test(df_ttest_5c$notransturn, 
       df_ttest_5c$TJnotrans, 
       paired=TRUE, 
       conf.level=0.95)




hist(sad_excl_p$n, xlab = "", main = "Distribution of the number of null sites greater \n than logseries weights excluding transients")
abline(v=mean(na.omit(logseries_excl$weights)), col = "blue", lwd = 2)

hist(sad_incl_p$n, xlab = "", main = "Distribution of the number of null sites greater \n than logseries weights including transients")
abline(v=mean(na.omit(logseries_incl$weights)), col = "blue", lwd = 2)
##### Figure 5d ##### only scaled vars
bbs_below = read.csv("Z:/Snell/data/bbs_below.csv", header = TRUE)

null_5d = c()
sites = unique(bbs_below$site)

for(site in sites){
    sitedata = bbs_below[bbs_below$site == site,]
    notrans = sitedata[sitedata$occ > 1/3,]
    trans = sitedata[sitedata$occ <= 1/3,]
    size = length(notrans$occ) - length(trans$occ)
    for(r in 1:1000){
      print(r)
      subsar = sample_n(notrans, abs(size), replace = FALSE) 
      null_sample = rbind(subsar, trans)
    
      notransbbsscale = bbs_below %>% filter(occ > 1/3) %>% dplyr::count(site)
      names(notransbbsscale) = c("site","notrans")

      allbbsscale = bbs_below %>% dplyr::count(site) 
      names(allbbsscale) = c("site", "spRich")

      bbs_occ_scale.5 = merge(notransbbsscale, allbbsscale, by = "site")
      bbs_occ_scale = unique(merge(bbs_occ_scale.5, bbs_below[, c("site", "area")], by = "site"))
      
      
      scaleIDs = unique(bbs_occ_scale$site)
      slopes_bbs = data.frame(stateroute = NULL,
                        site = NULL,
                        taxa = NULL,
                        areaSlope = NULL,
                        areaSlope_noTrans = NULL)

      for(id in scaleIDs){
        print(id)
        plotsub = subset(bbs_occ_scale,site == id) 
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





####  non bbs ####

# rbind propOcc files
propOccfiles = list.files("data/spatialGrainAnalysis/propOcc_datasets")
allpropOcc = c()
for(file in propOccfiles){
  nfile= read.csv(paste("data/spatialGrainAnalysis/propOcc_datasets/", file, sep = ""), header= TRUE)
  nfile$scale = as.numeric(substring(file, 18,last = 18))
  nfile$site = as.factor(nfile$site)
  allpropOcc = rbind(allpropOcc, nfile)
}
allpropOcc = data.frame(allpropOcc)




scaleIDs = unique(occ_trans_area$datasetID)

scaleIDs = scaleIDs[! scaleIDs %in% c(279,225,248,254, 282,291)] # 248 tbd

slopes = data.frame(datasetID = NULL,
                    taxa = NULL,
                    areaSlope = NULL,
                    areaSlope_noTrans = NULL)
null_5dB = c()
occ_trans = c()
for(r in 1:100)
for(id in scaleIDs){
  subdata = subset(allpropOcc, datasetID == id)
  sites = unique(subdata$site)
  print(paste("Calculating SAR: dataset", id))
  for(site in sites){
    sitedata = allpropOcc[allpropOcc$site == site,]
    notrans = sitedata[sitedata$propOcc > 1/3,]
    trans = sitedata[sitedata$propOcc <= 1/3,]
    size = abs(length(notrans$propOcc) - length(trans$propOcc))
  
    if(size < length(notrans$propOcc)){
      subsar = sample_n(notrans, abs(size), replace = FALSE) 
      null_sample = rbind(subsar, trans)
    }
  }
  # count up spRich with and without transients (for Fig 4)
    notransrich = allpropOcc %>% filter(propOcc > 1/3) %>% dplyr::count(datasetID, site, scale)
    allrich  = allpropOcc %>% dplyr::count(datasetID, site, scale)
  # merge sp rich and minus trans sprich other datasets
    notransrich$notrans = notransrich$n

    datasetrich = merge(allrich, notransrich[,c("datasetID", "site", "scale","notrans")], by = c("datasetID", "site", "scale"), all.x = TRUE)
    colnames(datasetrich)[4] <- "spRich" # rename a single column - make sure index is right

    occ_trans_area = merge(areamerge[,c("datasetID", "site", "area")], datasetrich, by = c("datasetID", "site"))
    occ_trans_area = merge(occ_trans_area, dataformattingtable[,c("dataset_ID", "taxa")], by.x = "datasetID", by.y = "dataset_ID")
    occ_trans_area = rbind(occ_trans, occ_trans_area)
  
    
    plotsub = merge(null_sample[, c("datasetID", "site", "species", "propOcc")], occ_trans_area, by = c("datasetID","site"))
    
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
      
      null_5dB = rbind(null_5dB, c(r, id, mod.n.slope,mod.t.slope, as.numeric(length(notrans$propOcc))))
       }
    }
  }
}

null_5dB = data.frame(null_5dB)
colnames(null_5dB) = c("r", "datasetID", "all_area_slope", "notrans_area_slope", "numnon")

slopes$bbs = 'no'

all_slopes =  rbind(slopes, slopes_bbs)


plot_relationship = merge(slopes, taxcolors, by = "taxa")
slopes_bbs = merge(slopes_bbs, taxcolors, by = "taxa")

plot_relationship$taxa = factor(plot_relationship$taxa,
                                levels = c('Invertebrate','Fish','Plankton','Mammal','Plant','Bird','Benthos'),ordered = TRUE)
colscales = c("gray","#1D6A9B","turquoise2","gold2","purple4", "red", "forestgreen") 

p <- ggplot(plot_relationship, aes(x = areaSlope, y = areaSlope_noTrans))
four_d <-p + geom_abline(intercept = 0,slope = 1, lwd =1.5,linetype="dashed") +geom_point(data=slopes_bbs, aes(colour = taxa),alpha = 5/100, size = 2)+  geom_point(aes(colour = taxa), size = 5)+ theme_classic() + scale_color_manual("Taxa", breaks = plot_relationship$taxa,values = colscales)+ xlab(expression(paste(italic("z "), "(all species)"))) + ylab(expression(paste(italic("z "), "(excluding transients)"))) +ylim(0,1)+xlim(0,1) + theme(axis.text.x=element_text(size=30, color = "black"),axis.ticks.x=element_blank(),axis.text.y=element_text(size=30, color = "black"),axis.title.x=element_text(size=46, color = "black"),axis.title.y=element_text(size=46,angle=90,vjust = 2))+ theme(legend.text = element_text(size = 38), legend.title = element_blank(), legend.key.height=unit(3,"line")) #,legend.position = c(.75, .3))


ggsave(file="C:/Git/core-transient/output/plots/5d_sparea.pdf", height = 10, width = 15)

