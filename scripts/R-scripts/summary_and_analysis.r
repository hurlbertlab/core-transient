###############################################
# Code for running core-transient analysis
# and data summaries over all formatted datasets.
#
# Input files are named propOcc_XXX.csv where
# XXX is the dataset ID.

setwd("C:/git/core-transient")

library(dplyr)
library(ggplot2)
library(tidyr)
library(maps)
library(viridis)
library(RColorBrewer)

source('scripts/R-scripts/core-transient_functions.R')

# Maximum occupancy of transient species
# (and hence the minimum occupancy of core species is 1 - threshold)
threshold = 1/3

# Number of replicates for randomization tests
reps = 999

##################################################################

# If running summaries for the first time (or wanting to start
# anew because all formatted datasets have changed) and a
# 'core-transient_summary.csv' file does not exist yet in the 
# output/tabular_data folder, or if you just want to get summary
# stats for one or a few datasets into R, run this section

# Specify here the datasetIDs and then run the code below.
dataformattingtable = read.csv('data_formatting_table.csv', header = T) 

datasetIDs = dataformattingtable$dataset_ID[dataformattingtable$format_flag == 1]

datasetIDs = datasetIDs[!datasetIDs %in% c(317)]

summaries = c()
for (d in datasetIDs) {
  newsumm = summaryStatsFun(d, threshold, reps)
  summaries = rbind(summaries, newsumm)
  print(d)
}

write.csv(summaries, 'output/tabular_data/core-transient_summary.csv', 
          row.names = T)

##################################################################

# If running summaries for the newly updated or created formatted
# datasets to be appended to the existing 'core-transient_summary.csv'
# file, then run this section.

# If you do not want to re-write the existing file, set write = FALSE.

# Also, this command can be used instead of the section above to
# create the 'core-transient_summary.csv' file from scratch for all
# datasets with formatted data.

summ = addNewSummariesFun(threshold, reps, write = TRUE)


#####################lump reptile and ampibian into herptile, get rid of invert if possible - other category?, do a table of communities

# Plotting summary results across datasets for Core-Transient analysis

summ = read.csv('output/tabular_data/core-transient_summary.csv', header=T)
summ$taxa = factor(summ$taxa)
summ$system = factor(summ$system)
summ2 = subset(summ, !datasetID %in% c(99, 85, 90, 91, 92, 97, 124))
dsets = unique(summ2[, c('datasetID', 'system','taxa')])
summ$taxa[summ$taxa == "Arthropod"] <- "Invertebrate"
summ$taxa[summ$taxa == "Herptile"] <- NA
summ = na.omit(summ)
taxorder = c('Bird', 'Plant', 'Mammal', 'Fish', 'Invertebrate', 'Benthos', 'Plankton')

dsetsBySystem = table(dsets$system)
dsetsByTaxa = table(dsets$taxa)
sitesBySystem = table(summ2$system)
sitesByTaxa = table(summ2$taxa)

colors7 = c(rgb(29/255, 106/255, 155/255), # invert
            colors()[552], # plankton
            colors()[612],# plant
            colors()[144], # arth
            rgb(0, 54/255, 117/255), #herp
            colors()[600], #fish
            colors()[551])#mammal

symbols7 = c(16, 18, 167, 15, 17, 1, 3) 

summ$taxa <-droplevels(summ$taxa, exclude = c("Arthropod","Amphibian", "Reptile"))
taxcolors = data.frame(taxa = unique(summ$taxa), color = colors7, pch = symbols7)

pdf('output/plots/data_summary_hists.pdf', height = 8, width = 10)
par(mfrow = c(2, 2), mar = c(6,6,1,1), cex = 1.25, oma = c(0,0,0,0), las = 1,
    cex.lab = 1)
b1=barplot(dsetsBySystem, col = c('skyblue', 'navy', 'burlywood')) 
#text(b1, par("usr")[3], dsetsBySystem, srt = 60, adj = c(1, 1), xpd = TRUE, cex = 1)
mtext("# Datasets", 2, cex = 1, las = 0, line = 2.5)
barplot(log10(sitesBySystem), col = c('skyblue', 'navy', 'burlywood'), cex.names = 1, 
        yaxt = "n", ylim = c(0,3)) 
axis(2, 0:3)
mtext(expression(log[10] ~ " # Assemblages"), 2, cex = 1.5, las = 0, line = 2.5)
bar1 = barplot(dsetsByTaxa[taxorder], xaxt = "n", axisnames = F,
               col = as.character(taxcolors$color[match(taxorder, taxcolors$taxa)]))
text(bar1, par("usr")[3], taxorder, srt = 60, adj = c(1, 1), xpd = TRUE, cex = 1) # srt controls angle of text

mtext("# Datasets", 2, cex = 1.5, las = 0, line = 2.5)
bar2 = barplot(log10(sitesByTaxa[taxorder]), axes = F, axisnames = F, ylim = c(0,3),
               col = as.character(taxcolors$color[match(taxorder, taxcolors$taxa)]))
text(bar2, par("usr")[3], taxorder, srt = 60, adj = c(1, 1), xpd = TRUE, cex = 1)
axis(2, 0:3)
mtext(expression(log[10] ~ " # Assemblages"), 2, cex = 1.5, las = 0, line = 2.5)
dev.off()

######################################################################################### legend not right
# Summarizing datasets based on beta distribution parameters
pdf('output/plots/alpha_vs_beta.pdf', height = 6, width = 8)
par(mfrow = c(1,1), mar = c(5,5,1,1), mgp = c(3,1, 0), cex.axis = 1, cex.lab = 1, las = 1)
plot(summ3$alpha, summ3$beta, type = "n", xlim = c(-0.25,3.5), xlab = "alpha", ylab = "beta",
     ylim = c(0,4), yaxt = "n")
axis(2, 0:4, cex = 1)
points(summ3$alpha, summ3$beta, pch = summ3$pch, col = summ3$color, font = 5, cex = 1)
abline(a=0, b=1, lty = 'dotted', lwd = 4)
rect(-1, -1, 1, 1, lty = 'dashed', lwd = 2)
legend('topleft', legend = unique(summ$taxa), pch = symbols7, 
       col = c(colors7, 'white', colors7), pt.cex = 1.5, cex = 1.25)
points(-.28, 2.65, pch = symbols7[6], font = 5, col = colors7[6], cex = 1.7)
text(3,3.2, substitute(paste(alpha, " = ", beta)), srt = 40, cex = 2)
dev.off()

# Summarizing datasets based on beta distribution parameters
pdf('output/plots/alpha_vs_beta_log.pdf', height = 6, width = 8)
par(mfrow = c(1,1), mar = c(5,5,1,1), mgp = c(3,1, 0), cex.axis = 1, cex.lab = 1, las = 1)
plot(summ3$alpha, summ3$beta, type = "n", xlim = c(-0.25,3.5), xlab = "alpha", ylab = "beta",
     ylim = c(0,4), yaxt = "n")
axis(2, 0:4, cex = 1)
points(log(summ3$alpha), log(summ3$beta), pch = summ3$pch, col = summ3$color, font = 5, cex = 1)
abline(a=0, b=1, lty = 'dotted', lwd = 4)
rect(-1, -1, 1, 1, lty = 'dashed', lwd = 2)
legend('topleft', legend = unique(summ$taxa), pch = symbols7, 
       col = c(colors7, 'white', colors7), pt.cex = 1.5, cex = 1.25)
points(-.28, 2.65, pch = symbols7[6], font = 5, col = colors7[6], cex = 1.7)
text(3,3.2, substitute(paste(alpha, " = ", beta)), srt = 40, cex = 2)
dev.off()
l = c(col1, col2, col3, col4), pch = 16, cex = 1.5, pt.cex = 2)
dev.off()

##################################################################
# Summary of % transients versus community size using regression lines
#occ_taxa=read.csv("occ_taxa.csv",header=TRUE)
#datasetIDs = filter(dataformattingtable, spatial_scale_variable == 'Y',
                   # format_flag == 1)$dataset_ID
#datasetIDs = datasetIDs[datasetIDs  != 317]

############################################# ADD IN BBS!!!!!!
pdf('output/plots/sara_scale_transient_reg.pdf', height = 6, width = 7.5)
par(mfrow = c(1, 1), mar = c(6, 6, 1, 1), mgp = c(4, 1, 0), 
    cex.axis = 1.5, cex.lab = 2, las = 1)
palette(colors7)

### Have to cut out stuff that have mean abundance NA
datasetIDs =  datasetIDs[!datasetIDs %in% c(67,270,271,319,325)]

for(id in datasetIDs){
  print(id)
  plotsub = subset(summ2, datasetID == id)
  mod3 = lm(plotsub$propTrans ~ log10(plotsub$meanAbundance))
  xnew = range(log10(plotsub$meanAbundance))
  xhat <- predict(mod3, newdata = data.frame((xnew)))
  xhats = range(xhat)
  print(xhats)
  taxcolor = subset(taxcolors, taxa == as.character(plotsub$taxa)[1])
  y=summary(mod3)$coef[1] + (xhats)*summary(mod3)$coef[2]
  plot(NA, xlim = c(-1, 7), ylim = c(0,1), col = as.character(taxcolors$color), xlab = expression("Log"[10]*" Community Size"), ylab = "% Transients", cex = 1.5)
  lines(log10(plotsub$meanAbundance), fitted(mod3), col=as.character(taxcolors$color),lwd=3)
  par(new=TRUE)
}
legend('topright', legend = taxcolors$taxa, lty=1,lwd=3,col = as.character(taxcolors$color), cex = 1.25, border = "white")
dev.off()

pdf('output/plots/sara_scale_core_reg.pdf', height = 6, width = 7.5)
par(mfrow = c(1, 1), mar = c(6, 6, 1, 1), mgp = c(4, 1, 0), 
    cex.axis = 1.5, cex.lab = 2, las = 1)
palette(colors7)
for(id in datasetIDs){
  print(id)
  plotsub = subset(summ2,datasetID == id)
  mod3 = lm((1-plotsub$propTrans) ~ log10(plotsub$meanAbundance))
  xnew=range(log10(plotsub$meanAbundance))
  xhat <- predict(mod3, newdata = data.frame((xnew)))
  xhats = range(xhat)
  print(xhats)
  taxcolor=subset(taxcolors, taxa == as.character(plotsub$taxa)[1])
  y=summary(mod3)$coef[1] + (xhats)*summary(mod3)$coef[2]
  plot(NA, xlim = c(-1, 7), ylim = c(0,1), col = as.character(taxcolor$color), xlab = expression("Log"[10]*" Community Size"), ylab = "% Core", cex = 1.5)
  lines(log10(plotsub$meanAbundance), fitted(mod3), col=as.character(taxcolor$color),lwd=3)
  par(new=TRUE)
}
legend('bottomright', legend = taxcolors$taxa, lty=1,lwd=3,col = as.character(taxcolors$color), cex = 1.25, border = "white")
dev.off()


#### barplot of mean occ by taxa #####
numCT_plot$taxa = as.factor(numCT_plot$taxa)
numCT_plot$taxa <-droplevels(numCT_plot$taxa, exclude = c("","All","Amphibian", "Reptile"))

# n calculates number of sites by taxa -nested sites
n = numCT_taxa %>%
  dplyr::count(site, taxa) %>%
  group_by(taxa) %>%
  tally(n)
n = data.frame(n)
# calculates number of sites by taxa -raw
sitetally = summ %>%
  dplyr::count(site, taxa) %>%
  group_by(taxa) %>%
  dplyr::tally()
sitetally = data.frame(sitetally)

numCT_box=merge(numCT_taxa, taxcolors, by="taxa")

nrank = summ %>% 
  group_by(taxa) %>%
  dplyr::summarize(mean(mu)) 
nrank = data.frame(nrank)
nrank = arrange(nrank, desc(mean.mu.))

summ_plot = merge(summ, nrank, by = "taxa", all.x=TRUE)

summ$taxa <- factor(summ$taxa,
                       levels = c('Bird','Mammal','Plankton','Benthos','Invertebrate','Plant','Fish'),ordered = TRUE)
rankedtaxorder = c('Bird','Mammal','Plankton','Benthos','Invertebrate','Plant','Fish')

dsetsBySystem = table(dsets$system)
dsetsByTaxa = table(dsets$taxa)
sitesBySystem = table(summ2$system)
sitesByTaxa = table(summ2$taxa)

colorsrank = c(rgb(0, 54/255, 117/255), #bird
               colors()[551],#mammal
               colors()[552], # plankton
               rgb(29/255, 106/255, 155/255), # benthos
               colors()[144], # arth
               colors()[612],# plant
               colors()[600]) #fish


symbols7 = c(16, 18, 167, 15, 17, 1, 3) 
taxcolorsrank = data.frame(taxa = unique(summ$taxa), color = colorsrank, pch = symbols7)

w <- ggplot(summ, aes(factor(taxa), mu))+theme_classic()+
  theme(axis.text.x=element_text(angle=90,size=10,vjust=0.5)) + xlab("Taxa") + ylab("Mean Occupancy\n")
w + geom_boxplot(width=1, position=position_dodge(width=0.6),aes(x=taxa, y=mu), fill = taxcolorsrank$color)+
  scale_fill_manual(labels = taxcolors$taxa, values = taxcolors$color)+theme(axis.ticks=element_blank(),axis.text.x=element_text(size=14),axis.text.y=element_text(size=14),axis.title.x=element_text(size=19),axis.title.y=element_text(size=19,angle=90,vjust = 1)) + guides(fill=guide_legend(title=""))+ theme(plot.margin = unit(c(.5,.5,.5,.5),"lines")) + annotate("text", x = nrank$taxa, y = 1.05, label = sitetally$n,size=5,vjust=0.8, color = "black")
ggsave("C:/Git/core-transient/output/plots/meanOcc.pdf", height = 8, width = 12)

####################  ######################################################
# Explaining variation in mean occupancy within BBS
# Merge taxa color and symbol codes into summary data
summ$taxa = factor(summ$taxa)
summ$system = factor(summ$system)
summ2 = subset(summ, !datasetID %in% c(99, 85, 90, 91, 92, 97, 124))
summ3 = merge(summ2, taxcolors, by = 'taxa', all.x = T)
summ3$color = as.character(summ3$color)
notbbs = subset(summ3, datasetID != 1)
bbssumm = subset(summ3, datasetID == 1)
env = read.csv('data/raw_datasets/dataset_1RAW/env_data.csv')
bbsumm = merge(bbssumm, env, by.x = 'site', by.y = 'stateroute')

par(mfrow = c(2,1), mar = c(6, 4, 1, 1), mgp = c(3, 1, 0), 
    oma = c(0, 4, 0, 0), las = 1, cex.axis = 1.5, cex.lab = 2)
plot(bbsumm$sum.NDVI.mean, bbsumm$mu, xlab = "NDVI", ylab = "", pch = 16, col = 'gray40')
lm.ndvi = lm(mu ~ sum.NDVI.mean, data = bbsumm)
abline(lm.ndvi, col = 'red', lty = 'dashed', lwd = 4)
text(0.25, 0.85, bquote(R^2 ~ "=" ~ .(round(summary(lm.ndvi)$r.squared, 2))), cex = 1.5)
plot(bbsumm$elev.mean, bbsumm$mu, xlab = "Elevation (m)", ylab = "", pch = 16, col = 'gray40')
lm.elev = lm(mu ~ elev.mean, data = bbsumm)
abline(lm.elev, col = 'red', lty = 'dashed', lwd = 4)
text(2600, 0.85, bquote(R^2 ~ "=" ~ .(round(summary(lm.elev)$r.squared, 2))), cex = 1.5)
mtext("Mean occupancy", 2, outer = T, cex = 2, las = 0)



# Boxplots showing distribution of core and transient species by taxon.
core = summ2 %>%
  dplyr::group_by(taxa) %>%
  dplyr::summarize(mean(propCore)) 
trans = summ2 %>%
  dplyr::group_by(taxa) %>%
  dplyr::summarize(mean(propTrans)) 


propCT = merge(core, trans, by = "taxa")
propCT = data.frame(propCT)
propCT$mean.propNeither. = 1 - propCT$mean.propCore. - propCT$mean.propTrans.


propCT_long = gather(propCT, "class","value", c(mean.propCore.:mean.propNeither.))
propCT_long = arrange(propCT_long, class)
colscale = c("#7fcdbb", "#1d91c0", "#225ea8")

ggplot(data=propCT_long, aes(factor(taxa), y=value, fill=factor(class))) + geom_bar(stat = "identity")  + theme_classic() + xlab("Taxa") + ylab("Proportion of Species")+
  scale_fill_manual(labels = c("Core", "Other", "Transient"),
                    values = colscale)+theme(axis.ticks=element_blank(),axis.text.x=element_text(size=18, angle=45, vjust = 0.7),axis.text.y=element_text(size=18),axis.title.x=element_text(size=20),axis.title.y=element_text(size=20,angle=90,vjust = 2.5))+guides(fill=guide_legend(title="", reverse=TRUE)) + theme(legend.text=element_text(size=20))

ggsave("C:/Git/core-transient/output/plots/pctCTO.pdf", height = 8, width = 12)
##################################################################
# barplot of % transients versus community size at diff thresholds
summaryTransFun = function(datasetID){
  # Get data:
  dataList = getDataList(datasetID)
  sites  = as.character(dataList$siteSummary$site)
  # Get summary stats for each site:         #where is the problem coming from?!
  outList = list(length = length(sites))
  for(i in 1:length(sites)){
    propOcc = subset(dataList$propOcc, site == sites[i])$propOcc
    siteSummary = subset(dataList$siteSummary, site == sites[i])
    nTime = siteSummary$nTime
    spRichTotal = siteSummary$spRich
    spRichCore33 = length(propOcc[propOcc >= 1 - .33])
    spRichTrans33 = length(propOcc[propOcc <= .33])
    spRichTrans25 = length(propOcc[propOcc <= .25])
    spRichTrans10 = length(propOcc[propOcc <= .1])
    propCore33 = spRichCore33/spRichTotal
    propTrans33 = spRichTrans33/spRichTotal
    propTrans25 = spRichTrans25/spRichTotal
    propTrans10 = spRichTrans10/spRichTotal
    outList[[i]] = data.frame(datasetID, site = sites[i],
                              system = dataList$system, taxa = dataList$taxa,
                              nTime, spRichTotal, spRichCore33, spRichTrans33,
                              propCore33,  propTrans33, propTrans25, propTrans10)
  }
  return(rbind.fill(outList))
}

percTransSummaries = c()
for (d in datasetIDs) {
  percTransSumm = summaryTransFun(d)
  percTransSummaries = rbind(percTransSummaries, percTransSumm)
  print(d)
}

CT_plot=merge(percTransSummaries, taxcolors, by="taxa")
CT_long = gather(CT_plot, "level_trans","pTrans", propTrans33:propTrans10)

uniqTaxa = unique(CT_plot$taxa)
#### barplot of percent transients by taxa ---FIXED
p <- ggplot(CT_long, aes(taxa, level_trans))+theme_classic()
p+geom_boxplot(aes(x=taxa, y=pTrans, fill = level_trans))

cols <- (CT_long$color)
colscale=c("#ece7f2","#9ecae1",  "#225ea8")

p+geom_boxplot(width=0.8,position=position_dodge(width=0.8),aes(x=taxa, y=pTrans, fill=level_trans))+ 
  scale_colour_manual(breaks = CT_long$level_trans,
                      values = taxcolors$color)  + xlab("Taxa") + ylab("Percent Transient")+
  scale_fill_manual(labels = c("Occupancy <= 10%", "Occupancy <= 25%", "Occupancy <= 33%"),
                    values = colscale)+theme(axis.ticks=element_blank(),axis.text.x=element_text(size=18, angle=45),axis.text.y=element_text(size=18),axis.title.x=element_text(size=20),axis.title.y=element_text(size=20,angle=90,vjust = 2))+guides(fill=guide_legend(title="")) + theme(legend.text=element_text(size=20))
ggsave("C:/Git/core-transient/output/plots/boxCT_perc.pdf", height = 8, width = 12)




