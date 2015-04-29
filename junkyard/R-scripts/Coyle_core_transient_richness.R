#This script calculates occupancy distributions and core and occasional species richness
#for BBS route with continuous data from 1996 - 2010.  It averages across values calculated for all possible 
#windows of size t (from 1:15) within the date range



##################################################################################
####PACKAGES####
##################################################################################


##################################################################################
####DATA####
##################################################################################

setwd('/largefs/jrcoyle/BBS/core16')

load('Core Richness 1996-2010_7.Rdata')


##################################################################################
####FUNCTIONS####
##################################################################################

#This function estimates the minimum of the occupancy distribution that has already been scales between 0 and 1

find.min = function(distribution){
  
  curve = density(distribution, n=100)
  X = curve$x[curve$x>0.1 & curve$x<1]
  Y = curve$y[curve$x>0.1 & curve$x<1]
  
  X[which(Y==min(Y))]
  
}

#A function that returns a list of core species for a particular route
#route is a row in sproute.matrix
#years is the number of years the route was surveyed
#breakpoint defines the occupancy proportions above and below which species should
#be considered core or occasional

find.core = function(route, years, breakpoint){
  #removes species never present on a route
  distribution = route[route>0]
  
  #scale between 0 and 1
  distribution = distribution/years
  
  #finds AOU of species that are core
  species = if(length(breakpoint)==1) species = list(core=names(distribution[distribution>breakpoint]))
  if(length(breakpoint)==2){
    core = names(distribution[distribution>breakpoint[2]])
    occa = names(distribution[distribution<breakpoint[1]])
    species=list(core=core,occa=occa)
  }
  return(species)
}


#A function that calculates core and occasional richness within a sample of data
#data should all be from one route between a set period of years
calc.rich = function(data, species.list){
  species.table = table(data$Aou)
  R=length(species.table)
  if(length(species.list)==1){
    Rcore=sum(names(species.table)%in%species.list[[1]])
    Rocca=R-Rcore
  }
  if(length(species.list)==2){
    Rcore=sum(names(species.table)%in%species.list[[1]])
    Rocca=sum(names(species.table)%in%species.list[[2]])
  }	
  return(cbind(R,Rcore,Rocca))
}

#A function that calculates the abundance of core versus occasional species in a route
#data should be all from one route within a given time interval

calc.abun = function(data, species.list){
  A = sum(data$SpeciesTotal)
  if(length(species.list)==1){
    Acore=sum(data[(data$Aou %in% species.list[[1]]),'SpeciesTotal'])
    Aocca=A-Acore
  }
  if(length(species.list)==2){
    Acore=sum(data[(data$Aou %in% species.list[[1]]),'SpeciesTotal'])
    Aocca=sum(data[(data$Aou %in% species.list[[2]]),'SpeciesTotal'])
  }	
  return(cbind(A,Acore,Aocca))
}


#A function that calculates occupancy distribution statistics given data for a particular route within a certain time window
calc.occu.dist = function(data,years){
  #Generate a vector of species occupancies
  distribution = table(data$Aou)
  distribution = distribution/years
  
  #Calculate cdf of distribution
  quants = sapply(seq(0.1,0.9,0.1),function(x) sum(distribution <= x)/length(distribution))
  
  dist.min = find.min(distribution)
  
  return(c(P0. = quants,dist.min=dist.min))
}	

#A function that calculates occupancy stats and richness based on a given data table and list of core species
#route - the names of the route (use stateroute)
#data - counts data
#core.species - a list whose elements are lists of core species' Aous
#t.window - an ordered pair of dates between which richness should be summed
#remove - a list of species lists to remove, make sure to specify names of lists i.e. list(ncore = never.core)

calc.occu.rich = function(route, data, species.list, t.window, remove){
  
  #Get data for this route
  use.data = subset(data, (stateroute == route)&(Year >= t.window[1])&(Year <= t.window[2]))
  use.cores = species.list[rownames(species.list)==as.character(route)]
  
  #Calculate richness of occasional and core species
  Rs = calc.rich(use.data,use.cores)
  
  #Calculate occupancy distribution over the time window
  Qs = calc.occu.dist(use.data,(1+t.window[2]-t.window[1]))
  
  #Calculate abundance of core and occasional birds
  As = calc.abun(use.data,use.cores)
  As = As/(1+t.window[2]-t.window[1])#Average abundance per year
  
  #Go through each of the species lists to remove
  NewRs = c()
  if(!is.null(remove)){
    for(i in 1:length(remove)){
      removed.data = use.data[!(use.data$Aou %in% as.numeric(remove[[i]])),]
      NewRs = cbind(NewRs,calc.rich(removed.data,use.cores)[3])
    }
    colnames(NewRs) = sapply(names(remove), function(x) paste('R',x,sep='.'))
  }
  
  return.row = c(Rs,NewRs,As,Qs)
  names(return.row) = c(colnames(Rs),colnames(NewRs),colnames(As),names(Qs))
  return.row
}

#A function that generates time windows of a particular size between two dates
make.windows = function(date.range,window.size){
  start = date.range[1]
  stop = start + window.size-1
  windows = c()
  while(stop <= date.range[2]){
    windows=rbind(windows,c(start,stop))
    start = start+1
    stop = stop+1
  }
  return(windows)
}


##################################################################################
####CODE####
##################################################################################
Bc = 0.5 #core breakpoint
Bo = 0.5 #occasional breakpoint

sproute = table(counts5all$stateroute,counts5all$Aou)
short.yroute = table(counts5all$stateroute,counts5all$Year)
num.years = apply(short.yroute, 1, function(x) length(x[x>0]))

short.core.list = sapply(1:nrow(sproute), function(x) find.core(sproute[x,],num.years[x],c(Bo,Bc)))
cores = short.core.list[1,]
names(cores) = rownames(sproute)
occas = short.core.list[2,]
names(occas) = rownames(sproute)
short.core.list = cbind(core = cores, occa = occas)


### Find always occasional and never core species

species = unique(counts5$Aou)
use.sproute = sproute[,colnames(sproute) %in% species] # removing excess species that do not occur in 1996-2010
occupancy.matrix = apply(use.sproute,2,function(x) x/num.years)

core.mat = ifelse(occupancy.matrix>Bc,T,F)
always.occa.mat=ifelse(occupancy.matrix>=Bo,T,F)
half.mat = ifelse(occupancy.matrix >= 0.5, T, F)
core.freq = apply(core.mat,2,sum)
always.occa.freq = apply(always.occa.mat,2,sum)
half.freq = apply(half.mat,2,sum)
never.core = names(core.freq[core.freq==0])
always.occa = names(always.occa.freq[always.occa.freq==0])
less.than.half = names(half.freq[half.freq==0])

### Calculate richness across routes
routes = unique(counts5$stateroute)

occu.rich.stats = c()
for(t in c(5,10,15)){
  windows = make.windows(c(1996,2010),t)
  
  #Do routes one by one
  for(i in 1:length(routes)){
    
    #Do each window 
    stats = c()
    for(j in 1:nrow(windows)){
      stats = rbind(stats,calc.occu.rich(routes[i],counts5,short.core.list,windows[j,],
                                         remove = list(aocca = always.occa, lthalf = less.than.half, ncore = never.core)))
    }
    
    mean.values = apply(stats,2,mean)
    
    occu.rich.stats = rbind(occu.rich.stats,c(t = t,stateroute = routes[i],mean.values))
  }
  
}

write.csv(occu.rich.stats, 'Core Richness 1996-2010_14.csv', row.names=F)

save.image('Core Richness 1996-2010_14.Rdata')




