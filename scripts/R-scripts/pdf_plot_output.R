# creating a propOcc plot for each dataset
library(gtools)
setwd("C:/git/core-transient")
# beta = matrix(NA, nrow = length(uniq2), ncol = 19)

pdf('propOcc.pdf', height = 8, width = 10)
par(mfrow = c(3, 4))

source('scripts/R-scripts/core-transient_functions.R')

path = "data/propOcc_datasets/"

out.file<-"output/tabular_data/propOcc.pdf"

file.names <- dir(path, pattern = "*.csv")

file.names <- list.files(path, pattern="*.csv")
file.names = mixedsort(file.names)
#myfiles = lapply(file.names, read.delim)
setwd("C:/git/core-transient/data/propOcc_datasets/")
for(i in 1:length(file.names)){
  dataid <- read.csv(file=file.names[i], header=TRUE, sep=",", stringsAsFactors=FALSE) 
  hist(dataid$propOcc, xlab = "Frequency", ylab = "Occupancy", main = file.names[i])
  # text(x =tempR2.pos, y = .1, paste("R2 = ", round(beta[i,4], 2)), col = "red")
}
#write.table(out.file, file = "cand_Brazil.txt",sep=";", 
 #           row.names = FALSE, qmethod = "double",fileEncoding="windows-1252")
  


dev.off()    #closes plotting device, screen or connxn to a file

