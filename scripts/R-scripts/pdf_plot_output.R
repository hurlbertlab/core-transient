# creating a propOcc plot for each dataset

setwd("C:/git/core-transient")
# beta = matrix(NA, nrow = length(uniq2), ncol = 19)

pdf('propOcc.pdf', height = 8, width = 10)
par(mfrow = c(3, 4))

source('scripts/R-scripts/core-transient_functions.R')

path = "data/propOcc_datasets/"

out.file<-"output/tabular_data/propOcc.pdf"

file.names <- dir(path, pattern = "*.csv")

file.names <- list.files(path, pattern="*.csv")

#myfiles = lapply(file.names, read.delim)
setwd("C:/git/core-transient/data/propOcc_datasets/")
for(i in 1:length(file.names)){
  file <- read.csv(file=file.names[i], header=TRUE, sep=",", stringsAsFactors=FALSE) 
  plot= hist(file$propOcc)
  plot(hist(file$propOcc), xlab = "Frequency", ylim = c(0,1), ylab = "Occupancy", main = file.names[i])
  # text(x =tempR2.pos, y = .1, paste("R2 = ", round(beta[i,4], 2)), col = "red")
  
plot
}
#write.table(out.file, file = "cand_Brazil.txt",sep=";", 
 #           row.names = FALSE, qmethod = "double",fileEncoding="windows-1252")
  


dev.off()    #closes plotting device, screen or connxn to a file

