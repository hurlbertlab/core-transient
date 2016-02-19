
# title         : MODIS_download.R
# purpose       : Download of MODIS EVI images for the British Colombia;
# reference     : http://spatial-analyst.net/wiki/index.php?title=Download_and_resampling_of_MODIS_images
# producer      : Prepared by T. Hengl
# last update   : In Amsterdam, NL, 14 Oct 2010.
# inputs        : Coordinates of the area of interest; proj4 parameters; ftp addresses etc.;
# outputs       : a series of EVI images (geoTIFF) projected in the local coordinate system;
# remarks 1     : To run this script, you need to obtain and install the MODIS resampling tool from [https://lpdaac.usgs.gov/lpdaac/tools/modis_reprojection_tool];
# remarks 2     : You should also obtain the WGET from [http://users.ugent.be/~bpuype/wget/]  --- simply copy the wget exe to windows system folder;
# remarks 3     : make sure you disable your antivirus tools such as Norton or McAfee otherwise it might block wget from running!

library(rgdal)
library(RCurl)
# Obtain the MODIS tool from: http://lpdaac.usgs.gov/landdaac/tools/modis/index.asp
# setwd("E:/PineBeetleBC/MODIS")
# location of the MODIS 1 km monthly blocks:
MOD13A3 <- "ftp://e4ftl01u.ecs.nasa.gov/MOLT/MOD13A3.005/"
MOD13A3a <- "ftp://anonymous:test@e4ftl01u.ecs.nasa.gov/MOLT/MOD13A3.005/"

# location of the mosiacing tool:
MRT <- 'E:\\MODIS\\MRT\\bin\\'
workd <- 'E:\\PineBeetleBC\\MODIS\\'
options(download.file.method="auto")

# get the list of directories (thanks to Barry Rowlingson):
items <- strsplit(getURL(MOD13A3), "\n")[[1]]
# items[2]
# [1] "drwxr-xr-x  2 90 118784 Jan  5  2009 2000.02.01\r"
# you get the folders (and files) but the folder names are in the form of a unix directory listing
# get the last word of any lines that start with 'd':
folderLines <- items[substr(items, 1, 1)=='d']
# get the directory names and create a new data frame:
dirs <- unlist(lapply(strsplit(folderLines, " "), function(x){x[length(x)]}))
dates <- data.frame(dirname=unlist(strsplit(dirs, "\r")))

# get the list of *.hdf files:
dates$BLOCK1 <- rep(NA, length(dates$dirname))
dates$BLOCK2 <- rep(NA, length(dates$dirname))
dates$BLOCK3 <- rep(NA, length(dates$dirname))
dates$BLOCK4 <- rep(NA, length(dates$dirname))
dates$BLOCK5 <- rep(NA, length(dates$dirname))
dates$BLOCK6 <- rep(NA, length(dates$dirname))
dates$BLOCK7 <- rep(NA, length(dates$dirname))
dates$BLOCK8 <- rep(NA, length(dates$dirname))
dates$BLOCK9 <- rep(NA, length(dates$dirname))

for (i in 9:length(dates$dirname)){
getlist <- strsplit(getURL(paste(MOD13A3, dates$dirname[[i]], "/", sep=""), .opts=curlOptions(ftplistonly=TRUE)), "\r\n")[[1]]
BLOCK1 <- getlist[grep(getlist, pattern="MOD13A3.*.h09v03.*.hdf")[1]]
BLOCK2 <- getlist[grep(getlist, pattern="MOD13A3.*.h09v04.*.hdf")[1]]
BLOCK3 <- getlist[grep(getlist, pattern="MOD13A3.*.h10v02.*.hdf")[1]]
BLOCK4 <- getlist[grep(getlist, pattern="MOD13A3.*.h10v03.*.hdf")[1]]
BLOCK5 <- getlist[grep(getlist, pattern="MOD13A3.*.h10v04.*.hdf")[1]]
BLOCK6 <- getlist[grep(getlist, pattern="MOD13A3.*.h11v02.*.hdf")[1]]
BLOCK7 <- getlist[grep(getlist, pattern="MOD13A3.*.h11v03.*.hdf")[1]]
BLOCK8 <- getlist[grep(getlist, pattern="MOD13A3.*.h12v03.*.hdf")[1]]
BLOCK9 <- getlist[grep(getlist, pattern="MOD13A3.*.h12v02.*.hdf")[1]]

# write up the file names back to the dates.txt:
for(j in 2:10){
   dates[i,j] <- get(paste("BLOCK", j-1, sep=""))
}

# Download all blocks from the list to a local drive:
# while(!is.na(dates[i,2])&!is.na(dates[i,3])&!is.na(dates[i,4])&!is.na(dates[i,5])&!is.na(dates[i,6])&!is.na(dates[i,7])&!is.na(dates[i,8])&!is.na(dates[i,9])&!is.na(dates[i,10])){
download.file(paste(MOD13A3a, dates$dirname[[i]], "/", BLOCK1,sep=""), destfile=paste(getwd(), "/", BLOCK1, sep=""), mode='wb', method='wget', quiet=T, cacheOK=FALSE)
download.file(paste(MOD13A3, dates$dirname[[i]], "/", BLOCK2,sep=""), destfile=paste(getwd(), "/", BLOCK2,sep=""), mode='wb', method='wget', quiet=T, cacheOK=FALSE)
download.file(paste(MOD13A3, dates$dirname[[i]], "/", BLOCK3,sep=""), destfile=paste(getwd(), "/", BLOCK3,sep=""), mode='wb', method='wget', quiet=T, cacheOK=FALSE)
download.file(paste(MOD13A3, dates$dirname[[i]], "/", BLOCK4,sep=""), destfile=paste(getwd(), "/", BLOCK4,sep=""), mode='wb', method='wget', quiet=T, cacheOK=FALSE)
download.file(paste(MOD13A3, dates$dirname[[i]], "/", BLOCK5,sep=""), destfile=paste(getwd(), "/", BLOCK5,sep=""), mode='wb', method='wget', quiet=T, cacheOK=FALSE)
download.file(paste(MOD13A3, dates$dirname[[i]], "/", BLOCK6,sep=""), destfile=paste(getwd(), "/", BLOCK6,sep=""), mode='wb', method='wget', quiet=T, cacheOK=FALSE)
download.file(paste(MOD13A3, dates$dirname[[i]], "/", BLOCK7,sep=""), destfile=paste(getwd(), "/", BLOCK7,sep=""), mode='wb', method='wget', quiet=T, cacheOK=FALSE)
download.file(paste(MOD13A3, dates$dirname[[i]], "/", BLOCK8,sep=""), destfile=paste(getwd(), "/", BLOCK8,sep=""), mode='wb', method='wget', quiet=T, cacheOK=FALSE)
download.file(paste(MOD13A3, dates$dirname[[i]], "/", BLOCK9,sep=""), destfile=paste(getwd(), "/", BLOCK9,sep=""), mode='wb', method='wget', quiet=T, cacheOK=FALSE)

# remove "." from the file name:
dirname1 <- sub(sub(pattern="\\.", replacement="_", dates$dirname[[i]]), pattern="\\.", replacement="_", dates$dirname[[i]])
# mosaic the blocks:
mosaicname = file(paste(MRT, "TmpMosaic.prm", sep=""), open="wt")
write(paste(workd, BLOCK1, sep=""), mosaicname)
write(paste(workd, BLOCK2, sep=""), mosaicname, append=T)
write(paste(workd, BLOCK3, sep=""), mosaicname, append=T)
write(paste(workd, BLOCK4, sep=""), mosaicname, append=T)
write(paste(workd, BLOCK5, sep=""), mosaicname, append=T)
write(paste(workd, BLOCK6, sep=""), mosaicname, append=T)
write(paste(workd, BLOCK7, sep=""), mosaicname, append=T)
write(paste(workd, BLOCK8, sep=""), mosaicname, append=T)
write(paste(workd, BLOCK9, sep=""), mosaicname, append=T)
close(mosaicname)
# generate temporary mosaic:
shell(cmd=paste(MRT, 'mrtmosaic -i ', MRT, 'TmpMosaic.prm -s "0 1 0 0 0 0 0 0 0 0 0" -o ', workd, 'TmpMosaic.hdf', sep=""))

# resample to epsg=3005:
filename = file(paste(MRT, "mrt", dirname1, ".prm", sep=""), open="wt")
write(paste('INPUT_FILENAME = ', workd, 'TmpMosaic.hdf', sep=""), filename) 
# write(paste('INPUT_FILENAMES = ( ', workd, BLOCK1, ' ', workd, BLOCK2, ' ', workd, BLOCK3, ' ', workd, BLOCK4, ' ', workd, BLOCK5, ' ', workd, BLOCK6, ' ', workd, BLOCK7, ' ', workd, BLOCK8, ' ', workd, BLOCK9, ' )', sep=""), filename)  # unfortunatelly does not work via command line  :(
write('  ', filename, append=TRUE) 
# write('SPECTRAL_SUBSET = ( 0 1 0 0 0 0 0 0 0 0 0 )', filename, append=TRUE)
write('SPECTRAL_SUBSET = ( 1 )', filename, append=TRUE)
write('  ', filename, append=TRUE)
write('SPATIAL_SUBSET_TYPE = OUTPUT_PROJ_COORDS', filename, append=TRUE)
write('  ', filename, append=TRUE)
write('SPATIAL_SUBSET_UL_CORNER = ( 637278.0 1701350.0 )', filename, append=TRUE)
write('SPATIAL_SUBSET_LR_CORNER = ( 1907278.0 335350.0 )', filename, append=TRUE)
write('  ', filename, append=TRUE)
write(paste('OUTPUT_FILENAME = ', workd, 'tmp', dirname1, '.tif', sep=""), filename, append=TRUE)
write('  ', filename, append=TRUE)
write('RESAMPLING_TYPE = NEAREST_NEIGHBOR', filename, append=TRUE)
write('  ', filename, append=TRUE)
write('OUTPUT_PROJECTION_TYPE = AEA', filename, append=TRUE)
write('  ', filename, append=TRUE)
write('OUTPUT_PROJECTION_PARAMETERS = ( ', filename, append=TRUE)
write(' 0.0 0.0 50.0', filename, append=TRUE)
write(' 58.5 -126.0 45.0', filename, append=TRUE)
write(' 1000000.0 0.0 0.0', filename, append=TRUE)
write(' 0.0 0.0 0.0', filename, append=TRUE)
write(' 0.0 0.0 0.0 )', filename, append=TRUE)
write('  ', filename, append=TRUE)
write('DATUM = NAD83', filename, append=TRUE)
write('  ', filename, append=TRUE)
write('OUTPUT_PIXEL_SIZE = 1000', filename, append=TRUE)
write('  ', filename, append=TRUE)
close(filename)

# Mosaic the images to get the whole area:
shell(cmd=paste(MRT, 'resample -p ', MRT, 'mrt', dirname1, '.prm', sep=""))
# delete all hdf files!
unlink(paste(getwd(), '/', BLOCK1, sep=""))
unlink(paste(getwd(), '/', BLOCK2, sep=""))
unlink(paste(getwd(), '/', BLOCK3, sep=""))
unlink(paste(getwd(), '/', BLOCK4, sep=""))
unlink(paste(getwd(), '/', BLOCK5, sep=""))
unlink(paste(getwd(), '/', BLOCK6, sep=""))
unlink(paste(getwd(), '/', BLOCK7, sep=""))
unlink(paste(getwd(), '/', BLOCK8, sep=""))
unlink(paste(getwd(), '/', BLOCK9, sep=""))
}
#}

# Check the validity:
GDALinfo("tmp2000_02_01.1_km_monthly_EVI.tif")


# end of script!