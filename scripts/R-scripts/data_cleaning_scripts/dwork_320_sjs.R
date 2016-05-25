### reformatting the original moth time series dataset from Preston 1948



speciesnum = c(rep(1/22, 45), rep(2/22, 21), rep(3/22, 19), rep(4/22, 13), rep(5/22, 12), rep(6/22, 10),
               rep(7/22, 8), rep(8/22, 9), rep(9/22, 9), rep(10/22, 17), rep(11/22, 11), rep(12/22, 5),
               rep(13/22, 7), rep(14/22, 7), rep(15/22, 7), rep(16/22, 6), rep(17/22, 8), rep(18/22, 15),
              rep(19/22, 16), rep(20/22, 16), rep(21/22, 15), rep(22/22, 15))

d320 = data.frame(speciesnum)
write.csv(d320, "C:/git/core-transient/data/propOcc_datasets/propOcc_320_1.csv")
