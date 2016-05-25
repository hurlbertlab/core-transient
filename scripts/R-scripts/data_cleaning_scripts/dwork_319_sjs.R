### reformatting the original moth time series dataset from Preston 1948



speciesnum = c(rep(1/22, 49), rep(2/22, 29), rep(3/22, 9), rep(4/22, 16), rep(5/22, 8), rep(6/22, 10),
               rep(7/22, 12), rep(8/22, 6), rep(9/22, 11), rep(10/22, 8), rep(11/22, 11), rep(12/22, 15),
               rep(13/22, 3), rep(14/22, 6), rep(15/22, 6), rep(16/22, 11), rep(17/22, 8), rep(18/22, 1),
              rep(19/22, 7), rep(20/22, 7), rep(21/22, 6), rep(22/22, 38))

d319 = data.frame(speciesnum)
write.csv(d319, "C:/git/core-transient/data/propOcc_datasets/propOcc_319_1.csv")
