# Install and load necessary packages:

install.packages('reshape2')
library(reshape2)

# Get data:

setwd('/Users/bsevans/Desktop/core-transient-datasets/dataset_228RAW')

list.files()

hb = read.csv('hb_bird.txt')
mk = read.csv('mk_bird.txt')
rp = read.csv('rp_bird.txt')
sm = read.csv('sm_bird.txt')

# Files need to be reshaped from wide to long format prior to binding.

### melt(hb)
