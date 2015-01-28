# Pre-formatting for dataset 239

library(reshape2)

table1 = read.csv('data/raw_datasets/dataset_239RAW/dataset_239_table1.csv')
table3 = read.csv('data/raw_datasets/dataset_239RAW/dataset_239_table3.csv')

# Explore:

names(table1)
head(names(table3))

# Convert from wide to long format (reshape2):

table3long = melt(table3, id.vars = c('X'))

# Rename to have the same ID column:

names(table3long)[1] = names(table1)[1]

# Rename the species and count columns meaningfully:

names(table3long)[2:3] = c('species','count')

# Give a look:

head(table3long)

dim(table3long)

summary(table3long)

# With nearly 1.4 million records and a median count of 0, remove zeros to save storage space and processing time:

table3long = subset(table3long, count >0)

# Merge table 1 and table3long

tableMerge = merge(table1, table3long, all = T)

# Write file:

write.csv(tableMerge, 'data/raw_datasets/dataset_239.csv', row.names = F)


