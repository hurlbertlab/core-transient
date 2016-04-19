# Code for producing simpler summaries from data_formatting_table
dataFormattingTable = read.csv('data_formatting_table.csv', header=T)

#Site notes
subsetNotes = function(dataFormattingTable, subsetCols) {
  d2 = dataFormattingTable[dataFormattingTable$format_flag == 1,]
  d2[order(d2$taxa, d2$dataset_ID), c('dataset_ID', 'dataset_name', 'taxa', 'location', subsetCols)]
}

