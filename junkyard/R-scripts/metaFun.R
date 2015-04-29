# "Metadata" maker
# This function will compile the info from the summary table and put it into a text
# file to be stored in the core-transient-datasets folder.

metaFun = function(dataset, working.directory) {
  # Set working directory for output:
      setwd(working.directory)
  # Get summary_data_table from the core-transient GitHub site:
      url = 'https://raw.githubusercontent.com/hurlbertlab/core-transient/master/data_source_table_csv'
      df = read.csv('https://raw.githubusercontent.com/hurlbertlab/core-transient/master/data_source_table.csv')
  # Subset data file to just the dataset of interest:
      df1 = df[df$dataset_ID == dataset,]
  # Create a name for the output
      output = paste(dataset,'.txt',sep = '')
      sink(output)
  # Pull out the data summary columns:
      cat(paste(names(df1[1]),': ', df1[,1], sep =''))
      cat('\n')
      cat(paste(names(df1[2]),': ', df1[,2], sep =''))
      cat('\n')
      cat(paste(names(df1[4]),': ', df1[,4], sep =''))
      cat('\n')
      cat(paste(names(df1[9]),': ', df1[,9], sep =''))
      cat('\n\n')
      cat(paste(names(df1[6]),': ', df1[,6], sep =''))
      cat('\n')
      cat(paste(names(df1[7]),': ', df1[,7], sep =''))
      cat('\n')
      cat(paste(names(df1[8]),': ', df1[,8], sep =''))
      cat('\n\n')
      cat(paste(names(df1[10]),': ', df1[,10], sep =''))
      cat('\n')
      cat(paste(names(df1[11]),': ', df1[,11], sep =''))
      cat('\n')
      cat(paste(names(df1[12]),': ', df1[,12], sep =''))
      cat('\n')
      cat(paste(names(df1[13]),': ', df1[,13], sep =''))
      cat('\n')
      if (df1$end_year == 'present')
        {cat(paste('Years: ', 
                  2014-as.numeric(df1$start_year), sep =''))} else 
        {cat(paste('Years: ', as.numeric(as.character(df1$end_year))-
                     as.numeric(as.character(df1$start_year)), 
                   sep =''))}
      cat('\n\n')
      cat(paste(names(df1[18]),': ', df1[,18], sep =''))
      sink()
}

metaFun('COM02Q','C:/Users/Brian/Desktop')

file.show('COM02Q.txt')
