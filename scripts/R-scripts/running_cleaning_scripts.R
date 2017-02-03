# Script for running all data cleaning scripts

dft = read.csv('data_formatting_table.csv', header=T)

ids = dft$dataset_ID[dft$format_flag==1 & !dft$dataset_ID %in% c(1, 222, 67, 317, 270, 271, 319, 325, 255, 257, 260)]

files = list.files('scripts/R-scripts/data_cleaning_scripts')
d_id = as.numeric(substr(files, 7, 9))

files2 = files[d_id %in% ids]

for (i in files2) {

  tryCatch({
    source(paste('scripts/R-scripts/data_cleaning_scripts/', i, sep = ''))
  }, warning = function(w) {
    print(paste("dataset", i, ":", w))
  }, error = function(e) {
    print(paste("dataset", i, ":", e))
  }, finally = {
    print(paste("dataset", i, "OK"))
  })
}