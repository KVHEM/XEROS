
library(rvest)
library(data.table)
library(RDS)

#----------download----------------------
path <- 'https://www1.ncdc.noaa.gov/pub/data/paleo/reconstructions/hydroclimate/ljungqvist2016/hydro_proxies/'
dir_folder <- '../../data/input/point/raw/ljungqvist/'


html_files <- read_html(path) 
ul_text <- html_files %>% html_nodes("td") %>% html_text() # save text from source page into character string
all_files <- as.data.table(ul_text[grep('.txt', ul_text)]) # choose only text that contains .txt

for (i in 1:nrow(all_files)) {
  download.file(paste0(path, all_files[i]), paste0(dir_folder, all_files[i]))
}

#------------------melt data ------------------------
all_files <- all_files[V1 != 'Readme.txt'] # exluding readme file from further work

melt_data <- data.table() # blank dta table
melt_meta <- data.table()

for (i in 1: nrow(all_files)) {
  one_file <- as.data.table(read.delim(paste0(path, all_files[i]), skip=2, header = F, 
                                       sep = '',col.names = c('time', 'value')))
  one_file[,id:= paste0('ljun_', formatC(i, width = 3, flag = '0'))] # using 3 digits number id
  melt_data <- rbind(melt_data, one_file)
  
  one_meta <- as.data.table(read.delim(paste0(path, all_files[i]), nrows = 1, header = F, sep='\t', 
                            col.names = c('long', 'lat', 'proxy', 'season', 'ref', 'name')))
  one_meta[,id:= paste0('ljun_', formatC(i, width = 3, flag = '0'))]
  melt_meta <- rbind(melt_meta, one_meta)
}

#--------------------save--------------------------------

saveRDS(melt_data, file = '../../data/input/point/ljungvist.rds')
saveRDS(melt_meta, file = '../../data/input/point/ljungvist_meta.rds')
