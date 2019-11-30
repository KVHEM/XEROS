source('../../main.R')
library(rvest)
library(leaflet)

#----------download----------------------
path <- 'https://www1.ncdc.noaa.gov/pub/data/paleo/reconstructions/hydroclimate/ljungqvist2016/temperature_proxies/'
dir_name <- '../../data/input/point/ljungqvist_t/'
dload_dir <- paste0(dir_name, 'raw/')

html_files <- read_html(path) 
ul_text <- html_files %>% html_nodes("td") %>% html_text() # save text from source page into character string
all_files <- as.data.table(ul_text[grep('.txt', ul_text)]) # choose only text that contains .txt

for (i in 1:nrow(all_files)) {
  download.file(paste0(path, all_files[i]), paste0(dload_dir, all_files[i]))
}

#------------------melt data ------------------------
all_files <- all_files[V1 != 'Readme_temperature.txt'] # exluding readme file from further work

melt_data <- data.table() # blank dta table
melt_meta <- data.table()

for (i in 1:nrow(all_files)) {
  one_file <- as.data.table(read.delim(paste0(path, all_files[i]), skip = 2, header = FALSE, 
                                       sep = '', col.names = c('time', 'value')))
  one_file[, id := paste0('ljun_', formatC(i, width = 3, flag = '0'))] # using 3 digits number id
  melt_data <- rbind(melt_data, one_file)
  
  one_meta <- as.data.table(read.delim(paste0(path, all_files[i]), nrows = 1, header = FALSE, sep='\t', 
                                       col.names = c('lon', 'lat', 'proxy', 'season', 'ref', 'name')))
  one_meta[, id := paste0('ljun_', formatC(i, width = 3, flag = '0'))]
  melt_meta <- rbind(melt_meta, one_meta)
}
melt_meta[, 1] <- sapply(melt_meta[, 1], as.character)
melt_meta[, 1] <- sapply(melt_meta[, 1], as.numeric)

#--------------------grid id-----------------------
grid_bounds <- readRDS('../../data/geodata/grid_cells.rds')
grid_bounds <- grid_bounds[1:5791, ]
dt <- unique(grid_bounds[melt_meta, .(id, cell_id), 
                         on = .(lat_l <= lat, lat_u > lat,  
                                lon_l <= lon, lon_u > lon)])
melt_meta_id <- melt_meta[dt, on = 'id']
melt_meta_id <- melt_meta_id[complete.cases(melt_meta_id)]

leaflet() %>% addTiles() %>%
  addMarkers(melt_meta_id$lon, melt_meta_id$lat, popup = melt_meta_id$name)

#--------------------save--------------------------------
melt_data[, value := as.numeric(as.character(value))]
melt_data <- melt_data[complete.cases(melt_data)]
saveRDS(melt_data, file = paste0(dir_name, 'ljungvist_t.rds'))
saveRDS(melt_meta, file = paste0(dir_name, 'ljungvist_t_meta.rds'))
saveRDS(melt_meta_id, file = paste0(dir_name, 'ljungvist_t_meta_eu_grid.rds'))

for (i in 1: nrow(all_files)) {
  print(plot(melt_data[id == unique(melt_data$id)[i], .(time, value)], type = 'l'))
}
