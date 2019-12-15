source('../../main.R')
library(leaflet)
#-----------download txt files-----------------------
file_url_1 <- 'https://zenodo.org/record/3552420/files/precip_d_source_knmi.txt?download=1'
download_path_1 <- "../../data/input/point/ghcn/precip_d_source_knmi.txt"
download.file(file_url_1,download_path_1)
file_url_2 <- 'https://zenodo.org/record/3552420/files/precip_m_source_knmi.txt?download=1'
download_path_2 <- "../../data/input/point/ghcn/precip_m_source_knmi.txt"
download.file(file_url_2,download_path_2)

path_d <- '../../data/input/point/ghcn/raw/precip_d/'
path_m <- '../../data/input/point/ghcn/raw/precip_m/'


#-----------download daily files-----------------------
list_files_d <- read.delim('../../code/database/data_prep/ghcn/precip_d_source_knmi.txt', header = FALSE) #read txt file with url adress of every station data
file_name_d <- as.data.table(list_files_d$V1)
file_name_d [, paste0('V1', 1:2) := tstrsplit(V1, 'data/')]

file_url_d <- as.character(file_name_d$V1)

for (i in 1:nrow(list_files_d)) {
  name <- file_name_d$V12[i]
  download.file(file_url_d[i], paste0(path_d, name)) #downloads DO NOT RUN
}

#-----------download monthly files------------
list_files_m <-read.delim('../../code/database/data_prep/ghcn/precip_m_source_knmi.txt', header = FALSE) #read txt file with url adress of every station data
file_name_m <- as.data.table(list_files_m$V1)
file_name_m [, paste0('V1', 1:2) := tstrsplit(V1, 'data/')]

file_url_m <- as.character(file_name_m$V1)

for (i in 1:nrow(list_files_m)) {
  name <- file_name_m$V12[i]
  download.file(file_url_m[i], paste0(path_m, name)) #downloads DO NOT RUN
}
# If downloads don't work from txt files download directly from:
# https://owncloud.cesnet.cz/index.php/s/NiY6dF2JfYTN1Ds?path=%2Fdata%2Finput%2Fpoint%2Fghcn%2Fraw

#---------------create one file for daily logs ----------------
list_names_d <- list.files(path = path_d, pattern = "*.dat", full.names = TRUE)

all_daily_precip <- data.table()
metadata_d_precip <- data.table()

for (i in 1:length(list_names_d)) {
  
  daily_precip <- read.delim(list_names_d[i], skip = 21, header = F, sep = '') 
  daily_precip$date <- as.Date(with(daily_precip, paste(V1, V2, V3, sep = "-")), "%Y-%m-%d") # one char string from numeric date logs
  daily_precip <- as.data.table(cbind(as.character(daily_precip$date), as.numeric(daily_precip$V4))) # bind only date and precip
  setnames(daily_precip, old = c('V1', 'V2'), new = c('date', 'precip')) 
  
  rawmeta <- read.delim(list_names_d[i], skip = 1, nrows = 1, header = FALSE, sep = '') # using metadata from files
  meta <- as.data.table(rawmeta[, -c(1, 2, 5, 6, 7, 8, 9, 12)]) # cutting out values that are not needed
  meta[, paste0('V3', 1) := tstrsplit(V3, 'N,')] # cleaning lon lat
  meta[, 'V3' := NULL]
  meta[, paste0('V4', 1) := tstrsplit(V4, 'E,')]
  meta[, 'V4' := NULL]
  meta[, id := i]
  
  daily_precip[, station_name := meta[, 1]]  #columns with info about station country and coordinates
  daily_precip[, station_country := meta[, 2]]
  daily_precip[, lon := meta[, 4]]
  daily_precip[, lat := meta[, 3]]
  daily_precip[, id := factor(i)]
  
  metadata_d_precip <- rbind(metadata_d_precip, meta)  # metadata file with matching id
  all_daily_precip <- rbind(all_daily_precip, daily_precip) # save all files to one data table
  print(i)
}

all_daily_precip <- as.data.frame(all_daily_precip)  # change format of lon lat and precip to numeric
levels(all_daily_precip$station_country)[13] <- 'UK'
cols.num <- c('precip', 'lon', 'lat')
all_daily_precip[cols.num] <- sapply(all_daily_precip[cols.num], as.numeric)

setnames(metadata_d_precip, old = c('V10', 'V11', 'V31', 'V41'), new = c('station_name', 'station_country', 'lat', 'lon')) # change format in metadata file
metadata_d_precip <- as.data.frame(metadata_d_precip)
levels(metadata_d_precip$station_country)[13] <- 'UK'

cols.num <- c('lon', 'lat')
metadata_d_precip[cols.num] <- sapply(metadata_d_precip[cols.num], as.numeric)


#------------------------create one file for monthly logs------------------

list_names_m <- list.files(path = path_m, pattern = "*.dat", full.names = TRUE)

all_monthly_precip <- data.table()
metadata_m_precip <- data.table()

for (i in 1:length(list_names_m)){
  
  monthly_precip <- as.data.table(read.delim(list_names_m[i], skip = 17, header = FALSE, sep = '')) 
  setnames(monthly_precip, 
           old = c('V1', 'V2', 'V3', 'V4', 'V5', 'V6', 'V7', 'V8', 'V9', 'V10', 'V11', 'V12', 'V13'),
           new = c('year', 'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec'))
  monthly_precip <- melt(monthly_precip, id.vars = 'year')
  
  rawmeta <- read.delim(list_names_m[i], skip = 9, nrows = 5, header = FALSE, sep = '') # creating usable metadata file 
  rawmeta <- as.data.table(rawmeta)
  meta <- as.data.table(rawmeta[-c(3), -c(1, 2, 3, 5)])
  meta <- transpose(meta)
  meta[, id := i]

  monthly_precip[, station_name := meta[, 1]]  #columns with info about station country and coordinates
  monthly_precip[, station_country := meta[, 2]]
  monthly_precip[, lon := meta[, 4]]
  monthly_precip[, lat := meta[, 3]]
  monthly_precip[, id := factor(i)]
  setnames(monthly_precip, old = c('variable', 'value'), new = c('month', 'precip'))

  metadata_m_precip <- rbind (metadata_m_precip, meta)  # metadata file with matching id
  all_monthly_precip <- rbind(all_monthly_precip, monthly_precip) # save all files to one data table
  print(i)
}

all_monthly_precip <- as.data.frame(all_monthly_precip)  # change format of lon lat and precip to numeic
all_monthly_precip$station_country[all_monthly_precip$station_country == 'UNITED'] <- 'UK'
all_monthly_precip$precip [all_monthly_precip$precip == -999.90] <- NA
all_monthly_precip$precip [all_monthly_precip$precip == -888.80] <- NA
cols.num <- c('precip', 'lon', 'lat', 'id')
all_monthly_precip [cols.num] <- sapply(all_monthly_precip[cols.num], as.numeric)


setnames( metadata_m_precip, old = c('V1', 'V2', 'V3', 'V4'), 
          new = c('station_name', 'station_country', 'lat', 'lon')) # change format in metadata file
metadata_m_precip <- as.data.frame(metadata_m_precip)
cols.num <- c('lat', 'lon', 'id')
metadata_m_precip [cols.num] <- sapply(metadata_m_precip[cols.num], as.numeric)
metadata_m_precip$station_country[metadata_m_precip$station_country == 'UNITED'] <- 'UK'

#----------------save RDS-------------------------
saveRDS(data.table(all_daily_precip), file = '../../data/input/point/ghcn/ghcn_daily_p.rds')
saveRDS(data.table(all_monthly_precip), file = '../../data/input/point/ghcn/ghcn_monthly_p.rds')
save(metadata_m_precip, metadata_d_precip, file = '../../data/input/point/ghcn/ghcn_meta.rdata')

#-------------check position of used station----------------
load('../../data/input/point/ghcn/ghcn_meta.rdata')

# daily data
leaflet() %>% addTiles() %>%
  addMarkers(metadata_d_precip$lon, metadata_d_precip$lat, popup = metadata_d_precip$station_name)

# monthly data
leaflet() %>% addTiles() %>%
  addMarkers(metadata_m_precip$lon, metadata_m_precip$lat, popup = metadata_m_precip$station_name)

