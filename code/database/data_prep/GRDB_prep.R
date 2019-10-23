library(icesTAF)
library(foreach)
library(data.table)
library(doParallel)

#-------------creates metadata file----------------------------
dir.create('./data/other/grdb/')
dir.create('./data/other/grdb/raw')
GRDB <- read.csv('./data/other/grdb/raw/GRDB_long.csv')
stream_meta <- GRDB[, -c(1, 12, 13, 14, 15)]
stream_meta <- as.data.table(unique(stream_meta))
saveRDS(stream_meta, file = './data/other/grdb/grdb_meta.rds')

#---------daily data-----------------

# choosing only data that is in grdc_dat file to run foreach without warnings
all_day_files <- as.data.table(list.files(path = './data/other/grdb/raw/grdcdat_day/', pattern = '.day'))
all_day_files <- tstrsplit(all_day_files$V1, '.d')
all_day_files <- as.data.table(all_day_files[1])
all_day_files <- as.numeric(all_day_files$V1)
all_day_files <- as.data.table(all_day_files)
setnames(all_day_files, old = 'all_day_files', new = 'V1')
id_day <- as.data.table(stream_meta$ID)
id_day <- merge(id_day, all_day_files, by = 'V1')

# string of paths and filenames to create id column with filename to every element of list
path_day <- paste0('./data/other/grdb/raw/grdcdat_day/', id_day$V1, '.day')
string_id_day <- id_day$V1

cl <- makeCluster(detectCores() - 1)
stream_80y_global_day <- foreach(i = path_day, j = string_id_day, .combine = rbind) %dopar%
  as.data.table(read.table(i, header = TRUE, sep = ';'))[, id := j]
stopCluster(cl)

#-----------same for monthly data----------------------------
all_mon_files <- as.data.table(list.files(path = './data/other/grdb/raw/grdcdat_mon/', pattern = '.mon'))
all_mon_files <- tstrsplit(all_mon_files$V1, '.m')
all_mon_files <- as.data.table(all_mon_files[1])
all_mon_files <- as.numeric(all_mon_files$V1)
all_mon_files <- as.data.table(all_mon_files)
setnames(all_mon_files, old = 'all_mon_files', new = 'V1')
id_mon <- as.data.table(stream_meta$ID)
id_mon <- merge(id_mon, all_mon_files, by = 'V1')

path_mon <- paste0('./data/other/grdb/raw/grdcdat_mon/', id_mon$V1, '.mon')
string_id_mon <- id_mon$V1

stream_80y_global_month <- foreach(i = path_mon, j = string_id_mon, .combine = rbind) %do% 
  as.data.table(read.table(i, header = T, sep = ';'))[, id := j]

stream_80y_global_day[, hh.mm := NULL]
stream_80y_global_day[, Flag := NULL]
stream_80y_global_day[, date := as.Date(YYYY.MM.DD)]
stream_80y_global_day[, YYYY.MM.DD := NULL]
colnames(stream_80y_global_day)[1:2] <- c('runoff_orig', 'runoff_calc')
setorder(stream_80y_global_day, id, date, Original, Calculated)
stream_80y_global_day <- stream_80y_global_day[, c(3, 4, 1, 2)]


stream_80y_global_month[, hh.mm := NULL]
stream_80y_global_month[, Flag := NULL]
stream_80y_global_month[, date := as.Date(gsub('-00', '-01', YYYY.MM.DD))]
stream_80y_global_month[, YYYY.MM.DD := NULL]
colnames(stream_80y_global_month)[1:2] <- c('runoff_orig', 'runoff_calc')
stream_80y_global_month <- stream_80y_global_month[, c(3, 4, 1, 2)]                   

#-----------------save RDS file------------------------------
saveRDS(stream_80y_global_day, file = './data/other/grdb/stream_80y_global_day.rds')
saveRDS(stream_80y_global_month, file = './data/other/grdb/stream_80y_global_month.rds')

#---------150 years European-------------

#subset metadata
stream_meta_EU <- stream_meta[Continent == 'EU' & N.Years >= 150]

# creates data table from list and choose only EU, years log >= 150
stream_150y_eu_day <- subset(stream_80y_global_day, id %in% stream_meta_EU$ID)
stream_150y_eu_month <- subset(stream_80y_global_month, id %in% stream_meta_EU$ID) #no stations with 150 months of data

#-----------------save RDS file EU------------------------------
saveRDS(stream_150y_eu_day, file = './data/other/grdb/stream_150y_eu_day.rds')
