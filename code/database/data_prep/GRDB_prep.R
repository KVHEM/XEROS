library(icesTAF)
library(foreach)
library(data.table)
library(doParallel)

#-------------creates metadata file----------------------------
GRDB <- read.csv('../../data/other/rivers/raw/GRDB_long.csv')
GRDB <- read.csv('../../Projects/2018XEROS/data/other/rivers/raw/GRDB_long.csv')
stream_meta <- GRDB[,-c(1,12,13,14,15)]
stream_meta <- as.data.table(unique(stream_meta))
saveRDS(stream_meta, file = '../../data/other/rivers/stream_meta.rds')
saveRDS(stream_meta, file = '../../Projects/2018XEROS/data/other/rivers/stream_meta.rds')


#---------daily data-----------------

# choosing only data that is in grdc_dat file to run foreach without warnings
all_day_files <- as.data.table(list.files(path = '../../data/other/rivers/raw/grdcdat_day/', pattern = '.day'))
all_day_files <- as.data.table(list.files(path = '../../Projects/2018XEROS/data/other/rivers/raw/grdcdat_day/', pattern = '.day'))
all_day_files <- tstrsplit(all_day_files$V1, '.d')
all_day_files <- as.data.table(all_day_files[1])
all_day_files <- as.numeric(all_day_files$V1)
all_day_files <- as.data.table(all_day_files)
setnames(all_day_files, old = 'all_day_files', new = 'V1')
id_day <- as.data.table(stream_meta$ID)
id_day <- merge(id_day, all_day_files, by = 'V1')

# string of paths and filenames to create id column with filename to every element of list
path_day <- paste0('../../data/other/rivers/raw/grdcdat_day/', id_day$V1, '.day')
path_day <- paste0('../../Projects/2018XEROS/data/other/rivers/raw/grdcdat_day/', id_day$V1, '.day')
string_id_day <- id_day$V1

cl <- makeCluster(detectCores() - 1)
stream_80y_global_day <- foreach(i = path_day, j = string_id_day, .combine = rbind) %dopar%
  as.data.table(read.table(i, header = TRUE, sep = ';'))[, id := j]
stopCluster(cl)

#-----------same for monthly data----------------------------
all_mon_files <- as.data.table(list.files(path = '../../data/other/rivers/raw/grdcdat_mon/', pattern = '.mon'))
all_mon_files <- tstrsplit(all_mon_files$V1, '.m')
all_mon_files <- as.data.table(all_mon_files[1])
all_mon_files <- as.numeric(all_mon_files$V1)
all_mon_files <- as.data.table(all_mon_files)
setnames(all_mon_files, old = 'all_mon_files', new = 'V1')
id_mon <- as.data.table(stream_meta$ID)
id_mon <- merge(id_mon, all_mon_files, by = 'V1')

path_mon <- paste0('../../data/other/rivers/raw/grdcdat_mon/', id_mon$V1, '.mon')
path_mon <- paste0('../../data/other/rivers/raw/grdcdat_mon/', id_mon$V1, '.mon')
string_id_mon <- id_mon$V1

stream_80y_global_month <- foreach(i = path_mon, j = string_id_mon) %do% 
  as.data.table(read.table(i, header = T, sep = ';'))[, id:=j]
#---------------unlist-------------
stream_80y_global_day <- rbindlist(stream_80y_global_day)
stream_80y_global_month <- rbindlist(stream_80y_global_month)

#-----------------save RDS file------------------------------
saveRDS(stream_80y_global_day, file = '../../data/other/rivers/stream_80y_global_day.rds')
saveRDS(stream_80y_global_month, file = '../../data/other/rivers/stream_80y_global_month.rds')

#---------150 years Europian-------------

#subset metadata
stream_meta_EU <- stream_meta[Continent == 'EU' & N.Years >= 150]

# creates data table from list and choose only EU, years log >= 150
stream_150y_eu_day<-subset(stream_150y_eu_day, id %in% stream_meta_EU$ID)
stream_150y_eu_month<-subset(stream_150y_eu_month, id %in% stream_meta_EU$ID)

#-----------------save RDS file EU------------------------------
saveRDS(stream_150y_eu_day, file = '../../data/other/rivers/stream_150y_eu_day.rds')
saveRDS(stream_150y_eu_month, file = '../../data/other/rivers/stream_150y_eu_month.rds')
