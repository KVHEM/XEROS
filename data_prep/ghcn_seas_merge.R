library(data.table)
library(RDS)

#---------load data----------------
daily <- as.data.table(readRDS('../../data/input/point/ghcn_daily_p.rds'))
monthly <- as.data.table(readRDS('../../data/input/point/ghcn_monthly_p.rds'))

#---------------NA values monthly data---------------
NA_val <- monthly[is.na(precip)]
NA_val[month == 'dec', season := 'wi']
NA_val[month == 'jan', season := 'wi']
NA_val[month == 'feb', season := 'wi']
NA_val[month == 'mar', season := 'sp']
NA_val[month == 'apr', season := 'sp']
NA_val[month == 'may', season := 'sp']
NA_val[month == 'jun', season := 'su']
NA_val[month == 'jul', season := 'su']
NA_val[month == 'aug', season := 'su']
NA_val[month == 'sep', season := 'au']
NA_val[month == 'oct', season := 'au']
NA_val[month == 'nov', season := 'au']
seas_to_del <- NA_val[, sum(precip), by = list(id, year, season)] # seasons that contain NA values from monthly logs, no matter number of months in season that have NA value
seas_to_del[, V1 := NULL]

#-------------- create seasonal logs from monthly data-------------

monthly[month == 'dec', season := 'wi']
monthly[month == 'jan', season := 'wi']
monthly[month == 'feb', season := 'wi']
monthly[month == 'mar', season := 'sp']
monthly[month == 'apr', season := 'sp']
monthly[month == 'may', season := 'sp']
monthly[month == 'jun', season := 'su']
monthly[month == 'jul', season := 'su']
monthly[month == 'aug', season := 'su']
monthly[month == 'sep', season := 'au']
monthly[month == 'oct', season := 'au']
monthly[month == 'nov', season := 'au']

table(unlist(monthly$season)) # check

monthly_all <- monthly[ ,sum(precip), by = list(id, year, season, station_country, station_name, lon, lat)] # sum of precip to seasonal values
clean_monthly <- monthly_all[!seas_to_del, on =.(id, year, season)] # delete all incomplete season based on season to del table

#----------------NA values daily data----------------

daily[, paste0('date', 1:3) := tstrsplit(date, '-')]
setnames(daily, old = c('date1', 'date2', 'date3'), new = c('year', 'month', 'day'))
daily[, date := NULL]
daily[month == '12', season := 'wi']
daily[month == '01', season := 'wi']
daily[month == '02', season := 'wi']
daily[month == '03', season := 'sp']
daily[month == '04', season := 'sp']
daily[month == '05', season := 'sp']
daily[month == '06', season := 'su']
daily[month == '07', season := 'su']
daily[month == '08', season := 'su']
daily[month == '09', season := 'au']
daily[month == '10', season := 'au']
daily[month == '11', season := 'au']

daily[,9] <- sapply(daily[,9], as.numeric)
# sum of days in month, value must be bigger than 21 in order to have at least 21 days logs in month
daily[, count := 1]
sum_of_days <- daily[,sum(count), by=list(id, year, month, season)] 
month_to_del <- sum_of_days[V1 <= 21]
daily[, count := NULL] # get rid of counting column

#-------------- create seasonal logs from daily data-------------

daily_all <- daily[ ,sum(precip), by = list(id, year, season, station_country, station_name, lon, lat)]
clean_daily <- daily_all[!month_to_del, on = .(id, year, season)]

#---------adjust metadata--------------------
meta_d <- load('../../data/input/point/ghcn_meta.rdata')
metadata_d_precip <- as.data.table(metadata_d_precip)
metadata_d_precip[, id := paste0('d', id)]
metadata_m_precip <- as.data.table(metadata_m_precip)
metadata_m_precip[, id := paste0('m', id)]
ghcn_meta_seas <- rbind(metadata_m_precip, metadata_d_precip)

#---------assign points to grid --------------------
load('../../data/geodata/grid_cells.rdata')
dt <- unique(bounds[ghcn_meta_seas, .(id, cell_id), 
              on = .(lat_l <= lat, lat_u > lat,  
                     long_l <= lon, long_u > lon)])
ghcn_meta_seas <- ghcn_meta_seas[dt, on = 'id']
ghcn_meta_seas <- ghcn_meta_seas[complete.cases(ghcn_meta_seas)]

#---------adjust ids--------------------
clean_monthly[, id := NULL]
cl_monthly_new_id <- clean_monthly[metadata_m_precip, on = .(station_name, station_country, lon, lat)] # creating new id in clean daily

clean_daily[, id := NULL]
cl_daily_new_id <- clean_daily[metadata_d_precip, on = .(station_name, station_country, lon, lat)] # creating new id in clean daily

#---------- melt and save as seasonal data--------------
ghcn_seasonal_p <- rbind(cl_monthly_new_id, cl_daily_new_id)
setnames(ghcn_seasonal_p, old = 'V1', new = 'precip')

saveRDS(ghcn_seasonal_p, '../../data/input/point/ghcn_seas_p.rds')
save(ghcn_meta_seas, file = '../../data/input/point/ghcn_meta_seas.rdata')
