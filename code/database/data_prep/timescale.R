library(RDS)

#--------load data--------

luterbacher <- readRDS('../../data/input/gridded/luterbacher/luterbacher.rds')
pauling <- readRDS('../../data/input/gridded/pauling/pauling.rds')
owda <- readRDS('../../data/input/gridded/owda/owda_1000.rds')
ljungvist <- readRDS('../../data/input/point/ljungvist_t.rds')
pages <- readRDS('../../data/input/point/pages2k_eu.rds')

#-------------creating averaged value by 10 and 5 years-----------------

#pauling
pauling_anual <- pauling[,mean(precip), by=c('year', 'long', 'lat', 'cell_id')]
pauling_anual[, period := factor(paste0(year - (year %% 10), '-', year - (year %% 10)+9))]
pauling_decade <- pauling_anual[,mean(V1), by=c('period', 'long', 'lat', 'cell_id')]
pauling_10yr <- merge(pauling_anual, pauling_decade, by = c('period', 'cell_id'))
pauling_10yr[, long.y := NULL]
pauling_10yr[, lat.y := NULL]
setnames(pauling_10yr, old = c( 'year', 'long.x', 'lat.x', 'V1.x', 'V1.y'),
         new = c( 'time','long', 'lat', 'avg_annual', 'avg_10yr'))

pauling_10yr$time <- as.numeric(pauling_10yr$time)
pauling_10yr$avg_10yr <- as.numeric(pauling_10yr$avg_10yr)
pauling_10yr$avg_annual <- as.numeric(pauling_10yr$avg_annual)

pauling_5 <- pauling_anual[, period := factor(ifelse((year %% 10) >= 5,
  paste0(year - (year %% 10)+5, '-', year - (year %% 10)+9),
  paste0(year - (year %% 10), '-', year - (year %% 10)+4)
  ))]
pauling_5_mean <- pauling_5[,mean(V1), by=c('period', 'long', 'lat', 'cell_id')]
pauling_5yr <- merge(pauling_5, pauling_5_mean, by = c('period', 'cell_id'))
pauling_5yr[, long.y := NULL]
pauling_5yr[, lat.y := NULL]
setnames(pauling_5yr, old = c( 'year', 'long.x', 'lat.x', 'V1.x', 'V1.y'),
         new = c( 'time','long', 'lat', 'avg_annual', 'avg_5yr'))

pauling_5yr$time <- as.numeric(pauling_10yr$time)
pauling_5yr$avg_5yr <- as.numeric(pauling_10yr$avg_5yr)
pauling_5yr$avg_annual <- as.numeric(pauling_10yr$avg_annual)


#luterbacher

luterbacher[, time := NULL]
luterbacher[, temp_yr := NULL]
luterbacher$year <- as.numeric(luterbacher$year)
luterbacher_anual <- luterbacher[,mean(temp), by=c('year', 'long', 'lat', 'cell_id')]
luterbacher_anual[, period := factor(paste0(year - (year %% 10), '-', year - (year %% 10)+9))]
luterbacher_decade <- luterbacher_anual[,mean(V1), by=c('period', 'long', 'lat', 'cell_id')]
luterbacher_10yr <- merge(luterbacher_anual, luterbacher_decade, by = c('period', 'cell_id'))
luterbacher_10yr[, long.y := NULL]
luterbacher_10yr[, lat.y := NULL]
setnames(luterbacher_10yr, old = c( 'year', 'long.x', 'lat.x', 'V1.x', 'V1.y'),
         new = c( 'time','long', 'lat', 'avg_annual', 'avg_10yr'))

luterbacher_10yr$time <- as.numeric(luterbacher_10yr$time)
luterbacher_10yr$avg_10yr <- as.numeric(luterbacher_10yr$avg_10yr)
luterbacher_10yr$avg_annual <- as.numeric(luterbacher_10yr$avg_annual)

luterbacher_5 <- luterbacher_anual[, period := factor(ifelse((year %% 10) >= 5,
                                                paste0(year - (year %% 10)+5, '-', year - (year %% 10)+9),
                                                paste0(year - (year %% 10), '-', year - (year %% 10)+4)
))]
luterbacher_5_mean <- luterbacher_5[,mean(V1), by=c('period', 'long', 'lat', 'cell_id')]
luterbacher_5yr <- merge(luterbacher_5, luterbacher_5_mean, by = c('period', 'cell_id'))
luterbacher_5yr[, long.y := NULL]
luterbacher_5yr[, lat.y := NULL]
setnames(luterbacher_5yr, old = c( 'year', 'long.x', 'lat.x', 'V1.x', 'V1.y'),
         new = c( 'time','long', 'lat', 'avg_annual', 'avg_5yr'))

luterbacher_5yr$time <- as.numeric(luterbacher_5yr$time)
luterbacher_5yr$avg_5yr <- as.numeric(luterbacher_5yr$avg_5yr)
luterbacher_5yr$avg_annual <- as.numeric(luterbacher_5yr$avg_annual)


#owda
east <- owda[,3] + 0.25
orig <- owda
owda[,Lon := NULL]
owda <- cbind(east, owda)
owda[, cell_id := NA]
grid_bounds <- readRDS('../../data/geodata/grid_cells.rds')
grid_bounds <- grid_bounds[1:5791, ]
owda <- grid_bounds[owda, .(cell_id, Time, Lat, Lon, scPDSI), 
                   on = .(lat_l <= Lat, lat_u > Lat,  
                          long_l <= Lon, long_u > Lon)]

owda_anual <- owda[,mean(scPDSI), by=c('Time', 'Lon', 'Lat', 'cell_id')]
owda_anual[, period := factor(paste0(Time - (Time %% 10), '-', Time - (Time %% 10)+9))]
owda_decade <- owda_anual[,mean(V1), by=c('period', 'Lon', 'Lat', 'cell_id')]
owda_10yr <- merge(owda_anual, owda_decade, by = c('period', 'cell_id'))
owda_10yr[, Lon.y := NULL]
owda_10yr[, Lat.y := NULL]
setnames(owda_10yr, old = c('Lon.x', 'Lat.x', 'V1.x', 'V1.y'),
         new = c( 'long', 'lat', 'avg_annual', 'avg_10yr'))

owda_10yr$time <- as.numeric(owda_10yr$Time)
owda_10yr$avg_10yr <- as.numeric(owda_10yr$avg_10yr)
owda_10yr$avg_annual <- as.numeric(owda_10yr$avg_annual)

owda_5 <- owda_anual[, period := factor(ifelse((Time %% 10) >= 5,
                                         paste0(Time - (Time %% 10)+5, '-', Time - (Time %% 10)+9),
                                         paste0(Time - (Time %% 10), '-', Time - (Time %% 10)+4)
))]
owda_5_mean <- owda_5[,mean(V1), by=c('Lon', 'Lat', 'cell_id', 'period')]
owda_5yr <- merge(owda_5, owda_5_mean, by = c('period', 'cell_id'))
owda_5yr[, Lon.y := NULL]
owda_5yr[, Lat.y := NULL]
setnames(owda_5yr, old = c('Lon.x', 'Lat.x', 'V1.x', 'V1.y'),
         new = c( 'long', 'lat', 'avg_annual', 'avg_5yr'))

owda_5yr$time <- as.numeric(owda_5yr$time)
owda_5yr$avg_5yr <- as.numeric(owda_5yr$avg_5yr)
owda_5yr$avg_annual <- as.numeric(owda_5yr$avg_annual)


#ljungvist

ljungvist_anual <- ljungvist[,mean(value), by=c('time', 'id')]
ljungvist_anual[, period := factor(paste0(time - (time %% 10), '-', time - (time %% 10)+9))]
ljungvist_decade <- ljungvist_anual[,mean(V1), by=c('period', 'id')]
ljungvist_10yr <- merge(ljungvist_anual, ljungvist_decade, by = c('period', 'id'))
setnames(ljungvist_10yr, old = c('V1.x', 'V1.y'),
         new = c('avg_annual', 'avg_10yr'))

ljungvist_10yr$avg_10yr <- as.numeric(ljungvist_10yr$avg_10yr)
ljungvist_10yr$avg_annual <- as.numeric(ljungvist_10yr$avg_annual)

ljungvist_5 <- ljungvist_anual[, period := factor(ifelse((time %% 10) >= 5,
                                            paste0(time - (time %% 10)+5, '-', time - (time %% 10)+9),
                                            paste0(time - (time %% 10), '-', time - (time %% 10)+4)
))]
ljungvist_5_mean <- ljungvist_5[,mean(V1), by=c('id', 'period')]
ljungvist_5yr <- merge(ljungvist_5, ljungvist_5_mean, by = c('period', 'id'))
setnames(ljungvist_5yr, old = c('V1.x', 'V1.y'),
         new = c('avg_annual', 'avg_5yr'))

ljungvist_5yr$avg_5yr <- as.numeric(ljungvist_5yr$avg_5yr)
ljungvist_5yr$avg_annual <- as.numeric(ljungvist_5yr$avg_annual)


#pages

pages_anual <- pages[,mean(temp), by=c('time', 'id')]
pages_anual[, period := factor(paste0(time - (time %% 10), '-', time - (time %% 10)+9))]
pages_decade <- pages_anual[,mean(V1), by=c('period', 'id')]
pages_10yr <- merge(pages_anual, pages_decade, by = c('period', 'id'))
setnames(pages_10yr, old = c('V1.x', 'V1.y'),
         new = c('avg_annual', 'avg_10yr'))

pages_10yr$avg_10yr <- as.numeric(pages_10yr$avg_10yr)
pages_10yr$avg_annual <- as.numeric(pages_10yr$avg_annual)

pages_5 <- pages_anual[, period := factor(ifelse((time %% 10) >= 5,
                                             paste0(time - (time %% 10)+5, '-', time - (time %% 10)+9),
                                              paste0(time - (time %% 10), '-', time - (time %% 10)+4)
))]

pages_5_mean <- pages_5[,mean(V1), by=c('id', 'period')]
pages_5yr <- merge(pages_5, pages_5_mean, by = c('period', 'id'))
setnames(pages_5yr, old = c('V1.x', 'V1.y'),
         new = c('avg_annual', 'avg_5yr'))

pages_5yr$avg_5yr <- as.numeric(pages_5yr$avg_5yr)
pages_5yr$avg_annual <- as.numeric(pages_5yr$avg_annual)


#------------save RDS file-----------------------
saveRDS(pauling_10yr, '../../data/input/point/timescales/pauling_10yr.rds')
saveRDS(pauling_5yr, '../../data/input/point/timescales/pauling_5yr.rds')
saveRDS(luterbacher_10yr, '../../data/input/point/timescales/luterbacher_10yr.rds')
saveRDS(luterbacher_5yr, '../../data/input/point/timescales/luterbacher_5yr.rds')
saveRDS(owda_10yr, '../../data/input/point/timescales/owda_10yr.rds')
saveRDS(owda_5yr, '../../data/input/point/timescales/owda_5yr.rds')
saveRDS(ljungvist_10yr, '../../data/input/point/timescales/ljungvist_10yr.rds')
saveRDS(ljungvist_5yr, '../../data/input/point/timescales/ljungvist_5yr.rds')
saveRDS(pages_10yr, '../../data/input/point/timescales/ljungvist_10yr.rds')
saveRDS(pages_5yr, '../../data/input/point/timescales/ljungvist_5yr.rds')
