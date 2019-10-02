source('./code/main.R')

#-------------load data--------------
luterbacher <- as.data.table(readRDS("./data/input/gridded/luterbacher/luterbacher.rds"), na.rm=F)
cru_ts <- as.data.table(readRDS('./data/input/gridded/cru_ts4.01/cru_ts4.01.rds'))

#-----create same cols and its names in all files-----------
luterbacher[, var := 'tavg']
setnames(luterbacher, old = c('temp'), new = c('value'))
cru_ts_tavg <- cru_ts[var == 'tavg']
cru_ts_tavg[, temp_yr := mean(value)]

#------------cut to same area--------------
luterbacher_cut <- luterbacher[lon >= -10.75]
cru_ts_cut <- cru_ts_tavg[lat <= 69.75]

#----------rbind in one file-----------
tavg_merge <- unique(rbind(luterbacher_cut, cru_ts_cut))
tavg_merge <- tavg_merge[complete.cases(tavg_merge)]

#-----------save to .rds file-----------
saveRDS(tavg_merge, './data/input/gridded/tavg_merge.rds')

#-------------validate-----------------
try <- tavg_merge[year == 1850] #change
ggplot(try, aes(x = lon, y = lat, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "deepskyblue", high = 'dark red', na.value = "navyblue") + 
  facet_grid(season ~ year) +
  theme_minimal()
