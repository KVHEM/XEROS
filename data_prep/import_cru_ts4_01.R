library(ncdf4)
library(raster)
library(data.table)
library(ggplot2)

#-----------read .nc file from working directory to raster-------------
ncpath <- '.../.../data/input/gridded/cru_ts4.01/'
ncname_1 <- 'cru_pet_mon'
ncfname_1 <- paste(ncpath, ncname_1, '.nc', sep='')
raster_pet <- stack(ncfname_1)
ncname_2 <- 'cru_pre_mon'
ncfname_2 <- paste(ncpath, ncname_2, '.nc', sep='')
raster_pre<- stack(ncfname_2)
ncname_3 <- 'cru_tavg_mon'
ncfname_3 <- paste(ncpath, ncname_3, '.nc', sep='')
raster_tavg<- stack(ncfname_3)

#---------prepare data-----------------
seas_month <- rep(seq(1:460), each = 3) # vector of indeces for new layers
seas_mean_pet <- stackApply(raster_pet, indices = seas_month, fun = mean) # means of varibles for seasonal pet
pet_array <- as.array(seas_mean_pet)
seas_sum_pre <- stackApply(raster_pre, indices = seas_month, fun = 'sum', na.rm = F) # sum of varibles for seasonal pre
pre_array <- as.array(seas_sum_pre)
seas_mean_tavg <- stackApply(raster_tavg, indices = seas_month, fun = mean) # means of varibles for seasonal tavg
tavg_array <- as.array(seas_mean_tavg)

lat <- seq(raster_pet@extent@xmin + 0.25, raster_pet@extent@xmax - 0.25, by = 0.5) # array of coordinants
lon <- seq(raster_pet@extent@ymax - 0.25, raster_pet@extent@ymin + 0.25, by = -0.5)
lonlat <- as.matrix(expand.grid(lon, lat))

nc_pet <- nc_open(ncfname_1)
time_logs_pet <- ncvar_get(nc_pet, 'time') 
num_of_years <- length(time_logs_pet) / 12
last_year <- 1900 + num_of_years # time logs are in days unit from 1-1-1900 but the variable values logs start at day 380
year <- t(rep(c(1901:last_year), each = 4)) # vector of years prepare for 'for cycle'
season <- t(rep(c('wi', 'sp', 'su', 'au'), num_of_years)) # vector of seasons prepare for 'for cycle'
cell_id <- c(1:length(lonlat[, 1]))

#-------------making data table from pet file---------------------
prep_pet <- data.table() # data table for pet only .nc file

for (i in 1:length(year)) {
  pet_slice <- pet_array[, , i]   # creating array with one year and one season values
  pet_vec <- as.vector(pet_slice)
  dt <- as.data.table(cbind(pet_vec, lonlat))
  setnames(dt, old = c('pet_vec', 'Var1', 'Var2'), new = c('value', 'long', 'lat'))
  dt[, year := year [, i]]
  dt[, season := season [, i]]
  dt[, cell_id := cell_id]
  dt <- dt[, c(6, 1, 4, 5, 3, 2)]  # column order
  dt[,var := 'pet']
  prep_pet <- rbind(prep_pet, dt)
}

#-------------making data table from pre file---------------------
prep_pre <- data.table() # data table for pre only .nc file

for (i in 1:length(year)) {
  pre_slice <- pre_array[, , i]   
  pre_vec <- as.vector(pre_slice)
  dt <- as.data.table(cbind(pre_vec, lonlat))
  setnames(dt, old = c('pre_vec', 'Var1', 'Var2'), new = c('value', 'long', 'lat'))
  dt[, year := year [, i]]
  dt[, season := season [, i]]
  dt[, cell_id := cell_id]
  dt <- dt[, c(6, 1, 4, 5, 3, 2)]  
  dt[,var := 'precip']
  prep_pre <- rbind(prep_pre, dt)
}

#-------------making data table from tavg file---------------------
prep_tavg <- data.table() # data table for pre only .nc file

for (i in 1:length(year)) {
  tavg_slice <- tavg_array[, , i]   
  tavg_vec <- as.vector(tavg_slice)
  dt <- as.data.table(cbind(tavg_vec, lonlat))
  setnames(dt, old = c('tavg_vec', 'Var1', 'Var2'), new = c('value', 'long', 'lat'))
  dt[, year := year [, i]]
  dt[, season := season [, i]]
  dt[, cell_id := cell_id]
  dt <- dt[, c(6, 1, 4, 5, 3, 2)]  
  dt[,var := 'tavg']
  prep_tavg <- rbind(prep_tavg, dt)
}


#------------melting and save to RDS-------------

cru_ts4.01 <-  unique(rbind(prep_pet, prep_pre, prep_tavg))

saveRDS(cru_ts4.01, '.../.../data/input/gridded/cru_ts4.01/cru_ts4.01.rds')

#------------validate---------------------------
try <- prep_pre[year == 1968] #change
ggplot(try, aes(x = lat, y = long, fill = pre)) +
  geom_tile() +
  scale_fill_gradient(low = "deepskyblue", high = 'dark red', na.value = "navyblue") + 
  facet_grid(season ~ year) +
  theme_minimal()




