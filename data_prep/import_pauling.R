library(ncdf4)
library(ggplot2)
library(data.table)
library(RDS)

#-----------read .nc file from working directory-------------
ncpath <- '../../data/input/gridded/pauling/'  # directory of .nc file
#ncpath <- "../../Projects/2018XEROS/data/input/gridded/pauling/" Secondary path

ncname <- 'pauling'  
ncfname <- paste(ncpath, ncname, '.nc', sep='')
ncin <- nc_open(ncfname)

#---------prepare data-----------------
dname <- 'precip'
lon <- ncvar_get(ncin, 'lon')    # longitude/ latitude vector
lat <- ncvar_get(ncin, 'lat')
time_logs <- ncvar_get(ncin, 'time') # time information from .nc file
time_units <- ncatt_get(ncin, 'time', 'units')
time_units_string <- strsplit(time_units$value, ' ')
time_string <- strsplit(unlist(time_units_string)[3], '-')
time_first_year <- as.integer(unlist(time_string)[1]) # first year of precipitation logs
num_of_years <- length(time_logs) / 4  # number of years
last_year <- time_first_year + num_of_years 
season <- t(rep(c('wi', 'sp', 'su', 'au'), num_of_years)) # vector of years prepare for 'for cycle'
year <- t(rep(c(time_first_year:last_year), each = 4)) # vector of seasons prepare for 'for cycle'
precip_array <- ncvar_get(ncin, dname)  # array of all precipitation values
lonlat <- as.matrix(expand.grid(lon, lat)) # lon lat matrix for slice of precip array
cell_id <- c(1:length(lonlat[, 1])) # vector for cell id


#---------making raw files--------
pauling <- data.table() #final data table

for (i in 1:length(time_logs)) {
   precip_slice <- precip_array[, , i]   # creating array with one year and one season values
   precip_vec <- as.vector(precip_slice)
   dt <- as.data.table(cbind(precip_vec, lonlat))
   setnames(dt, old = c('precip_vec', 'Var1', 'Var2'), new = c('precip', 'lat', 'long'))
   dt[, year := year [, i]]
   dt[, season := season [, i]]
   dt[, cell_id := cell_id]
   dt <- dt[, c(6, 1, 4, 5, 3, 2)]  # column order
   dt[, precip_yr := sum(precip), .(year, cell_id)]
   pauling <- rbind(pauling, dt) # adding every new slice to same data table
#---------if you want to create text files------
   #j <- year[, i]
   #k <- season [, i]
   #name <- paste0(j, '-', k, '.txt')     
   #path <- paste0('.../.../data/input/gridded/pauling/raw/', name)
   #fwrite(dt, path, sep = ' ')
#-------------------------------------   
}

pauling[, season := factor(season, levels =  c('wi', 'sp', 'su', 'au'))]
pauling <- pauling[complete.cases(pauling)]

#---------save to RDS -----------

saveRDS(pauling, '../../data/input/gridded/pauling/pauling.rds')

#------------validate-----------
pauling <- readRDS('../../data/input/gridded/pauling/pauling.rds')

try <- paul[year == 1800] #change
ggplot(try, aes(x = long, y = lat, fill = precip)) +
  geom_tile() +
  scale_fill_gradient(low = "deepskyblue", high = 'dark red', na.value = "navyblue") + 
  facet_grid(season ~ year) +
  theme_minimal()
