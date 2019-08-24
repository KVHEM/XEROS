
library(data.table)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tmap)
library(maps)
library(gridExtra)



#----------load pauling data--------

pauling <- as.data.table(readRDS('../../data/input/gridded/pauling/pauling.rds'))
pauling2 <- as.data.table(readRDS('../../data/input/gridded/pauling/pauling.rds'))

#----computation of variation for every season and  two time periods: old(1500-1600) and new (1900-2000)
#----and ratio between these variation in every point---------

raster_template <- pauling[, c('precip', 'year', 'season') := NULL]
raster_template <- raster_template %>% distinct(cell_id, .keep_all = TRUE) #only one raster layer

#--winter--
pau_wi_old <- pauling2[season %in% 'wi' & year >= 1500 & year <= 1600]
pau_wi_new <- pauling2[season %in% 'wi' & year >= 1900 & year <= 2000]

var_wi <- pau_wi_old[,list(wi_old = var(precip)),'cell_id']
wi_new <- pau_wi_new[,list(wi_new = var(precip)),'cell_id']
var_wi <- merge(var_wi, wi_new, by = 'cell_id')
var_wi <- merge(var_wi, raster_template, by = 'cell_id')
var_wi[, ratio := var_wi$wi_old / var_wi$wi_new]

#--spring--
pau_sp_old <- pauling2[season %in% 'sp' & year >= 1500 & year <= 1600]
pau_sp_new <- pauling2[season %in% 'sp' & year >= 1900 & year <= 2000]

var_sp <- pau_sp_old[,list(sp_old = var(precip)),'cell_id']
sp_new <- pau_sp_new[,list(sp_new = var(precip)),'cell_id']
var_sp <- merge(var_sp, sp_new, by = 'cell_id')
var_sp <- merge(var_sp, raster_template, by = 'cell_id')
var_sp[, ratio := var_sp$sp_old / var_sp$sp_new]

#--summer--
pau_su_old <- pauling2[season %in% 'su' & year >= 1500 & year <= 1600]
pau_su_new <- pauling2[season %in% 'su' & year >= 1900 & year <= 2000]

var_su <- pau_su_old[,list(su_old = var(precip)),'cell_id']
su_new <- pau_su_new[,list(su_new = var(precip)),'cell_id']
var_su <- merge(var_su, su_new, by = 'cell_id')
var_su <- merge(var_su, raster_template, by = 'cell_id')
var_su[, ratio := var_su$su_old / var_su$su_new]

#--autumn--
pau_au_old <- pauling2[season %in% 'au' & year >= 1500 & year <= 1600]
pau_au_new <- pauling2[season %in% 'au' & year >= 1900 & year <= 2000]

var_au <- pau_au_old[,list(au_old = var(precip)),'cell_id']
au_new <- pau_au_new[,list(au_new = var(precip)),'cell_id']
var_au <- merge(var_au, au_new, by = 'cell_id')
var_au <- merge(var_au, raster_template, by = 'cell_id')
var_au[, ratio := var_au$au_old / var_au$au_new]


#--------map output---------------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)


p1 <- ggplot(data = world) +
        stat_summary_2d(data=var_wi, aes(x=lat, y=long, z = var_wi$ratio),  bins = 80)  + 
        geom_sf(color="white", fill=NA) +
        coord_sf(xlim = c(-25, 40), ylim = c(33, 71), expand = FALSE)+
        labs(x = 'Latitude', y = 'Longitude', fill = 'Ratio', title = 'Winter')+
        theme(panel.background = element_rect(fill = '#999999'))
  
p2 <- ggplot(data = world) +
        stat_summary_2d(data=var_sp, aes(x=lat, y=long, z = var_sp$ratio),  bins = 80)  + 
        geom_sf(color="white", fill=NA) +
        coord_sf(xlim = c(-25, 40), ylim = c(33, 71), expand = FALSE)+
        labs(x = 'Latitude', y = 'Longitude', fill = 'Ratio', title = 'Spring')+
        theme(panel.background = element_rect(fill = '#999999'))

p3 <- ggplot(data = world) +
        stat_summary_2d(data=var_su, aes(x=lat, y=long, z = var_su$ratio),  bins = 80)  + 
        geom_sf(color="white", fill=NA) +
        coord_sf(xlim = c(-25, 40), ylim = c(33, 71), expand = FALSE)+
        labs(x = 'Latitude', y = 'Longitude', fill = 'Ratio', title = 'Summer')+
        theme(panel.background = element_rect(fill = '#999999'))

p4 <- ggplot(data = world) +
        stat_summary_2d(data=var_au, aes(x=lat, y=long, z = var_au$ratio),  bins = 80)  + 
        geom_sf(color="white", fill=NA) +
        coord_sf(xlim = c(-25, 40), ylim = c(33, 71), expand = FALSE)+
        labs(x = 'Latitude', y = 'Longitude', fill = 'Ratio', title = 'Autumn')+
        theme(panel.background = element_rect(fill = '#999999'))

grid.arrange(p1,p2,p3,p4, nrow = 2, top = 'Ratio between variance of precipitation data')


