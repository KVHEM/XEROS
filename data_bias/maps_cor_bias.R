library(data.table)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(sf)
library(dplyr)
library(gridExtra)

world <- ne_countries(scale = "medium", returnclass = "sf")

#-------function that compute cross correlation and give max value and lag------------
Find_Max_CCF <- function(a, b) 
{ 
  d <- ccf(a, b, plot = FALSE) 
  cor = d$acf[, , 1] 
  lag = d$lag[, , 1] 
  res = data.table(cor, lag) 
  res_max = res[which.max(res$cor), ] 
  return(res_max) 
}

#------------load data----------
pauling <- as.data.table(readRDS('../../data/input/gridded/pauling/pauling.rds'))
pauling <- pauling[complete.cases(pauling)]
luterbacher <- as.data.table(readRDS('../../data/input/gridded/luterbacher/luterbacher.rds'))
luterbacher <- luterbacher[complete.cases(luterbacher)]
luterbacher$year <- as.numeric(luterbacher$year)
CRU <- as.data.table(readRDS('../../data/input/gridded/cru_ts4.01/cru_ts4.01.rds')) 
CRU <- CRU[complete.cases(CRU)]

pauling2 <- as.data.table(readRDS('../../data/input/gridded/pauling/pauling.rds'))
raster_template <- pauling2[, c('precip', 'year', 'season') := NULL]
raster_template <- raster_template %>% distinct(cell_id, .keep_all = TRUE) #only one raster layer
raster_template <- raster_template[, .(cell_id, lat, long)]
  
CRU[, cell_id := NULL]  # making sure that cell_id and dimension match
CRU <- merge(CRU, raster_template, by = c('long', 'lat'))
luterbacher[, cell_id := NULL]
luterbacher <- merge(luterbacher, raster_template, by = c('long', 'lat'))

#------------computation of correlation------------

pau_wi <- pauling[season %in% 'wi' & year >= 1900 & year <= 2000]
pau_sp <- pauling[season %in% 'sp' & year >= 1900 & year <= 2000]
pau_su <- pauling[season %in% 'su' & year >= 1900 & year <= 2000]
pau_au <- pauling[season %in% 'au' & year >= 1900 & year <= 2000]

lut_wi <- luterbacher[season %in% 'wi' & year >= 1900 & year <= 2000]
lut_sp <- luterbacher[season %in% 'sp' & year >= 1900 & year <= 2000]
lut_su <- luterbacher[season %in% 'su' & year >= 1900 & year <= 2000]
lut_au <- luterbacher[season %in% 'au' & year >= 1900 & year <= 2000]

CRU_temp_wi <- CRU[season %in% 'wi' & year >= 1900 & year <= 2000 & var %in% 'tavg']
CRU_temp_sp <- CRU[season %in% 'sp' & year >= 1900 & year <= 2000 & var %in% 'tavg']
CRU_temp_su <- CRU[season %in% 'su' & year >= 1900 & year <= 2000 & var %in% 'tavg']
CRU_temp_au <- CRU[season %in% 'au' & year >= 1900 & year <= 2000 & var %in% 'tavg']

CRU_precip_wi <- CRU[season %in% 'wi' & year >= 1900 & year <= 2000 & var %in% 'precip']
CRU_precip_sp <- CRU[season %in% 'sp' & year >= 1900 & year <= 2000 & var %in% 'precip']
CRU_precip_su <- CRU[season %in% 'su' & year >= 1900 & year <= 2000 & var %in% 'precip']
CRU_precip_au <- CRU[season %in% 'au' & year >= 1900 & year <= 2000 & var %in% 'precip']

precip_wi <- merge(pau_wi, CRU_precip_wi, by=c('cell_id', 'year'))
precip_su <- merge(pau_su, CRU_precip_su, by=c('cell_id', 'year'))
precip_sp <- merge(pau_sp, CRU_precip_sp, by=c('cell_id', 'year'))
precip_au <- merge(pau_au, CRU_precip_au, by=c('cell_id', 'year'))

temp_wi <- merge(lut_wi, CRU_temp_wi, by=c('cell_id', 'year'))
temp_su <- merge(lut_su, CRU_temp_su, by=c('cell_id', 'year'))
temp_sp <- merge(lut_sp, CRU_temp_sp, by=c('cell_id', 'year'))
temp_au <- merge(lut_au, CRU_temp_au, by=c('cell_id', 'year'))

ccorr_wi_precip <- precip_wi[,cbind(a = Find_Max_CCF(precip, value)),'cell_id']
ccorr_su_precip <- precip_su[,cbind(a = Find_Max_CCF(precip, value)),'cell_id']
ccorr_sp_precip <- precip_sp[,cbind(a = Find_Max_CCF(precip, value)),'cell_id']
ccorr_au_precip <- precip_au[,cbind(a = Find_Max_CCF(precip, value)),'cell_id']

ccorr_wi_precip <- merge(ccorr_wi_precip, raster_template, by = 'cell_id')
ccorr_su_precip <- merge(ccorr_su_precip, raster_template, by = 'cell_id')
ccorr_sp_precip <- merge(ccorr_sp_precip, raster_template, by = 'cell_id')
ccorr_au_precip <- merge(ccorr_au_precip, raster_template, by = 'cell_id')

ccorr_wi_temp <- temp_wi[,cbind(a = Find_Max_CCF(temp, value)),'cell_id']
ccorr_su_temp <- temp_su[,cbind(a = Find_Max_CCF(temp, value)),'cell_id']
ccorr_sp_temp <- temp_sp[,cbind(a = Find_Max_CCF(temp, value)),'cell_id']
ccorr_au_temp <- temp_au[,cbind(a = Find_Max_CCF(temp, value)),'cell_id']

ccorr_wi_temp <- merge(ccorr_wi_temp, raster_template, by = 'cell_id')
ccorr_su_temp <- merge(ccorr_su_temp, raster_template, by = 'cell_id')
ccorr_sp_temp <- merge(ccorr_sp_temp, raster_template, by = 'cell_id')
ccorr_au_temp <- merge(ccorr_au_temp, raster_template, by = 'cell_id')


#----------map output-------------
world <- ne_countries(scale = "medium", returnclass = "sf")
palette_RdBu = colorRampPalette(rev(c('#d73027','#f46d43','#fdae61','#fee090','#fef0d9','#e0f3f8','#abd9e9','#74add1','#4575b4')), space = "rgb")

p1 <- ggplot(data = world) +
  stat_summary_2d(data = ccorr_wi_precip, aes(x=lat, y=long, z = a.cor))  + 
  geom_sf(color="white", fill=NA) +
  coord_sf(xlim = c(-15, 40), ylim = c(35, 71), expand = FALSE)+
  labs(x = 'Latitude', y = 'Longitude', fill = 'C-corr', title = 'Winter')+
  theme(panel.background = element_rect(fill = '#999999'))



p2 <- ggplot(data = world) +
  stat_summary_2d(data = ccorr_sp_precip, aes(x=lat, y=long, z = a.cor))  + 
  geom_sf(color="white", fill=NA) +
  coord_sf(xlim = c(-15, 40), ylim = c(35, 71), expand = FALSE)+
  labs(x = 'Latitude', y = 'Longitude', fill = 'C-corr', title = 'Spring')+
  theme(panel.background = element_rect(fill = '#999999'))


p3 <- ggplot(data = world) +
  stat_summary_2d(data = ccorr_su_precip, aes(x=lat, y=long, z = a.cor))  + 
  geom_sf(color="white", fill=NA) +
  coord_sf(xlim = c(-15, 40), ylim = c(35, 71), expand = FALSE)+
  labs(x = 'Latitude', y = 'Longitude', fill = 'C-corr', title = 'Summer')+
  theme(panel.background = element_rect(fill = '#999999'))


p4 <- ggplot(data = world) +
  stat_summary_2d(data = ccorr_au_precip, aes(x=lat, y=long, z = a.cor))  + 
  geom_sf(color="white", fill=NA) +
  coord_sf(xlim = c(-15, 40), ylim = c(35, 71), expand = FALSE)+
  labs(x = 'Latitude', y = 'Longitude', fill = 'C-corr', title = 'Autumn')+
  theme(panel.background = element_rect(fill = '#999999'))


grid.arrange(p1,p2,p3,p4, nrow = 2, top = 'Cross-correlation between Pauling and CRU precipitation data')

ggsave("../../results/bias/cor_precip.pdf", gg)

p5 <- ggplot(data = world) +
  stat_summary_2d(data = ccorr_wi_temp, aes(x = lat, y =long, z = a.cor))  + 
  geom_sf(color="white", fill=NA) +
  coord_sf(xlim = c(-25, 40), ylim = c(33, 71), expand = FALSE)+
  labs(x = 'Latitude', y = 'Longitude', fill = 'C-corr', title = 'Winter')+
  theme(panel.background = element_rect(fill = '#999999'))


p6 <- ggplot(data = world) +
  stat_summary_2d(data = ccorr_sp_temp, aes(x=lat, y=long, z = a.cor))  + 
  geom_sf(color="white", fill=NA) +
  coord_sf(xlim = c(-25, 40), ylim = c(33, 71), expand = FALSE)+
  labs(x = 'Latitude', y = 'Longitude', fill = 'C-corr', title = 'Spring')+
  theme(panel.background = element_rect(fill = '#999999'))

p7 <- ggplot(data = world) +
  stat_summary_2d(data = ccorr_su_temp, aes(x=lat, y=long, z = a.cor))  + 
  geom_sf(color="white", fill=NA) +
  coord_sf(xlim = c(-25, 40), ylim = c(33, 71), expand = FALSE)+
  labs(x = 'Latitude', y = 'Longitude', fill = 'C-corr', title = 'Summer')+
  theme(panel.background = element_rect(fill = '#999999'))

p8 <- ggplot(data = world) +
  stat_summary_2d(data = ccorr_au_temp, aes(x=lat, y=long, z = a.cor))  + 
  geom_sf(color="white", fill=NA) +
  coord_sf(xlim = c(-25, 40), ylim = c(33, 71), expand = FALSE)+
  labs(x = 'Latitude', y = 'Longitude', fill = 'C-corr', title = 'Autumn')+
  theme(panel.background = element_rect(fill = '#999999'))

grid.arrange(p5,p6,p7,p8, nrow = 2, top = 'Cross-correlation between Luterbacher and CRU temperature data')
