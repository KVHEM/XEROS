source('./code/main.R')
source('./code/graphics.R')
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
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
pauling <- as.data.table(readRDS('./data/input/gridded/pauling/pauling.rds'))
pauling <- pauling[complete.cases(pauling)]
CRU <- as.data.table(readRDS('./data/input/gridded/cru_ts4.01/cru_ts4.01.rds')) 
CRU <- CRU[complete.cases(CRU)]

pauling2 <- as.data.table(readRDS('./data/input/gridded/pauling/pauling.rds'))
raster_template <- pauling2[, c('precip', 'year', 'season') := NULL]
raster_template <- raster_template %>% distinct(cell_id, .keep_all = TRUE) #only one raster layer
raster_template <- raster_template[, .(cell_id, lat, lon)]
  
CRU[, cell_id := NULL]  # making sure that cell_id and dimension match
CRU <- merge(CRU, raster_template, by = c('lon', 'lat'))

#------------computation of correlation------------

pau_wi <- pauling[season %in% 'wi' & year >= 1900 & year <= 2000]
pau_sp <- pauling[season %in% 'sp' & year >= 1900 & year <= 2000]
pau_su <- pauling[season %in% 'su' & year >= 1900 & year <= 2000]
pau_au <- pauling[season %in% 'au' & year >= 1900 & year <= 2000]

CRU_precip_wi <- CRU[season %in% 'wi' & year >= 1900 & year <= 2000 & var %in% 'precip']
CRU_precip_sp <- CRU[season %in% 'sp' & year >= 1900 & year <= 2000 & var %in% 'precip']
CRU_precip_su <- CRU[season %in% 'su' & year >= 1900 & year <= 2000 & var %in% 'precip']
CRU_precip_au <- CRU[season %in% 'au' & year >= 1900 & year <= 2000 & var %in% 'precip']

precip_wi <- merge(pau_wi, CRU_precip_wi, by=c('cell_id', 'year'))
precip_su <- merge(pau_su, CRU_precip_su, by=c('cell_id', 'year'))
precip_sp <- merge(pau_sp, CRU_precip_sp, by=c('cell_id', 'year'))
precip_au <- merge(pau_au, CRU_precip_au, by=c('cell_id', 'year'))

ccorr_wi_precip <- precip_wi[,cbind(a = Find_Max_CCF(precip, value)),'cell_id']
ccorr_su_precip <- precip_su[,cbind(a = Find_Max_CCF(precip, value)),'cell_id']
ccorr_sp_precip <- precip_sp[,cbind(a = Find_Max_CCF(precip, value)),'cell_id']
ccorr_au_precip <- precip_au[,cbind(a = Find_Max_CCF(precip, value)),'cell_id']

ccorr_wi_precip <- merge(ccorr_wi_precip, raster_template, by = 'cell_id')
ccorr_su_precip <- merge(ccorr_su_precip, raster_template, by = 'cell_id')
ccorr_sp_precip <- merge(ccorr_sp_precip, raster_template, by = 'cell_id')
ccorr_au_precip <- merge(ccorr_au_precip, raster_template, by = 'cell_id')

#----------map output-------------

world <- ne_countries(scale = "medium", returnclass = "sf")

p1 <- ggplot(data = world) +
  stat_summary_2d(data = ccorr_wi_precip, aes(x=lon, y=lat, z = a.cor))  + 
  geom_sf(color="white", fill=NA) +
  scale_fill_gradientn(colours = palette_RdBu(100),
                       limits = c(min(ccorr_wi_precip$a.cor),  
                                  max(ccorr_wi_precip$a.cor)),
                       breaks = seq(0, 1, 0.2),
                       guide = guide_colorbar(nbin = 20,
                                              title.position = "top",
                                              title.hjust = 0.5, 
                                              raster = TRUE)) + 
  coord_sf(xlim = c(-15, 40), ylim = c(35, 71), expand = FALSE)+
  labs(x = '', y = '', fill = 'Cor. coef.', title = 'Winter')+
  theme(panel.background = element_rect(fill = '#999999')) +
  theme_bw()

p2 <- ggplot(data = world) +
  stat_summary_2d(data = ccorr_sp_precip, aes(x=lon, y=lat, z = a.cor))  + 
  geom_sf(color="white", fill=NA) +
  scale_fill_gradientn(colours = palette_RdBu(100),
                       limits = c(min(ccorr_sp_precip$a.cor),  
                                  max(ccorr_sp_precip$a.cor)),
                       breaks = seq(0, 1, 0.2),
                       guide = guide_colorbar(nbin = 20,
                                              title.position = "top",
                                              title.hjust = 0.5, 
                                              raster = TRUE)) + 
  coord_sf(xlim = c(-15, 40), ylim = c(35, 71), expand = FALSE)+
  labs(x = '', y = '', fill = 'Cor. coef.', title = 'Spring')+
  theme(panel.background = element_rect(fill = '#999999')) +
  theme_bw()


p3 <- ggplot(data = world) +
  stat_summary_2d(data = ccorr_su_precip, aes(x=lon, y=lat, z = a.cor))  + 
  geom_sf(color="white", fill=NA) +
  scale_fill_gradientn(colours = palette_RdBu(100),
                       limits = c(min(ccorr_su_precip$a.cor),  
                                  max(ccorr_su_precip$a.cor)),
                       breaks = seq(0, 1, 0.2),
                       guide = guide_colorbar(nbin = 20,
                                              title.position = "top",
                                              title.hjust = 0.5, 
                                              raster = TRUE)) + 
  coord_sf(xlim = c(-15, 40), ylim = c(35, 71), expand = FALSE)+
  labs(x = '', y = '', fill = 'Cor. coef.', title = 'Summer')+
  theme(panel.background = element_rect(fill = '#999999')) +
  theme_bw()


p4 <- ggplot(data = world) +
  stat_summary_2d(data = ccorr_au_precip, aes(x=lon, y=lat, z = a.cor))  + 
  geom_sf(color="white", fill=NA) +
  scale_fill_gradientn(colours = palette_RdBu(100),
                       limits = c(min(ccorr_au_precip$a.cor),  
                                  max(ccorr_au_precip$a.cor)),
                       breaks = seq(0, 1, 0.2),
                       guide = guide_colorbar(nbin = 20,
                                              title.position = "top",
                                              title.hjust = 0.5, 
                                              raster = TRUE)) + 
  coord_sf(xlim = c(-15, 40), ylim = c(35, 71), expand = FALSE)+
  labs(x = '', y = '', fill = 'Cor. coef.', title = 'Autumn')+
  theme(panel.background = element_rect(fill = '#999999')) +
  theme_bw()


gg <- grid.arrange(p1,p2,p3,p4, nrow = 2, top = 'Cross-correlation coefficient between Pauling and CRU precipitation data')

ggsave("./results/database/cor_Pauling_CRU_20_century.pdf", gg, height = 24, width = 24, units = "cm")
