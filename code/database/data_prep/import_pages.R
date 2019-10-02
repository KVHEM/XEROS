source('./code/main.R')

library(rlist)
library(rgdal)

data_dir <- './data/input/point/pages2k/'
dir.create(data_dir)
data_name <- 'PAGES2k_v2.0.0.RData'
load(paste0(data_dir, data_name))
#The file should be uploaded in a public repository and downloaded here see issue #31

raw_dataset <- D; rm(D); gc()
data_size <- length(raw_dataset)

pages2k <- list()
var_names <- vector()

raw_dataset_eu <- raw_dataset[grepl("Eur-", names(raw_dataset))]

pages2k <- raw_dataset_eu %>%
  subset(max(paleoData[[1]]$paleoMeasurementTable[[1]]$year$values) > 1500) #Keep records with at least one value after 1500
pages2k_size = length(pages2k)

pages2k_meta <- pages2k %>% #These properties exist in all records
  list.select(name = dataSetName, archive = archiveType, region = geo$pages2kRegion, lon = geo$longitude, lat = geo$latitude, 
              min_year = minYear, max_year = maxYear) %>%
  list.stack %>%
  data.table

pages2k_ts <- pages2k %>% ## Time series
  list.select(paleoData[[1]]$paleoMeasurementTable[[1]]$trsgi$values)
pages2k_ts_2 <- pages2k %>% ## Time series
  list.select(paleoData[[1]]$paleoMeasurementTable[[1]]$temperature$values)
pages2k_ts <- sapply(pages2k_ts, unlist)
pages2k_ts <- pages2k_ts[-which(sapply(pages2k_ts, is.null))]
pages2k_ts_2 <- sapply(pages2k_ts_2, unlist)
pages2k_ts_2 <- pages2k_ts_2[-which(sapply(pages2k_ts_2, is.null))]
pages2k_ts <- c(pages2k_ts, pages2k_ts_2)

pages2k_ts_time <- pages2k %>%
  list.select(paleoData[[1]]$paleoMeasurementTable[[1]]$year$values)
pages2k_ts_time <- pages2k_ts_time[names(pages2k_ts)]
pages2k_ts_time <- sapply(pages2k_ts_time, unlist)

pages2k_ts <- mapply(cbind, pages2k_ts_time, pages2k_ts, SIMPLIFY = FALSE)

ts_id <- pages2k %>% 
  list.select(paleoData[[1]]$paleoMeasurementTable[[1]]$trsgi$pages2kID)
ts_id_2 <- pages2k %>% 
  list.select(paleoData[[1]]$paleoMeasurementTable[[1]]$temperature$pages2kID)

ts_id <- c(ts_id, ts_id_2)
ts_id <- sapply(ts_id, unlist)
ts_id <- melt(ts_id[-which(sapply(ts_id, is.null))])
colnames(ts_id) = c('id', 'name')
pages2k_meta <- merge(ts_id, pages2k_meta)

pages2k_ts <- pages2k_ts[ts_id$name]
names(pages2k_ts) <- ts_id$id
pages2k_eu <- rbindlist(lapply(pages2k_ts, data.table), idcol = T)
colnames(pages2k_eu) <- c("id", "time", "temp")
pages2k_eu <- pages2k_eu[time >= 1500]

pages2k_meta = cbind(pages2k_meta,  #add wintri projection of coordinates to plot data sites on map
                     data.table(project(cbind(lon_wintri = pages2k_meta$lon, 
                                              lat_wintri = pages2k_meta$lat), 
                                        proj = "+proj=wintri"))) 

#--------------------grid id-----------------------
grid_bounds <- readRDS('./data/geodata/grid_cells.rds')
grid_bounds <- grid_bounds[1:5791, ]
dt <- unique(grid_bounds[pages2k_meta, .(id, cell_id), 
                         on = .(lat_l <= lat, lat_u > lat,  
                                lon_l <= lon, lon_u > lon)])
pages2k_meta <- as.data.table(pages2k_meta)
pages2k_meta_id <- pages2k_meta[dt, on = 'id']
pages2k_meta_id <- pages2k_meta_id[complete.cases(pages2k_meta_id)]

leaflet::leaflet() %>% leaflet::addTiles() %>%
  leaflet::addMarkers(pages2k_meta$lon, pages2k_meta$lat,popup = pages2k_meta$archive)

saveRDS(pages2k_eu, file = paste0(data_dir, 'pages2k_eu.rds')) 
saveRDS(pages2k_meta, file = paste0(data_dir, 'pages2k_eu_meta.rds'))  
saveRDS(pages2k_meta_id, file = paste0(data_dir, 'pages2k_eu_meta_grid.rds'))

