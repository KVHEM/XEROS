library(pipeR)
library(data.table)
library(rlist)

load('../../data/input/point/raw/PAGES2k_v2.0.0.RData')

raw_dataset <- D; rm(D); gc()
data_size <- length(raw_dataset)

pages2k <- list()
var_names <- vector()

raw_dataset_eu <- raw_dataset[grepl("Eur-", names(raw_dataset))]

pages2k <- raw_dataset_eu %>>%
  subset(max(paleoData[[1]]$paleoMeasurementTable[[1]]$year$values) > 1500) #Keep records with at least one value after 1500
pages2k_size = length(pages2k)

pages2k_meta <- pages2k %>>% #These properties exist in all records
  list.select(name = dataSetName, archive = archiveType, region = geo$pages2kRegion, long = geo$longitude, lat = geo$latitude, 
              min_year = minYear, max_year = maxYear) %>>%
  list.stack %>>%
  data.table

pages2k_ts <- pages2k %>>% ## Time series
  list.select(paleoData[[1]]$paleoMeasurementTable[[1]]$trsgi$values)
pages2k_ts_2 <- pages2k %>>% ## Time series
  list.select(paleoData[[1]]$paleoMeasurementTable[[1]]$temperature$values)
pages2k_ts <- sapply(pages2k_ts, unlist)
pages2k_ts <- pages2k_ts[-which(sapply(pages2k_ts, is.null))]
pages2k_ts_2 <- sapply(pages2k_ts_2, unlist)
pages2k_ts_2 <- pages2k_ts_2[-which(sapply(pages2k_ts_2, is.null))]
pages2k_ts <- c(pages2k_ts, pages2k_ts_2)

pages2k_ts_time <- pages2k %>>%
  list.select(paleoData[[1]]$paleoMeasurementTable[[1]]$year$values)
pages2k_ts_time <- pages2k_ts_time[names(pages2k_ts)]
pages2k_ts_time <- sapply(pages2k_ts_time, unlist)

pages2k_ts <- mapply(cbind, pages2k_ts_time, pages2k_ts, SIMPLIFY = FALSE)

ts_id <- pages2k %>>% 
  list.select(paleoData[[1]]$paleoMeasurementTable[[1]]$trsgi$pages2kID)
ts_id_2 <- pages2k %>>% 
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

saveRDS(pages2k_eu, file = "../../data/input/point/pages2k_eu.rds")
saveRDS(pages2k_meta, file = "../../data/input/point/pages2k_eu_meta.rds")


