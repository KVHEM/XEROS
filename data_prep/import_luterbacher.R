library(data.table)
library(readtext)
library(ggplot2)

dload_path <- '../../data/input/gridded/t_luterbacher/raw/'

#-----download--------
sez <- c('au', 'sp', 'su', 'wi')  #seasons
for (i in 1500:1900) {            #time period you want to download (years)  
  for (j in 1:length(sez)) {      #all seasons in year
    k <- sez[j]
    raw_dta <- paste0(i, '-', k, '.txt')
    url <- paste0('https://www1.ncdc.noaa.gov/pub/data/paleo/historical/europe-seasonal-files/', raw_dta)
    download.file(url, paste0(dload_path, raw_dta))   #target folder for download files
  }
}

#-----read-----------
list_files <- list.files(path = dload_path, pattern = "*.txt", full.names = T) #list of all downloaded files
load_dta <- as.data.table(melt(t(sapply(list_files, FUN = scan))))  #reading of files, transpose, melting and save as one data table

#-------tidy----------
num_of_files <- length(list_files)
load_dta[, paste0('Var1', 1:2) := tstrsplit(Var1, '-')]
load_dta[, Var1 := NULL]  #use a folder name to create column of year and season 
load_dta[, paste0('Var11', 1:2) := tstrsplit(Var11, 'raw/')]
load_dta[, Var11 := NULL]
load_dta[, Var111 := NULL]
load_dta[, paste0('Var12', 1:2) := tstrsplit(Var12, '.t')]
load_dta[, Var12:=NULL]
load_dta[, Var122 := NULL]
setnames(load_dta, old = c("value", 'Var112', 'Var121', 'Var2'), new = c("temp", 'year', 'season', 'cell_id'))
load_dta[, lat := rep(seq(-24.75, 39.75, 0.5), each = num_of_files, 70)]  #latilude based on given order after melt
load_dta[, long := rep(seq(69.75, 35.25, -0.5), each = 130 * num_of_files)]  #longitude based on given order after melt
load_dta[load_dta == -999.99] <- NA   # -999.99 to NA values
load_dta[, season := factor(season, levels =  c('wi', 'sp', 'su', 'au'))] 
setorder(load_dta, "cell_id", "year", "season")

#------save-----------
saveRDS(load_dta, "../../data/input/temperature/luterbacher/luterbacher.rds")

#-----validate-----------
test_dta <- load_dta[year <= 1502]
ggplot(test_dta, aes(x = lat, y = long, fill = temp)) +
  geom_tile() +
  scale_fill_gradient(low = "deepskyblue", high = 'red', na.value = "navyblue") + 
  facet_grid(season ~ year) +
  theme_minimal()

