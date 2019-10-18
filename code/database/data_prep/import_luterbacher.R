source('./code/main.R')

library(readtext)
library(lubridate)
library(magrittr)

data_dir <- './data/input/gridded/luterbacher/'
dir.create(data_dir)
dload_path <- paste0(data_dir, 'raw/')
dir.create(dload_path)

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
list_files <- list.files(path = dload_path, pattern = "*.txt", full.names = TRUE) #list of all downloaded files
luterbacher <- as.data.table(melt(t(sapply(list_files, FUN = scan))))  #reading of files, transpose, melting and save as one data table

#-------tidy----------
num_of_files <- length(list_files)
luterbacher[, paste0('Var1', 1:2) := tstrsplit(Var1, '-')]
luterbacher[, Var1 := NULL]  #use a folder name to create column of year and season 
luterbacher[, paste0('Var11', 1:2) := tstrsplit(Var11, 'raw/')]
luterbacher[, Var11 := NULL]
luterbacher[, Var111 := NULL]
luterbacher[, paste0('Var12', 1:2) := tstrsplit(Var12, '.t')]
luterbacher[, Var12:=NULL]
luterbacher[, Var122 := NULL]
setnames(luterbacher, old = c("value", 'Var112', 'Var121', 'Var2'), new = c("temp", 'year', 'season', 'cell_id'))
luterbacher[, lon := rep(seq(-24.75, 39.75, 0.5), each = num_of_files, 70)]  #longitude based on given order after melt
luterbacher[, lat := rep(seq(69.75, 35.25, -0.5), each = 130 * num_of_files)]  #latilude based on given order after melt
luterbacher[luterbacher == -999.99] <- NA   # -999.99 to NA values
luterbacher[, season := factor(season, levels =  c('wi', 'sp', 'su', 'au'))] 
luterbacher[, temp_yr := mean(temp), .(year, cell_id)]
setorder(luterbacher, "cell_id", "year", "season")
luterbacher <- luterbacher[complete.cases(luterbacher)]

#-------date format time logs--------
luterbacher[ season == 'wi', mo:=1]    
luterbacher[ season == 'au', mo:=10]
luterbacher[ season == 'sp', mo:=4]
luterbacher[ season == 'su', mo:=7]
luterbacher[, day := factor(15)]
luterbacher[, time := NA]
luterbacher$time <- as.Date(with(luterbacher, paste(year, mo, day, sep = "-")), "%Y-%m-%d")
luterbacher[, mo := NULL]
luterbacher[, day := NULL]
head(luterbacher)

#------save-----------
fname <- paste0(data_dir, "luterbacher.rds")
saveRDS(luterbacher, fname)

#-----validate-----------
luterbacher <- readRDS(fname)

try <- luterbacher[year == 1802]
ggplot(try, aes(x = lon, y = lat , fill = temp)) +
  geom_tile() +
  scale_fill_gradient(low = "deepskyblue", high = 'dark red', na.value = "navyblue") + 
  facet_grid(season ~ year) +
  theme_minimal()

