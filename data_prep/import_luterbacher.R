library(data.table)
library(readtext)
library(ggplot2)

dload_path <- '../../data/input/gridded/luterbacher/raw/'

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
luterbacher[, lat := rep(seq(-24.75, 39.75, 0.5), each = num_of_files, 70)]  #latilude based on given order after melt
luterbacher[, long := rep(seq(69.75, 35.25, -0.5), each = 130 * num_of_files)]  #longitude based on given order after melt
luterbacher[luterbacher == -999.99] <- NA   # -999.99 to NA values
luterbacher[, season := factor(season, levels =  c('wi', 'sp', 'su', 'au'))] 
luterbacher[, temp_yr := mean(temp), .(year, cell_id)]
setorder(luterbacher, "cell_id", "year", "season")
luterbacher <- luterbacher[complete.cases(luterbacher)]

#------save-----------
saveRDS(luterbacher, "../../data/input/gridded/luterbacher/luterbacher.rds")

#-----validate-----------
luterbacher <- readRDS("../../data/input/gridded/luterbacher/luterbacher.rds")
#luterbacher <- readRDS("../../Projects/2018XEROS/data/input/gridded/luterbacher/luterbacher.rds")

try <- luterbacher[year == 1802]
ggplot(try, aes(x = long, y = lat , fill = temp)) +
  geom_tile() +
  scale_fill_gradient(low = "deepskyblue", high = 'dark red', na.value = "navyblue") + 
  facet_grid(season ~ year) +
  theme_minimal()

