#Merges and normalizes GHCN, Pauling and OWDA for comparison

source('./code/main.R')
source('./code/graphics.R')
library(plyr)

#---------------load and prepare data---------------------
dta <- readRDS(file = "./data/output/database/ghcn_pauling.rds")
pauling <- data.table(readRDS('./data/input/gridded/pauling/pauling.rds')) 
owda <- data.table(readRDS('./data/input/gridded/owda/owda_1000.rds'))
dtb <- pauling[cell_id %in% unique(dta$cell_id)]
meta_print <- unique(dta[, c('cell_id', 'lon', 'lat', 'period', 'season', 'n_val')])
colnames(meta_print) <- c('id', 'Lon', 'Lat', 'Period', 'Season', 'N')
setnames(owda, old = 'Time', new = 'year')

# owda grid corection
east <- owda[, 3] + 0.25
orig <- owda
owda[, Lon := NULL]
owda <- cbind(east, owda)

# creating cell_id based on transfering stations id into grid id
grid_bounds <- readRDS('./data/geodata/grid_cells.rds')
grid_bounds <- grid_bounds[1:5791, ]
dtc <- grid_bounds[owda, .(cell_id, year, Lat, Lon, scPDSI), 
                   on = .(lat_l <= Lat, lat_u > Lat,  
                          lon_l <= Lon, lon_u > Lon)]

# choosing only cells that are in dta
dtc <- dtc[cell_id %in% dta$cell_id]

dtc <- dtc[year >= 1697 & year <= 2000] # cutting owda years to be same time period as ghcn_pauling
dtc[, season := factor('su')]

# renaming columns to be same as dta table for easier manipulation in shiny app
setnames(dtc, old = c('Lon', 'Lat', 'scPDSI'), new = c('lon', 'lat', 'precip')) 
dtc[year < 1900, period := factor("pre_1900")] # creating periods
dtc[year >= 1900, period := factor("aft_1900")]
dtc[, dataset := factor('OWDA')]
dtc[, n_val := .N, .(cell_id, season, period)]

# cut dta to dtc because owda has smaller area
dta <- dta[cell_id %in% dtc$cell_id]
dtc <- dtc[dta, .( cell_id, year, season, precip, lon, lat, period, n_val, dataset), on = .(cell_id == cell_id, year == year)]
dtc <- unique(dtc)

# merge owda data with ghcn_pauling
dta <- rbind(dta[, .( cell_id, year, season, precip, lon, lat, period, n_val, dataset)], dtc)

dta[, precip_scale := scale(precip), .(cell_id, season, dataset)] # scaling owda data separetly
saveRDS(dta, './data/output/database//norm_ghcn_paul_owda.rds')
