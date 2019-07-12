#Merges and normalizes GHCN, Pauling and OWDA for comparison

library(plyr)
library(data.table)
library(RDS)

#---------------load and prepare data---------------------
dta <- readRDS(file = "../../data/input/ghcn_pauling.rds")
pauling <- data.table(readRDS('../../data/input/gridded/pauling/pauling.rds')) 

dta <- readRDS(file = "../../Projects/2018XEROS/data/input/ghcn_pauling.rds") #alternative path
pauling <- data.table(readRDS('../../Projects/2018XEROS/data/input/gridded/pauling/pauling.rds')) #alternative path

dtb <- pauling[cell_id %in% unique(dta$cell_id)]
meta_print <- unique(dta[, c('cell_id', 'long', 'lat', 'period', 'season', 'n_val')])
colnames(meta_print) <- c('id', 'Long', 'Lat', 'Period', 'Season', 'N')

owda <- data.table(readRDS('../../data/input/gridded/owda/owda_1000.rds'))
owda <- data.table(readRDS('../../Projects/2018XEROS/data/input/gridded/owda/owda_1000.rds')) #alternative path
setnames(owda, old = 'Time', new = 'year')

# owda grid corection
east <- owda[,3] + 0.25
orig <- owda
owda[,Lon := NULL]
owda <- cbind(east, owda)

# creating cell_id based on transfering stations id into grid id
grid_bounds <- readRDS('../../data/geodata/grid_cells.rds')
grid_bounds <- readRDS('../../Projects/2018XEROS/data/geodata/grid_cells.rds') #alternative path
grid_bounds <- grid_bounds[1:5791, ]
dtc <- grid_bounds[owda, .(cell_id, year, Lat, Lon, scPDSI), 
                   on = .(lat_l <= Lat, lat_u > Lat,  
                          long_l <= Lon, long_u > Lon)]

# choosing only cells that are in dta
dtc <- dtc[cell_id %in% dta$cell_id]

dtc <- dtc[year >= 1697 & year <= 2000] # cutting owda years to be same time period as ghcn_pauling
dtc[, season := factor('su')]

# renaming columns to be same as dta table for easier manipulation in shiny app
setnames(dtc, old = c('Lon', 'Lat', 'scPDSI'), new = c('long', 'lat', 'precip')) 
dtc[year < 1900, period := factor("pre_1900")] # creating periods
dtc[year >= 1900, period := factor("aft_1900")]
dtc[, dataset := factor('OWDA')]
dtc[, n_val := .N, .(cell_id, season, period)]

# cut dta to dtc because owda has smaller area
dta <- dta[cell_id %in% dtc$cell_id]
dtc <- dtc[dta, .( cell_id, year, season, precip, long, lat, period, n_val, dataset), on = .(cell_id == cell_id, year == year)]
dtc <- unique(dtc)

# merge owda data with ghcn_pauling
dta <- rbind(dta, dtc)

dta[, precip_scale := scale(precip), .(cell_id, season, dataset)] # scaling owda data separetly
saveRDS(dta, '../../data/other/norm_ghcn_paul_owda.rds')
