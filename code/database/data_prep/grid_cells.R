source('./code/main.R')

#-----Extract grid cells--------
pauling <- readRDS('./data/input/gridded/pauling/pauling.rds')
grid_cells <- unique(pauling[, .(cell_id, lat, lon)])
grid_bounds <- grid_cells[, .(cell_id, 
                   lat_u = lat + 0.25, lat_l = lat - 0.25,
                   lon_u = lon + 0.25, lon_l = lon - 0.25)]

saveRDS(grid_bounds, file = './data/geodata/grid_cells.rds')

