library(data.table)

#-----Extract grid cells--------
pauling <- readRDS('../../data/input/gridded/pauling/pauling.rds')
grid_cells <- unique(pauling[, .(cell_id, lat, long)])
grid_bounds <- grid_cells[, .(cell_id, 
                   lat_u = lat + 0.25, lat_l = lat - 0.25,
                   long_u = long + 0.25, long_l = long - 0.25)]
save(grid_cells, grid_bounds, file = '../../data/geodata/grid_cells.rdata')

