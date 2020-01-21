# Creates directory structure for first time and installs all the packages needed for the project
proj_packages <- c("tidyverse", "data.table", "ncdf4", "raster", "leaflet",
                   "rvest", "shiny")

if (length(setdiff(proj_packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(proj_packages, rownames(installed.packages())))  
}

dir.create('./results/database', recursive = TRUE)
dir.create('./data/geodata', recursive = TRUE)
dir.create('./data/input/gridded/cru_ts4.01', recursive = TRUE)
dir.create('./data/input/gridded/luterbacher/raw', recursive = TRUE)
dir.create('./data/input/gridded/owda')
dir.create('./data/input/gridded/pauling')
dir.create('./data/input/point/ghcn/raw', recursive = TRUE)
dir.create('./data/input/point/ghcn/raw/precip_d')
dir.create('./data/input/point/ghcn/raw/precip_m')
dir.create('./data/input/point/ljungqvist_p/raw', recursive = TRUE)
dir.create('./data/input/point/ljungqvist_t/raw', recursive = TRUE)
dir.create('./data/input/point/pages2k', recursive = TRUE)
dir.create('./data/input/used_in_models/mhm', recursive = TRUE)
dir.create('./data/other/grdb/raw', recursive = TRUE)
dir.create('./data/output/database', recursive = TRUE)

