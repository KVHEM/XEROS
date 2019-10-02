# Creates directory structure for first time and installs all the packages needed for the project
proj_packages <- c("tidyverse", "data.table", "ncdf4", "raster")

if (length(setdiff(proj_packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(proj_packages, rownames(installed.packages())))  
}

dir.create('./data')
dir.create('./results')
dir.create('./results/database')
dir.create('./data/input')
dir.create('./data/input/gridded')
dir.create('./data/input/point')
dir.create('./data/input/used_in_models')
dir.create('./data/input/used_in_models/mhm')
