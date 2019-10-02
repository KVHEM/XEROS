# Creates directory structure for first time and installs all the packages needed for the project
proj_packages <- c("tidyverse", "data.table")

if (length(setdiff(proj_packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(proj_packages, rownames(installed.packages())))  
}

dir.create('./data')
dir.create('./data/input')
dir.create('./data/input/gridded')
dir.create('./data/input/point')
