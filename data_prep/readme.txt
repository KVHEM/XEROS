Scripts for the database of XEROS project

import_luterbacher.R :  Downloads, cleans and saves in rds format Luterbacher-Xoplaki gridded T 
                        reconstructoon. 
                        years: 1500-1900
                        T: seasonal values (°C)

import_pauling.R :  Rearranges and saves .nc file in rds format gridded P reconstruction.
                    years: 1500-2000
                    P: (mm/season)


import_cru_ts.R : Rearranges and saves CRU TS 4.01 version of gridded P, T and PET .nc files in rds format.
                  years: 1901-2015
                  PET: monthly values (mm/day) values were averaged for the season in (mm/day)
                  P: monthly values (mm/month) values were sumed for the season in (mm/season)
                  T: near surface averaged monthly temperature (°C) were averaged for the season
                  
import_pages.R : Imports PAGES2k v2.0.0 temperature reconstruction and transform it from list to tidy format. Creates metadata file for all station called pages2k_meta and metadata file called pages2k_meta_id that is only for stations in europe that are conected to grid cell by id.

tavg_merge.R :  Merged luterbacher and cru_ts temperature data that are cut in same area. Merged data creates continous time serie from 1500 to 2015.  

import_ghcn_p.R : It downloads daily and monthly precipitation data from europian stations. Daily data contains at least 130 years back and monthly 70 years between 1800-1900. All stations data were merged and saved into daily and monthly RDS files and daily and monthly metadata file

import_ljungvist_p.R :  It downloads ljungvist hydroclimate data from database and merge them. Data were saved into RDS file and metadata file. Stations from europe were picked from metadata, conected to grid cell ids and saved separetly into eu metadata.

import_ljungvist_T.R :  It downloads ljungvist temperature data from database and merge them. Data were saved into RDS file and metadata file. Stations from europe were picked from metadata, conected to grid cell ids and saved separetly into eu metadata. 

timescale.R : Creates averaged values in 5 and 10 years time periods of pages, luterbacher, owda and ljungvist datasets. 
