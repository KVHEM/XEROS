Scripts for the database of XEROS project

import_luterbacher.R :  download, clean and save in rds format Luterbacher-Xoplaki gridded T reconstruction.
                        years: 1500-1900
                        T: seasonal values (°C)

import_pauling.R :  rearrange and save .nc file in rds format gridded P reconstruction.
                    years: 1500-2000
                    P: (mm/season)


import_cru_ts.R : rearrange and save CRU TS 4.01 version of gridded P, T and PET .nc files in rds format.
                  years: 1901-2015
                  PET: monthly values (mm/day) values were averaged for the season in (mm/day)
                  P: monthly values (mm/month) values were sumed for the season in (mm/season)
                  T: near surface averaged monthly temperature (°C) were averaged for the season
                  
import_pages.R : Import PAGES2k v2.0.0 Temperature reconstruction for Europe and transform from list to tidy format.
                 Create meta file with the reconstruction properties. 