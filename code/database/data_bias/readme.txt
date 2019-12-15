quantify_bias.R:    Picks Pauling gridded data that matches the GHNC stations and uses at least 20 years long time series in order to create ghnc_pauling.rds dataset.

merge_datasets.R :  Merges and normalizes GHCN, Pauling and OWDA for comparison

shiny_bias.R:       Shiny app that depicts empirical cumulative distribution function (ecdf) of GHNC and Pauling dataset and comparision of GHNC and Pauling dataset divided by pre 1900 and after 1900. Last figure depicts only Pauling timeseries from 1500 to 2000 in chosen station.
                    
shiny_bias_norm.R:  Shiny app that depicts ecdf of GHNC, Pauling and OWDA dataset divided by pre 1900 and after 1900. Tyalor diagram that compares GHCN and Pauling (red) and GHCN and OWDA (blue). Last figure depicts scaled precipitation from OWDA, Pauling and GHNC dataset.

map_var_bias.R:  Computes variation of Pauling precipitation data between periods 1500 - 1600 and 1900 - 2000 in every grid point. Than is computed ratio between these two variations that is plotted into a map.

map_cor_bias.R: Computes cross-correlation between luterbacher and CRU temperature data and pauling and CRU precipitation data. Both time series are between 1900-2000.

