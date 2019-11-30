source('../../main.R')

acf.1 = function(x, ...) {
  tryCatch(acf(x[!is.na(x)], plot = FALSE, ...)$acf[2], error = function(e) NA)
}

ghcn <- readRDS('../../data/input/point/ghcn/ghcn_seas_p.rds')
load(file = '../../data/input/point/ghcn/ghcn_meta_seas.rdata')
pauling <- data.table(readRDS('../../data/input/gridded/pauling/pauling.rds'))

#----------Merge Datasets------------------------------

pauling <- pauling[cell_id %in% ghcn_meta_seas$cell_id] 
ghcn <- ghcn_meta_seas[ghcn, on = "id"]
ghcn <- ghcn[, .(cell_id, year = as.numeric(year), season, p_obs = precip)]
colnames(pauling)[2] <- "p_rec"
dta <- ghcn[pauling, on = c("cell_id", "year", "season")]
dtb <- dta[complete.cases(dta)]

#----------Extra variables for comparison and tidy format--------------
dtb <- unique(dtb)
dtb[year < 1900, period := factor("pre_1900")]
dtb[year >= 1900, period := factor("aft_1900")]
dtb[, n_val := .N, .(cell_id, season, period)]

dtc <- dtb[n_val >= 20] #at least 20 years (values) per time series
dtca <- dtc[, !"p_obs", with = FALSE]  
dtcb <- dtc[, !"p_rec", with = FALSE]  
setnames(dtca, "p_rec", "precip")
setnames(dtcb, "p_obs", "precip")
dtca$dataset <- factor("Pauling")
dtcb$dataset <- factor("GHCN")
dtd <- rbind(dtca, dtcb)
saveRDS(dtd, file = "../../data/output/database/ghcn_pauling.rds")

#----------Stats---------------------------------------
ghcn_stat <- dtd[, 
                .(m = mean(precip), 
                  sd = sd(precip), 
                  acf_1 = acf.1(precip)), 
                .(cell_id, season, n_val, period, dataset)]

ggplot(ghcn_stat[period == "aft_1900"], aes(x = m, fill = dataset)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~season) +
  theme_bw()
ggplot(ghcn_stat[period == "pre_1900"], aes(x = m, fill = dataset)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~season) +
  theme_bw()

ggplot(ghcn_stat[period == "aft_1900"], aes(x = sd, fill = dataset)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~season) +
  theme_bw()
ggplot(ghcn_stat[period == "pre_1900"], aes(x = sd, fill = dataset)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~season) +
  theme_bw()

to_plot <- dtd[cell_id == 8632]
ggplot(to_plot[period == "aft_1900"], aes(x = precip, fill = dataset)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~season) +
  theme_bw()
ggplot(to_plot[period == "pre_1900"], aes(x = precip, fill = dataset)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~season) +
  theme_bw()





