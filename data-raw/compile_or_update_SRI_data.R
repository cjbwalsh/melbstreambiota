#### Step through this script (takes ~ 30 min) every January to update SRI
#### with the previous year's daily discharge data in dl_dir (see below).
#### Once the script is run, upload the new sri48moW.rda to
#### https://osf.io/mcxrq/ (over-writing the existing version)

library(terra); library(dplyr)
# AWRA source data
url <- paste0("https://dapds00.nci.org.au/thredds/fileServer/iu04/",
              "australian-water-outlook/historical/v1/AWRALv7/")
# downloaded to:
dl_dir <- "~/uomShare/wergData/BoM runoff/qtot_AWRALv7/"

# # Originally looped through all years
# for(i in 1911:2022) {
# file_name_web <- paste(url, "qtot_", i, ".nc", sep = "")
# file_name_network <- paste(dl_dir, "qtot_", i, ".nc", sep = "")
# download.file(url = file_name_web, dest = file_name_network)
#   }

# # But to update with the previous year, run this
last_year <- lubridate::year(lubridate::today()) - 1
file_name_web <- paste(url, "qtot_", last_year, ".nc", sep = "")
file_name_network <- paste(dl_dir, "qtot_", last_year, ".nc", sep = "")
download.file(url = file_name_web, dest = file_name_network)

#list of the *.nc files
daily_grids <- list.files(dl_dir, pattern = "\\.nc$")
daily_grid_years <- as.numeric(substr(daily_grids,6,9))


# restrict calculations to post-1981
daily_grids <- daily_grids[daily_grid_years > 1980]
daily_grid_years <- daily_grid_years[daily_grid_years > 1980]

# Clip to MW region and create a stack of mean monthly runoff rasters.
subcs <- sf::st_read("~/uomShare/wergSpatial/MWRegion/Vectors/Catchments/DCI2017/MWregion_subcs_260117.shp")
subcs_4326 <- sf::st_transform(subcs, crs = 4326)
#This can also be downloaded as a gpkg file from osf: See compile_data_for_melbstreambiota_package.R
subcs$scarea <- as.numeric(sf::st_area(subcs))
subcs_ig <- igraph::graph_from_data_frame(subcs[c("subc","nextds")])
system.time({
  subcs$carea_km2 <- subcs$scarea*1e-6
  for(i in 1:nrow(subcs)){
    allusi <- subcs[igraph::subcomponent(subcs_ig, subcs$subc[i], "in"),1]$subc
    if(length(allusi) > 1)
    {
      subcs$carea_km2[i] <- sum(subcs$scarea[subcs$subc %in% allusi])*1e-6
    }
  }
}) # 40 s

## Problem subs
# [1] "MARI1"  "MARI3"  "MARI2"  "MARI4"  "MARI6"  "MARI16" "MARI9"  "MARI5"  "MARI21" "MARI8"  "MARI19"

# Old script reprojected the monthly raster (to crs 28355), but it is
# better to reproject the vector subc layer to match the raster.
subcs_4326 <- terra::vect(subcs_4326) # ~20 s
subcs_4326 <- terra::makeValid(subcs_4326) # ~20 s
mw_ext <- terra::buffer(terra::vect(terra::ext(subcs_4326)), 0.06)

system.time({
  for(i in 1:length(daily_grid_years)){
  xi <- suppressWarnings(terra::rast(paste0(dl_dir,"/",daily_grids[i])))
  xi <- suppressWarnings(terra::crop(xi,mw_ext))
  for(j in 1:12){
    if(!"mw_monthly_grid" %in% ls()){
        mw_monthly_grid <- terra::mean(xi[[lubridate::month(terra::time(xi)) == j]],
                                       na.rm = TRUE)
      }else{
        mw_monthly_grid <- c(mw_monthly_grid,
                             terra::mean(xi[[lubridate::month(terra::time(xi)) == j]],
                                         na.rm = TRUE))
      }
}
  }
}) # ~40 s

month_ts <- seq.Date(as.Date("1981-01-01"),
                     as.Date(paste0(last_year,"-12-01")),by = "months")
system.time({
x <- terra::extract(mw_monthly_grid, subcs_4326, weights = TRUE,
                    exact = TRUE, fun = mean, na.rm = TRUE)
})  # ~3 min

# x is a matrix with 16346 rows (subcs) and 469 cols (ID + 468 months)
x <- x[,-1]
# transpose so that subcs are columns, months are rows
awra_local_runoff_q_mm_d <- data.frame(date = month_ts, t(data.frame(x)))
colnames(awra_local_runoff_q_mm_d)[-1] <- c(subcs$subc)
row.names(awra_local_runoff_q_mm_d) <- 1:nrow(awra_local_runoff_q_mm_d)

# # Check sense of streamline map compared to raster for 8th month
first_month_data <- unname(unlist(as.vector(awra_local_runoff_q_mm_d[8,-1])))
col_bins_8 <- as.numeric(cut(first_month_data,
                             breaks = seq(0,max(first_month_data,na.rm = TRUE),length = 8)))
col <- RColorBrewer::brewer.pal(8,"Spectral")[col_bins_8]

par(mfrow = c(1,2), mar = c(0,0,0,0))
plot(melbstreambiota::mwstreams_map$geom, col = col[match(melbstreambiota::mwstreams_map$subc,
                                                          names(awra_local_runoff_q_mm_d)[-1])])
terra::plot(mw_monthly_grid[[8]])
# # Seems to match ok

#convert runoff depth in mm to runoff Q in ML/day by multiplying mm/day by m^2 then dividiing by 10^3 to go to m depth then 10^3
system.time({
awra_local_runoff_q_ML_d <- awra_local_runoff_q_mm_d
for(i in 2:ncol(awra_local_runoff_q_ML_d)){
awra_local_runoff_q_ML_d[,i] <- awra_local_runoff_q_mm_d[,i] * 1e-6 *
               subcs$scarea[subcs$subc == names(awra_local_runoff_q_ML_d)[i]]
}
}) # 11 s
# col_bins_8 <- as.numeric(cut(unname(unlist(log(awra_local_runoff_q_ML_d[1,-1] + 0.01))),
#                              breaks = seq(log(min(awra_local_runoff_q_ML_d[1,-1]) + 0.01),
#                                           log(max(awra_local_runoff_q_ML_d[1,-1]) + 0.01),length = 8)))
# col <- RColorBrewer::brewer.pal(8,"Spectral")[col_bins_8]
# par(mfrow = c(1,2), mar = c(0,0,0,0))
# plot(melbstreambiota::mwstreams_map$geom, col = col[match(melbstreambiota::mwstreams_map$subc,
#                                                           names(awra_local_runoff_q_ML_d)[-1])])
# plot(mw_monthly_grid_mga[[1]])
# # Seems to match ok

system.time({
awra_cat_runoff_q_ML_d <- awra_local_runoff_q_ML_d
for(i in 2:ncol(awra_cat_runoff_q_ML_d)){
  subci <- names(awra_cat_runoff_q_ML_d)[i]
  allusi <- subcs[igraph::subcomponent(subcs_ig, subci, "in"),1]$subc
  if(length(allusi) > 1)
  awra_cat_runoff_q_ML_d[,i] <- apply(awra_local_runoff_q_ML_d[allusi],1,FUN = sum)
}
}) # 90 s

meanqMLd_1980 <- apply(awra_cat_runoff_q_ML_d[,-(1)],2,FUN = mean)
col_bins_8 <- as.numeric(cut(unname(unlist(log(meanqMLd_1980))),
                             breaks = seq(log(min(meanqMLd_1980)),
                                          log(max(meanqMLd_1980)),length = 8)))
col <- RColorBrewer::brewer.pal(8,"Spectral")[col_bins_8]
par(mfrow = c(1,1))
plot(melbstreambiota::mwstreams_map$geom, col = col[match(melbstreambiota::mwstreams_map$subc, names(meanqMLd_1980))])
# Looks good

# Convert back to mm/d and reduce to just those subcs in mwstreams (i.e. with stream lines)
 # Trying without doing that

awra_cat_runoff_q_ML_d_str <- awra_cat_runoff_q_ML_d
# [,c(1,which(names(awra_cat_runoff_q_ML_d) %in% melbstreambiota::mwstreams$subc))]
awra_cat_runoff_q_mm_d_str <- awra_cat_runoff_q_ML_d_str
system.time({
  for(i in 2:ncol(awra_cat_runoff_q_mm_d_str)){
    if(sum(awra_cat_runoff_q_ML_d_str[,i]/
       subcs$carea_km2[subcs$subc == names(awra_cat_runoff_q_mm_d_str)[i]] > 500, na.rm = TRUE) > 0) stop()
awra_cat_runoff_q_mm_d_str[,i] <- awra_cat_runoff_q_ML_d_str[,i]/
  subcs$carea_km2[subcs$subc == names(awra_cat_runoff_q_mm_d_str)[i]]
}
  })  # 4 s

# # crosscheck that this makes sense with meanQ estimates...
# meanq_1980 <- apply(awra_cat_runoff_q_mm_d_str[,-(1)],2,FUN = mean)
# meanq_y_1980 <- meanq_1980 * 365.25
# col_bins_8 <- as.numeric(cut(unname(unlist(log(meanq_y_1980))),
#                              breaks = seq(log(min(meanq_y_1980)),
#                                           log(max(meanq_y_1980)),length = 8)))
# col <- RColorBrewer::brewer.pal(8,"Spectral")[col_bins_8]
# par(mfrow = c(1,1))
# plot(melbstreambiota::mwstreams_map$geom, col = col[match(melbstreambiota::mwstreams_map$subc, names(meanq_y_1980))])
# # # plot(mw_monthly_grid_mga[[1]])
# # # Seems to match ok
#
# par(mfrow = c(1,1))
# plot(meanq_y_1980,melbstreambiota::mwstreams$meanAnnQ_mm[match(names(meanq_y_1980),melbstreambiota::mwstreams$subc)])
# # Not perfect (as expected), but a pretty good match

#SPEI analysis.
#calculate the standardised runoff metric (SRI) using runoff depth and a 1 month window, with 1990-2020 as the reference period.
#Then take the 1-month values and calculate a 48 month window using a linear decay function.

#Do the initial 1-month window calculations
system.time({
x <- SPEI::spei(data = as.matrix(awra_cat_runoff_q_mm_d_str[,-1]),
           scale = 1, fit = 'ub-pwm', #without this fit argument a small percentage of results are -Inf
           kernel = list(type = "rectangular", shift=0))
}) # 26 min min

awra_cat_runoff_spei <- awra_cat_runoff_sri <- data.frame(date = awra_cat_runoff_q_mm_d_str$date,
                                                          x$fitted)
awra_cat_runoff_spei_bu <- awra_cat_runoff_spei

# Still a fair number of -Infs for no apparent reason.  I'm going to suggest giving them the value of
# the nearest upstream or downstream subc or the average of allupstream subcs if none nearby.
system.time({
for(i in (2:ncol(awra_cat_runoff_spei))){
  for(j in 1:nrow(awra_cat_runoff_spei)){
    if(awra_cat_runoff_spei[j,i] == -Inf) {
      all_ds <- as.vector(igraph::subcomponent(subcs_ig, names(awra_cat_runoff_spei)[i], "out"))
      all_ds <- subcs$subc[all_ds[-length(all_ds)]]
      all_ds_spei <- as.vector(awra_cat_runoff_spei[j,match(all_ds,names(awra_cat_runoff_spei))])
      if(sum(all_ds_spei != -Inf) > 0){
      ds_non_inf <- min(which(all_ds_spei != -Inf))
      awra_cat_runoff_spei[j,i] <- all_ds_spei[[ds_non_inf]]
      }
      next_us <- subcs$subc[subcs$nextds == names(awra_cat_runoff_spei)[i]]
      next_us_spei <- as.vector(awra_cat_runoff_spei[j,match(next_us,names(awra_cat_runoff_spei))])
      if(ds_non_inf > 1){
        if(length(next_us) > 0){
          if(sum(next_us_spei != -Inf) > 0){
          awra_cat_runoff_spei[j,i] <- next_us_spei[[min(which(next_us_spei != -Inf))]]
          }
        }
      }
      if(length(next_us_spei) > 0 & length(next_us_spei) > 0){
      if(sum(next_us_spei != -Inf) == 0 & sum(next_us_spei != -Inf) == 0){
        allusi <- as.vector(igraph::subcomponent(subcs_ig, "WERR1753", "in"))
        allusi <- subcs$subc[allusi[-length(allusi)]]
        allusi_spei <-unlist(awra_cat_runoff_spei[j,match(allusi,names(awra_cat_runoff_spei))])
        awra_cat_runoff_spei[j,i] <- mean(allusi_spei[allusi_spei != -Inf])
      }
        }
    }
  }
}
}) # 8 min

system.time({
for(i in 2:ncol(awra_cat_runoff_sri)){
  awra_cat_runoff_sri[,i] <- TTR::WMA(awra_cat_runoff_spei[,i], n = 48, wts = 1:48)
}
}) # 5 s

#convert to long format (remove first 47 months, for which sri48wgthd is NA)
awra_cat_runoff_q_mm_d_long <- tidyr::gather(awra_cat_runoff_q_mm_d_str[-(1:47),], subc, q_mm_d,
                                             MARI1:MARI27, factor_key=TRUE)
awra_cat_runoff_spei_long <- tidyr::gather(awra_cat_runoff_spei[-(1:47),], subc, spei,
                                             MARI1:MARI27, factor_key=TRUE)
awra_cat_runoff_sri_long <- tidyr::gather(awra_cat_runoff_sri[-(1:47),], subc, sri48wghtd,
                                           MARI1:MARI27, factor_key=TRUE)

awra_cat <- cbind(awra_cat_runoff_q_mm_d_long,
                  spei = awra_cat_runoff_spei_long$spei,
                  sri48wghtd = awra_cat_runoff_sri_long$sri48wghtd,
                  year = lubridate::year(awra_cat_runoff_q_mm_d_long$date),
                  month = lubridate::month(awra_cat_runoff_q_mm_d_long$date))


# sri_1 <- melbstreambiota::sri48moW
# sri_1 <- sri_1[match(paste0(awra_cat$subc, awra_cat$date),paste0(sri_1$subc, sri_1$date)),]
# the version of sri48moW in melbstreambiota was a very poor match for the version of the variable
# used to build the fish and platypus models.
# It is a better match for the version used by Yung for model development
# sri_2 <- as.data.frame(get(load("~/uomShare/wergSpatial/MWRegion/RData/Catchments/SRI_48_weighted.RData")))
# names(sri_2)[match(c("SITE","Date","SRI","SRI_48_triang"),names(sri_2))] <- c("subc","date","sri","sri_48_triang")
# sri_2 <- sri_2[match(paste0(awra_cat$subc, awra_cat$date),paste0(sri_2$subc, sri_2$date)),]
# sri_2 <- sri_2[match(paste0(sri_1$subc, sri_1 $date),paste0(sri_2$subc, sri_2$date)),]
# ss <- sample(1:nrow(sri_2),1000)
# par(mar = c(4,4,1,1), mfrow = c(1,1))
# plot(sri_1$SRI_48mth_weighted[ss], sri_2$sri_48_triang[ss])

# in July 2023, check that the new version of AWRA-L is a good match for the earlier version used in March
sri_3 <- get(load("~/uomShare/wergStaff/ChrisW/git-data/melbstreambiota/sri48moW_version5_April2023.rda"))

awra_cat_match <- awra_cat[match(paste0(sri_3$subc, sri_3$date),paste0(awra_cat$subc, awra_cat$date)),]
ss <- sample(1:length(awra_cat_match$spei),1000)
par(mar = c(4,4,1,1), mfrow = c(1,2))
plot(sri_3$SRI_48mth_weighted[ss], awra_cat_match$spei[ss])  #sri48wghtd
abline(0,1)
plot(sri_3$SRI_48mth_weighted[ss], awra_cat_match$sri[ss])  #sri48wghtd
abline(0,1)

# A strong match: therefore awra_cat to be used for new version of melbstreambiota

#Keep record of each recalculation
save(awra_cat, file = paste0("~/uomShare/wergStaff/ChrisW/git-data/melbstreambiota/awra_cat_",lubridate::today(),".rda"), compress = "xz")

#Save new version of sri48moW for uploading to OSF
awra_cat$SRI_48mth_weighted <- awra_cat$sri48wghtd
sri48moW <- awra_cat[,c("subc","date","SRI_48mth_weighted")]
save(sri48moW, file = "~/uomShare/wergStaff/ChrisW/git-data/melbstreambiota/sri48moW.rda", compress = "xz")
