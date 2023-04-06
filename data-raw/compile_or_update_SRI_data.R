#### Run this script (takes ~ 30 min) every time new daily AWRA grids are updated
#### in dl_dir (see below).  Request the new years daily grid each January,
#### run the script and upload the new sri48moW.rda to https://osf.io/mcxrq/
#### (over-writing the existing version)

library(terra); library(dplyr)
#Functions to mw stream network
source("https://tools.thewerg.unimelb.edu.au/documents/mwstr/mwstr_functions.R")

#R Code to compile BOM gridded runoff data for each MW sub-catchment
#AUSTRALIAN LANDSCAPE WATER BALANCE AWRA-L MODEL DATA
# Matthew J Burns, Jan-2021, adapted by Chris Walsh for melbstreambiota package 03-04-2023

#Runoff (qtot) in mm
#The file format of the data is netCDF-4
#It is a platform-independent file format for storing scientific data and is commonly used for climate data
#Each file is one year of daily runoff data in mm, across Australia (5 km * 5 km grid)
#The files were downloaded via FTP in terminal - see word document for details
#As the ultimate goal here is to calculate mean annual runoff from this data, I first used CDO tools (https://code.mpimet.mpg.de/projects/cdo/)
#To aggregate the daily grids to annual runs. Doing so reduced file size from ~700 mb to ~4 mb

# Download directory
dl_dir <- "~/uomShare/wergData/BoM runoff/GRIDS/ftp.bom.gov.au/register/bom868/outgoing/MatthewJamesBurns"

#list of the *.nc files (the larger files for all of Australia)
daily_grids <- list.files(dl_dir, pattern = "\\.nc$")
daily_grid_years <- as.numeric(substr(daily_grids,6,9))

# restrict calculations to 1981 to 2019 (2020 stack incomplete)
daily_grids <- daily_grids[daily_grid_years > 1980 & daily_grid_years < 2020]
daily_grid_years <- daily_grid_years[daily_grid_years > 1980 & daily_grid_years < 2020]

# Clip to MW region and create a stack of mean monthly runoff rasters.
mwsubcs_sf <- sf::st_read("/servers/home/cwalsh/uomShare/wergSpatial/MWRegion/Vectors/Catchments/DCI2017/MWregion_subcs_260117.shp")
#This can also be downloaded as a gpkg file from osf: See compile_data_for_melbstreambiota_package.R
mwsubcs_sf$scarea <- as.numeric(sf::st_area(mwsubcs_sf))
#use the list of upstream subcs in melbstreambiota to accumulate scarea to catchment area
allus_list <- get(load("/servers/home/cwalsh/uomShare/wergSpatial/MWRegion/Vectors/Catchments/DCI2017/subcs.allus2017.RData"))
system.time({
  mwsubcs_sf$carea_km2 <- mwsubcs_sf$scarea*1e-6
  for(i in 1:nrow(mwsubcs_sf)){
    if(length(allus_list[[mwsubcs_sf$subc[i]]]) > 1)
      {
      mwsubcs_sf$carea_km2[i] <- sum(mwsubcs_sf$scarea[mwsubcs_sf$subc %in% allus_list[[mwsubcs_sf$subc[i]]]])*1e-6
    }
  }
})
mwstreams_map <- melbstreambiota::mwstreams_map
mwstreams_map$carea_km2 <- mwsubcs_sf$carea_km2[match(mwstreams_map$subc,mwsubcs_sf$subc)]
# # Check carea_km2 calculation
# col_bins_8 <- as.numeric(cut(log(mwstreams_map$carea_km2),breaks = seq(log(min(mwstreams_map$carea_km2)),
#                                                                   log(max(mwstreams_map$carea_km2)),length = 8)))
# col <- RColorBrewer::brewer.pal(8,"Spectral")[col_bins_8]
# plot(mwstreams_map$geom, col = col)

mwsubcs_sf_ll <- sf::st_transform(mwsubcs_sf, crs = 4326)
# Extend this extent a little to make sure everything will be captured
mw_bbox <- st_as_sfc(st_bbox(c(xmin = 143.975, xmax = 146.225, ymax = -36.975, ymin = -38.575), crs = st_crs(4326)))
mw_bbox <- terra::vect(mw_bbox)

system.time({
  for(i in 1:length(daily_grid_years)){
    # The following generates this warning which I think can be ignored
    # `GDAL Message 1: No UNIDATA NC_GLOBAL:Conventions attribute`
    xi <- suppressWarnings(terra::rast(paste0(dl_dir,"/",daily_grids[i])))
    xi <- suppressWarnings(terra::crop(xi,mw_bbox))
    daily_ts <- seq.Date(as.Date(paste0(daily_grid_years[1],"-01-01")),as.Date(paste0(daily_grid_years[1],"-12-31")), by = "days")
    month_ts <- lubridate::month(daily_ts)
    for(j in 1:12){
      if(!"mw_monthly_grid" %in% ls()){
        mw_monthly_grid <- mean(xi[[month_ts == j]], na.rm = TRUE)
      }else{
        mw_monthly_grid <- c(mw_monthly_grid,
                             mean(xi[[month_ts == j]], na.rm = TRUE))
      }
    }
  }
})  # 26 s

#Reproject the stacks to zone 55, need to use nearest neighbor
mw_monthly_grid_mga <- terra::project(mw_monthly_grid, "epsg:28355", method = "bilinear")
system.time({
  mwsubcs_t <- terra::vect(mwsubcs_sf)
})  #17 s

month_ts <- seq.Date(as.Date("1981-01-01"),as.Date("2019-12-01"),by = "months")
system.time({
x <- terra::extract(mw_monthly_grid_mga,mwsubcs_t, weights = TRUE, fun = mean, na.rm = TRUE)
})  # 50 s
# x is a matrix with 16346 rows (subcs) and 469 cols (ID + 468 months)
x <- x[,-1]
# transpose so that subcs are columns, months are rows
awra_local_runoff_q_mm_d <- data.frame(date = month_ts, t(data.frame(x)))
colnames(awra_local_runoff_q_mm_d)[-1] <- c(mwsubcs_sf$subc)
row.names(awra_local_runoff_q_mm_d) <- 1:nrow(awra_local_runoff_q_mm_d)

# # Check sense of streamline map compared to raster for first month
col_bins_8 <- as.numeric(cut(unname(unlist(as.vector((awra_local_runoff_q_mm_d[1,-1])))),
                             breaks = seq(0,max(awra_local_runoff_q_mm_d[1,-1]),length = 8)))
col <- RColorBrewer::brewer.pal(8,"Spectral")[col_bins_8]
par(mfrow = c(1,2), mar = c(0,0,0,0))
plot(melbstreambiota::mwstreams_map$geom, col = col[match(melbstreambiota::mwstreams_map$subc,
                                                          names(awra_local_runoff_q_mm_d)[-1])])
plot(mw_monthly_grid_mga[[1]])
# # Seems to match ok

#convert runoff depth in mm to runoff Q in ML/day by multiplying mm/day by m^2 then dividiing by 10^3 to go to m depth then 10^3
system.time({
awra_local_runoff_q_ML_d <- awra_local_runoff_q_mm_d
for(i in 2:ncol(awra_local_runoff_q_ML_d)){
awra_local_runoff_q_ML_d[,i] <- awra_local_runoff_q_mm_d[,i] * 1e-6 *
               mwsubcs_sf$scarea[mwsubcs_sf$subc == names(awra_local_runoff_q_ML_d)[i]]
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
  allusi <- allus_list[[subci]]
  if(length(allusi) > 1)
  awra_cat_runoff_q_ML_d[,i] <- apply(awra_local_runoff_q_ML_d[allusi],1,FUN = sum)
}
}) # 80 s
meanqMLd_1980 <- apply(awra_cat_runoff_q_ML_d[,-(1)],2,FUN = mean)
# col_bins_8 <- as.numeric(cut(unname(unlist(log(meanqMLd_1980))),
#                              breaks = seq(log(min(meanqMLd_1980)),
#                                           log(max(meanqMLd_1980)),length = 8)))
# col <- RColorBrewer::brewer.pal(8,"Spectral")[col_bins_8]
# par(mfrow = c(1,1))
# plot(melbstreambiota::mwstreams_map$geom, col = col[match(melbstreambiota::mwstreams_map$subc, names(meanqMLd_1980))])
# # Looks good

# Convert back to mm/d and reduce to just those subcs in mwstreams (i.e. with stream lines)

awra_cat_runoff_q_ML_d_str <- awra_cat_runoff_q_ML_d[,c(1,which(names(awra_cat_runoff_q_ML_d) %in%
                                                   melbstreambiota::mwstreams$subc))]
awra_cat_runoff_q_mm_d_str <- awra_cat_runoff_q_ML_d_str
system.time({
  for(i in 2:ncol(awra_cat_runoff_q_mm_d_str)){
    if(sum(awra_cat_runoff_q_ML_d_str[,i]/
       mwsubcs_sf$carea_km2[mwsubcs_sf$subc == names(awra_cat_runoff_q_mm_d_str)[i]] > 500) > 0) stop()
awra_cat_runoff_q_mm_d_str[,i] <- awra_cat_runoff_q_ML_d_str[,i]/
  mwsubcs_sf$carea_km2[mwsubcs_sf$subc == names(awra_cat_runoff_q_mm_d_str)[i]]
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
           scale = 1,
           kernel = list(type = "rectangular", shift=0))
}) # 17 min

awra_cat_runoff_spei <- awra_cat_runoff_sri <- data.frame(date = awra_cat_runoff_q_mm_d_str$date,
                                                          x$fitted)

system.time({
for(i in 2:ncol(awra_cat_runoff_sri)){
  awra_cat_runoff_sri[,i] <- TTR::WMA(awra_cat_runoff_spei[,i], n = 48, wts = 1:48)
}
}) # 6 s

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


sri_1 <- melbstreambiota::sri48moW
sri_1 <- sri_1[match(paste0(awra_cat$subc, awra_cat$date),paste0(sri_1$subc, sri_1$date)),]
# the version of sri48moW in melbstreambiota was a very poor match for the version of the variable
# used to build the fish and platypus models.
# It is a better match for the version used by Yung for model development
sri_2 <- as.data.frame(get(load("~/uomShare/wergSpatial/MWRegion/RData/Catchments/SRI_48_weighted.RData")))
names(sri_2)[match(c("SITE","Date","SRI","SRI_48_triang"),names(sri_2))] <- c("subc","date","sri","sri_48_triang")
sri_2 <- sri_2[match(paste0(awra_cat$subc, awra_cat$date),paste0(sri_2$subc, sri_2$date)),]
sri_2 <- sri_2[match(paste0(sri_1$subc, sri_1$date),paste0(sri_2$subc, sri_2$date)),]
ss <- sample(1:nrow(sri_2),1000)
# par(mar = c(4,4,1,1), mfrow = c(1,1))
# plot(sri_1$SRI_48mth_weighted[ss], sri_2$sri_48_triang[ss])

awra_cat_match <- awra_cat[match(paste0(sri_2$subc, sri_2$date),paste0(awra_cat$subc, awra_cat$date)),]
ss <- sample(1:length(awra_cat_match$spei),1000)
# par(mar = c(4,4,1,1), mfrow = c(1,2))
# plot(sri_2$sri_48_triang[ss], awra_cat_match$spei[ss])  #sri48wghtd
# abline(0,1)
# plot(sri_2$sri_48_triang[ss], awra_cat_match$sri[ss])  #sri48wghtd
# abline(0,1)

# Therefore awra_cat to be used for new version of melbstreambiota

#Keep record of each recalculation
save(awra_cat, file = paste0("~/uomShare/wergStaff/ChrisW/git-data/melbstreambiota/awra_cat_",today(),".rda"), compress = "xz")

#Save new version of sri48moW for uploading to OSF
awra_cat$SRI_48mth_weighted <- awra_cat$sri48wghtd
sri48moW <- awra_cat[,c("subc","date","SRI_48mth_weighted")]
save(sri48moW, file = "~/uomShare/wergStaff/ChrisW/git-data/melbstreambiota/sri48moW.rda", compress = "xz")
