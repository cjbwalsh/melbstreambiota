
delayedAssign("mwstreams_map", local({
  try(
    sf::read_sf(
      system.file("extdata/mwstream_map.gpkg", package = "melbstreambiota"),
      layer = "streams"
    ),
    silent = TRUE
  )
}))

delayedAssign("mwcoast_map", local({
  try(
    sf::read_sf(
      system.file("extdata/mwstream_map.gpkg", package = "melbstreambiota"),
      layer = "coast"
    ),
    silent = TRUE
  )
}))

x <- hoardr::hoard()
invisible(x$cache_path_set("melbstreambiota", type = 'user_data_dir'))
x$mkdir()
  dl_files <- osfr::osf_ls_files(osfr::osf_retrieve_node("mcxrq"))
  dl_files <- dl_files[dl_files$name %in%
                         c("bestModelsVerts.rda","bestModelsBugfams.rda",
                           "bestModelsSIGNAL.rda","CIs.rda","sri48moW.rda"),]
  invisible(osfr::osf_download(dl_files, path = x$cache_path_get(),
                               conflicts = "skip"))

delayedAssign("bestModelsVerts", local({
  try(get(load(paste0(x$cache_path_get(),"/bestModelsVerts.rda"))),silent = TRUE)
}))

delayedAssign("bestModelsBugfams", local({
  try(get(load(paste0(x$cache_path_get(),"/bestModelsBugfams.rda"))),silent = TRUE)
}))

delayedAssign("bestModelsSIGNAL", local({
  try(get(load(paste0(x$cache_path_get(),"/bestModelsSIGNAL.rda"))),silent = TRUE)
}))

delayedAssign("CIs", local({
  try(get(load(paste0(x$cache_path_get(),"/CIs.rda"))),silent = TRUE)
}))

delayedAssign("sri48moW", local({
  try(get(load(paste0(x$cache_path_get(),"/sri48moW.rda"))),silent = TRUE)
}))

