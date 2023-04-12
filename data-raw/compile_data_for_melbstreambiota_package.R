# revision of melbstreambiota 28 mar 2023
library(melbstreambiota)
mwStreamsMap <- sf::st_as_sf(mwStreamsMap)
mwstreams_sf <- sf::st_read("/servers/home/cwalsh/uomShare/wergSpatial/MWRegion/Vectors/DrainsStreams/DCI2017/MWregion_streams_310117.shp")
mwsubcs_sf <- sf::st_read("/servers/home/cwalsh/uomShare/wergSpatial/MWRegion/Vectors/Catchments/DCI2017/MWregion_subcs_260117.shp")
mwcoast_sf <- sf::st_read("/servers/home/cwalsh/uomShare/wergSpatial/MWRegion/Vectors/Lakesetc/Melb coast9455_polyline.shp")
# Create gpkg to be placed in melbstreambiota/inst/extdata
sf::st_write(mwstreams_sf, "~/Documents/git/melbstreambiota/inst/extdata/mwstream_map.gpkg",
             layer = "streams")
sf::st_write(mwcoast_sf, "~/Documents/git/melbstreambiota/inst/extdata/mwstream_map.gpkg",
             layer = "coast", append = TRUE)
# Create gpkg to be placed in melbstreambiota OSF repository (that includes subcatchments as well)
mwsubcs_sf <- mwsubcs_sf[c("subc","nextds","PPd2ol_m","geometry")]
names(mwsubcs_sf)[3] <- "pp_d2ol_km"
mwsubcs_sf$pp_d2ol_km <- round(mwsubcs_sf$pp_d2ol_km/1000, 2)
sf::st_write(mwstreams_sf, "~/temp/mwstreams_subcs_coast.gpkg",
             layer = "streams")
sf::st_write(mwsubcs_sf, "~/temp/mwstreams_subcs_coast.gpkg",
             layer = "subcs", append = TRUE)
sf::st_write(mwcoast_sf, "~/temp/mwstreams_subcs_coast.gpkg",
             layer = "coast", append = TRUE)

# Replacement of old fish and platypus models with ones used in publications

# Reduce the number of fish species modelled to 11 native species.
vertSpp <- vertSpp[vertSpp$vertcode %in% c(toupper(c("anguaust", "gadomarm", "galabrev",
                "galamacu", "galaolid", "galatrut","MORDMORD", "GEOTAUST","nannaust", "philgran",
                "pseuurvi", "retrsemo")),"femPlaty","allPlaty"),]
vertSpp$vertcode[match(c("MORDMORD", "GEOTAUST","GALAOLID"),vertSpp$vertcode)] <- c("LAMPREYS","LAMPREYS","GALAORN")
vertSpp <- rbind(vertSpp, vertSpp[nrow(vertSpp),])
vertSpp$vertcode[vertSpp$vertcode == "allPlaty"] <- c("allPlatyHWS","allPlatyColeman")

# Replace model objects in sysdata.rda
load("/servers/home/cwalsh/Documents/git/melbstreambiota/sysdata.rda")

new_fish_model_dir <- "~/uomShare/wergStaff/Yung En Chee/Fish-Platy-BRTs-Nov2017"
contents <- dir(new_fish_model_dir)

for(i in c(1:7,9:12)){
assign(vertSpp$vertcode[i], get(load(paste0(new_fish_model_dir,"/",
                                            contents[grep(vertSpp$vertcode[i],contents)]))))
}
save(vertSpp, file = "data/vertSpp.rda", compress = "xz")

femPlaty <- get(load("~/uomShare/wergStaff/Yung En Chee/Fish-Platy-BRTs-Nov2017/FemPlatyPA.CArea+1.AirTm-1.annQ+1.AI-1.AFB+1.SRI+1.Rdata"))
allPlatyHWS <- get(load("~/uomShare/wergStaff/Yung En Chee/Fish-Platy-BRTs-Nov2017/AllPlatyPA.CArea0.AirTm-1.annQ+1.CI0.AI-1.AFB+1.SRI+1.Rdata"))
allPlatyColeman <- get(load("~/uomShare/wergStaff/Rhys/Platypus/BRT-model-obj-paper/Oct21/AllPlatyPA.CArea0.AirTm-1.annQ+1.AI-1.AFB+1.SRI+1.Rdata"))

bestModelsVerts <- list(ANGUAUST = ANGUAUST, GADOMARM = GADOMARM, GALABREV = GALABREV,
                        GALAMACU = GALAMACU,GALAORN = GALAORN, GALATRUT = GALATRUT,
                        LAMPREYS = LAMPREYS, NANNAUST = NANNAUST, PHILGRAN = PHILGRAN,
                        PSEUURVI = PSEUURVI, RETRSEMO = RETRSEMO, femPlaty = femPlaty,
                        allPlatyHWS = allPlatyHWS, allPlatyColeman = allPlatyColeman)
# save(bestModelsBugfams,bestModelsSIGNAL,bestModelsVerts,CIs, file = "sysdata.rda", compress = "xz")

save(bestModelsBugfams, file = "~/uomShare/wergStaff/ChrisW/git-data/melbstreambiota/bestModelsBugfams.rda", compress = "xz")
save(bestModelsSIGNAL, file = "~/uomShare/wergStaff/ChrisW/git-data/melbstreambiota/bestModelsSIGNAL.rda", compress = "xz")
save(bestModelsVerts, file = "~/uomShare/wergStaff/ChrisW/git-data/melbstreambiota/bestModelsVerts.rda", compress = "xz")
save(CIs, file = "~/uomShare/wergStaff/ChrisW/git-data/melbstreambiota/CIs.rda", compress = "xz")

fullBarrs <- read.csv("~/uomShare/wergSpatial/MWRegion/Vectors/Barriers/BarrierDsCalcs/full_barriers_ds_scens7Dec17.csv", stringsAsFactors = F)
partBarrs <- read.csv("~/uomShare/wergSpatial/MWRegion/Vectors/Barriers/BarrierDsCalcs/part_barriers_ds_scens7Dec17.csv", stringsAsFactors = F)
rev_partBarrs_names <- gsub("PARTBARSUM.","nPartBarriersDS_",names(partBarrs))
rev_fullBarrs_names <- gsub("FULLBARSUM.","nFullBarriersDS_",names(fullBarrs))
# par(mfrow = c(3,4))
 # for(i in 2:14){
 #   plot(partBarrs[match(as.vector(mwstreams$subc),partBarrs$site),i],mwstreams[,rev_partBarrs_names[i]])
 # }  #There are slight differences in the new data, so they should replace the old...
for(i in 2:14){
  mwstreams[,rev_partBarrs_names[i]] <- partBarrs[match(as.vector(mwstreams$subc),partBarrs$site),i]
}
# par(mfrow = c(3,4))
#  for(i in 2:7){
#    plot(fullBarrs[match(as.vector(mwstreams$subc),fullBarrs$site),i],mwstreams[,rev_fullBarrs_names[i]])
#  }  #These are identical and do not need to be replaced
save(mwstreams, file="data/mwstreams.rda", compress = "xz")



#sources of datafiles for melbstreambiota package (Original version)
#1. bugfams, derived here.
bugfams <- read.csv("~/uomShare/wergMonitoringData/EcologicalMonitoring/InvertebrateComposition/OriginalMelbourneBugsdb/mdb_SIGNAL grades.csv")
names(bugfams)[match(c("shortcode","Family","Field1"),names(bugfams))] <- c("bugcode","family","note")
save(bugfams, file = "~/wergdrobo_Staff/ChrisW/newBugModelsMay2017/dataForMelbstreambiotaPackage/bugfams.rda",
     compress = "gzip")
save(bugfams, file = "~/Dropbox/Rstudio_projects/lumar2Development/dataForMelbstreambiotaPackage/bugfams.rda",
     compress = "gzip")

#2. taxon.classes.rda, derived in "~/Dropbox/rstudio_projects/Lumar2Development/May2017recast/derive optimal lumar formulationMay2017.R"
load("~/wergdrobo_Staff/ChrisW/newBugModelsMay2017/dataForMelbstreambiotaPackage/taxon.classes.rda")
save(taxon.classes,
     file = "~/Dropbox/Rstudio_projects/lumar2Development/dataForMelbstreambiotaPackage/taxon.classes.rda",
     compress = "gzip")

#3. mwstreams.rda, compiled in SpatialData/Projects/DCIrevSEsubcs/RevSEsubcs.Rmd
mwstreams <- read.csv("~/uomShare/wergSpatial/MWRegion/Vectors/DrainsStreams/DCI2017/streamSubcPreds2017.csv")
mwstreams <- mwstreams[!is.na(mwstreams$segmentno),]
monthlyQ <- as.data.frame(get(load("~/uomShare/wergSpatial/MWRegion/RData/DrainsStreams/agg_runoff_mm_day_subcs_2017.RData")))
# mwstreams$meanAnnQ_mm[mwstreams$subc == "YARR673"]  #below Merri confluence 294
# mwstreams$meanAnnQ_mm[mwstreams$subc == "YARR513"]  #at City  276
# mwstreams$meanAnnQ_mm[mwstreams$subc == "YARR476"]  #below Maribyrnong confluence  224
# yarra673RM <- rollmean(356.25*monthlyQ$YARR673/mwstreams$meanAnnQ_mm[mwstreams$subc == "YARR673"],48)
# yarra513RM <- rollmean(356.25*monthlyQ$YARR513/mwstreams$meanAnnQ_mm[mwstreams$subc == "YARR513"],48)
# yarra476RM <- rollmean(356.25*monthlyQ$YARR476/mwstreams$meanAnnQ_mm[mwstreams$subc == "YARR476"],48)
# plot(monthlyQ$date[-(1:47)], yarra673RM, type= 'l')
# lines(monthlyQ$date[-(1:47)], yarra513RM, type= 'l', col = "blue")
# lines(monthlyQ$date[-(1:47)], yarra476RM, type= 'l', col = "orange")
# abline(h= 0.75, lty = 3)
# abline(h= 0.5, lty = 3)
# identify(monthlyQ$date[-(1:47)], yarra673RM, monthlyQ$date[-(1:47)])
# yarra673RM[c(514,1283)]
# monthlyQ$date[-(1:47)][c(514,1283)]
# #4 years prior to "1946-09-01 AEST" = 75% of long-term average
# #4 years prior to "2010-10-01" = 50% of long-term average
# #reduce allus lists to the 8256 streams used for mapping
dryMeanQ <- 356.25*apply(monthlyQ[514:(514+47),-1], 2, FUN = mean)
vdryMeanQ <- 356.25*apply(monthlyQ[1283:(1283 + 47),-1], 2, FUN = mean)
mwstreams$dryMeanQ <- dryMeanQ[match(mwstreams$subc, names(monthlyQ)[-1])]
mwstreams$vdryMeanQ <- vdryMeanQ[match(mwstreams$subc, names(monthlyQ)[-1])]
#mwstreams[mwstreams$subc %in% "YARR673",]

#revised AF values for 1996, 2006, 2016: done for MARI first - rest to be updated soon
#Mari revised values calculated on Mac ~/Work/data/spatial/MWregion/projects/MarivegMapping/MARIvegMappingFinal.Rmd
mariSubcs <- get(load("~/Dropbox/Rstudio_projects/lumar2Development/May2017recast/mariSubcs_Final.rda"))
mariSubcs$subc <- as.vector(mariSubcs$subc)
mwstreams$AttForest_L35W1000_1996 <- mwstreams$AttForest_L35W1000_2016 <- mwstreams$AttForest_L35W1000
mwstreams$AttForest_L35W1000_1996[match(mariSubcs$subc, mwstreams$subc)] <- mariSubcs$af1996
#There were ~15 subcs that differed significantly from the AttForest_L35W1000 estimate used in model building.
#This should make no difference to the models, but I will leave it as it is for now, and, when the whole region is revised,
#I will create a new 2006 value and retain the old version that was used to build the models.
mwstreams$AttForest_L35W1000_2016[match(mariSubcs$subc, mwstreams$subc)] <- mariSubcs$af2016
mwstreams$nspring <- 0
mwstreams$nriff <- 1
mwstreams$processN <- 0 #0 = lab-sort, 1 = field-sort
mwstreams$SRI_48mth_weighted <- 1 #average antecedent conditions.
save(mwstreams,
     file = "~/wergdrobo_Staff/ChrisW/rstudio_projects/lumar2Scenarios/dataForLumarPackage/mwstreams.rda",
     compress = "xz")
save(mwstreams,
     file = "~/Dropbox/Rstudio_projects/lumar2Development/dataForMelbstreambiotaPackage/mwstreams.rda",
     compress = "xz")

#4. mwStreamsMap.rda, derived in "~/wergdrobo_Staff/ChrisW/rstudio_projects/lumar2Scenarios/compileMWstreamScenarioPredictions.R"
load("~/wergdrobo_Staff/ChrisW/rstudio_projects/lumar2Scenarios/dataForLumarPackage/mwStreamsMap.rda")
save(mwStreamsMap,
     file = "~/wergdrobo_Staff/ChrisW/newBugModelsMay2017/dataForMelbstreambiotaPackage/mwStreamsMap.rda",
     compress = "xz")
save(mwStreamsMap,
     file = "~/Dropbox/Rstudio_projects/lumar2Development/dataForMelbstreambiotaPackage/mwStreamsMap.rda",
     compress = "xz")

#5. mwCoastMap.rda, derived in "~/wergdrobo_Staff/ChrisW/rstudio_projects/lumar2Scenarios/compileMWstreamScenarioPredictions.R"
load("~/wergdrobo_Staff/ChrisW/rstudio_projects/lumar2Scenarios/dataForLumarPackage/mwCoastMap.rda")
save(mwCoastMap,
     file = "~/wergdrobo_Staff/ChrisW/newBugModelsMay2017/dataForMelbstreambiotaPackage/mwCoastMap.rda",
     compress = "xz")
save(mwCoastMap,
     file = "~/Dropbox/Rstudio_projects/lumar2Development/dataForMelbstreambiotaPackage/mwCoastMap.rda",
     compress = "xz")

#6. bestModelsSIGNAL.rda, derived in  "~/wergdrobo_Staff/ChrisW/newBugModelsMay2017/SIGNAL_BRTModels_May2017.R"
load("~/wergdrobo_Staff/ChrisW/newBugModelsMay2017/dataForMelbstreambiotaPackage/bestModelsSIGNAL.rda")
save(bestModelsSIGNAL,
     file = "~/Dropbox/Rstudio_projects/lumar2Development/dataForMelbstreambiotaPackage/bestModelsSIGNAL.rda",
     compress = "xz")

#7. CIs.rda , derived in  "~/wergdrobo_Staff/ChrisW/newBugModelsMay2017/SIGNAL_BRTModels_May2017.R"
load("~/wergdrobo_Staff/ChrisW/newBugModelsMay2017/dataForMelbstreambiotaPackage/CIs.rda")
save(CIs,
     file = "~/Dropbox/Rstudio_projects/lumar2Development/dataForMelbstreambiotaPackage/CIs.rda",
     compress = "gzip")

#8. streamsubcs.allus2017.rda, derived in "~/uomShare/wergSpatial/Projects/DCI_RevisedSEsubcs/RevSEsubcs.Rmd"
load("~/uomShare/wergSpatial/MWRegion/Vectors/DrainsStreams/DCI2017/streamsubcs.allus2017.rda")
save(streamsubcs.allus2017,
     file = "~/wergdrobo_Staff/ChrisW/newBugModelsMay2017/dataForMelbstreambiotaPackage/streamsubcs.allus2017.rda",
     compress = "xz")
save(streamsubcs.allus2017,
     file = "~/Dropbox/Rstudio_projects/lumar2Development/dataForMelbstreambiotaPackage/streamsubcs.allus2017.rda",
     compress = "xz")

#9. sri48moW.rda, derived in "~/wergdrobo_Staff/ChrisW/newBugModelsMay2017/family BRTs May 2017.R"
load("~/wergdrobo_Staff/ChrisW/newBugModelsMay2017/dataForMelbstreambiotaPackage/sri84moW.rda")
save(sri48moW,
        file = "~/Dropbox/Rstudio_projects/lumar2Development/dataForMelbstreambiotaPackage/sri48moW.rda",
        compress = "xz")

#10. bestModelsBugfams.rda, derived in "~/Dropbox/rstudio_projects/Lumar2Development/May2017recast/derive optimal lumar formulationMay2017.R" (commented out lines 39-49)
start <- Sys.time()
load("~/wergdrobo_Staff/ChrisW/newBugModelsMay2017/dataForMelbstreambiotaPackage/bestModelsBugfams.rda")
Sys.time() - start
save(bestModelsBugfams,
     file = "~/Dropbox/Rstudio_projects/lumar2Development/dataForMelbstreambiotaPackage/bestModelsBugfams.rda",
     compress = "xz")

#11. bestModelVerts.rda, derived in "~/Dropbox/rstudio_projects/Lumar2Development/May2017recast/compile vertebrate models.R"
load("~/wergdrobo_Staff/ChrisW/newBugModelsMay2017/dataForMelbstreambiotaPackage/bestModelsVerts.rda")
save(bestModelsVerts,
     file = "~/Dropbox/Rstudio_projects/lumar2Development/dataForMelbstreambiotaPackage/bestModelsVerts.rda",
     compress = "xz")

#12. vertSpp.rda, derived in "~/Dropbox/rstudio_projects/Lumar2Development/May2017recast/compile vertebrate models.R"
load("~/wergdrobo_Staff/ChrisW/newBugModelsMay2017/dataForMelbstreambiotaPackage/vertSpp.rda")
save(vertSpp,
     file = "~/Dropbox/Rstudio_projects/lumar2Development/dataForMelbstreambiotaPackage/vertSpp.rda",
     compress = "gzip")

#Example dataset
load("~/Dropbox/Rstudio_projects/lumar2Development/LSC bug data.RData")
load("~/Dropbox/Rstudio_projects/lumar2Development/lsc.sampprs1.RData")
lsc.sampprs1$subc <- lsc.sampprs1$site
exampleSampprs <- lsc.sampprs1[lsc.sampprs1$sitecode %in% c("BRS0015","OLN0009") & lsc.sampprs1$date < "2009/07/01",
                               c("samppr", "sitecode", "subc", "nriff", "nspring", "date", "process")]
exampleSampprs$sitecode <- substr(exampleSampprs$sitecode,1,3)
names(exampleSampprs)[names(exampleSampprs) == "sitecode"] <- "stream"

exampleBugList <- lsc.bugs.sampprs[lsc.bugs.sampprs$samppr %in% exampleSampprs$samppr,c("samppr","bugcode","count")]
names(exampleBugList)[3] <- "abundance"
exampleBugList <- exampleBugList[order(exampleBugList$samppr),]

source("~/Dropbox/R functions/ct.R")
exampleBugCT <- with(exampleBugList, ct(cols = bugcode, rows = samppr, values = abundance))

library(xlsx)
write.xlsx(exampleSampprs, file = "/home/chris/wergdrobo_Staff/ChrisW/newBugModelsMay2017/dataForMelbstreambiotaPackage/exampleData.xlsx", sheetName = "exampleSampprs", row.names = FALSE)
write.xlsx(exampleBugList, file = "/home/chris/wergdrobo_Staff/ChrisW/newBugModelsMay2017/dataForMelbstreambiotaPackage/exampleData.xlsx", sheetName = "exampleBugList", row.names = FALSE, append = TRUE)
write.xlsx(exampleBugCT, file = "/home/chris/wergdrobo_Staff/ChrisW/newBugModelsMay2017/dataForMelbstreambiotaPackage/exampleData.xlsx", sheetName = "exampleBugCT", row.names = TRUE, append = TRUE)

write.xlsx(exampleSampprs, file = "/home/chris/Dropbox/Rstudio_projects/lumar2Development/dataForMelbstreambiotaPackage/exampleData.xlsx", sheetName = "exampleSampprs", row.names = FALSE)
write.xlsx(exampleBugList, file = "/home/chris/Dropbox/Rstudio_projects/lumar2Development/dataForMelbstreambiotaPackage/exampleData.xlsx", sheetName = "exampleBugList", row.names = FALSE, append = TRUE)
write.xlsx(exampleBugCT, file = "/home/chris/Dropbox/Rstudio_projects/lumar2Development/dataForMelbstreambiotaPackage/exampleData.xlsx", sheetName = "exampleBugCT", row.names = TRUE, append = TRUE)

#Note, I have deleted the help file from data.R for bsampprsPreds. If it is resurrected, then retrieve the helpfile from melbstreambugs
# save(bsampprsPreds,
#      file = "~/wergdrobo_Staff/ChrisW/rstudio_projects/lumar2Scenarios/dataForLumarPackage/bsampprsPreds.rda",
#      compress = "gzip")


