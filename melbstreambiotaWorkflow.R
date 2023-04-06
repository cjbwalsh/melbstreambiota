# devtools::create("melbstreambiota")
#populate data with data files,
#Data compiled with "~/wergdrobo/Staff_Folders/ChrisW/archive/bug_models_using_DCI_subcs/newBugModelsMay2017/compileDataforMelbstreambiotaPackage.R"
#R folders with function files
#use command in data-raw to make bestModelsBugfams.rda, bestModelsSIGNAL.rda,
#bestModelsVerts.rda,and CIs.rda  a system file
#####Regular update of sri48moW.rda.
## 5 August 2020, revised data provided by Nick Bond until August 2018
#load("~/uomShare/wergStaff/ChrisW/rstudio_projects/melbstreambiota/data/sri48moW.rda")
## save record of v1.0 sri48moW
# save(sri48moW, file = "~/uomShare/wergStaff/ChrisW/rstudio_projects/melbstreambiota/data/sri48moW_v1.0.rda")
# new_sri <- get(load("~/uomShare/wergSpatial/MWRegion/RData/Catchments/mw_spei48_weighted_allsubc_late_2019.RData"))
# rm(spei_data_weighted)
# new_sri <- as.data.frame(new_sri)
# new_sri <- new_sri[,c("SITE","Date","SRI")]
# names(new_sri) <- names(sri48moW)
# sri48moW <- new_sri
# #save copy of new sri48moW to uomShare and to local git directory
# save(sri48moW, file = "~/uomShare/wergStaff/ChrisW/rstudio_projects/melbstreambiota/data/sri48moW.rda", compress = "xz")
# save(sri48moW, file = "data/sri48moW.rda", compress = "xz")

#(and remove the original from data into "LUMaR/package")
#put exampleData.xlsx in inst/extdata
#To check it is all running ok
devtools::load_all()
devtools::document()
#to check documentation works, produce a pdf manual (you need to rerun the document() command after each edit of the R files)
#in terminal...
# R CMD Rd2pdf ~/Documents/git/melbstreambiota
#place vignette folder elsewhere to build the package the first time
devtools::check(cran = FALSE)
devtools::build()
#then check to see if the vignette knits with the package built: if so move the vignette folder back and run check and build again
system.time({
install.packages("../melbstreambiota_0.1.2.tar.gz", repos = NULL, type = "source")
})
#or from the web:
#install.packages("http://urbanstreams.net/resources/melbstreambiota_0.1.1.tar.gz", repos = NULL, type = "source")
Sys.time() - start  #At least 10 minutes to install
library(melbstreambiota)


#improvements for v0.1.1
#add a column to bugfams identifying the 59 modelled families. #done as follows
# #Using output of  melbstreambiota::predCurrNHI59(sampprsL)
# model_fams <- names(expTables$predCurr)[-1]
# bugfams$modelled <- 0
# bugfams$modelled[bugfams$bugcode %in% model_fams] <- 1
# bugfams$family <- as.vector(bugfams$family)
# bugfams$bugcode <- as.vector(bugfams$bugcode)
# bugfams$Comment <- as.vector(bugfams$Commment)
# bugfams$note <- as.vector(bugfams$note)
# save(bugfams, file = "~/Documents/git/melbstreambiota/data/bugfams.rda", compress = "xz")
#check if collateObsTable() should be combining QO21 with QO12....
#convert large tables to data.tables
#convert mapping functions to sf
#corrections to error warnings in collateBugSamppr(), collateSampleFP(), collateObsTable()  #done

