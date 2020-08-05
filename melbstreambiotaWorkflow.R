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
# R CMD Rd2pdf /Users/cwalsh/Work/projects/LUMaR/packageBuild/melbstreambiota
#place vignette folder elsewhere to build the package the first time
devtools::check()
devtools::build()
#then check to see if the vignette knits with the package built: if so move the vignette folder back and run check and build again
start <- Sys.time()
install.packages("../melbstreambiota_0.1.1.tar.gz", repos = NULL, type = "source")
#or from the web:
#install.packages("http://urbanstreams.net/resources/melbstreambiota_0.1.1.tar.gz", repos = NULL, type = "source")
Sys.time() - start  #At least 10 minutes to install
library(melbstreambiota)
