usethis::create_package("melbstreambiota")
#populate data with data files,
#Data compiled with "~/wergdrobo/Staff_Folders/ChrisW/newBugModelsMay2017/compileDataforMelbstreambiotaPackage.R"
#R folders with function files
#use command in data-raw to make bestModelsBugfams.rda, bestModelsSIGNAL.rda,
#bestModelsVerts.rda,and CIs.rda  a system file
#(and remove the original from data into "LUMaR/package")
#put exampleData.xlsx in inst/extdata
#To check it is all running ok
devtools::load_all()
devtools::document()
#to check documentation works, produce a pdf manual (you need to rerun the document() command after each edit of the R files)
#in terminal...
R CMD Rd2pdf /Users/cwalsh/Work/projects/LUMaR/packageBuild/melbstreambiota
#place vignette folder elsewhere to build the package the first time
devtools::check()
devtools::build()
#then check to see if the vignette knits with the package built: if so move the vignette folder back and run check and build again
start <- Sys.time()
install.packages("~/Work/projects/LUMaR/packageBuild/melbstreambiota_0.1.0.tar.gz", repos = NULL, type = "source")
#or from the web:
#install.packages("http://urbanstreams.net/resources/melbstreambiota_0.1.0.tar.gz", repos = NULL, type = "source")
Sys.time() - start  #At least 10 minutes to install
library(melbstreambiota)
